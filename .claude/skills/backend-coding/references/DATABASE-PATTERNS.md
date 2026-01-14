# Database Patterns and Optimization

This reference covers database design patterns, query optimization, migrations, connection pooling, and best practices for SQL and NoSQL databases.

## Database Design Patterns

### Entity Relationships

```typescript
// One-to-Many: User has many Posts
// users table
CREATE TABLE users (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  email VARCHAR(255) UNIQUE NOT NULL,
  name VARCHAR(255) NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

// posts table
CREATE TABLE posts (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  title VARCHAR(255) NOT NULL,
  content TEXT,
  author_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  INDEX idx_author_id (author_id)
);

// TypeORM entities
@Entity('users')
export class User {
  @PrimaryGeneratedColumn('uuid')
  id: string;

  @Column({ unique: true })
  email: string;

  @Column()
  name: string;

  @OneToMany(() => Post, post => post.author)
  posts: Post[];

  @CreateDateColumn()
  createdAt: Date;
}

@Entity('posts')
export class Post {
  @PrimaryGeneratedColumn('uuid')
  id: string;

  @Column()
  title: string;

  @Column('text')
  content: string;

  @ManyToOne(() => User, user => user.posts, { onDelete: 'CASCADE' })
  @JoinColumn({ name: 'author_id' })
  author: User;

  @Column({ name: 'author_id' })
  authorId: string;

  @CreateDateColumn()
  createdAt: Date;
}

// Many-to-Many: Posts and Tags
CREATE TABLE tags (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  name VARCHAR(100) UNIQUE NOT NULL
);

CREATE TABLE post_tags (
  post_id UUID REFERENCES posts(id) ON DELETE CASCADE,
  tag_id UUID REFERENCES tags(id) ON DELETE CASCADE,
  PRIMARY KEY (post_id, tag_id),
  INDEX idx_post_id (post_id),
  INDEX idx_tag_id (tag_id)
);

@Entity('tags')
export class Tag {
  @PrimaryGeneratedColumn('uuid')
  id: string;

  @Column({ unique: true })
  name: string;

  @ManyToMany(() => Post, post => post.tags)
  posts: Post[];
}

@Entity('posts')
export class Post {
  // ... other fields

  @ManyToMany(() => Tag, tag => tag.posts)
  @JoinTable({
    name: 'post_tags',
    joinColumn: { name: 'post_id' },
    inverseJoinColumn: { name: 'tag_id' }
  })
  tags: Tag[];
}

// One-to-One: User and Profile
CREATE TABLE profiles (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id UUID UNIQUE NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  bio TEXT,
  avatar_url VARCHAR(500),
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

@Entity('profiles')
export class Profile {
  @PrimaryGeneratedColumn('uuid')
  id: string;

  @OneToOne(() => User, user => user.profile)
  @JoinColumn({ name: 'user_id' })
  user: User;

  @Column('text', { nullable: true })
  bio: string;

  @Column({ name: 'avatar_url', nullable: true })
  avatarUrl: string;
}

@Entity('users')
export class User {
  // ... other fields

  @OneToOne(() => Profile, profile => profile.user)
  profile: Profile;
}
```

### Soft Deletes

```typescript
// With deleted_at column
CREATE TABLE users (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  email VARCHAR(255) UNIQUE NOT NULL,
  name VARCHAR(255) NOT NULL,
  deleted_at TIMESTAMP NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

// TypeORM soft delete
@Entity('users')
export class User {
  @PrimaryGeneratedColumn('uuid')
  id: string;

  @Column({ unique: true })
  email: string;

  @Column()
  name: string;

  @DeleteDateColumn({ name: 'deleted_at' })
  deletedAt?: Date;

  @CreateDateColumn()
  createdAt: Date;
}

// Repository with soft delete support
export class UserRepository {
  private repository: Repository<User>;

  constructor() {
    this.repository = AppDataSource.getRepository(User);
  }

  // Find non-deleted users
  async findAll(): Promise<User[]> {
    return this.repository.find({
      where: { deletedAt: IsNull() }
    });
  }

  // Soft delete
  async softDelete(id: string): Promise<void> {
    await this.repository.softDelete(id);
  }

  // Restore soft-deleted
  async restore(id: string): Promise<void> {
    await this.repository.restore(id);
  }

  // Find including deleted
  async findAllWithDeleted(): Promise<User[]> {
    return this.repository.find({ withDeleted: true });
  }

  // Permanent delete
  async hardDelete(id: string): Promise<void> {
    await this.repository.delete(id);
  }
}
```

### Optimistic Locking

```typescript
// Using version column
@Entity('products')
export class Product {
  @PrimaryGeneratedColumn('uuid')
  id: string;

  @Column()
  name: string;

  @Column('decimal', { precision: 10, scale: 2 })
  price: number;

  @Column('int')
  stock: number;

  @VersionColumn()
  version: number;
}

// Service with optimistic locking
export class ProductService {
  async updateStock(id: string, quantity: number, expectedVersion: number) {
    const result = await this.productRepository
      .createQueryBuilder()
      .update(Product)
      .set({ 
        stock: () => `stock - ${quantity}`,
        version: () => 'version + 1'
      })
      .where('id = :id', { id })
      .andWhere('version = :version', { version: expectedVersion })
      .andWhere('stock >= :quantity', { quantity })
      .execute();

    if (result.affected === 0) {
      throw new ConflictError('Product was modified by another transaction or insufficient stock');
    }
  }
}

// Pessimistic locking
export class ProductService {
  async updateStockWithLock(id: string, quantity: number) {
    const queryRunner = this.dataSource.createQueryRunner();
    await queryRunner.connect();
    await queryRunner.startTransaction();

    try {
      // Lock the row
      const product = await queryRunner.manager
        .createQueryBuilder(Product, 'product')
        .setLock('pessimistic_write')
        .where('product.id = :id', { id })
        .getOne();

      if (!product) {
        throw new NotFoundError('Product not found');
      }

      if (product.stock < quantity) {
        throw new BadRequestError('Insufficient stock');
      }

      product.stock -= quantity;
      await queryRunner.manager.save(product);

      await queryRunner.commitTransaction();
      return product;
    } catch (error) {
      await queryRunner.rollbackTransaction();
      throw error;
    } finally {
      await queryRunner.release();
    }
  }
}
```

## Query Optimization

### Indexing Strategies

```sql
-- Single column index
CREATE INDEX idx_users_email ON users(email);

-- Composite index (order matters!)
CREATE INDEX idx_posts_author_created ON posts(author_id, created_at DESC);

-- Partial index (PostgreSQL)
CREATE INDEX idx_active_users ON users(email) WHERE deleted_at IS NULL;

-- Expression index
CREATE INDEX idx_users_lower_email ON users(LOWER(email));

-- Covering index (includes extra columns)
CREATE INDEX idx_posts_author_covering ON posts(author_id) INCLUDE (title, created_at);

-- Full-text search index
CREATE INDEX idx_posts_fulltext ON posts USING GIN(to_tsvector('english', title || ' ' || content));

-- Unique index
CREATE UNIQUE INDEX idx_users_email_unique ON users(email) WHERE deleted_at IS NULL;
```

### Query Performance

```typescript
// Bad: N+1 query problem
async function getBadPosts() {
  const posts = await postRepository.find();
  
  // This creates N additional queries!
  for (const post of posts) {
    post.author = await userRepository.findOne({ where: { id: post.authorId } });
  }
  
  return posts;
}

// Good: Use joins
async function getGoodPosts() {
  return postRepository.find({
    relations: ['author']
  });
}

// Better: Select only needed fields
async function getOptimizedPosts() {
  return postRepository
    .createQueryBuilder('post')
    .leftJoinAndSelect('post.author', 'author')
    .select([
      'post.id',
      'post.title',
      'post.createdAt',
      'author.id',
      'author.name'
    ])
    .getMany();
}

// Pagination with proper indexing
async function getPaginatedPosts(page: number, limit: number) {
  // Use cursor-based pagination for better performance
  return postRepository
    .createQueryBuilder('post')
    .where('post.createdAt < :cursor', { cursor: lastCreatedAt })
    .orderBy('post.createdAt', 'DESC')
    .take(limit)
    .getMany();
}

// Aggregation with proper grouping
async function getPostCountsByAuthor() {
  return postRepository
    .createQueryBuilder('post')
    .select('post.authorId', 'authorId')
    .addSelect('COUNT(post.id)', 'count')
    .groupBy('post.authorId')
    .getRawMany();
}

// Using raw queries for complex operations
async function getTopAuthors() {
  return postRepository.query(`
    SELECT 
      u.id,
      u.name,
      COUNT(p.id) as post_count,
      AVG(p.views) as avg_views
    FROM users u
    LEFT JOIN posts p ON u.id = p.author_id
    WHERE u.deleted_at IS NULL
    GROUP BY u.id, u.name
    HAVING COUNT(p.id) > 10
    ORDER BY post_count DESC
    LIMIT 10
  `);
}
```

### Database Connection Pooling

```typescript
// TypeORM connection with pooling
import { DataSource } from 'typeorm';

export const AppDataSource = new DataSource({
  type: 'postgres',
  host: process.env.DB_HOST || 'localhost',
  port: parseInt(process.env.DB_PORT || '5432'),
  username: process.env.DB_USER,
  password: process.env.DB_PASSWORD,
  database: process.env.DB_NAME,
  
  // Connection pool settings
  extra: {
    max: 20, // Maximum connections in pool
    min: 5,  // Minimum connections
    idleTimeoutMillis: 30000, // Close idle connections after 30s
    connectionTimeoutMillis: 2000, // Timeout for acquiring connection
    
    // Statement timeout (PostgreSQL)
    statement_timeout: 10000, // 10 seconds
    
    // For better performance
    keepAlive: true,
    keepAliveInitialDelayMillis: 10000
  },
  
  // Logging
  logging: process.env.NODE_ENV === 'development',
  
  // Synchronize (never use in production!)
  synchronize: false,
  
  // Migrations
  migrations: ['src/migrations/*.ts'],
  
  entities: ['src/entities/*.entity.ts']
});

// Prisma connection pooling
// prisma/schema.prisma
datasource db {
  provider = "postgresql"
  url      = env("DATABASE_URL")
  
  // Connection pool settings
  connectionLimit = 20
}

// Connection string with pooling
// DATABASE_URL="postgresql://user:password@localhost:5432/db?connection_limit=20&pool_timeout=10"

// Node.js pg pool
import { Pool } from 'pg';

const pool = new Pool({
  host: process.env.DB_HOST,
  port: parseInt(process.env.DB_PORT || '5432'),
  user: process.env.DB_USER,
  password: process.env.DB_PASSWORD,
  database: process.env.DB_NAME,
  
  max: 20, // Maximum pool size
  min: 5,  // Minimum pool size
  idleTimeoutMillis: 30000,
  connectionTimeoutMillis: 2000,
  
  // Health check
  keepAlive: true,
  keepAliveInitialDelayMillis: 10000
});

// Graceful shutdown
process.on('SIGTERM', async () => {
  await pool.end();
  process.exit(0);
});

// Using the pool
export async function query(text: string, params?: any[]) {
  const start = Date.now();
  const result = await pool.query(text, params);
  const duration = Date.now() - start;
  
  console.log('Executed query', { text, duration, rows: result.rowCount });
  return result;
}
```

## Database Migrations

### TypeORM Migrations

```typescript
// Generate migration
// npm run typeorm migration:generate -- -n CreateUsersTable

// src/migrations/1704067200000-CreateUsersTable.ts
import { MigrationInterface, QueryRunner, Table, TableIndex } from 'typeorm';

export class CreateUsersTable1704067200000 implements MigrationInterface {
  public async up(queryRunner: QueryRunner): Promise<void> {
    await queryRunner.createTable(
      new Table({
        name: 'users',
        columns: [
          {
            name: 'id',
            type: 'uuid',
            isPrimary: true,
            generationStrategy: 'uuid',
            default: 'gen_random_uuid()'
          },
          {
            name: 'email',
            type: 'varchar',
            length: '255',
            isUnique: true,
            isNullable: false
          },
          {
            name: 'name',
            type: 'varchar',
            length: '255',
            isNullable: false
          },
          {
            name: 'password',
            type: 'varchar',
            length: '255',
            isNullable: false
          },
          {
            name: 'role',
            type: 'enum',
            enum: ['user', 'admin'],
            default: "'user'"
          },
          {
            name: 'created_at',
            type: 'timestamp',
            default: 'CURRENT_TIMESTAMP'
          },
          {
            name: 'updated_at',
            type: 'timestamp',
            default: 'CURRENT_TIMESTAMP'
          }
        ]
      }),
      true
    );

    // Create index
    await queryRunner.createIndex(
      'users',
      new TableIndex({
        name: 'idx_users_email',
        columnNames: ['email']
      })
    );
  }

  public async down(queryRunner: QueryRunner): Promise<void> {
    await queryRunner.dropIndex('users', 'idx_users_email');
    await queryRunner.dropTable('users');
  }
}

// Add column migration
export class AddDeletedAtToUsers1704067300000 implements MigrationInterface {
  public async up(queryRunner: QueryRunner): Promise<void> {
    await queryRunner.addColumn(
      'users',
      new TableColumn({
        name: 'deleted_at',
        type: 'timestamp',
        isNullable: true
      })
    );

    await queryRunner.createIndex(
      'users',
      new TableIndex({
        name: 'idx_users_deleted_at',
        columnNames: ['deleted_at']
      })
    );
  }

  public async down(queryRunner: QueryRunner): Promise<void> {
    await queryRunner.dropIndex('users', 'idx_users_deleted_at');
    await queryRunner.dropColumn('users', 'deleted_at');
  }
}

// Data migration
export class MigrateUserRoles1704067400000 implements MigrationInterface {
  public async up(queryRunner: QueryRunner): Promise<void> {
    // Update existing users with default role
    await queryRunner.query(`
      UPDATE users 
      SET role = 'user' 
      WHERE role IS NULL
    `);
  }

  public async down(queryRunner: QueryRunner): Promise<void> {
    // Revert changes if needed
  }
}
```

### Prisma Migrations

```prisma
// prisma/schema.prisma
model User {
  id        String   @id @default(uuid())
  email     String   @unique
  name      String
  password  String
  role      Role     @default(USER)
  posts     Post[]
  createdAt DateTime @default(now()) @map("created_at")
  updatedAt DateTime @updatedAt @map("updated_at")
  deletedAt DateTime? @map("deleted_at")

  @@index([email])
  @@index([deletedAt])
  @@map("users")
}

model Post {
  id        String   @id @default(uuid())
  title     String
  content   String   @db.Text
  published Boolean  @default(false)
  authorId  String   @map("author_id")
  author    User     @relation(fields: [authorId], references: [id], onDelete: Cascade)
  tags      Tag[]
  createdAt DateTime @default(now()) @map("created_at")
  updatedAt DateTime @updatedAt @map("updated_at")

  @@index([authorId, createdAt])
  @@map("posts")
}

model Tag {
  id    String @id @default(uuid())
  name  String @unique
  posts Post[]

  @@map("tags")
}

enum Role {
  USER
  ADMIN
}
```

```bash
# Create migration
npx prisma migrate dev --name add_deleted_at

# Apply migrations
npx prisma migrate deploy

# Reset database (dev only)
npx prisma migrate reset

# Generate Prisma Client
npx prisma generate
```

## NoSQL Patterns

### MongoDB Patterns

```typescript
// Mongoose schemas
import mongoose, { Schema, Document } from 'mongoose';

// Embedded documents (One-to-Few)
interface IComment {
  author: string;
  text: string;
  createdAt: Date;
}

interface IPost extends Document {
  title: string;
  content: string;
  author: mongoose.Types.ObjectId;
  comments: IComment[]; // Embedded
  createdAt: Date;
  updatedAt: Date;
}

const CommentSchema = new Schema({
  author: { type: String, required: true },
  text: { type: String, required: true },
  createdAt: { type: Date, default: Date.now }
});

const PostSchema = new Schema({
  title: { type: String, required: true, index: true },
  content: { type: String, required: true },
  author: { type: Schema.Types.ObjectId, ref: 'User', required: true, index: true },
  comments: [CommentSchema], // Embed comments
  createdAt: { type: Date, default: Date.now, index: true },
  updatedAt: { type: Date, default: Date.now }
});

// Compound index
PostSchema.index({ author: 1, createdAt: -1 });

// Text index for full-text search
PostSchema.index({ title: 'text', content: 'text' });

export const Post = mongoose.model<IPost>('Post', PostSchema);

// Referenced documents (One-to-Many)
interface IUser extends Document {
  email: string;
  name: string;
  // Posts are stored separately, referenced by user ID
}

const UserSchema = new Schema({
  email: { type: String, required: true, unique: true, index: true },
  name: { type: String, required: true },
  createdAt: { type: Date, default: Date.now }
});

export const User = mongoose.model<IUser>('User', UserSchema);

// Query with population
async function getPostsWithAuthor() {
  return Post.find()
    .populate('author', 'name email') // Populate author field
    .select('title content createdAt') // Select specific fields
    .sort({ createdAt: -1 })
    .limit(10)
    .lean(); // Return plain JavaScript objects (better performance)
}

// Aggregation pipeline
async function getPostStats() {
  return Post.aggregate([
    // Match active posts
    { $match: { published: true } },
    
    // Group by author
    {
      $group: {
        _id: '$author',
        postCount: { $sum: 1 },
        avgComments: { $avg: { $size: '$comments' } },
        totalViews: { $sum: '$views' }
      }
    },
    
    // Lookup author details
    {
      $lookup: {
        from: 'users',
        localField: '_id',
        foreignField: '_id',
        as: 'author'
      }
    },
    
    // Unwind author array
    { $unwind: '$author' },
    
    // Project final shape
    {
      $project: {
        _id: 0,
        authorId: '$_id',
        authorName: '$author.name',
        postCount: 1,
        avgComments: { $round: ['$avgComments', 2] },
        totalViews: 1
      }
    },
    
    // Sort by post count
    { $sort: { postCount: -1 } },
    
    // Limit results
    { $limit: 10 }
  ]);
}

// Bucket pattern (for large arrays)
// Instead of embedding all comments, use buckets
interface ICommentBucket extends Document {
  postId: mongoose.Types.ObjectId;
  bucket: number;
  comments: IComment[];
  count: number;
}

const CommentBucketSchema = new Schema({
  postId: { type: Schema.Types.ObjectId, ref: 'Post', required: true, index: true },
  bucket: { type: Number, required: true },
  comments: [CommentSchema],
  count: { type: Number, default: 0 }
});

CommentBucketSchema.index({ postId: 1, bucket: 1 }, { unique: true });

export const CommentBucket = mongoose.model<ICommentBucket>('CommentBucket', CommentBucketSchema);

// Add comment with bucketing
async function addComment(postId: string, comment: IComment) {
  const BUCKET_SIZE = 50;
  
  // Try to add to current bucket
  const result = await CommentBucket.findOneAndUpdate(
    {
      postId,
      count: { $lt: BUCKET_SIZE }
    },
    {
      $push: { comments: comment },
      $inc: { count: 1 }
    },
    { 
      sort: { bucket: -1 },
      new: true,
      upsert: true,
      setDefaultsOnInsert: true
    }
  );

  return result;
}
```

### Redis Patterns

```typescript
import Redis from 'ioredis';

export class RedisService {
  private redis: Redis;

  constructor() {
    this.redis = new Redis({
      host: process.env.REDIS_HOST || 'localhost',
      port: parseInt(process.env.REDIS_PORT || '6379'),
      password: process.env.REDIS_PASSWORD,
      retryStrategy: (times) => {
        const delay = Math.min(times * 50, 2000);
        return delay;
      }
    });
  }

  // String operations
  async set(key: string, value: string, ttl?: number): Promise<void> {
    if (ttl) {
      await this.redis.setex(key, ttl, value);
    } else {
      await this.redis.set(key, value);
    }
  }

  async get(key: string): Promise<string | null> {
    return this.redis.get(key);
  }

  // Hash operations (for objects)
  async hset(key: string, field: string, value: string): Promise<void> {
    await this.redis.hset(key, field, value);
  }

  async hgetall(key: string): Promise<Record<string, string>> {
    return this.redis.hgetall(key);
  }

  // List operations (for queues)
  async lpush(key: string, ...values: string[]): Promise<void> {
    await this.redis.lpush(key, ...values);
  }

  async rpop(key: string): Promise<string | null> {
    return this.redis.rpop(key);
  }

  // Set operations (for unique items)
  async sadd(key: string, ...members: string[]): Promise<void> {
    await this.redis.sadd(key, ...members);
  }

  async smembers(key: string): Promise<string[]> {
    return this.redis.smembers(key);
  }

  // Sorted set operations (for leaderboards, time-series)
  async zadd(key: string, score: number, member: string): Promise<void> {
    await this.redis.zadd(key, score, member);
  }

  async zrange(key: string, start: number, stop: number): Promise<string[]> {
    return this.redis.zrange(key, start, stop);
  }

  async zrevrange(key: string, start: number, stop: number): Promise<string[]> {
    return this.redis.zrevrange(key, start, stop);
  }

  // Pub/Sub
  async publish(channel: string, message: string): Promise<void> {
    await this.redis.publish(channel, message);
  }

  subscribe(channel: string, callback: (message: string) => void): void {
    const subscriber = this.redis.duplicate();
    subscriber.subscribe(channel);
    subscriber.on('message', (ch, message) => {
      if (ch === channel) {
        callback(message);
      }
    });
  }

  // Distributed lock
  async acquireLock(
    key: string,
    ttl: number = 10000
  ): Promise<string | null> {
    const lockId = Math.random().toString(36);
    const result = await this.redis.set(key, lockId, 'PX', ttl, 'NX');
    return result === 'OK' ? lockId : null;
  }

  async releaseLock(key: string, lockId: string): Promise<boolean> {
    const script = `
      if redis.call("get", KEYS[1]) == ARGV[1] then
        return redis.call("del", KEYS[1])
      else
        return 0
      end
    `;
    
    const result = await this.redis.eval(script, 1, key, lockId);
    return result === 1;
  }
}

// Usage examples
const redis = new RedisService();

// Cache user data
await redis.set(`user:${userId}`, JSON.stringify(user), 3600);

// Session storage
await redis.hset(`session:${sessionId}`, 'userId', userId);
await redis.hset(`session:${sessionId}`, 'createdAt', Date.now().toString());

// Job queue
await redis.lpush('jobs:email', JSON.stringify({ to: 'user@example.com', subject: 'Welcome' }));

// Leaderboard
await redis.zadd('leaderboard', score, userId);
const topUsers = await redis.zrevrange('leaderboard', 0, 9); // Top 10

// Distributed lock for critical section
const lockId = await redis.acquireLock('lock:update:user:123', 5000);
if (lockId) {
  try {
    // Critical section
    await updateUser();
  } finally {
    await redis.releaseLock('lock:update:user:123', lockId);
  }
}
```

This reference provides comprehensive database design patterns and optimization techniques for both SQL and NoSQL databases.
