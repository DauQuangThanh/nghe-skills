---
name: backend-coding
description: Expert backend development guidance covering Node.js, Python, Java, Go, API design (REST/GraphQL/gRPC), database patterns, authentication, caching, message queues, microservices, and testing. Produces production-ready, scalable, and secure backend code with industry best practices. Use when building APIs, implementing business logic, designing data models, integrating services, or when users mention backend development, server-side code, APIs, databases, or microservices.
license: MIT
metadata:
  author: Dau Quang Thanh
  version: "1.0"
  category: development
---

# Backend Coding Skill

This skill provides expert guidance for modern backend development, covering frameworks, architectural patterns, best practices, and implementation techniques for building production-ready server-side applications.

## Core Capabilities

When activated, this skill enables you to:

1. **API Development**
   - RESTful API design and implementation
   - GraphQL schemas and resolvers
   - gRPC services
   - API versioning and documentation
   - Rate limiting and throttling

2. **Framework Expertise**
   - Node.js (Express, NestJS, Fastify)
   - Python (Django, Flask, FastAPI)
   - Java (Spring Boot)
   - Go (Gin, Echo, but prefer no-framework approach for Golang)

3. **Database Integration**
   - SQL databases (PostgreSQL, MySQL)
   - NoSQL databases (MongoDB, Redis)
   - ORMs and query builders
   - Database migrations
   - Query optimization

4. **Authentication & Authorization**
   - JWT and session management
   - OAuth 2.0 and OpenID Connect
   - Role-based access control (RBAC)
   - API key management
   - Security best practices

5. **Microservices Architecture**
   - Service communication patterns
   - API gateways
   - Service discovery
   - Circuit breakers
   - Distributed tracing

6. **Caching & Performance**
   - Redis caching strategies
   - Cache invalidation patterns
   - Query optimization
   - Database indexing
   - Load balancing

7. **Message Queues & Events**
   - RabbitMQ, Kafka, Redis Pub/Sub
   - Event-driven architecture
   - Background jobs
   - Saga patterns

8. **Testing & Quality**
   - Unit testing
   - Integration testing
   - API testing
   - Test-driven development
   - Mocking and fixtures

## Node.js Development

### Express.js REST API

**Modern Express API with TypeScript**:
```typescript
// src/app.ts
import express, { Request, Response, NextFunction } from 'express';
import helmet from 'helmet';
import cors from 'cors';
import compression from 'compression';
import rateLimit from 'express-rate-limit';
import { userRouter } from './routes/users';
import { errorHandler } from './middleware/errorHandler';
import { requestLogger } from './middleware/logger';
import { authenticate } from './middleware/auth';

const app = express();

// Security middleware
app.use(helmet());
app.use(cors({
  origin: process.env.ALLOWED_ORIGINS?.split(',') || ['http://localhost:3000'],
  credentials: true
}));

// Rate limiting
const limiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100, // Limit each IP to 100 requests per windowMs
  message: 'Too many requests from this IP, please try again later.'
});
app.use('/api/', limiter);

// Body parsing
app.use(express.json({ limit: '10mb' }));
app.use(express.urlencoded({ extended: true }));
app.use(compression());

// Request logging
app.use(requestLogger);

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'ok', timestamp: new Date().toISOString() });
});

// Routes
app.use('/api/users', authenticate, userRouter);

// 404 handler
app.use((req, res) => {
  res.status(404).json({ error: 'Route not found' });
});

// Error handler (must be last)
app.use(errorHandler);

export default app;

// src/server.ts
import app from './app';
import { connectDatabase } from './config/database';
import { logger } from './utils/logger';

const PORT = process.env.PORT || 3000;

async function startServer() {
  try {
    await connectDatabase();
    logger.info('Database connected successfully');

    app.listen(PORT, () => {
      logger.info(`Server running on port ${PORT}`);
    });
  } catch (error) {
    logger.error('Failed to start server:', error);
    process.exit(1);
  }
}

// Graceful shutdown
process.on('SIGTERM', async () => {
  logger.info('SIGTERM received, shutting down gracefully');
  process.exit(0);
});

startServer();
```

**Controllers and Services Pattern**:
```typescript
// src/types/user.ts
export interface User {
  id: string;
  email: string;
  name: string;
  role: 'user' | 'admin';
  createdAt: Date;
  updatedAt: Date;
}

export interface CreateUserDto {
  email: string;
  name: string;
  password: string;
}

export interface UpdateUserDto {
  name?: string;
  email?: string;
}

// src/services/user.service.ts
import { User, CreateUserDto, UpdateUserDto } from '../types/user';
import { UserRepository } from '../repositories/user.repository';
import { hashPassword } from '../utils/crypto';
import { ApiError } from '../utils/errors';

export class UserService {
  constructor(private userRepository: UserRepository) {}

  async createUser(data: CreateUserDto): Promise<User> {
    // Check if user exists
    const existing = await this.userRepository.findByEmail(data.email);
    if (existing) {
      throw new ApiError(409, 'User with this email already exists');
    }

    // Hash password
    const hashedPassword = await hashPassword(data.password);

    // Create user
    const user = await this.userRepository.create({
      ...data,
      password: hashedPassword
    });

    return user;
  }

  async getUserById(id: string): Promise<User> {
    const user = await this.userRepository.findById(id);
    if (!user) {
      throw new ApiError(404, 'User not found');
    }
    return user;
  }

  async updateUser(id: string, data: UpdateUserDto): Promise<User> {
    const user = await this.getUserById(id);

    // Check email uniqueness if changing
    if (data.email && data.email !== user.email) {
      const existing = await this.userRepository.findByEmail(data.email);
      if (existing) {
        throw new ApiError(409, 'Email already in use');
      }
    }

    const updated = await this.userRepository.update(id, data);
    return updated;
  }

  async deleteUser(id: string): Promise<void> {
    await this.getUserById(id); // Ensure exists
    await this.userRepository.delete(id);
  }

  async listUsers(page = 1, limit = 10): Promise<{ users: User[]; total: number }> {
    const offset = (page - 1) * limit;
    const [users, total] = await Promise.all([
      this.userRepository.findMany({ offset, limit }),
      this.userRepository.count()
    ]);
    return { users, total };
  }
}

// src/controllers/user.controller.ts
import { Request, Response, NextFunction } from 'express';
import { UserService } from '../services/user.service';
import { validateCreateUser, validateUpdateUser } from '../validators/user';

export class UserController {
  constructor(private userService: UserService) {}

  createUser = async (req: Request, res: Response, next: NextFunction) => {
    try {
      const validation = validateCreateUser(req.body);
      if (!validation.success) {
        return res.status(400).json({ errors: validation.errors });
      }

      const user = await this.userService.createUser(validation.data);
      res.status(201).json({ data: user });
    } catch (error) {
      next(error);
    }
  };

  getUser = async (req: Request, res: Response, next: NextFunction) => {
    try {
      const user = await this.userService.getUserById(req.params.id);
      res.json({ data: user });
    } catch (error) {
      next(error);
    }
  };

  updateUser = async (req: Request, res: Response, next: NextFunction) => {
    try {
      const validation = validateUpdateUser(req.body);
      if (!validation.success) {
        return res.status(400).json({ errors: validation.errors });
      }

      const user = await this.userService.updateUser(req.params.id, validation.data);
      res.json({ data: user });
    } catch (error) {
      next(error);
    }
  };

  deleteUser = async (req: Request, res: Response, next: NextFunction) => {
    try {
      await this.userService.deleteUser(req.params.id);
      res.status(204).send();
    } catch (error) {
      next(error);
    }
  };

  listUsers = async (req: Request, res: Response, next: NextFunction) => {
    try {
      const page = parseInt(req.query.page as string) || 1;
      const limit = parseInt(req.query.limit as string) || 10;

      const result = await this.userService.listUsers(page, limit);
      
      res.json({
        data: result.users,
        meta: {
          page,
          limit,
          total: result.total,
          totalPages: Math.ceil(result.total / limit)
        }
      });
    } catch (error) {
      next(error);
    }
  };
}

// src/routes/users.ts
import { Router } from 'express';
import { UserController } from '../controllers/user.controller';
import { UserService } from '../services/user.service';
import { UserRepository } from '../repositories/user.repository';
import { authorize } from '../middleware/auth';

const router = Router();
const userRepository = new UserRepository();
const userService = new UserService(userRepository);
const userController = new UserController(userService);

router.post('/', userController.createUser);
router.get('/', userController.listUsers);
router.get('/:id', userController.getUser);
router.patch('/:id', userController.updateUser);
router.delete('/:id', authorize(['admin']), userController.deleteUser);

export { router as userRouter };
```

**Repository Pattern with TypeORM**:
```typescript
// src/entities/User.entity.ts
import { Entity, PrimaryGeneratedColumn, Column, CreateDateColumn, UpdateDateColumn } from 'typeorm';

@Entity('users')
export class UserEntity {
  @PrimaryGeneratedColumn('uuid')
  id: string;

  @Column({ unique: true })
  email: string;

  @Column()
  name: string;

  @Column()
  password: string;

  @Column({ type: 'enum', enum: ['user', 'admin'], default: 'user' })
  role: 'user' | 'admin';

  @CreateDateColumn()
  createdAt: Date;

  @UpdateDateColumn()
  updatedAt: Date;
}

// src/repositories/user.repository.ts
import { Repository } from 'typeorm';
import { AppDataSource } from '../config/database';
import { UserEntity } from '../entities/User.entity';
import { User, CreateUserDto, UpdateUserDto } from '../types/user';

export class UserRepository {
  private repository: Repository<UserEntity>;

  constructor() {
    this.repository = AppDataSource.getRepository(UserEntity);
  }

  async create(data: CreateUserDto & { password: string }): Promise<User> {
    const user = this.repository.create(data);
    await this.repository.save(user);
    return this.toModel(user);
  }

  async findById(id: string): Promise<User | null> {
    const user = await this.repository.findOne({ where: { id } });
    return user ? this.toModel(user) : null;
  }

  async findByEmail(email: string): Promise<User | null> {
    const user = await this.repository.findOne({ where: { email } });
    return user ? this.toModel(user) : null;
  }

  async findMany(options: { offset: number; limit: number }): Promise<User[]> {
    const users = await this.repository.find({
      skip: options.offset,
      take: options.limit,
      order: { createdAt: 'DESC' }
    });
    return users.map(this.toModel);
  }

  async count(): Promise<number> {
    return this.repository.count();
  }

  async update(id: string, data: UpdateUserDto): Promise<User> {
    await this.repository.update(id, data);
    const user = await this.findById(id);
    if (!user) throw new Error('User not found after update');
    return user;
  }

  async delete(id: string): Promise<void> {
    await this.repository.delete(id);
  }

  private toModel(entity: UserEntity): User {
    const { password, ...user } = entity;
    return user as User;
  }
}
```

### NestJS Application

```typescript
// src/users/users.module.ts
import { Module } from '@nestjs/common';
import { TypeOrmModule } from '@nestjs/typeorm';
import { UsersController } from './users.controller';
import { UsersService } from './users.service';
import { User } from './entities/user.entity';

@Module({
  imports: [TypeOrmModule.forFeature([User])],
  controllers: [UsersController],
  providers: [UsersService],
  exports: [UsersService]
})
export class UsersModule {}

// src/users/users.service.ts
import { Injectable, NotFoundException, ConflictException } from '@nestjs/common';
import { InjectRepository } from '@nestjs/typeorm';
import { Repository } from 'typeorm';
import { User } from './entities/user.entity';
import { CreateUserDto } from './dto/create-user.dto';
import { UpdateUserDto } from './dto/update-user.dto';
import * as bcrypt from 'bcrypt';

@Injectable()
export class UsersService {
  constructor(
    @InjectRepository(User)
    private usersRepository: Repository<User>
  ) {}

  async create(createUserDto: CreateUserDto): Promise<User> {
    const existing = await this.usersRepository.findOne({
      where: { email: createUserDto.email }
    });

    if (existing) {
      throw new ConflictException('User with this email already exists');
    }

    const hashedPassword = await bcrypt.hash(createUserDto.password, 10);
    
    const user = this.usersRepository.create({
      ...createUserDto,
      password: hashedPassword
    });

    return this.usersRepository.save(user);
  }

  async findAll(page = 1, limit = 10): Promise<{ data: User[]; total: number }> {
    const [data, total] = await this.usersRepository.findAndCount({
      skip: (page - 1) * limit,
      take: limit,
      order: { createdAt: 'DESC' }
    });

    return { data, total };
  }

  async findOne(id: string): Promise<User> {
    const user = await this.usersRepository.findOne({ where: { id } });
    if (!user) {
      throw new NotFoundException(`User with ID ${id} not found`);
    }
    return user;
  }

  async update(id: string, updateUserDto: UpdateUserDto): Promise<User> {
    const user = await this.findOne(id);

    if (updateUserDto.email && updateUserDto.email !== user.email) {
      const existing = await this.usersRepository.findOne({
        where: { email: updateUserDto.email }
      });
      if (existing) {
        throw new ConflictException('Email already in use');
      }
    }

    Object.assign(user, updateUserDto);
    return this.usersRepository.save(user);
  }

  async remove(id: string): Promise<void> {
    const result = await this.usersRepository.delete(id);
    if (result.affected === 0) {
      throw new NotFoundException(`User with ID ${id} not found`);
    }
  }
}

// src/users/users.controller.ts
import {
  Controller,
  Get,
  Post,
  Body,
  Patch,
  Param,
  Delete,
  Query,
  UseGuards,
  ParseIntPipe,
  ParseUUIDPipe
} from '@nestjs/common';
import { UsersService } from './users.service';
import { CreateUserDto } from './dto/create-user.dto';
import { UpdateUserDto } from './dto/update-user.dto';
import { JwtAuthGuard } from '../auth/guards/jwt-auth.guard';
import { RolesGuard } from '../auth/guards/roles.guard';
import { Roles } from '../auth/decorators/roles.decorator';

@Controller('users')
@UseGuards(JwtAuthGuard, RolesGuard)
export class UsersController {
  constructor(private readonly usersService: UsersService) {}

  @Post()
  @Roles('admin')
  create(@Body() createUserDto: CreateUserDto) {
    return this.usersService.create(createUserDto);
  }

  @Get()
  async findAll(
    @Query('page', ParseIntPipe) page = 1,
    @Query('limit', ParseIntPipe) limit = 10
  ) {
    const { data, total } = await this.usersService.findAll(page, limit);
    return {
      data,
      meta: {
        page,
        limit,
        total,
        totalPages: Math.ceil(total / limit)
      }
    };
  }

  @Get(':id')
  findOne(@Param('id', ParseUUIDPipe) id: string) {
    return this.usersService.findOne(id);
  }

  @Patch(':id')
  update(
    @Param('id', ParseUUIDPipe) id: string,
    @Body() updateUserDto: UpdateUserDto
  ) {
    return this.usersService.update(id, updateUserDto);
  }

  @Delete(':id')
  @Roles('admin')
  remove(@Param('id', ParseUUIDPipe) id: string) {
    return this.usersService.remove(id);
  }
}
```

## Python Development

### FastAPI Application

```python
# main.py
from fastapi import FastAPI, HTTPException, Depends, status
from fastapi.middleware.cors import CORSMiddleware
from fastapi.middleware.gzip import GZipMiddleware
from slowapi import Limiter, _rate_limit_exceeded_handler
from slowapi.util import get_remote_address
from slowapi.errors import RateLimitExceeded
from contextlib import asynccontextmanager
import logging

from app.config import settings
from app.database import engine, Base
from app.routers import users, auth
from app.middleware.logging import LoggingMiddleware

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Rate limiter
limiter = Limiter(key_func=get_remote_address)

@asynccontextmanager
async def lifespan(app: FastAPI):
    # Startup
    logger.info("Starting application...")
    Base.metadata.create_all(bind=engine)
    yield
    # Shutdown
    logger.info("Shutting down application...")

app = FastAPI(
    title="User Management API",
    version="1.0.0",
    lifespan=lifespan
)

# Middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=settings.ALLOWED_ORIGINS,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"]
)
app.add_middleware(GZipMiddleware, minimum_size=1000)
app.add_middleware(LoggingMiddleware)

# Rate limiting
app.state.limiter = limiter
app.add_exception_handler(RateLimitExceeded, _rate_limit_exceeded_handler)

# Routes
@app.get("/health")
async def health_check():
    return {"status": "ok", "timestamp": datetime.utcnow().isoformat()}

app.include_router(auth.router, prefix="/api/auth", tags=["auth"])
app.include_router(users.router, prefix="/api/users", tags=["users"])

# app/models/user.py
from sqlalchemy import Column, String, DateTime, Enum
from sqlalchemy.dialects.postgresql import UUID
from datetime import datetime
import uuid
import enum

from app.database import Base

class UserRole(str, enum.Enum):
    USER = "user"
    ADMIN = "admin"

class User(Base):
    __tablename__ = "users"

    id = Column(UUID(as_uuid=True), primary_key=True, default=uuid.uuid4)
    email = Column(String, unique=True, index=True, nullable=False)
    name = Column(String, nullable=False)
    hashed_password = Column(String, nullable=False)
    role = Column(Enum(UserRole), default=UserRole.USER)
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

# app/schemas/user.py
from pydantic import BaseModel, EmailStr, constr
from datetime import datetime
from uuid import UUID
from typing import Optional

class UserBase(BaseModel):
    email: EmailStr
    name: constr(min_length=1, max_length=100)

class UserCreate(UserBase):
    password: constr(min_length=8, max_length=100)

class UserUpdate(BaseModel):
    name: Optional[constr(min_length=1, max_length=100)] = None
    email: Optional[EmailStr] = None

class UserResponse(UserBase):
    id: UUID
    role: str
    created_at: datetime
    updated_at: datetime

    class Config:
        from_attributes = True

class UserListResponse(BaseModel):
    data: list[UserResponse]
    meta: dict

# app/services/user_service.py
from sqlalchemy.orm import Session
from sqlalchemy.exc import IntegrityError
from fastapi import HTTPException, status
from passlib.context import CryptContext
from uuid import UUID

from app.models.user import User
from app.schemas.user import UserCreate, UserUpdate

pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")

class UserService:
    @staticmethod
    def get_password_hash(password: str) -> str:
        return pwd_context.hash(password)

    @staticmethod
    async def create_user(db: Session, user_data: UserCreate) -> User:
        # Check if user exists
        existing = db.query(User).filter(User.email == user_data.email).first()
        if existing:
            raise HTTPException(
                status_code=status.HTTP_409_CONFLICT,
                detail="User with this email already exists"
            )

        hashed_password = UserService.get_password_hash(user_data.password)
        db_user = User(
            email=user_data.email,
            name=user_data.name,
            hashed_password=hashed_password
        )

        try:
            db.add(db_user)
            db.commit()
            db.refresh(db_user)
            return db_user
        except IntegrityError:
            db.rollback()
            raise HTTPException(
                status_code=status.HTTP_409_CONFLICT,
                detail="User with this email already exists"
            )

    @staticmethod
    async def get_user(db: Session, user_id: UUID) -> User:
        user = db.query(User).filter(User.id == user_id).first()
        if not user:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail=f"User with ID {user_id} not found"
            )
        return user

    @staticmethod
    async def get_users(
        db: Session,
        page: int = 1,
        limit: int = 10
    ) -> tuple[list[User], int]:
        offset = (page - 1) * limit
        users = db.query(User).offset(offset).limit(limit).all()
        total = db.query(User).count()
        return users, total

    @staticmethod
    async def update_user(
        db: Session,
        user_id: UUID,
        user_data: UserUpdate
    ) -> User:
        user = await UserService.get_user(db, user_id)

        if user_data.email and user_data.email != user.email:
            existing = db.query(User).filter(User.email == user_data.email).first()
            if existing:
                raise HTTPException(
                    status_code=status.HTTP_409_CONFLICT,
                    detail="Email already in use"
                )

        for field, value in user_data.model_dump(exclude_unset=True).items():
            setattr(user, field, value)

        db.commit()
        db.refresh(user)
        return user

    @staticmethod
    async def delete_user(db: Session, user_id: UUID) -> None:
        user = await UserService.get_user(db, user_id)
        db.delete(user)
        db.commit()

# app/routers/users.py
from fastapi import APIRouter, Depends, Query
from sqlalchemy.orm import Session
from uuid import UUID
import math

from app.database import get_db
from app.schemas.user import UserCreate, UserUpdate, UserResponse, UserListResponse
from app.services.user_service import UserService
from app.dependencies.auth import get_current_user, require_admin

router = APIRouter()

@router.post("/", response_model=UserResponse, status_code=201)
async def create_user(
    user_data: UserCreate,
    db: Session = Depends(get_db)
):
    return await UserService.create_user(db, user_data)

@router.get("/", response_model=UserListResponse)
async def list_users(
    page: int = Query(1, ge=1),
    limit: int = Query(10, ge=1, le=100),
    db: Session = Depends(get_db),
    current_user = Depends(get_current_user)
):
    users, total = await UserService.get_users(db, page, limit)
    
    return UserListResponse(
        data=[UserResponse.from_orm(user) for user in users],
        meta={
            "page": page,
            "limit": limit,
            "total": total,
            "total_pages": math.ceil(total / limit)
        }
    )

@router.get("/{user_id}", response_model=UserResponse)
async def get_user(
    user_id: UUID,
    db: Session = Depends(get_db),
    current_user = Depends(get_current_user)
):
    return await UserService.get_user(db, user_id)

@router.patch("/{user_id}", response_model=UserResponse)
async def update_user(
    user_id: UUID,
    user_data: UserUpdate,
    db: Session = Depends(get_db),
    current_user = Depends(get_current_user)
):
    return await UserService.update_user(db, user_id, user_data)

@router.delete("/{user_id}", status_code=204)
async def delete_user(
    user_id: UUID,
    db: Session = Depends(get_db),
    current_user = Depends(require_admin)
):
    await UserService.delete_user(db, user_id)
```

## Authentication & Authorization

### JWT Authentication

```typescript
// Node.js JWT implementation
import jwt from 'jsonwebtoken';
import { Request, Response, NextFunction } from 'express';
import { UserRepository } from '../repositories/user.repository';
import { ApiError } from '../utils/errors';

interface JwtPayload {
  userId: string;
  email: string;
  role: string;
}

export class AuthService {
  private readonly JWT_SECRET = process.env.JWT_SECRET!;
  private readonly JWT_EXPIRES_IN = process.env.JWT_EXPIRES_IN || '7d';
  private readonly REFRESH_TOKEN_EXPIRES_IN = '30d';

  constructor(private userRepository: UserRepository) {}

  generateAccessToken(payload: JwtPayload): string {
    return jwt.sign(payload, this.JWT_SECRET, {
      expiresIn: this.JWT_EXPIRES_IN
    });
  }

  generateRefreshToken(userId: string): string {
    return jwt.sign({ userId }, this.JWT_SECRET, {
      expiresIn: this.REFRESH_TOKEN_EXPIRES_IN
    });
  }

  verifyToken(token: string): JwtPayload {
    try {
      return jwt.verify(token, this.JWT_SECRET) as JwtPayload;
    } catch (error) {
      throw new ApiError(401, 'Invalid or expired token');
    }
  }

  async login(email: string, password: string) {
    const user = await this.userRepository.findByEmail(email);
    if (!user || !(await this.verifyPassword(password, user.password))) {
      throw new ApiError(401, 'Invalid credentials');
    }

    const payload: JwtPayload = {
      userId: user.id,
      email: user.email,
      role: user.role
    };

    const accessToken = this.generateAccessToken(payload);
    const refreshToken = this.generateRefreshToken(user.id);

    return {
      accessToken,
      refreshToken,
      user: { id: user.id, email: user.email, name: user.name, role: user.role }
    };
  }

  async refreshAccessToken(refreshToken: string) {
    const payload = this.verifyToken(refreshToken);
    const user = await this.userRepository.findById(payload.userId);

    if (!user) {
      throw new ApiError(401, 'User not found');
    }

    const newPayload: JwtPayload = {
      userId: user.id,
      email: user.email,
      role: user.role
    };

    return {
      accessToken: this.generateAccessToken(newPayload)
    };
  }
}

// Middleware
export const authenticate = async (
  req: Request,
  res: Response,
  next: NextFunction
) => {
  try {
    const authHeader = req.headers.authorization;
    if (!authHeader || !authHeader.startsWith('Bearer ')) {
      throw new ApiError(401, 'No token provided');
    }

    const token = authHeader.substring(7);
    const authService = new AuthService(new UserRepository());
    const payload = authService.verifyToken(token);

    req.user = payload;
    next();
  } catch (error) {
    next(error);
  }
};

export const authorize = (roles: string[]) => {
  return (req: Request, res: Response, next: NextFunction) => {
    if (!req.user) {
      return next(new ApiError(401, 'Authentication required'));
    }

    if (!roles.includes(req.user.role)) {
      return next(new ApiError(403, 'Insufficient permissions'));
    }

    next();
  };
};
```

## Database Patterns

### Transaction Management

```typescript
// TypeORM transactions
import { DataSource } from 'typeorm';

export class TransferService {
  constructor(private dataSource: DataSource) {}

  async transferMoney(fromAccountId: string, toAccountId: string, amount: number) {
    const queryRunner = this.dataSource.createQueryRunner();
    await queryRunner.connect();
    await queryRunner.startTransaction();

    try {
      // Deduct from sender
      await queryRunner.manager.decrement(
        Account,
        { id: fromAccountId },
        'balance',
        amount
      );

      // Add to receiver
      await queryRunner.manager.increment(
        Account,
        { id: toAccountId },
        'balance',
        amount
      );

      // Create transaction record
      const transaction = queryRunner.manager.create(Transaction, {
        fromAccountId,
        toAccountId,
        amount,
        status: 'completed'
      });
      await queryRunner.manager.save(transaction);

      await queryRunner.commitTransaction();
      return transaction;
    } catch (error) {
      await queryRunner.rollbackTransaction();
      throw error;
    } finally {
      await queryRunner.release();
    }
  }
}

// Prisma transactions
import { PrismaClient } from '@prisma/client';

const prisma = new PrismaClient();

async function transferMoney(fromId: string, toId: string, amount: number) {
  return await prisma.$transaction(async (tx) => {
    // Deduct from sender
    const sender = await tx.account.update({
      where: { id: fromId },
      data: { balance: { decrement: amount } }
    });

    if (sender.balance < 0) {
      throw new Error('Insufficient funds');
    }

    // Add to receiver
    await tx.account.update({
      where: { id: toId },
      data: { balance: { increment: amount } }
    });

    // Create transaction record
    return await tx.transaction.create({
      data: {
        fromAccountId: fromId,
        toAccountId: toId,
        amount,
        status: 'completed'
      }
    });
  });
}
```

## Caching Strategies

```typescript
// Redis caching layer
import Redis from 'ioredis';

export class CacheService {
  private redis: Redis;
  private readonly DEFAULT_TTL = 3600; // 1 hour

  constructor() {
    this.redis = new Redis({
      host: process.env.REDIS_HOST || 'localhost',
      port: parseInt(process.env.REDIS_PORT || '6379'),
      password: process.env.REDIS_PASSWORD
    });
  }

  async get<T>(key: string): Promise<T | null> {
    const data = await this.redis.get(key);
    return data ? JSON.parse(data) : null;
  }

  async set(key: string, value: any, ttl = this.DEFAULT_TTL): Promise<void> {
    await this.redis.setex(key, ttl, JSON.stringify(value));
  }

  async del(key: string): Promise<void> {
    await this.redis.del(key);
  }

  async delPattern(pattern: string): Promise<void> {
    const keys = await this.redis.keys(pattern);
    if (keys.length > 0) {
      await this.redis.del(...keys);
    }
  }

  // Cache-aside pattern
  async getOrSet<T>(
    key: string,
    factory: () => Promise<T>,
    ttl = this.DEFAULT_TTL
  ): Promise<T> {
    const cached = await this.get<T>(key);
    if (cached) return cached;

    const data = await factory();
    await this.set(key, data, ttl);
    return data;
  }
}

// Usage with service
export class UserService {
  constructor(
    private userRepository: UserRepository,
    private cacheService: CacheService
  ) {}

  async getUserById(id: string): Promise<User> {
    const cacheKey = `user:${id}`;
    
    return this.cacheService.getOrSet(
      cacheKey,
      () => this.userRepository.findById(id),
      3600 // 1 hour TTL
    );
  }

  async updateUser(id: string, data: UpdateUserDto): Promise<User> {
    const user = await this.userRepository.update(id, data);
    
    // Invalidate cache
    await this.cacheService.del(`user:${id}`);
    
    return user;
  }
}
```

## Best Practices

### Error Handling

```typescript
// Custom error classes
export class ApiError extends Error {
  constructor(
    public statusCode: number,
    message: string,
    public isOperational = true
  ) {
    super(message);
    Object.setPrototypeOf(this, ApiError.prototype);
  }
}

export class ValidationError extends ApiError {
  constructor(message: string, public errors: any[]) {
    super(400, message);
  }
}

// Global error handler
export const errorHandler = (
  err: Error,
  req: Request,
  res: Response,
  next: NextFunction
) => {
  if (err instanceof ApiError) {
    return res.status(err.statusCode).json({
      error: {
        message: err.message,
        ...(err instanceof ValidationError && { errors: err.errors })
      }
    });
  }

  // Log unexpected errors
  console.error('Unexpected error:', err);

  res.status(500).json({
    error: {
      message: 'Internal server error'
    }
  });
};
```

### Input Validation

```typescript
// Zod validation
import { z } from 'zod';

export const createUserSchema = z.object({
  email: z.string().email(),
  name: z.string().min(1).max(100),
  password: z.string().min(8).max(100)
});

export const updateUserSchema = z.object({
  email: z.string().email().optional(),
  name: z.string().min(1).max(100).optional()
});

export function validateCreateUser(data: unknown) {
  try {
    const validated = createUserSchema.parse(data);
    return { success: true, data: validated };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return { success: false, errors: error.errors };
    }
    throw error;
  }
}
```

### Logging

```typescript
// Winston logger
import winston from 'winston';

export const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.errors({ stack: true }),
    winston.format.json()
  ),
  transports: [
    new winston.transports.File({ filename: 'error.log', level: 'error' }),
    new winston.transports.File({ filename: 'combined.log' }),
    ...(process.env.NODE_ENV !== 'production'
      ? [new winston.transports.Console({
          format: winston.format.simple()
        })]
      : [])
  ]
});

// Request logging middleware
export const requestLogger = (req: Request, res: Response, next: NextFunction) => {
  const start = Date.now();

  res.on('finish', () => {
    const duration = Date.now() - start;
    logger.info({
      method: req.method,
      url: req.url,
      status: res.statusCode,
      duration: `${duration}ms`,
      userAgent: req.get('user-agent')
    });
  });

  next();
};
```

## Activation Guidelines

This skill should be activated when:
- Building REST APIs or GraphQL services
- Implementing backend business logic
- Designing database schemas and queries
- Setting up authentication and authorization
- Implementing caching strategies
- Working with message queues
- Building microservices
- Writing backend tests
- Optimizing database performance
- Implementing API rate limiting
- Setting up logging and monitoring
- Handling file uploads
- Integrating third-party services

The skill provides the most value when given clear requirements, preferred tech stack, scalability needs, and security considerations.
