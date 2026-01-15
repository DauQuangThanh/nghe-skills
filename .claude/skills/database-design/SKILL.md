---
name: database-design
description: Designs comprehensive database schemas including relational and NoSQL models, normalization, indexing strategies, relationship modeling, data types, constraints, and performance optimization. Covers entity-relationship diagrams, schema migrations, partitioning, and best practices for PostgreSQL, MySQL, MongoDB, and other databases. Use when designing databases, creating schemas, modeling data, optimizing queries, or when users mention "database design", "schema design", "data modeling", "ERD", "normalization", "indexing", or "database architecture".
---


# Database Design

## Overview

Designs comprehensive database schemas including relational and NoSQL models, normalization, indexing strategies, relationship modeling, data types, constraints, and performance optimization.


## Core Capabilities

1. **Schema Design** - Relational and NoSQL data modeling
2. **Normalization** - 1NF, 2NF, 3NF, BCNF design
3. **Relationship Modeling** - One-to-one, one-to-many, many-to-many
4. **Indexing Strategy** - Performance optimization
5. **Constraints** - Primary keys, foreign keys, unique, check
6. **Data Types** - Appropriate type selection


## Quick Start

**Basic E-commerce Schema:**

```sql
-- Users table
CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  email VARCHAR(255) UNIQUE NOT NULL,
  password_hash VARCHAR(255) NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Products table
CREATE TABLE products (
  id SERIAL PRIMARY KEY,
  name VARCHAR(255) NOT NULL,
  description TEXT,
  price DECIMAL(10,2) NOT NULL,
  stock_quantity INTEGER DEFAULT 0,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  INDEX idx_name (name)
);

-- Orders table
CREATE TABLE orders (
  id SERIAL PRIMARY KEY,
  user_id INTEGER REFERENCES users(id),
  total_amount DECIMAL(10,2) NOT NULL,
  status VARCHAR(50) DEFAULT 'pending',
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  INDEX idx_user_created (user_id, created_at)
);
```


## Critical Tips

1. **Normalize appropriately** - Balance normalization with performance
2. **Index strategically** - Cover common queries, avoid over-indexing
3. **Use constraints** - Enforce data integrity at database level
4. **Choose right types** - Optimize storage and performance
5. **Plan for growth** - Partitioning, sharding strategies

## Detailed Topics

Load reference files based on specific needs:

- **Advanced Design Patterns**: See [advanced-design-patterns.md](references/advanced-design-patterns.md) when:
  - Working with related functionality
  - Need specific patterns or examples
  - Require detailed guidance

- **Common Anti Patterns To Avoid**: See [common-anti-patterns-to-avoid.md](references/common-anti-patterns-to-avoid.md) when:
  - Working with related functionality
  - Need specific patterns or examples
  - Require detailed guidance

- **Database Design Checklist**: See [database-design-checklist.md](references/database-design-checklist.md) when:
  - Working with related functionality
  - Need specific patterns or examples
  - Require detailed guidance

- **Database Design Workflow**: See [database-design-workflow.md](references/database-design-workflow.md) when:
  - Working with related functionality
  - Need specific patterns or examples
  - Require detailed guidance

- **Nosql Database Design**: See [nosql-database-design.md](references/nosql-database-design.md) when:
  - Working with related functionality
  - Need specific patterns or examples
  - Require detailed guidance

- **Notes**: See [notes.md](references/notes.md) when:
  - Working with related functionality
  - Need specific patterns or examples
  - Require detailed guidance

- **Overview**: See [overview.md](references/overview.md) when:
  - Working with related functionality
  - Need specific patterns or examples
  - Require detailed guidance

- **Performance Optimization**: See [performance-optimization.md](references/performance-optimization.md) when:
  - Working with related functionality
  - Need specific patterns or examples
  - Require detailed guidance

- **Schema Migration Best Practices**: See [schema-migration-best-practices.md](references/schema-migration-best-practices.md) when:
  - Working with related functionality
  - Need specific patterns or examples
  - Require detailed guidance

