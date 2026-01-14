---
name: database-design
description: Designs comprehensive database schemas including relational and NoSQL models, normalization, indexing strategies, relationship modeling, data types, constraints, and performance optimization. Covers entity-relationship diagrams, schema migrations, partitioning, and best practices for PostgreSQL, MySQL, MongoDB, and other databases. Use when designing databases, creating schemas, modeling data, optimizing queries, or when users mention "database design", "schema design", "data modeling", "ERD", "normalization", "indexing", or "database architecture".
---

# Database Design

## Overview

This skill provides comprehensive guidance for designing robust, scalable, and maintainable database schemas. It covers both relational (SQL) and NoSQL database design, from conceptual modeling to physical implementation.

## Database Design Workflow

### 1. Requirements Analysis

**Gather Requirements:**

```markdown
Business Requirements:
- What data needs to be stored?
- What operations will be performed?
- What queries will be most common?
- What are the performance requirements?
- What is the expected data volume and growth?
- What are compliance/security requirements?

Example: E-commerce Platform
- Store: products, customers, orders, payments, reviews
- Operations: browse products, place orders, process payments, track shipments
- Common queries: product search, order history, inventory checks
- Performance: <100ms for product searches, <50ms for checkout
- Volume: 1M products, 100K customers, 10K orders/day
- Compliance: PCI-DSS for payments, GDPR for customer data
```

**Identify Entities and Attributes:**

```markdown
Entities:
- Customer: id, email, name, address, phone, created_at
- Product: id, name, description, price, category, stock, sku
- Order: id, customer_id, order_date, status, total_amount
- OrderItem: id, order_id, product_id, quantity, price
- Payment: id, order_id, amount, method, status, transaction_id
- Review: id, product_id, customer_id, rating, comment, created_at
```

### 2. Conceptual Data Modeling

**Create Entity-Relationship Diagram (ERD):**

```
Customer (1) ----< (M) Order (1) ----< (M) OrderItem (M) >---- (1) Product
    |                                                                  |
    | (1)                                                              | (1)
    |                                                                  |
    v (M)                                                              v (M)
  Review                                                             Review
```

**Define Relationships:**

```markdown
Customer-Order: One-to-Many
- One customer can have many orders
- One order belongs to one customer

Order-OrderItem: One-to-Many
- One order contains many items
- One item belongs to one order

Product-OrderItem: One-to-Many
- One product can appear in many order items
- One order item references one product

Customer-Review: One-to-Many
Product-Review: One-to-Many
- One customer can write many reviews
- One product can have many reviews
```

**Determine Cardinality and Optionality:**

```markdown
Customer -> Order: 1:M (optional)
- A customer can exist without orders
- An order must have a customer

Order -> OrderItem: 1:M (mandatory)
- An order must have at least one item

Product -> OrderItem: 1:M (optional)
- A product can exist without being ordered

Order -> Payment: 1:1 (mandatory)
- Each order must have a payment
- Each payment belongs to one order
```

### 3. Logical Data Modeling

**Apply Normalization:**

**First Normal Form (1NF):**
- Eliminate repeating groups
- Each cell contains atomic values

```sql
-- Violates 1NF: Multiple values in one column
BAD:
customers (id, name, phone_numbers)
1, 'John', '123-456, 789-012'

-- Compliant with 1NF
GOOD:
customers (id, name)
1, 'John'

customer_phones (id, customer_id, phone_number, phone_type)
1, 1, '123-456', 'mobile'
2, 1, '789-012', 'home'
```

**Second Normal Form (2NF):**
- Must be in 1NF
- Remove partial dependencies (non-key attributes depend on entire primary key)

```sql
-- Violates 2NF: product_name depends only on product_id, not the composite key
BAD:
order_items (order_id, product_id, product_name, quantity, price)
PK: (order_id, product_id)

-- Compliant with 2NF
GOOD:
order_items (order_id, product_id, quantity, price)
PK: (order_id, product_id)

products (product_id, product_name)
PK: product_id
```

**Third Normal Form (3NF):**
- Must be in 2NF
- Remove transitive dependencies (non-key attributes depend on other non-key attributes)

```sql
-- Violates 3NF: city_name depends on zip_code, not on customer_id
BAD:
customers (id, name, zip_code, city_name, state)

-- Compliant with 3NF
GOOD:
customers (id, name, zip_code)
zip_codes (zip_code, city_name, state)
```

**When to Denormalize:**

```markdown
Acceptable Denormalization Scenarios:

1. Read-Heavy Systems
   - Duplicate data to avoid expensive joins
   - Example: Store product name in order_items for order history queries

2. Reporting and Analytics
   - Aggregate tables for dashboard queries
   - Example: daily_sales_summary table

3. Caching Computed Values
   - Store calculated values to avoid repeated computation
   - Example: order.total_amount instead of SUM(order_items.price)

4. Historical Snapshots
   - Preserve data as it was at transaction time
   - Example: Store product price in order_items, not reference products.price
```

### 4. Physical Data Modeling

**Define Tables with Data Types:**

```sql
-- PostgreSQL Example
CREATE TABLE customers (
    id BIGSERIAL PRIMARY KEY,
    email VARCHAR(255) UNIQUE NOT NULL,
    first_name VARCHAR(100) NOT NULL,
    last_name VARCHAR(100) NOT NULL,
    phone VARCHAR(20),
    date_of_birth DATE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    is_active BOOLEAN DEFAULT true,
    
    CONSTRAINT email_format CHECK (email ~* '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}$')
);

CREATE TABLE products (
    id BIGSERIAL PRIMARY KEY,
    sku VARCHAR(50) UNIQUE NOT NULL,
    name VARCHAR(255) NOT NULL,
    description TEXT,
    price DECIMAL(10, 2) NOT NULL CHECK (price >= 0),
    category_id INTEGER REFERENCES categories(id),
    stock_quantity INTEGER DEFAULT 0 CHECK (stock_quantity >= 0),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    
    CONSTRAINT price_positive CHECK (price > 0)
);

CREATE TABLE orders (
    id BIGSERIAL PRIMARY KEY,
    customer_id BIGINT NOT NULL REFERENCES customers(id),
    order_number VARCHAR(50) UNIQUE NOT NULL,
    order_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    status VARCHAR(20) DEFAULT 'pending',
    total_amount DECIMAL(10, 2) NOT NULL,
    shipping_address_id BIGINT REFERENCES addresses(id),
    billing_address_id BIGINT REFERENCES addresses(id),
    
    CONSTRAINT status_values CHECK (status IN ('pending', 'processing', 'shipped', 'delivered', 'cancelled'))
);

CREATE TABLE order_items (
    id BIGSERIAL PRIMARY KEY,
    order_id BIGINT NOT NULL REFERENCES orders(id) ON DELETE CASCADE,
    product_id BIGINT NOT NULL REFERENCES products(id),
    quantity INTEGER NOT NULL CHECK (quantity > 0),
    unit_price DECIMAL(10, 2) NOT NULL,
    subtotal DECIMAL(10, 2) NOT NULL,
    
    UNIQUE (order_id, product_id)
);
```

**Choose Appropriate Data Types:**

```markdown
PostgreSQL/MySQL Data Type Guidelines:

Integers:
- SMALLINT: -32,768 to 32,767 (2 bytes)
- INTEGER: -2B to 2B (4 bytes)
- BIGINT: -9 quintillion to 9 quintillion (8 bytes)
- Use BIGINT for IDs that might grow large

Strings:
- CHAR(n): Fixed length, padded with spaces
- VARCHAR(n): Variable length, up to n characters
- TEXT: Unlimited length (use for descriptions, comments)
- Use VARCHAR for most text fields with reasonable limit

Numbers:
- DECIMAL(p, s): Fixed precision (p=total digits, s=decimal places)
- FLOAT/REAL: 4 bytes, approximate
- DOUBLE: 8 bytes, approximate
- Use DECIMAL for money, FLOAT for scientific data

Dates/Times:
- DATE: Date only (2024-01-14)
- TIME: Time only (14:30:00)
- TIMESTAMP: Date and time with timezone
- INTERVAL: Duration (3 days, 2 hours)

Boolean:
- BOOLEAN: true/false/null

Binary:
- BYTEA (PostgreSQL): Binary data
- BLOB (MySQL): Binary large object
- Use for files, images (consider object storage instead)

JSON:
- JSON: Validates JSON, stores as text
- JSONB (PostgreSQL): Binary JSON, indexable
- Use for flexible schemas, API responses
```

### 5. Indexing Strategy

**Primary Key Indexes:**

```sql
-- Automatically created
CREATE TABLE users (
    id BIGSERIAL PRIMARY KEY,  -- Implicit index on id
    email VARCHAR(255) UNIQUE   -- Implicit index on email
);
```

**Single-Column Indexes:**

```sql
-- For frequently queried columns
CREATE INDEX idx_orders_customer_id ON orders(customer_id);
CREATE INDEX idx_orders_status ON orders(status);
CREATE INDEX idx_products_category ON products(category_id);
CREATE INDEX idx_orders_date ON orders(order_date);
```

**Composite Indexes:**

```sql
-- For queries filtering on multiple columns
-- Order matters: most selective column first
CREATE INDEX idx_orders_customer_date ON orders(customer_id, order_date);
CREATE INDEX idx_products_category_price ON products(category_id, price);

-- Query using leftmost columns can use the index
-- ✅ Uses idx_orders_customer_date:
SELECT * FROM orders WHERE customer_id = 123;
SELECT * FROM orders WHERE customer_id = 123 AND order_date > '2024-01-01';

-- ❌ Cannot use idx_orders_customer_date:
SELECT * FROM orders WHERE order_date > '2024-01-01';
```

**Partial Indexes:**

```sql
-- Index only subset of rows (PostgreSQL)
CREATE INDEX idx_active_customers ON customers(id) WHERE is_active = true;
CREATE INDEX idx_pending_orders ON orders(id) WHERE status = 'pending';
```

**Full-Text Search Indexes:**

```sql
-- PostgreSQL
CREATE INDEX idx_products_search ON products 
USING GIN (to_tsvector('english', name || ' ' || description));

-- Query
SELECT * FROM products 
WHERE to_tsvector('english', name || ' ' || description) 
@@ to_tsquery('english', 'laptop & gaming');

-- MySQL
ALTER TABLE products ADD FULLTEXT INDEX idx_products_fulltext (name, description);

-- Query
SELECT * FROM products 
WHERE MATCH(name, description) AGAINST('gaming laptop' IN NATURAL LANGUAGE MODE);
```

**Covering Indexes:**

```sql
-- Include all columns needed by query
CREATE INDEX idx_orders_customer_covering 
ON orders(customer_id) 
INCLUDE (order_date, total_amount, status);

-- Query can be satisfied entirely from index
SELECT order_date, total_amount, status 
FROM orders 
WHERE customer_id = 123;
```

**Index Guidelines:**

```markdown
When to Add Indexes:
✅ Foreign key columns
✅ Columns in WHERE clauses
✅ Columns in JOIN conditions
✅ Columns in ORDER BY clauses
✅ Columns in GROUP BY clauses

When NOT to Add Indexes:
❌ Small tables (< 1000 rows)
❌ Columns with low cardinality (e.g., gender, boolean)
❌ Columns rarely queried
❌ Tables with high write volume and few reads

Index Maintenance:
- Monitor index usage: pg_stat_user_indexes (PostgreSQL)
- Drop unused indexes
- Rebuild fragmented indexes periodically
- Update statistics: ANALYZE table_name
```

### 6. Constraints and Data Integrity

**Primary Keys:**

```sql
-- Single column
CREATE TABLE customers (
    id BIGSERIAL PRIMARY KEY
);

-- Composite key
CREATE TABLE order_items (
    order_id BIGINT,
    product_id BIGINT,
    PRIMARY KEY (order_id, product_id)
);

-- Natural vs. Surrogate keys
-- Natural: email, sku (business meaningful)
-- Surrogate: id (system generated)
-- Recommendation: Use surrogate keys for primary keys
```

**Foreign Keys:**

```sql
CREATE TABLE orders (
    id BIGSERIAL PRIMARY KEY,
    customer_id BIGINT NOT NULL,
    
    FOREIGN KEY (customer_id) 
        REFERENCES customers(id) 
        ON DELETE RESTRICT      -- Prevent deletion if orders exist
        ON UPDATE CASCADE       -- Update orders if customer ID changes
);

-- Cascade options:
-- CASCADE: Delete/update child rows automatically
-- RESTRICT: Prevent if child rows exist (default)
-- SET NULL: Set foreign key to NULL
-- SET DEFAULT: Set foreign key to default value
-- NO ACTION: Similar to RESTRICT
```

**Unique Constraints:**

```sql
-- Single column
ALTER TABLE customers ADD CONSTRAINT unique_email UNIQUE (email);

-- Multiple columns
ALTER TABLE products ADD CONSTRAINT unique_sku_per_vendor 
    UNIQUE (vendor_id, sku);
```

**Check Constraints:**

```sql
-- Simple validation
ALTER TABLE products ADD CONSTRAINT price_positive 
    CHECK (price > 0);

-- Complex validation
ALTER TABLE orders ADD CONSTRAINT valid_order 
    CHECK (
        (status = 'cancelled' AND cancelled_at IS NOT NULL)
        OR (status != 'cancelled' AND cancelled_at IS NULL)
    );

-- Date range validation
ALTER TABLE promotions ADD CONSTRAINT valid_date_range 
    CHECK (end_date > start_date);
```

**Default Values:**

```sql
CREATE TABLE orders (
    id BIGSERIAL PRIMARY KEY,
    status VARCHAR(20) DEFAULT 'pending',
    order_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    is_paid BOOLEAN DEFAULT false
);
```

## Advanced Design Patterns

### 1. Many-to-Many Relationships

```sql
-- Junction/Bridge table pattern
CREATE TABLE students (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(100)
);

CREATE TABLE courses (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(100)
);

-- Junction table with additional attributes
CREATE TABLE enrollments (
    student_id BIGINT REFERENCES students(id),
    course_id BIGINT REFERENCES courses(id),
    enrolled_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    grade DECIMAL(3, 2),
    
    PRIMARY KEY (student_id, course_id)
);
```

### 2. Inheritance/Polymorphism

**Single Table Inheritance:**

```sql
-- All types in one table with discriminator column
CREATE TABLE users (
    id BIGSERIAL PRIMARY KEY,
    user_type VARCHAR(20) NOT NULL,  -- 'customer', 'admin', 'vendor'
    email VARCHAR(255) UNIQUE NOT NULL,
    name VARCHAR(100),
    
    -- Customer-specific
    shipping_address TEXT,
    
    -- Vendor-specific
    company_name VARCHAR(100),
    tax_id VARCHAR(50),
    
    -- Admin-specific
    access_level INTEGER,
    
    CHECK (user_type IN ('customer', 'admin', 'vendor'))
);

-- Pros: Simple queries, single table
-- Cons: Sparse columns, complex constraints
```

**Class Table Inheritance:**

```sql
-- Base table
CREATE TABLE users (
    id BIGSERIAL PRIMARY KEY,
    email VARCHAR(255) UNIQUE NOT NULL,
    name VARCHAR(100) NOT NULL
);

-- Subtype tables
CREATE TABLE customers (
    user_id BIGINT PRIMARY KEY REFERENCES users(id),
    shipping_address TEXT,
    loyalty_points INTEGER DEFAULT 0
);

CREATE TABLE vendors (
    user_id BIGINT PRIMARY KEY REFERENCES users(id),
    company_name VARCHAR(100) NOT NULL,
    tax_id VARCHAR(50) UNIQUE
);

-- Pros: Normalized, no NULL columns
-- Cons: Requires joins, complex queries
```

### 3. Temporal Data (Historization)

```sql
-- Version tracking approach
CREATE TABLE products (
    id BIGSERIAL,
    version INTEGER,
    name VARCHAR(255) NOT NULL,
    price DECIMAL(10, 2) NOT NULL,
    valid_from TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    valid_to TIMESTAMP,
    is_current BOOLEAN DEFAULT true,
    
    PRIMARY KEY (id, version)
);

-- Query current version
SELECT * FROM products WHERE is_current = true;

-- Query at specific date
SELECT * FROM products 
WHERE '2024-01-01' BETWEEN valid_from AND COALESCE(valid_to, 'infinity');
```

### 4. Soft Deletes

```sql
CREATE TABLE customers (
    id BIGSERIAL PRIMARY KEY,
    email VARCHAR(255) UNIQUE NOT NULL,
    name VARCHAR(100) NOT NULL,
    deleted_at TIMESTAMP,
    
    CONSTRAINT unique_active_email 
        UNIQUE (email) WHERE deleted_at IS NULL
);

-- Query active records
SELECT * FROM customers WHERE deleted_at IS NULL;

-- Soft delete
UPDATE customers SET deleted_at = CURRENT_TIMESTAMP WHERE id = 123;

-- Restore
UPDATE customers SET deleted_at = NULL WHERE id = 123;
```

### 5. Hierarchical Data

**Adjacency List:**

```sql
CREATE TABLE categories (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    parent_id BIGINT REFERENCES categories(id)
);

-- Pros: Simple, easy updates
-- Cons: Recursive queries needed for full tree
```

**Closure Table:**

```sql
CREATE TABLE categories (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL
);

CREATE TABLE category_paths (
    ancestor_id BIGINT REFERENCES categories(id),
    descendant_id BIGINT REFERENCES categories(id),
    depth INTEGER NOT NULL,
    
    PRIMARY KEY (ancestor_id, descendant_id)
);

-- Pros: Fast queries at all levels
-- Cons: More storage, complex updates
```

**Materialized Path:**

```sql
CREATE TABLE categories (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    path VARCHAR(500) NOT NULL  -- e.g., '1.3.7'
);

CREATE INDEX idx_categories_path ON categories USING GIST (path);

-- Pros: Fast queries, easy to find ancestors/descendants
-- Cons: Path updates on move
```

## NoSQL Database Design

### MongoDB Document Design

```javascript
// Embedded Documents (one-to-few)
{
  _id: ObjectId("..."),
  name: "John Doe",
  email: "john@example.com",
  addresses: [
    { type: "home", street: "123 Main St", city: "NYC" },
    { type: "work", street: "456 Office Blvd", city: "NYC" }
  ]
}

// Reference (one-to-many, many-to-many)
// Users collection
{ _id: ObjectId("user123"), name: "John Doe" }

// Orders collection
{
  _id: ObjectId("order456"),
  user_id: ObjectId("user123"),
  items: [
    { product_id: ObjectId("prod789"), quantity: 2 }
  ]
}

// Design Decision Rules:
// Embed when:
// - Data is accessed together
// - One-to-few relationship
// - Child data doesn't change often
// - Bounded growth (won't exceed 16MB limit)

// Reference when:
// - Data accessed separately
// - One-to-many or many-to-many
// - Frequently updated data
// - Unbounded growth
```

## Performance Optimization

### Query Optimization

```sql
-- Use EXPLAIN to analyze queries
EXPLAIN ANALYZE
SELECT c.name, o.order_date, o.total_amount
FROM customers c
JOIN orders o ON c.id = o.customer_id
WHERE o.order_date > '2024-01-01'
  AND c.is_active = true;

-- Optimization techniques:
-- 1. Add indexes on join columns and WHERE conditions
-- 2. Select only needed columns (avoid SELECT *)
-- 3. Use WHERE instead of HAVING when possible
-- 4. Use EXISTS instead of IN for subqueries
-- 5. Avoid functions on indexed columns in WHERE
```

### Partitioning

```sql
-- Range partitioning by date (PostgreSQL)
CREATE TABLE orders (
    id BIGSERIAL,
    order_date DATE NOT NULL,
    customer_id BIGINT,
    total_amount DECIMAL(10, 2)
) PARTITION BY RANGE (order_date);

CREATE TABLE orders_2024_q1 PARTITION OF orders
    FOR VALUES FROM ('2024-01-01') TO ('2024-04-01');

CREATE TABLE orders_2024_q2 PARTITION OF orders
    FOR VALUES FROM ('2024-04-01') TO ('2024-07-01');

-- List partitioning by region
CREATE TABLE customers (
    id BIGSERIAL,
    region VARCHAR(20),
    name VARCHAR(100)
) PARTITION BY LIST (region);

CREATE TABLE customers_us PARTITION OF customers
    FOR VALUES IN ('US', 'CA');

CREATE TABLE customers_eu PARTITION OF customers
    FOR VALUES IN ('UK', 'DE', 'FR');
```

## Schema Migration Best Practices

```sql
-- Always use transactions
BEGIN;

-- 1. Additive changes (safe)
ALTER TABLE customers ADD COLUMN middle_name VARCHAR(100);

-- 2. Create indexes concurrently (PostgreSQL)
CREATE INDEX CONCURRENTLY idx_orders_date ON orders(order_date);

-- 3. Make nullable first, then add NOT NULL
ALTER TABLE products ADD COLUMN brand_id BIGINT;
-- Populate data
UPDATE products SET brand_id = 1 WHERE brand_id IS NULL;
-- Add constraint
ALTER TABLE products ALTER COLUMN brand_id SET NOT NULL;

-- 4. Rename columns/tables
ALTER TABLE customers RENAME COLUMN phone_number TO phone;

-- 5. Drop columns (last resort)
-- First make sure no code references it
ALTER TABLE customers DROP COLUMN old_field;

COMMIT;
```

## Database Design Checklist

```markdown
Design Phase:
- [ ] Requirements gathered and documented
- [ ] Entities and attributes identified
- [ ] Relationships defined with cardinality
- [ ] ERD created and reviewed
- [ ] Normalization applied (at least 3NF)
- [ ] Denormalization justified where needed

Implementation Phase:
- [ ] Tables created with appropriate data types
- [ ] Primary keys defined
- [ ] Foreign keys with referential actions
- [ ] Unique constraints added
- [ ] Check constraints for data validation
- [ ] Default values set where appropriate
- [ ] Indexes created for common queries
- [ ] Partitioning strategy implemented if needed

Testing Phase:
- [ ] Sample data loaded
- [ ] Common queries tested and optimized
- [ ] EXPLAIN ANALYZE run on critical queries
- [ ] Constraint violations tested
- [ ] Concurrent access tested
- [ ] Backup and restore tested

Documentation:
- [ ] ERD documented
- [ ] Table relationships documented
- [ ] Index strategy documented
- [ ] Migration scripts version controlled
- [ ] Sample queries documented
```

## Common Anti-Patterns to Avoid

❌ **Entity-Attribute-Value (EAV)**: Flexible but unqueryable
❌ **Storing Arrays as Strings**: '1,2,3' - use arrays or junction tables
❌ **Premature Optimization**: Don't add indexes until you measure
❌ **UUID as Primary Key**: Can cause index fragmentation (use if needed for distribution)
❌ **Storing Files in Database**: Use object storage (S3, etc.) instead
❌ **Using NULL for Multiple Meanings**: NULL should mean "unknown", not "N/A" or 0
❌ **Over-Normalization**: Don't split tables excessively
❌ **Missing Foreign Keys**: They prevent orphaned records
❌ **No Indexes on Foreign Keys**: Critical for join performance
❌ **Generic Column Names**: Use specific names (user_id, not id)

## Notes

- Start with normalized design, denormalize only when proven necessary
- Index strategically based on actual query patterns
- Use constraints to enforce data integrity at database level
- Document design decisions and their rationale
- Plan for growth and scalability from the start
- Test with realistic data volumes
- Monitor and optimize based on production metrics
