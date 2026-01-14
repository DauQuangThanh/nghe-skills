# Backend Design Review Skill

```yaml
name: backend-design-review
description: >
  Conducts comprehensive backend design reviews covering API design quality, database architecture validation, microservices patterns assessment, integration strategies evaluation, security design review, and scalability analysis. Evaluates API specifications (REST, GraphQL, gRPC), database schemas, service boundaries, authentication/authorization flows, caching strategies, message queues, and deployment architectures. Identifies design flaws, security vulnerabilities, performance bottlenecks, and scalability issues. Produces detailed design review reports with severity-rated findings, architecture diagrams, and implementation recommendations. Use when reviewing backend system designs, validating API specifications, assessing database schemas, evaluating microservices architectures, reviewing integration patterns, or when users mention backend design review, API design validation, database design review, microservices assessment, or backend architecture evaluation.
license: MIT
author: Dau Quang Thanh
version: "1.0"
category: review
```

## Overview

This skill provides expert guidance for conducting thorough backend design reviews, covering all aspects of server-side architecture including API design, database modeling, microservices patterns, integration strategies, security design, and performance optimization. The skill helps identify design issues early in the development lifecycle, ensuring backend designs meet functional requirements, security standards, and scalability needs before implementation.

## Core Capabilities

### 1. API Design Review

- **RESTful API Assessment**: Evaluate REST API design, resource modeling, HTTP method usage, status codes, and versioning strategies
- **GraphQL Schema Review**: Assess schema design, type definitions, query complexity, resolver patterns, and N+1 query prevention
- **gRPC Service Review**: Evaluate protobuf definitions, service boundaries, streaming patterns, and error handling
- **API Documentation Quality**: Review OpenAPI/Swagger specs, API documentation completeness, and example quality
- **API Security Design**: Assess authentication, authorization, rate limiting, input validation, and API security best practices

### 2. Database Design Validation

- **Data Modeling**: Evaluate entity relationships, normalization level, denormalization strategies, and domain model alignment
- **Schema Design**: Review table structures, column types, constraints, indexes, and partitioning strategies
- **Query Patterns**: Assess query efficiency, index usage, N+1 query prevention, and database access patterns
- **Data Integrity**: Validate referential integrity, constraints, triggers, and data validation rules
- **Scalability Considerations**: Review sharding strategies, read replicas, caching layers, and database scaling approaches

### 3. Microservices Architecture Assessment

- **Service Boundaries**: Evaluate service decomposition, bounded contexts, domain-driven design alignment, and service cohesion
- **Communication Patterns**: Review synchronous vs. asynchronous communication, event-driven patterns, and service orchestration
- **Data Management**: Assess database-per-service pattern, eventual consistency, saga patterns, and distributed transactions
- **Service Discovery**: Review service registry, load balancing, client-side vs. server-side discovery
- **Resilience Patterns**: Evaluate circuit breakers, retries, timeouts, bulkheads, and fallback mechanisms

### 4. Integration Architecture Review

- **Integration Patterns**: Assess API integration, message queues, event streaming, webhooks, and batch processing
- **Message Queue Design**: Review queue selection, message schemas, dead letter queues, idempotency, and ordering guarantees
- **Event Streaming**: Evaluate event sourcing, CQRS, event schemas, stream processing, and eventual consistency handling
- **External API Integration**: Review third-party API integration, retry logic, circuit breakers, and API versioning handling
- **Batch Processing**: Assess ETL pipelines, job scheduling, error handling, and batch processing patterns

### 5. Security Architecture Review

- **Authentication Design**: Evaluate authentication mechanisms (JWT, OAuth 2.0, SAML), session management, and token handling
- **Authorization Design**: Assess RBAC, ABAC, permission models, policy enforcement, and authorization boundaries
- **Data Protection**: Review encryption at rest, encryption in transit, secrets management, and sensitive data handling
- **API Security**: Validate input validation, SQL injection prevention, XSS prevention, CSRF protection, and rate limiting
- **Security Monitoring**: Assess audit logging, security event monitoring, anomaly detection, and incident response

## Backend Design Review Process

### Phase 1: Pre-Review Preparation

**Activities:**
1. **Gather Design Documentation**
   - Collect architecture diagrams (C4 model, sequence diagrams)
   - Obtain API specifications (OpenAPI, GraphQL schemas, protobuf definitions)
   - Review database schemas (ERD, schema diagrams)
   - Gather ADRs (Architecture Decision Records)
   - Review requirements and technical specifications

2. **Understand Context**
   - Review functional requirements and business objectives
   - Understand scalability requirements (users, requests/second, data volume)
   - Identify compliance requirements (GDPR, HIPAA, PCI-DSS)
   - Note technical constraints (cloud provider, existing systems, budget)
   - Understand team expertise and organizational structure

3. **Define Review Scope**
   - Identify critical components to review in depth
   - Determine review depth (high-level vs. detailed)
   - Set review priorities based on risk and importance
   - Establish timeline and deliverables
   - Identify key stakeholders

**Deliverables:**
- Review scope document
- Documentation inventory
- Context summary with constraints
- Review timeline and milestones

### Phase 2: API Design Review

**Review Areas:**

**RESTful API Design:**

**Resource Modeling:**
- [ ] Resources represent domain entities clearly
- [ ] Resource hierarchies logical and intuitive
- [ ] Nested resources used appropriately (max 2 levels)
- [ ] Collection and singular resources distinguished
- [ ] Resource naming follows conventions (plural nouns: `/users`, `/orders`)

**HTTP Method Usage:**
- [ ] GET for retrieval (no side effects, idempotent)
- [ ] POST for creation (non-idempotent)
- [ ] PUT for full replacement (idempotent)
- [ ] PATCH for partial updates (idempotent recommended)
- [ ] DELETE for removal (idempotent)
- [ ] Methods used semantically correctly

**Status Codes:**
- [ ] 200 OK for successful GET, PUT, PATCH
- [ ] 201 Created for successful POST with `Location` header
- [ ] 204 No Content for successful DELETE
- [ ] 400 Bad Request for validation errors with error details
- [ ] 401 Unauthorized for authentication failures
- [ ] 403 Forbidden for authorization failures
- [ ] 404 Not Found for non-existent resources
- [ ] 409 Conflict for business logic conflicts
- [ ] 429 Too Many Requests for rate limiting
- [ ] 500 Internal Server Error for server errors
- [ ] 503 Service Unavailable for maintenance or overload

**URL Design:**
- [ ] URLs intuitive and readable
- [ ] Kebab-case or snake_case consistent (prefer kebab-case)
- [ ] Query parameters for filtering, sorting, pagination
- [ ] No verbs in URLs (use HTTP methods instead)
- [ ] Versioning strategy clear (URL path, header, or content negotiation)

**Example URL Patterns:**
```
✅ Good:
GET    /api/v1/users
GET    /api/v1/users/{id}
POST   /api/v1/users
PUT    /api/v1/users/{id}
PATCH  /api/v1/users/{id}
DELETE /api/v1/users/{id}
GET    /api/v1/users/{id}/orders
GET    /api/v1/orders?status=pending&sort=-createdAt&page=2&limit=20

❌ Bad:
GET    /api/v1/getUsers
POST   /api/v1/createUser
GET    /api/v1/user (singular for collection)
GET    /api/v1/users/{id}/orders/{id2}/items/{id3} (too nested)
```

**Request/Response Design:**
- [ ] Request bodies use JSON (or appropriate format)
- [ ] Response bodies consistent structure
- [ ] Field naming consistent (camelCase or snake_case)
- [ ] Enveloping avoided (return data directly, use HTTP for metadata)
- [ ] Pagination metadata included (total, page, limit, links)
- [ ] Error responses follow consistent format
- [ ] Date/time in ISO 8601 format (UTC recommended)
- [ ] Null vs. omitting fields strategy defined

**Pagination:**
```json
✅ Good (Cursor-based):
GET /api/v1/users?cursor=eyJpZCI6MTAwfQ==&limit=20

Response:
{
  "data": [...],
  "pagination": {
    "nextCursor": "eyJpZCI6MTIwfQ==",
    "hasMore": true
  }
}

✅ Good (Offset-based):
GET /api/v1/users?page=2&limit=20

Response:
{
  "data": [...],
  "pagination": {
    "page": 2,
    "limit": 20,
    "total": 150,
    "totalPages": 8
  }
}
```

**GraphQL Schema Design:**

**Type Definitions:**
- [ ] Types represent domain concepts clearly
- [ ] Scalar types used appropriately (ID, String, Int, Float, Boolean)
- [ ] Custom scalars defined where needed (DateTime, URL, Email)
- [ ] Enums used for fixed sets of values
- [ ] Non-null fields marked with `!` appropriately
- [ ] Lists marked with `[]` appropriately

**Query Design:**
- [ ] Queries follow naming conventions (noun-based)
- [ ] Arguments documented with descriptions
- [ ] Pagination implemented (cursor-based recommended)
- [ ] Filtering and sorting arguments provided
- [ ] Query complexity limits defined
- [ ] Query depth limits enforced

**Mutation Design:**
- [ ] Mutations follow naming conventions (verb-based: `createUser`, `updateOrder`)
- [ ] Input types used for complex arguments
- [ ] Mutations return updated objects or payloads
- [ ] Error handling strategy clear
- [ ] Idempotency considered for critical mutations

**N+1 Query Prevention:**
- [ ] DataLoader pattern implemented
- [ ] Batching strategy for related data
- [ ] Resolver complexity analyzed
- [ ] Query cost analysis performed

**gRPC Service Design:**

**Protobuf Definitions:**
- [ ] Message types well-defined and documented
- [ ] Field numbering consistent and never reused
- [ ] Required vs. optional fields appropriate
- [ ] Enums used for fixed value sets
- [ ] Nested messages used appropriately
- [ ] Backward compatibility maintained

**Service Definitions:**
- [ ] Service methods clearly named (verb + noun)
- [ ] Unary, server streaming, client streaming, bidirectional streaming used appropriately
- [ ] Request/response types specific (not generic)
- [ ] Error handling strategy defined (status codes, error details)
- [ ] Service versioning strategy clear

**API Security:**
- [ ] Authentication mechanism defined (JWT, OAuth 2.0, API keys)
- [ ] Authorization checks at API gateway and service level
- [ ] Input validation comprehensive (type, format, range, length)
- [ ] SQL injection prevention (parameterized queries, ORMs)
- [ ] Rate limiting implemented (per user, per IP, per endpoint)
- [ ] CORS configured appropriately
- [ ] Sensitive data not logged or exposed in errors

**API Documentation:**
- [ ] OpenAPI/Swagger spec complete and accurate
- [ ] All endpoints documented with descriptions
- [ ] Request/response examples provided
- [ ] Error responses documented
- [ ] Authentication requirements clear
- [ ] Deprecation warnings for legacy endpoints
- [ ] Changelog maintained for API versions

**Severity Ratings:**
- 🔴 **Critical**: Security vulnerabilities, data loss risks, or broken core functionality
- 🟠 **High**: Significant design flaws affecting scalability, performance, or reliability
- 🟡 **Medium**: Moderate issues or deviations from best practices
- 🟢 **Low**: Minor improvements or optimization opportunities

### Phase 3: Database Design Review

**Review Areas:**

**Data Modeling:**

**Entity Relationships:**
- [ ] Entities represent domain concepts clearly
- [ ] Relationships accurately model business rules
- [ ] One-to-many, many-to-many relationships correct
- [ ] Self-referencing relationships handled properly
- [ ] Recursive relationships (hierarchies) designed efficiently

**Normalization:**
- [ ] Appropriate normalization level (typically 3NF)
- [ ] Functional dependencies identified
- [ ] Redundancy eliminated where appropriate
- [ ] Denormalization justified for performance (documented)
- [ ] Update anomalies prevented

**Schema Design:**

**Table Structure:**
- [ ] Table naming conventions followed (plural nouns: `users`, `orders`)
- [ ] Column naming conventions followed (snake_case typical)
- [ ] Primary keys defined (prefer surrogate keys: UUID, bigint)
- [ ] Foreign keys defined with proper constraints
- [ ] Unique constraints defined where needed
- [ ] Check constraints for business rules
- [ ] Default values appropriate
- [ ] Nullable vs. NOT NULL decisions appropriate

**Data Types:**
- [ ] Column types appropriate for data (INT, BIGINT, VARCHAR, TEXT, JSON, etc.)
- [ ] String lengths appropriate (VARCHAR(255) vs. TEXT)
- [ ] Numeric precision appropriate (DECIMAL for money)
- [ ] Date/time types appropriate (TIMESTAMP vs. DATE vs. TIME)
- [ ] Boolean types used for true/false values
- [ ] JSON/JSONB used appropriately (not overused)
- [ ] Enum types vs. lookup tables decision justified

**Example Schema Issues:**

❌ **Bad:**
```sql
CREATE TABLE user (  -- singular
    id INT,  -- may overflow
    name VARCHAR(50),  -- too short
    email VARCHAR(100),  -- no unique constraint
    balance FLOAT,  -- precision issues for money
    created DATETIME  -- no timezone
);
```

✅ **Good:**
```sql
CREATE TABLE users (
    id BIGINT PRIMARY KEY AUTO_INCREMENT,
    name VARCHAR(255) NOT NULL,
    email VARCHAR(255) NOT NULL UNIQUE,
    balance DECIMAL(19, 4) NOT NULL DEFAULT 0,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    INDEX idx_email (email),
    INDEX idx_created_at (created_at)
);
```

**Indexes:**
- [ ] Primary keys automatically indexed
- [ ] Foreign keys indexed for join performance
- [ ] Indexes on frequently queried columns
- [ ] Composite indexes for multi-column queries (column order optimized)
- [ ] Unique indexes for uniqueness constraints
- [ ] Covering indexes considered for query optimization
- [ ] Index overhead balanced (too many indexes hurt writes)
- [ ] Index maintenance strategy defined

**Partitioning:**
- [ ] Partitioning strategy defined for large tables (> 100M rows)
- [ ] Partition key chosen appropriately (date, range, hash)
- [ ] Partition pruning considered in query design
- [ ] Partition maintenance strategy defined
- [ ] Partition limits considered (max partitions per table)

**Query Patterns:**

**Query Efficiency:**
- [ ] Queries use indexes effectively
- [ ] SELECT * avoided (select only needed columns)
- [ ] N+1 query problem prevented (eager loading, joins, batching)
- [ ] Unnecessary joins avoided
- [ ] Subqueries vs. joins trade-offs considered
- [ ] LIMIT used for pagination
- [ ] Query complexity reasonable (avoid deeply nested subqueries)

**Example N+1 Query Problem:**

❌ **Bad (N+1 queries):**
```python
# 1 query to get users
users = db.query("SELECT * FROM users")

# N queries to get orders for each user
for user in users:
    orders = db.query(f"SELECT * FROM orders WHERE user_id = {user.id}")
```

✅ **Good (2 queries with batching or 1 query with join):**
```python
# Approach 1: Batching (2 queries)
users = db.query("SELECT * FROM users")
user_ids = [user.id for user in users]
orders = db.query(f"SELECT * FROM orders WHERE user_id IN ({user_ids})")

# Approach 2: Join (1 query)
results = db.query("""
    SELECT u.*, o.*
    FROM users u
    LEFT JOIN orders o ON u.id = o.user_id
""")
```

**Data Integrity:**
- [ ] Foreign key constraints enforced
- [ ] Cascading deletes defined appropriately (CASCADE, SET NULL, RESTRICT)
- [ ] Check constraints for business rules
- [ ] Unique constraints for uniqueness requirements
- [ ] Triggers used sparingly (complexity, performance impact)
- [ ] Application-level validation complements database constraints

**Scalability Considerations:**

**Database Scaling Strategies:**
- [ ] Vertical scaling limits identified
- [ ] Horizontal scaling strategy defined (sharding, read replicas)
- [ ] Sharding key chosen appropriately (balanced distribution, query locality)
- [ ] Read replicas for read-heavy workloads
- [ ] Caching layer designed (Redis, Memcached)
- [ ] Connection pooling configured
- [ ] Database monitoring and alerting defined

**Caching Strategy:**
- [ ] Cache-aside pattern for reads
- [ ] Write-through or write-behind for writes
- [ ] Cache invalidation strategy clear
- [ ] TTL (Time-To-Live) defined for cached data
- [ ] Cache warming strategy for critical data
- [ ] Cache stampede prevention (locking, probabilistic early expiration)

### Phase 4: Microservices Architecture Review

**Review Areas:**

**Service Boundaries:**

**Service Decomposition:**
- [ ] Services aligned with bounded contexts (DDD)
- [ ] Services have clear responsibilities (high cohesion)
- [ ] Services loosely coupled (low coupling)
- [ ] Services independently deployable
- [ ] Services own their data (database-per-service pattern)
- [ ] Services sized appropriately (not too small, not too large)
- [ ] Service boundaries stable (infrequent changes)

**Bounded Context Alignment:**
- [ ] Each service represents a bounded context
- [ ] Domain model clear within each context
- [ ] Context boundaries well-defined
- [ ] Ubiquitous language used within context
- [ ] Context maps document relationships

**Anti-patterns to Avoid:**
- ❌ Distributed monolith (services tightly coupled)
- ❌ God service (one service doing too much)
- ❌ Anemic services (services with only CRUD operations)
- ❌ Shared database (multiple services accessing same tables)

**Communication Patterns:**

**Synchronous Communication (HTTP, gRPC):**
- [ ] Used for request/response scenarios
- [ ] Timeouts configured appropriately
- [ ] Circuit breakers implemented
- [ ] Retries with exponential backoff
- [ ] Service discovery mechanism defined
- [ ] Load balancing strategy clear
- [ ] API gateway for external access

**Asynchronous Communication (Message Queues, Event Streaming):**
- [ ] Used for fire-and-forget scenarios
- [ ] Message schemas versioned
- [ ] Idempotency handled (duplicate message processing)
- [ ] Message ordering guarantees understood
- [ ] Dead letter queues for failed messages
- [ ] Message TTL defined
- [ ] Poison message handling

**Event-Driven Architecture:**
- [ ] Events represent domain occurrences (past tense: `OrderCreated`, `UserRegistered`)
- [ ] Event schemas well-defined and versioned
- [ ] Event sourcing used appropriately (if applicable)
- [ ] CQRS pattern for read/write separation (if applicable)
- [ ] Eventual consistency handled gracefully
- [ ] Event replay capability for recovery
- [ ] Event ordering and causality maintained

**Data Management:**

**Database-per-Service Pattern:**
- [ ] Each service has its own database
- [ ] Services don't access other services' databases directly
- [ ] Data duplication justified and managed
- [ ] Eventual consistency strategy defined
- [ ] Data synchronization mechanisms clear

**Distributed Transactions:**
- [ ] Two-phase commit avoided (performance, complexity)
- [ ] Saga pattern used for distributed transactions
- [ ] Saga orchestration or choreography chosen
- [ ] Compensation logic defined for rollbacks
- [ ] Saga timeout and failure handling defined

**Saga Patterns:**

**Orchestration (centralized):**
- Saga orchestrator coordinates transaction steps
- Simpler to understand and debug
- Single point of failure (orchestrator)

**Choreography (decentralized):**
- Services react to events and publish new events
- More resilient (no single point of failure)
- Complex to understand and debug

**Review Checklist:**
- [ ] Saga pattern chosen appropriately
- [ ] Compensation actions defined for each step
- [ ] Saga timeout strategy defined
- [ ] Saga state persisted for recovery
- [ ] Idempotency ensured for saga steps

**Resilience Patterns:**

**Circuit Breaker:**
- [ ] Circuit breaker configured for external service calls
- [ ] Failure threshold defined
- [ ] Timeout configured
- [ ] Half-open state for recovery testing
- [ ] Fallback behavior defined

**Retry Logic:**
- [ ] Retries implemented with exponential backoff
- [ ] Maximum retry attempts defined
- [ ] Idempotency ensured (retries safe)
- [ ] Jitter added to prevent thundering herd
- [ ] Retries only for transient failures (not business errors)

**Timeouts:**
- [ ] Timeouts configured for all external calls
- [ ] Timeout values appropriate (not too short, not too long)
- [ ] Cascading timeouts prevented (child timeout < parent timeout)
- [ ] Timeout errors handled gracefully

**Bulkhead:**
- [ ] Thread pools isolated for different service calls
- [ ] Connection pools configured per dependency
- [ ] Resource exhaustion prevented
- [ ] Bulkhead limits configured appropriately

**Service Discovery:**
- [ ] Service registry mechanism defined (Consul, Eureka, etcd, Kubernetes)
- [ ] Health check endpoints implemented
- [ ] Service registration automatic
- [ ] Service deregistration on shutdown
- [ ] Load balancing strategy (round-robin, least connections, sticky sessions)
- [ ] Client-side vs. server-side discovery chosen

### Phase 5: Integration Architecture Review

**Review Areas:**

**Integration Patterns:**

**API Integration:**
- [ ] REST API integration for synchronous communication
- [ ] Error handling comprehensive (network errors, API errors, timeouts)
- [ ] Retry logic with exponential backoff
- [ ] Circuit breaker for failing APIs
- [ ] API versioning handled (version pinning, migration strategy)
- [ ] Authentication/authorization configured
- [ ] Rate limiting respected
- [ ] Webhooks for event notifications (if applicable)

**Message Queue Integration:**
- [ ] Message queue chosen appropriately (RabbitMQ, AWS SQS, Azure Service Bus)
- [ ] Message schemas defined and versioned
- [ ] Queue naming conventions followed
- [ ] Message durability configured (persistent messages)
- [ ] Dead letter queue configured
- [ ] Message TTL defined
- [ ] Poison message handling implemented
- [ ] Idempotent message processing

**Example Message Schema:**
```json
{
  "eventType": "order.created",
  "eventVersion": "1.0",
  "eventId": "uuid",
  "timestamp": "ISO 8601",
  "payload": {
    "orderId": "string",
    "userId": "string",
    "totalAmount": "decimal"
  },
  "metadata": {
    "correlationId": "uuid",
    "causationId": "uuid"
  }
}
```

**Event Streaming:**
- [ ] Event streaming platform chosen (Kafka, AWS Kinesis, Azure Event Hubs)
- [ ] Event schemas defined with schema registry
- [ ] Topic naming conventions followed
- [ ] Partitioning strategy defined (for parallelism and ordering)
- [ ] Consumer groups configured
- [ ] Offset management strategy (commit strategy)
- [ ] Event retention policy defined
- [ ] Stream processing framework chosen (Kafka Streams, Flink, Spark)

**Batch Processing:**
- [ ] Batch job scheduling defined (cron, scheduled tasks)
- [ ] ETL pipelines designed with proper error handling
- [ ] Incremental processing strategy (delta loads)
- [ ] Batch job monitoring and alerting
- [ ] Failed job retry and recovery
- [ ] Batch job idempotency
- [ ] Data validation in pipelines

**Third-Party API Integration:**
- [ ] API documentation reviewed
- [ ] Rate limits understood and respected
- [ ] API key management (secrets manager)
- [ ] Retry logic for transient failures
- [ ] Circuit breaker for availability issues
- [ ] Fallback behavior for API failures
- [ ] API versioning and deprecation strategy
- [ ] Webhook signature validation (if applicable)
- [ ] API cost monitoring (for paid APIs)

### Phase 6: Security Architecture Review

**Review Areas:**

**Authentication Design:**

**Authentication Mechanisms:**
- [ ] Mechanism chosen appropriately (JWT, OAuth 2.0, SAML, API keys)
- [ ] Password hashing with strong algorithm (bcrypt, Argon2, PBKDF2)
- [ ] Password requirements defined (length, complexity, history)
- [ ] Multi-factor authentication (MFA) supported for sensitive operations
- [ ] Session management secure (httpOnly, secure, sameSite cookies)
- [ ] Token expiration appropriate (access tokens short-lived, refresh tokens long-lived)
- [ ] Token revocation mechanism defined
- [ ] Account lockout after failed attempts

**JWT Design:**
- [ ] JWT signature algorithm strong (RS256, ES256, not HS256 with weak secret)
- [ ] JWT claims appropriate (sub, iss, aud, exp, iat)
- [ ] JWT expiration short (15 minutes for access tokens)
- [ ] Refresh token mechanism for token renewal
- [ ] JWT stored securely (not in localStorage for XSS risk)
- [ ] JWT validated on every request
- [ ] JWT blacklisting for revocation (if needed)

**OAuth 2.0 Design:**
- [ ] Grant type appropriate (authorization code, client credentials)
- [ ] PKCE used for public clients (mobile, SPA)
- [ ] Redirect URI validated strictly
- [ ] State parameter for CSRF protection
- [ ] Scope-based authorization
- [ ] Token endpoint authenticated
- [ ] Refresh token rotation for security

**Authorization Design:**

**Authorization Models:**
- [ ] Model chosen appropriately (RBAC, ABAC, ACL)
- [ ] Roles/permissions well-defined
- [ ] Principle of least privilege followed
- [ ] Authorization checks at API gateway and service level
- [ ] Fine-grained permissions for sensitive operations
- [ ] Permission inheritance strategy clear
- [ ] Permission caching for performance (with invalidation)

**RBAC (Role-Based Access Control):**
- [ ] Roles represent job functions (Admin, Editor, Viewer)
- [ ] Permissions assigned to roles, not users directly
- [ ] User-role assignments dynamic
- [ ] Role hierarchy supported (if needed)
- [ ] Default role for new users

**ABAC (Attribute-Based Access Control):**
- [ ] Attributes defined (user attributes, resource attributes, environment attributes)
- [ ] Policies express fine-grained rules
- [ ] Policy evaluation efficient
- [ ] Policy language expressive (XACML, OPA Rego)

**Data Protection:**

**Encryption at Rest:**
- [ ] Sensitive data encrypted in database (field-level or database-level)
- [ ] Encryption keys managed securely (KMS, HSM)
- [ ] Key rotation policy defined
- [ ] Encryption algorithm strong (AES-256)

**Encryption in Transit:**
- [ ] TLS/SSL for all external communication
- [ ] TLS version appropriate (TLS 1.2 minimum, TLS 1.3 preferred)
- [ ] Certificate management automated (Let's Encrypt, cert-manager)
- [ ] mTLS for service-to-service communication (if needed)

**Secrets Management:**
- [ ] Secrets not hardcoded in source code
- [ ] Secrets stored in secrets manager (AWS Secrets Manager, HashiCorp Vault, Azure Key Vault)
- [ ] Secrets rotated regularly
- [ ] Secrets access audited
- [ ] Least privilege access to secrets

**Sensitive Data Handling:**
- [ ] PII (Personally Identifiable Information) identified
- [ ] PII access logged and audited
- [ ] Data masking for non-production environments
- [ ] Data retention policy defined
- [ ] Data deletion mechanism (GDPR right to be forgotten)
- [ ] Sensitive data not logged (passwords, credit cards, SSNs)

**API Security:**

**Input Validation:**
- [ ] All inputs validated (type, format, range, length)
- [ ] Whitelist validation preferred over blacklist
- [ ] Parameterized queries for SQL (prevent SQL injection)
- [ ] Output encoding for HTML (prevent XSS)
- [ ] File upload validation (type, size, content)
- [ ] JSON schema validation for API requests

**Common Vulnerabilities Prevention:**
- [ ] SQL Injection: Use ORMs or parameterized queries
- [ ] XSS (Cross-Site Scripting): Output encoding, CSP headers
- [ ] CSRF (Cross-Site Request Forgery): CSRF tokens, SameSite cookies
- [ ] SSRF (Server-Side Request Forgery): Validate and sanitize URLs
- [ ] XXE (XML External Entity): Disable external entity processing
- [ ] Path Traversal: Validate and sanitize file paths
- [ ] Command Injection: Avoid shell execution, sanitize inputs

**Rate Limiting:**
- [ ] Rate limiting per user, per IP, per endpoint
- [ ] Rate limit thresholds appropriate (100 req/min per user typical)
- [ ] Rate limit headers included (X-RateLimit-Limit, X-RateLimit-Remaining, X-RateLimit-Reset)
- [ ] Rate limit algorithm appropriate (token bucket, leaky bucket, sliding window)
- [ ] DDoS protection at infrastructure level

**Security Headers:**
- [ ] Content-Security-Policy (CSP) for XSS prevention
- [ ] X-Content-Type-Options: nosniff
- [ ] X-Frame-Options: DENY or SAMEORIGIN
- [ ] Strict-Transport-Security (HSTS)
- [ ] X-XSS-Protection: 1; mode=block (legacy browsers)

**Security Monitoring:**

**Audit Logging:**
- [ ] Security events logged (authentication, authorization, data access)
- [ ] Logs include user ID, timestamp, action, resource, outcome
- [ ] Logs immutable (append-only)
- [ ] Logs centralized (ELK, Splunk, CloudWatch)
- [ ] Logs retained per compliance requirements
- [ ] Log analysis automated (alerts for anomalies)

**Security Event Monitoring:**
- [ ] Failed login attempts monitored and alerted
- [ ] Privilege escalation attempts detected
- [ ] Unusual data access patterns detected
- [ ] API abuse detected (rate limit violations)
- [ ] Security alerts routed to security team

### Phase 7: Performance & Scalability Review

**Review Areas:**

**Performance Requirements:**
- [ ] Response time targets defined (p50, p95, p99)
- [ ] Throughput targets defined (requests per second)
- [ ] Concurrent user targets defined
- [ ] Data volume targets defined (records, storage)
- [ ] Performance tested under load
- [ ] Performance budgets defined

**Caching Strategy:**
- [ ] Caching layers defined (CDN, API gateway, application, database)
- [ ] Cache-aside pattern for reads
- [ ] Write-through or write-behind for writes
- [ ] Cache invalidation strategy (TTL, event-based)
- [ ] Cache warming for critical data
- [ ] Cache key design (avoid cache key conflicts)
- [ ] Distributed cache for multi-instance deployments

**Database Performance:**
- [ ] Indexes on frequently queried columns
- [ ] Query optimization (EXPLAIN plans analyzed)
- [ ] Connection pooling configured
- [ ] Database read replicas for read-heavy workloads
- [ ] Database sharding for horizontal scaling
- [ ] Slow query logging enabled and monitored
- [ ] Database vacuum/optimization scheduled

**API Performance:**
- [ ] Response payloads minimal (only necessary data)
- [ ] Pagination for large datasets
- [ ] Compression enabled (gzip, brotli)
- [ ] CDN for static assets
- [ ] GraphQL query complexity limits
- [ ] API response caching (ETag, Cache-Control headers)
- [ ] Rate limiting prevents abuse

**Asynchronous Processing:**
- [ ] Long-running tasks offloaded to background jobs
- [ ] Message queues for async processing
- [ ] Job status tracking for users
- [ ] Job retry logic for failures
- [ ] Job prioritization for critical tasks

**Scalability Strategy:**

**Horizontal Scaling:**
- [ ] Stateless application design (scale by adding instances)
- [ ] Load balancer distributes traffic
- [ ] Auto-scaling policies defined (CPU, memory, request count)
- [ ] Session state externalized (Redis, database)
- [ ] File uploads to object storage (S3, Azure Blob)

**Vertical Scaling:**
- [ ] Vertical scaling limits identified (maximum instance size)
- [ ] Cost-effectiveness compared to horizontal scaling
- [ ] Downtime required for vertical scaling

**Database Scaling:**
- [ ] Read replicas for read scaling
- [ ] Sharding for write scaling and data partitioning
- [ ] Database connection pooling
- [ ] Caching layer to reduce database load

### Phase 8: Reporting & Recommendations

**Activities:**
1. **Consolidate Findings**
   - Categorize issues by severity and area
   - Document each finding with examples and evidence
   - Provide specific locations in design documents
   - Estimate effort required to address

2. **Prioritize Recommendations**
   - Critical issues must be fixed before implementation
   - High-priority issues should be fixed soon
   - Medium issues addressed in next iteration
   - Low issues tracked for future improvements

3. **Create Action Items**
   - Assign ownership for each issue
   - Set realistic timelines for fixes
   - Track progress on recommendations
   - Schedule follow-up review

## Backend Design Review Report Template

```markdown
# Backend Design Review Report

## Executive Summary
- **Project**: [Project name]
- **Review Date**: [Date]
- **Reviewer**: [Name]
- **Design Documents**: [Links to architecture diagrams, API specs, database schemas]
- **Overall Assessment**: [Summary of design quality]

## Review Scope
- Components reviewed: [List]
- APIs reviewed: [List]
- Databases reviewed: [List]
- Focus areas: [List]

## Findings Summary

### Critical Issues (🔴)
[Number of critical issues found]

### High Priority Issues (🟠)
[Number of high-priority issues]

### Medium Priority Issues (🟡)
[Number of medium-priority issues]

### Low Priority Issues (🟢)
[Number of low-priority suggestions]

## Detailed Findings

### API Design

#### Issue: [Issue Title]
- **Severity**: 🔴/🟠/🟡/🟢
- **Category**: API Design > REST/GraphQL/gRPC
- **Location**: [Endpoint or service name]
- **Description**: [Detailed description of the issue]
- **Current Design**: [Code snippet or description]
- **Impact**: [Security risk, performance issue, scalability problem, etc.]
- **Recommendation**: [Specific steps to resolve]
- **Effort**: [Low/Medium/High]

### Database Design

#### Issue: [Issue Title]
- **Severity**: 🔴/🟠/🟡/🟢
- **Category**: Database Design > Schema/Indexes/Query Patterns
- **Location**: [Table or query name]
- **Description**: [Detailed description]
- **Current Schema**: [SQL or diagram]
- **Impact**: [Data integrity issue, performance bottleneck, scalability concern]
- **Recommendation**: [Solution proposal]

### Microservices Architecture

#### Issue: [Issue Title]
- **Severity**: 🔴/🟠/🟡/🟢
- **Category**: Microservices > Service Boundaries/Communication/Data Management
- **Description**: [Service coupling, boundary issues, etc.]
- **Recommendation**: [How to improve service design]

### Security

#### Issue: [Issue Title]
- **Severity**: 🔴/🟠/🟡/🟢
- **Category**: Security > Authentication/Authorization/Data Protection
- **Description**: [Security vulnerability]
- **Risk**: [High/Medium/Low - potential for exploit, data breach, etc.]
- **Recommendation**: [How to remediate]

### Performance & Scalability

#### Issue: [Issue Title]
- **Severity**: 🔴/🟠/🟡/🟢
- **Category**: Performance > Caching/Database/API
- **Description**: [Performance bottleneck or scalability concern]
- **Impact**: [Response time, throughput, resource usage]
- **Recommendation**: [Optimization strategy]

## Positive Observations

### Strengths
- [What the design does well]
- [Effective patterns or approaches]
- [Particularly strong areas]

### Best Practices Followed
- [Good practices observed]
- [Security considerations]
- [Performance optimizations]

## Recommendations Summary

### Must Fix Before Implementation (🔴 Critical)
1. [Critical issue 1 with action item]
2. [Critical issue 2 with action item]

### Should Fix Soon (🟠 High)
1. [High-priority issue 1]
2. [High-priority issue 2]

### Consider for Next Iteration (🟡 Medium)
1. [Medium-priority improvement 1]
2. [Medium-priority improvement 2]

### Future Enhancements (🟢 Low)
1. [Low-priority suggestion 1]
2. [Low-priority suggestion 2]

## Action Items

| Issue | Severity | Owner | Deadline | Status |
|-------|----------|-------|----------|--------|
| [Issue] | 🔴 | [Name] | [Date] | Not Started |
| [Issue] | 🟠 | [Name] | [Date] | In Progress |

## Architecture Recommendations

### Suggested Architecture Improvements
1. [Architecture pattern or improvement]
2. [Technology or tool recommendation]
3. [Process or practice improvement]

### Technology Stack Evaluation
- [Assessment of chosen technologies]
- [Alternative suggestions if applicable]
- [Technology risk assessment]

## Compliance & Standards

### Security Compliance
- [ ] OWASP Top 10 vulnerabilities addressed
- [ ] Authentication and authorization implemented
- [ ] Data encryption (at rest and in transit)
- [ ] Audit logging configured

### Industry Standards
- [ ] RESTful API design principles followed
- [ ] Database normalization appropriate
- [ ] Microservices best practices applied
- [ ] Cloud-native patterns utilized

### Regulatory Compliance (if applicable)
- [ ] GDPR: Data privacy, right to be forgotten
- [ ] HIPAA: PHI protection, audit logs
- [ ] PCI-DSS: Payment data security
- [ ] SOC 2: Security controls

## Next Steps

1. **Immediate Actions** (This week)
   - [Action 1]
   - [Action 2]

2. **Short-term Actions** (Next 2 weeks)
   - [Action 1]
   - [Action 2]

3. **Follow-up Review**
   - Schedule: [Date]
   - Focus: [Areas to re-review]

## Appendix

### Review Criteria Used
- RESTful API Design Best Practices
- Database Design Principles
- Microservices Patterns (Sam Newman, Martin Fowler)
- OWASP Security Guidelines
- [Company] Architecture Standards

### Tools Used
- OpenAPI/Swagger Editor
- Database schema visualization tools
- Architecture diagramming tools (C4 Model, PlantUML)

### References
- [Link to ADRs]
- [Link to API specifications]
- [Link to database schemas]
- [Link to architecture diagrams]
```

## Severity Levels

### 🔴 Critical
- **Definition**: Issues that pose security risks, data loss, or broken core functionality
- **Examples**:
  - SQL injection vulnerability
  - Missing authentication on sensitive endpoints
  - Data loss risk from missing foreign key constraints
  - No input validation on critical APIs
- **Action Required**: Must be fixed before implementation

### 🟠 High
- **Definition**: Significant design flaws affecting scalability, performance, or reliability
- **Examples**:
  - N+1 query problems causing performance issues
  - Missing indexes on frequently queried columns
  - Tight coupling between microservices
  - No circuit breaker for external service calls
- **Action Required**: Should be fixed before go-live

### 🟡 Medium
- **Definition**: Moderate issues or deviations from best practices
- **Examples**:
  - Inconsistent API naming conventions
  - Suboptimal denormalization strategy
  - Missing API documentation for some endpoints
  - Inefficient caching strategy
- **Action Required**: Address in next iteration

### 🟢 Low
- **Definition**: Minor improvements or optimization opportunities
- **Examples**:
  - Additional API endpoints for convenience
  - Enhanced error messages
  - Additional indexes for edge case queries
  - Documentation improvements
- **Action Required**: Track for future improvements

## Best Practices for Backend Design Reviews

### 1. **Be Objective and Evidence-Based**
- Base feedback on established principles (RESTful design, database normalization, security best practices)
- Provide specific examples and code snippets
- Avoid subjective opinions without rationale
- Reference industry standards and guidelines

### 2. **Be Constructive and Solution-Oriented**
- Focus on problems and solutions, not personal preferences
- Provide specific, actionable recommendations
- Offer alternatives when criticizing designs
- Balance critique with recognition of strengths

### 3. **Consider Context**
- Understand business constraints and priorities
- Consider technical feasibility and team expertise
- Respect timeline and resource limitations
- Align feedback with project goals

### 4. **Prioritize Security and Reliability**
- Security issues always high priority
- Data integrity and reliability critical
- Performance and scalability important but not at expense of security
- Consider edge cases and failure scenarios

### 5. **Think Long-Term**
- Consider maintainability and evolution
- Evaluate scalability for future growth
- Assess technical debt implications
- Plan for deprecation and migration

### 6. **Collaborate with Designers**
- Involve designers in the review process
- Encourage discussion and questions
- Be open to designer perspectives
- Work together to find best solutions

### 7. **Document Thoroughly**
- Capture all findings with clear descriptions
- Include code examples or diagrams for clarity
- Provide specific locations in design documents
- Track recommendations and their status

### 8. **Follow Up**
- Schedule review of design iterations
- Verify critical issues resolved
- Track progress on recommendations
- Celebrate improvements

## Activation Guidelines

Use this skill when:

### Backend Design Scenarios
- Reviewing system architecture before implementation
- Validating API specifications (OpenAPI, GraphQL, gRPC)
- Evaluating database schemas and data models
- Assessing microservices architecture designs
- Reviewing integration patterns and message flows

### Security Review Scenarios
- Validating authentication and authorization design
- Reviewing data protection and encryption strategies
- Assessing API security measures
- Evaluating compliance with security standards (OWASP)

### Performance & Scalability Scenarios
- Reviewing caching strategies
- Assessing database query patterns and indexes
- Evaluating horizontal and vertical scaling approaches
- Reviewing load balancing and service discovery designs

### Collaboration Contexts
- Design critique sessions with backend team
- Architecture review board presentations
- Pre-implementation design validation
- Security architecture reviews
- Database design reviews

### User Mentions
- "Review this backend design"
- "Validate API specification"
- "Check database schema"
- "Review microservices architecture"
- "Assess API security"
- "Evaluate scalability approach"
- "Review integration patterns"
- "Check authentication design"

## Related Skills

- **backend-coding**: For implementing reviewed designs
- **software-solution-architecture**: For system-level architecture design
- **architecture-design-review**: For overall architecture assessment
- **integration-testing**: For testing implemented integrations
- **code-quality-security-review**: For reviewing implemented code

## Additional Resources

- RESTful API Design: https://restfulapi.net/
- GraphQL Best Practices: https://graphql.org/learn/best-practices/
- Database Design Principles: https://www.databasestar.com/database-design-process/
- Microservices Patterns: https://microservices.io/patterns/
- OWASP Top 10: https://owasp.org/www-project-top-ten/
- AWS Well-Architected Framework: https://aws.amazon.com/architecture/well-architected/
