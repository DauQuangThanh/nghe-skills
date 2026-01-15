# Core Capabilities


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
