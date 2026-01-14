---
name: software-solution-architecture
description: Designs comprehensive software solution architectures including system components, technology stacks, integration patterns, scalability strategies, and deployment models. Produces architecture diagrams, technical specifications, and implementation roadmaps. Use when planning new software systems, modernizing legacy applications, designing microservices, evaluating technology choices, creating architecture documentation, or when users mention system design, architecture patterns, scalability planning, or technical architecture decisions.
license: MIT
metadata:
  author: Dau Quang Thanh
  version: "1.0"
  category: development
---

# Software Solution Architecture Design Skill

This skill guides you through comprehensive software solution architecture design, from initial requirements analysis through detailed technical specifications and implementation planning.

## Core Capabilities

When activated, this skill enables you to:

1. **Requirements Analysis & Architecture Planning**
   - Analyze functional and non-functional requirements
   - Identify architectural drivers (scalability, security, performance)
   - Define system boundaries and constraints
   - Establish architecture goals and success criteria

2. **System Architecture Design**
   - Design layered/tiered architectures
   - Create microservices and domain-driven designs
   - Design event-driven and message-based systems
   - Plan serverless and cloud-native architectures
   - Design monolithic, modular monolithic, or distributed systems

3. **Technology Stack Selection**
   - Evaluate programming languages and frameworks
   - Select databases (SQL, NoSQL, time-series, graph)
   - Choose middleware and integration platforms
   - Select infrastructure and cloud platforms
   - Recommend CI/CD and DevOps tools

4. **Architecture Patterns & Best Practices**
   - Apply design patterns (MVC, MVVM, Clean Architecture, Hexagonal)
   - Implement integration patterns (REST, GraphQL, gRPC, message queues)
   - Design for scalability (horizontal/vertical, caching, CDN)
   - Implement security patterns (OAuth, JWT, zero-trust)
   - Apply resilience patterns (circuit breakers, retries, bulkheads)

5. **Documentation & Deliverables**
   - Create C4 model diagrams (Context, Container, Component, Code)
   - Generate UML diagrams (class, sequence, deployment)
   - Produce architecture decision records (ADRs)
   - Write technical specifications and API contracts
   - Create implementation roadmaps and migration plans

## Architecture Design Process

Follow this systematic approach when designing software architectures:

### Phase 1: Discovery & Requirements

1. **Gather Requirements**
   - Functional requirements (features, use cases)
   - Non-functional requirements (performance, scalability, security)
   - Business constraints (budget, timeline, compliance)
   - Technical constraints (existing systems, team skills)

2. **Analyze Architecture Drivers**
   - Performance requirements (latency, throughput)
   - Scalability needs (user growth, data volume)
   - Availability targets (uptime SLA, disaster recovery)
   - Security requirements (authentication, authorization, compliance)
   - Maintainability goals (testability, modularity)

3. **Define System Context**
   - Identify stakeholders
   - Map external systems and dependencies
   - Define system boundaries
   - Identify integration points

### Phase 2: Architecture Design

1. **Choose Architecture Style**
   - **Monolithic**: Single deployable unit, shared database
     - Use for: Simple applications, MVPs, small teams
   - **Modular Monolithic**: Organized modules, single deployment
     - Use for: Medium complexity, clear domain boundaries
   - **Microservices**: Independent services, separate databases
     - Use for: Large scale, team autonomy, different tech stacks
   - **Serverless**: Function-based, managed infrastructure
     - Use for: Event-driven, variable load, cost optimization
   - **Event-Driven**: Asynchronous communication, event streams
     - Use for: Real-time processing, loose coupling, scalability

2. **Design System Components**
   - **Presentation Layer**: Web, mobile, desktop interfaces
   - **API Layer**: REST, GraphQL, gRPC endpoints
   - **Application Layer**: Business logic, orchestration
   - **Domain Layer**: Core business entities and rules
   - **Data Layer**: Databases, caches, data stores
   - **Integration Layer**: External APIs, message brokers

3. **Define Data Architecture**
   - Data models and schemas
   - Database selection (transactional, analytical, operational)
   - Data flow and transformation pipelines
   - Caching strategies (in-memory, distributed)
   - Data consistency models (eventual, strong)

4. **Design Integration Patterns**
   - **Synchronous**: REST APIs, GraphQL, gRPC
   - **Asynchronous**: Message queues (Kafka, RabbitMQ), event streams
   - **Batch**: ETL processes, scheduled jobs
   - **Real-time**: WebSockets, Server-Sent Events

### Phase 3: Quality Attributes

Address key quality attributes in your design:

1. **Scalability**
   - Horizontal scaling: Load balancers, stateless services
   - Vertical scaling: Resource optimization
   - Database scaling: Read replicas, sharding, partitioning
   - Caching: Redis, Memcached, CDN
   - Auto-scaling: Cloud-native scaling policies

2. **Performance**
   - Response time optimization
   - Throughput maximization
   - Resource utilization efficiency
   - Database query optimization
   - Caching strategies
   - Asynchronous processing

3. **Security**
   - Authentication: OAuth 2.0, OpenID Connect, SAML
   - Authorization: RBAC, ABAC, policy-based
   - Data encryption: At rest, in transit (TLS/SSL)
   - API security: API keys, rate limiting, WAF
   - Network security: VPC, firewalls, security groups
   - Compliance: GDPR, HIPAA, PCI-DSS

4. **Reliability & Availability**
   - Fault tolerance: Redundancy, failover
   - Resilience patterns: Circuit breakers, retries, timeouts
   - Health monitoring: Liveness, readiness probes
   - Disaster recovery: Backups, replication, multi-region
   - SLA targets: 99.9%, 99.99%, 99.999%

5. **Maintainability**
   - Code organization: Clean architecture, SOLID principles
   - Modularity: Domain-driven design, bounded contexts
   - Testability: Unit, integration, e2e tests
   - Documentation: Code comments, API docs, architecture docs
   - Monitoring: Logging, metrics, tracing

### Phase 4: Technology Selection

Recommend appropriate technologies based on requirements:

1. **Backend Technologies**
   - **Java/Spring Boot**: Enterprise, microservices, strong typing
   - **Node.js/Express**: JavaScript, async I/O, real-time
   - **Python/Django/FastAPI**: Rapid development, data science, ML
   - **.NET Core**: Microsoft ecosystem, enterprise
   - **Go**: High performance, concurrency, cloud-native
   - **Rust**: System programming, performance-critical

2. **Frontend Technologies**
   - **React**: Component-based, large ecosystem, SPA
   - **Vue.js**: Progressive, flexible, easy learning curve
   - **Angular**: Full framework, TypeScript, enterprise
   - **Next.js**: React SSR, static generation, full-stack
   - **Mobile**: React Native, Flutter, native (Swift/Kotlin)

3. **Databases**
   - **Relational**: PostgreSQL, MySQL, Oracle (ACID, complex queries)
   - **NoSQL Document**: MongoDB, CouchDB (flexible schema, JSON)
   - **NoSQL Key-Value**: Redis, DynamoDB (caching, sessions)
   - **NoSQL Column**: Cassandra, HBase (time-series, high write)
   - **NoSQL Graph**: Neo4j, Amazon Neptune (relationships, networks)
   - **Search**: Elasticsearch, Solr (full-text search, analytics)

4. **Cloud Platforms**
   - **AWS**: Comprehensive services, market leader
   - **Azure**: Microsoft integration, enterprise
   - **GCP**: Data analytics, machine learning, Kubernetes
   - **Multi-cloud**: Avoid vendor lock-in, regional presence

5. **DevOps & Infrastructure**
   - **Containers**: Docker, Kubernetes, ECS, AKS
   - **CI/CD**: GitHub Actions, GitLab CI, Jenkins, CircleCI
   - **IaC**: Terraform, CloudFormation, Pulumi
   - **Monitoring**: Prometheus, Grafana, DataDog, New Relic
   - **Logging**: ELK Stack, Splunk, CloudWatch

### Phase 5: Documentation

Produce comprehensive architecture documentation:

1. **Architecture Diagrams**
   - **C4 Context Diagram**: System in its environment
   - **C4 Container Diagram**: High-level technology choices
   - **C4 Component Diagram**: Components within containers
   - **Sequence Diagrams**: Interaction flows
   - **Deployment Diagram**: Infrastructure and deployment

2. **Architecture Decision Records (ADRs)**
   ```markdown
   # ADR-001: Use Microservices Architecture
   
   ## Status
   Accepted
   
   ## Context
   The system needs to support multiple development teams, different
   technology stacks, and independent scaling of components.
   
   ## Decision
   We will implement a microservices architecture with separate services
   for user management, ordering, payment, and inventory.
   
   ## Consequences
   - Positive: Team autonomy, independent scaling, technology flexibility
   - Negative: Increased complexity, distributed system challenges
   - Risks: Network latency, data consistency, operational overhead
   ```

3. **Technical Specifications**
   - System overview and goals
   - Architecture patterns and principles
   - Component descriptions and responsibilities
   - Data models and schemas
   - API contracts (OpenAPI/Swagger)
   - Infrastructure requirements
   - Security controls
   - Monitoring and observability strategy

4. **Implementation Roadmap**
   - Phase 1: Foundation (core services, infrastructure)
   - Phase 2: Core features (business logic, APIs)
   - Phase 3: Integration (external systems, third-party)
   - Phase 4: Advanced features (analytics, ML)
   - Migration strategy (if applicable)
   - Risk mitigation plans

## Common Architecture Patterns

### Layered Architecture
```
┌─────────────────────────────────┐
│   Presentation Layer            │  (UI, Controllers)
├─────────────────────────────────┤
│   Application Layer             │  (Use Cases, Orchestration)
├─────────────────────────────────┤
│   Domain Layer                  │  (Business Logic, Entities)
├─────────────────────────────────┤
│   Infrastructure Layer          │  (DB, External APIs)
└─────────────────────────────────┘
```

### Microservices Architecture
```
┌──────────┐     ┌──────────┐     ┌──────────┐
│  API     │────▶│ Service  │────▶│ Service  │
│ Gateway  │     │    A     │     │    B     │
└──────────┘     └─────┬────┘     └─────┬────┘
                       │                 │
                  ┌────▼────┐       ┌───▼────┐
                  │   DB    │       │   DB   │
                  └─────────┘       └────────┘
```

### Event-Driven Architecture
```
┌─────────┐       ┌─────────────┐       ┌─────────┐
│Producer │──────▶│ Event Bus/  │──────▶│Consumer │
│Service  │       │Message Queue│       │Service  │
└─────────┘       └─────────────┘       └─────────┘
```

### Clean Architecture (Hexagonal)
```
         ┌────────────────────────┐
         │    UI / Controllers    │
         └───────────┬────────────┘
                     │
         ┌───────────▼────────────┐
         │   Application Layer    │
         │   (Use Cases/Ports)    │
         └───────────┬────────────┘
                     │
         ┌───────────▼────────────┐
         │     Domain Layer       │
         │  (Business Rules)      │
         └───────────┬────────────┘
                     │
         ┌───────────▼────────────┐
         │   Adapters Layer       │
         │ (DB, APIs, External)   │
         └────────────────────────┘
```

## Design Considerations

### For Different Scale Levels

**Small Scale (< 1K users)**
- Monolithic architecture
- Single database instance
- Simple deployment (VM or PaaS)
- Basic monitoring
- Manual scaling

**Medium Scale (1K - 100K users)**
- Modular monolithic or early microservices
- Database read replicas
- Load balancing
- Caching layer
- Container orchestration
- Auto-scaling

**Large Scale (100K - 1M+ users)**
- Full microservices architecture
- Distributed data stores
- Multiple caching layers
- CDN for static content
- Multi-region deployment
- Advanced monitoring and observability
- Chaos engineering

### For Different Domains

**E-Commerce**
- Product catalog service
- Shopping cart and order management
- Payment processing integration
- Inventory management
- Search and recommendation
- User reviews and ratings

**Financial Services**
- Account management
- Transaction processing
- Fraud detection
- Regulatory compliance
- Audit logging
- High security requirements

**SaaS Applications**
- Multi-tenancy architecture
- Subscription management
- Usage metering and billing
- User onboarding
- Analytics and reporting

**IoT/Real-Time Systems**
- Time-series data storage
- Event streaming
- Edge computing
- Device management
- Real-time analytics

## Best Practices

1. **Design Principles**
   - SOLID principles for object-oriented design
   - DRY (Don't Repeat Yourself)
   - KISS (Keep It Simple, Stupid)
   - YAGNI (You Aren't Gonna Need It)
   - Separation of concerns
   - Loose coupling, high cohesion

2. **API Design**
   - RESTful conventions (proper HTTP verbs, status codes)
   - Consistent naming and formatting
   - Versioning strategy
   - Pagination for large datasets
   - Rate limiting and throttling
   - Comprehensive documentation (OpenAPI)

3. **Security First**
   - Defense in depth
   - Principle of least privilege
   - Input validation and sanitization
   - Secure authentication and authorization
   - Encryption at rest and in transit
   - Regular security audits

4. **Operational Excellence**
   - Infrastructure as Code
   - Automated testing (unit, integration, e2e)
   - Continuous integration and deployment
   - Comprehensive monitoring and alerting
   - Distributed tracing
   - Disaster recovery planning

5. **Cost Optimization**
   - Right-size resources
   - Use auto-scaling
   - Leverage spot instances (where appropriate)
   - Implement caching effectively
   - Optimize database queries
   - Monitor and analyze costs regularly

## Output Format

When presenting architecture designs, structure your response as follows:

### 1. Executive Summary
- Brief overview of the solution
- Key architecture decisions
- Major benefits and trade-offs

### 2. System Overview
- High-level description
- System context diagram
- Key stakeholders

### 3. Architecture Design
- Architecture style and rationale
- Component diagram with descriptions
- Data architecture
- Integration approach

### 4. Technology Stack
- Backend, frontend, database selections
- Infrastructure and cloud choices
- Justification for each choice

### 5. Quality Attributes
- How scalability is addressed
- Security measures
- Reliability and availability approach
- Performance optimization strategies

### 6. Deployment Architecture
- Deployment diagram
- Infrastructure components
- CI/CD pipeline overview

### 7. Architecture Decision Records
- Key ADRs documenting important decisions

### 8. Implementation Roadmap
- Phased approach
- Key milestones
- Dependencies and risks

### 9. Risks and Mitigation
- Technical risks
- Mitigation strategies

## Examples

### Example 1: E-Commerce Platform

**Requirements**: Build a scalable e-commerce platform handling 100K daily active users

**Architecture Design**:
- **Style**: Microservices with event-driven communication
- **Services**: Product Catalog, User Service, Order Management, Payment Processing, Inventory
- **Technology**: Node.js/Express, React, PostgreSQL, MongoDB, Redis, Kafka
- **Infrastructure**: AWS with EKS, RDS, ElastiCache, S3, CloudFront
- **Key Patterns**: API Gateway, CQRS for orders, Event Sourcing for inventory

### Example 2: Real-Time Analytics Dashboard

**Requirements**: Process and visualize millions of events per minute

**Architecture Design**:
- **Style**: Event-driven, Lambda architecture
- **Components**: Event ingestion, stream processing, batch processing, query layer
- **Technology**: Kafka, Apache Flink, Elasticsearch, React, WebSocket
- **Infrastructure**: AWS with MSK, EMR, OpenSearch, Lambda
- **Key Patterns**: Event streaming, CQRS, real-time aggregation

## Migration Patterns

When migrating from legacy systems:

### Strangler Fig Pattern
- Gradually replace legacy functionality
- Run old and new systems in parallel
- Route traffic incrementally to new system
- Decommission old components progressively

### Anti-Corruption Layer
- Create abstraction layer between old and new
- Translate between different models
- Protect new system from legacy complexity

### Database Migration Strategies
- **Big Bang**: Complete cutover (high risk, minimal complexity)
- **Trickle Migration**: Gradual data migration (lower risk, higher complexity)
- **Change Data Capture**: Real-time synchronization during migration

## Common Anti-Patterns to Avoid

1. **Big Ball of Mud**: Unstructured, tangled architecture
2. **Golden Hammer**: Using same solution for all problems
3. **Premature Optimization**: Optimizing before measuring
4. **God Object**: Single class/service doing too much
5. **Distributed Monolith**: Microservices with tight coupling
6. **Database as Integration Point**: Services sharing database
7. **Chatty Interfaces**: Too many small network calls
8. **Ignoring Non-Functional Requirements**: Focusing only on features

## Resources and References

For additional guidance:

- **Books**: "Designing Data-Intensive Applications", "Software Architecture Patterns", "Building Microservices"
- **Frameworks**: AWS Well-Architected Framework, Azure Architecture Center, Google Cloud Architecture Framework
- **Patterns**: Enterprise Integration Patterns, Cloud Design Patterns
- **Standards**: ISO 25010 (Software Quality), ISO 27001 (Security)

## Activation Guidelines

This skill should be activated when:
- Designing a new software system from scratch
- Modernizing or re-architecting existing systems
- Evaluating technology stack choices
- Planning system scalability or performance improvements
- Creating architecture documentation
- Preparing for architecture review meetings
- Conducting architecture assessments
- Planning cloud migrations
- Designing microservices decomposition
- Making build vs buy decisions for system components

The skill provides the most value when given detailed requirements, constraints, and business context.
