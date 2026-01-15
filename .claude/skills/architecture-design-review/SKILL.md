---
name: architecture-design-review
description: Conducts comprehensive architecture design reviews including system design validation, architecture pattern assessment, quality attributes evaluation, technology stack review, and scalability analysis. Produces detailed review reports with findings, recommendations, and risk assessments. Use when reviewing software architecture designs, validating architecture decisions, assessing system scalability, evaluating technology choices, or when users mention architecture review, design assessment, technical review, or architecture validation.
---

# Architecture Design Review

Conduct systematic architecture design reviews to validate system design, assess quality attributes, evaluate technology choices, and identify risks before implementation.

## Review Process

Follow this structured approach for comprehensive architecture reviews:

### 1. Gather Architecture Documentation

Collect required materials before starting the review:

**Required Documents:**
- Architecture diagrams (C4: Context, Container, Component levels)
- Architecture Decision Records (ADRs) documenting key decisions
- Technical specifications and non-functional requirements
- Data models, schemas, and API specifications
- Technology stack documentation
- Deployment and infrastructure diagrams

**Context Information:**
- Business objectives and constraints (budget, timeline, compliance)
- Performance requirements (response time < 200ms for 95th percentile, throughput > 1000 req/s)
- Scalability targets (support 10x current load within 12 months)
- Security requirements (authentication, authorization, data protection)
- Integration requirements (internal systems, third-party APIs)

**Example Review Scope:**
```
System: E-commerce Platform
Architecture Type: Microservices
Scale: 100K daily active users, 5M products
Key Requirements:
- 99.9% availability (< 44 min downtime/month)
- < 500ms API response time for 95th percentile
- PCI-DSS compliance for payment processing
- Multi-region deployment (US, EU, APAC)
```

### 2. Assess Architecture Style and Patterns

Evaluate whether the chosen architecture style matches system requirements:

**Architecture Style Validation:**

Check if the selected style is appropriate:

- **Monolithic** - Best for: Small teams (<10), simple domains, low scale (<1000 users)
- **Microservices** - Best for: Large teams (>20), complex domains, high scale (>100K users)
- **Serverless** - Best for: Event-driven, variable load, stateless operations
- **Event-Driven** - Best for: Asynchronous workflows, loose coupling, high throughput

**Pattern Assessment Checklist:**

```
☐ Architecture style matches business and technical requirements
☐ Service boundaries align with business domains (Domain-Driven Design)
☐ Communication patterns appropriate (sync REST vs async messaging)
☐ Data management strategy defined (per-service DB vs shared)
☐ Integration patterns documented (API gateway, service mesh)
☐ Deployment model specified (containers, VMs, serverless)
```

**Anti-Pattern Detection:**

Identify common design flaws:
- **Big Ball of Mud** - No clear structure, tight coupling, shared database
- **God Service** - Single service handling multiple unrelated domains
- **Chatty Communication** - Excessive inter-service calls (>5 calls per request)
- **Distributed Monolith** - Microservices tightly coupled through shared database
- **Golden Hammer** - Using same technology for all problems

**Example Finding:**
```
Issue: Shopping Cart service handles cart, pricing, inventory, and promotions
Severity: High
Impact: Difficult to scale independently, complex deployments
Recommendation: Split into separate services:
  - Cart Service (manage cart items)
  - Pricing Service (calculate prices)
  - Inventory Service (check stock)
  - Promotion Service (apply discounts)
Timeline: 3-month refactoring effort
```

### 3. Evaluate Quality Attributes

Assess how the architecture addresses critical quality requirements:

**Scalability Review:**

Horizontal scaling capability:
- Load balancers distribute traffic across instances (NGINX, ALB, HAProxy)
- Stateless services enable adding/removing instances dynamically
- Database sharding or read replicas for data layer scaling
- Caching layers reduce database load (Redis, Memcached)

Vertical scaling limits:
- Identify single points requiring vertical scaling
- Document maximum capacity before horizontal scaling needed

**Example Scalability Assessment:**
```
Current: 10K concurrent users, 100 req/s
Scaling Strategy:
- API services: Auto-scale 3-30 pods (CPU > 70%)
- Database: Primary + 2 read replicas
- Cache: Redis cluster (3 nodes)
- Projected Capacity: 100K concurrent users, 1000 req/s
- Cost: $5K/month baseline, $15K/month at peak
✓ Recommendation: Architecture supports 10x scale
```

**Performance Review:**

Check performance design:
- Response time budgets allocated per service (API gateway: 50ms, services: 100ms, database: 50ms)
- Caching strategy defined (CDN for static, Redis for dynamic)
- Database query optimization (indexes, connection pooling)
- Asynchronous processing for long-running tasks (queues, background jobs)

**Performance Finding Example:**
```
Issue: Synchronous order processing blocks API response
Current: 3-5 second response time for order creation
Impact: Poor user experience, high server resource usage
Recommendation: Use async pattern:
  1. Accept order → Return 202 Accepted immediately
  2. Process asynchronously via message queue
  3. Notify user via webhook/polling
Expected Improvement: <200ms API response, 90% resource reduction
```

**Security Review:**

Validate security architecture:
- Authentication mechanism (OAuth 2.0, JWT, SAML)
- Authorization model (RBAC, ABAC, policy-based)
- API security (rate limiting, input validation, CORS)
- Data encryption (at-rest: AES-256, in-transit: TLS 1.3)
- Secret management (AWS Secrets Manager, HashiCorp Vault)
- Network security (VPC, security groups, WAF)

**Security Checklist:**
```
☐ Authentication tokens expire within 1 hour
☐ Password requirements: 12+ chars, complexity enforced
☐ API rate limiting: 100 req/min per user, 1000 req/min per IP
☐ Sensitive data encrypted at rest (PII, payment info)
☐ TLS 1.3 enforced for all external communication
☐ Secrets never hardcoded or committed to repositories
☐ Security headers configured (HSTS, CSP, X-Frame-Options)
☐ Input validation on all API endpoints (prevent injection)
```

**Availability and Reliability:**

Assess high availability design:
- Multi-AZ or multi-region deployment
- Circuit breakers prevent cascade failures (Hystrix, Resilience4j)
- Health checks and auto-recovery configured
- Backup and disaster recovery procedures (RPO < 1 hour, RTO < 4 hours)
- Graceful degradation for non-critical features

### 4. Review Technology Stack

Evaluate technology choices against requirements:

**Technology Fit Assessment:**

```
Technology Decision Matrix:

Backend Framework:
✓ Spring Boot - Best for: Enterprise Java, microservices, team Java expertise
✓ Node.js/Express - Best for: I/O-heavy, real-time, JavaScript team
✓ Django/Flask - Best for: Data-heavy, ML integration, Python team
✓ Go - Best for: High performance, low latency, concurrent operations

Database Selection:
✓ PostgreSQL - Best for: Complex queries, transactions, relational data
✓ MongoDB - Best for: Flexible schema, document storage, rapid iteration
✓ Cassandra - Best for: Write-heavy, time-series, multi-region
✓ Redis - Best for: Caching, sessions, real-time features

Deployment Platform:
✓ Kubernetes - Best for: Multi-cloud, complex orchestration, large scale
✓ ECS/Fargate - Best for: AWS-native, simpler than K8s, managed
✓ Cloud Run - Best for: GCP-native, serverless containers, auto-scale
```

**Technology Risk Assessment:**

Identify technology-related risks:
- **Vendor Lock-in**: Assess portability (high lock-in: AWS Lambda, low lock-in: Kubernetes)
- **Team Skills Gap**: Document required training (3-6 months for new technology)
- **Community Support**: Evaluate ecosystem maturity and longevity
- **Performance Constraints**: Validate technology meets performance requirements
- **License Compliance**: Check licensing compatibility with commercial use

**Example Technology Finding:**
```
Issue: Proposed NoSQL database for complex reporting queries
Technology: MongoDB for analytics workload
Risk: High - MongoDB not optimized for complex aggregations
Alternative: PostgreSQL with proper indexing or dedicated OLAP database
Rationale: 
  - 20+ complex JOIN operations required
  - Real-time aggregation across multiple collections
  - Team has strong SQL expertise
Cost Impact: 2-week delay to reconsider architecture
```

### 5. Analyze Data Architecture

Review data management strategy:

**Data Storage Decisions:**
- Database per service vs shared database
- SQL vs NoSQL selection criteria
- Data partitioning and sharding strategy
- Data replication and consistency model (strong vs eventual)

**Data Flow Validation:**
- Data flow diagrams show movement between services
- Data synchronization mechanisms defined (CDC, ETL, event streaming)
- Data ownership clearly assigned to services
- Cross-service queries avoided or minimized

**Example Data Architecture Review:**
```
Finding: Three services share customer database table
Services: Order, Notification, Analytics
Issue: Tight coupling through shared database
Recommendation: 
  1. Order Service owns customer data (source of truth)
  2. Publish customer events (CustomerCreated, CustomerUpdated)
  3. Notification and Analytics consume events
  4. Each service maintains local read model
Benefit: Independent deployment, clear ownership
Effort: 4-week migration with zero downtime
```

### 6. Review Monitoring and Observability

Assess operational visibility:

**Monitoring Requirements:**
```
Metrics Collection:
☐ Application metrics (request rate, error rate, latency)
☐ Infrastructure metrics (CPU, memory, disk, network)
☐ Business metrics (orders/min, revenue, conversion rate)
☐ Custom metrics per service

Logging:
☐ Centralized log aggregation (ELK, Splunk, CloudWatch)
☐ Structured logging with correlation IDs
☐ Log retention: 30 days hot, 1 year cold

Distributed Tracing:
☐ Request tracing across services (Jaeger, Zipkin, X-Ray)
☐ Performance bottleneck identification
☐ Dependency mapping

Alerting:
☐ Error rate > 1% triggers alert
☐ Response time p95 > 2s triggers alert
☐ Service availability < 99.5% triggers alert
☐ On-call rotation and escalation defined
```

### 7. Generate Review Report

Produce structured findings with severity and recommendations:

**Report Structure:**

1. **Executive Summary** (1 page)
   - Architecture style and key decisions
   - Overall assessment (Approved / Approved with Conditions / Not Approved)
   - Top 3 strengths and top 3 concerns

2. **Detailed Findings** (by category)
   - Finding description and severity (Critical / High / Medium / Low)
   - Impact assessment (performance, security, cost, maintainability)
   - Recommendation with effort estimate
   - Alternative approaches considered

3. **Risk Assessment**
   - Technical risks with mitigation plans
   - Resource and timeline risks
   - Operational risks

4. **Recommendations Priority**
   - Must Fix (before production): Critical issues blocking approval
   - Should Fix (within 3 months): High-impact improvements
   - Consider (within 6-12 months): Medium-impact enhancements

**Example Finding Format:**
```
Finding #5: Missing Circuit Breaker Pattern
Severity: High
Category: Reliability
Description: Inter-service calls lack circuit breaker protection. Service failures will cascade.
Impact: Entire platform unavailable if one service fails
Recommendation: Implement Resilience4j circuit breakers
  - Configure thresholds: 50% error rate, 10 requests minimum
  - Fallback behavior: Return cached data or graceful degradation
  - Monitor: Circuit state changes (open/closed/half-open)
Effort: 2 weeks implementation + 1 week testing
Priority: Must Fix before production
```

## Reference Documentation

Load detailed guidance for specific review areas:

- **[architecture-review-process.md](references/architecture-review-process.md)** - Complete review process with phase-by-phase checklists. Load when conducting full architecture review.

- **[review-checklists.md](references/review-checklists.md)** - Comprehensive checklists for all architecture aspects. Load when performing systematic validation.

- **[quality-attributes.md](references/quality-attributes.md)** - Detailed quality attribute assessment (scalability, performance, security, reliability). Load when evaluating non-functional requirements.

- **[common-patterns-to-validate.md](references/common-patterns-to-validate.md)** - Validation criteria for common architecture patterns. Load when assessing pattern implementation.

- **[anti-patterns.md](references/anti-patterns.md)** - Architecture anti-patterns with detection and remediation. Load when identifying design flaws.

- **[review-report-template.md](references/review-report-template.md)** - Report structure and examples. Load when generating review documentation.

- **[review-severity-levels.md](references/review-severity-levels.md)** - Severity classification criteria. Load when prioritizing findings.

- **[best-practices-for-architecture-reviews.md](references/best-practices-for-architecture-reviews.md)** - Review best practices and tips. Load for review guidance.

**Technology-Specific References:**

- **API Design**: [api-design.md](references/api-design.md) - REST, GraphQL, gRPC assessment
- **Data Architecture**: [data-management.md](references/data-management.md), [database-selection.md](references/database-selection.md)
- **Security**: [application-security.md](references/application-security.md), [authentication-and-authorization.md](references/authentication-and-authorization.md)
- **Scalability**: [horizontal-scalability.md](references/horizontal-scalability.md), [data-scalability.md](references/data-scalability.md)
- **Microservices**: [service-boundaries-microservices.md](references/service-boundaries-microservices.md)
- **DevOps**: [cicd-pipeline.md](references/cicd-pipeline.md), [container-strategy.md](references/container-strategy.md)
- **Cost**: [cost-analysis.md](references/cost-analysis.md), [infrastructure-costs.md](references/infrastructure-costs.md)
- **Monitoring**: [monitoring-and-observability.md](references/monitoring-and-observability.md)

## Critical Review Principles

**Focus on Architecture, Not Implementation:**
- Review designs and patterns, not code quality
- Validate decisions and trade-offs, not syntax
- Assess structure and boundaries, not variable names

**Be Specific with Findings:**
✅ "Circuit breaker missing on Order→Payment calls (avg 50 calls/sec). Add Resilience4j with 50% error threshold."
❌ "Need better error handling"

**Quantify Performance Requirements:**
✅ "API response time must be <200ms for 95th percentile at 1000 req/s"
❌ "API should be fast"

**Provide Actionable Recommendations:**
✅ "Split UserService into Authentication (identity) and Profile (data) services. Estimated 3-week effort. Use event bus for sync."
❌ "Consider improving service boundaries"

**Assess Based on Context:**
- Startup MVP has different requirements than enterprise system
- 100-user system doesn't need microservices complexity
- Evaluate appropriateness for scale, team, and timeline

