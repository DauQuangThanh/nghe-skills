---
name: architecture-design-review
description: Conducts comprehensive architecture design reviews including system design validation, architecture pattern assessment, quality attributes evaluation, technology stack review, and scalability analysis. Produces detailed review reports with findings, recommendations, and risk assessments. Use when reviewing software architecture designs, validating architecture decisions, assessing system scalability, evaluating technology choices, or when users mention architecture review, design assessment, technical review, or architecture validation.
license: MIT
metadata:
  author: Dau Quang Thanh
  version: "1.0"
  category: review
---

# Architecture Design Review Skill

This skill guides you through comprehensive architecture design reviews, from initial design assessment through detailed analysis and recommendation generation.

## Core Capabilities

When activated, this skill enables you to:

1. **Architecture Assessment**
   - Evaluate architecture styles and patterns
   - Assess system design against requirements
   - Validate architecture decisions
   - Identify design anti-patterns
   - Review system boundaries and contexts

2. **Quality Attributes Review**
   - Assess scalability design
   - Evaluate performance characteristics
   - Review security architecture
   - Analyze reliability and availability
   - Assess maintainability and testability

3. **Technology Stack Review**
   - Evaluate technology choices
   - Assess technology fit for requirements
   - Review integration approaches
   - Validate infrastructure decisions
   - Assess vendor lock-in risks

4. **Design Documentation Review**
   - Review C4 diagrams (Context, Container, Component)
   - Assess Architecture Decision Records (ADRs)
   - Evaluate technical specifications
   - Review API contracts and interfaces
   - Validate data models and schemas

5. **Risk and Recommendation**
   - Identify architecture risks
   - Provide improvement recommendations
   - Suggest alternative approaches
   - Prioritize findings
   - Create action plans

## Architecture Review Process

Follow this systematic approach when conducting architecture reviews:

### Phase 1: Pre-Review Preparation

**1. Gather Architecture Documentation**
```markdown
Required Documents:
- Architecture overview and context
- C4 diagrams (Context, Container, Component)
- Architecture Decision Records (ADRs)
- Technical specifications
- Non-functional requirements
- Technology stack documentation
- Data models and schemas
- API specifications
- Deployment diagrams
- Security architecture

Optional Documents:
- Performance test results
- Load test reports
- Security audit results
- Cost analysis
- Team skill matrix
```

**2. Understand Context**
```markdown
Project Context:
- Business objectives and goals
- Project constraints (budget, timeline, resources)
- Regulatory requirements
- Current system state (greenfield vs legacy)
- Team size and experience
- Timeline and milestones

Technical Context:
- Expected load and scale
- Performance requirements
- Availability targets (SLA)
- Security requirements
- Integration requirements
- Data volume and growth
```

**3. Define Review Scope**
```markdown
Review Focus Areas:
☐ Overall architecture style and patterns
☐ System decomposition and boundaries
☐ Technology stack appropriateness
☐ Scalability and performance
☐ Security architecture
☐ Data architecture
☐ Integration patterns
☐ Deployment architecture
☐ Monitoring and observability
☐ Cost optimization

Out of Scope:
- Implementation details
- Code review
- Project management
- Team processes
```

### Phase 2: Architecture Style Review

**1. Architecture Pattern Assessment**

```markdown
# Architecture Pattern Review Checklist

## Pattern Selection
☐ Architecture pattern clearly identified
☐ Pattern appropriate for requirements
☐ Pattern advantages understood
☐ Pattern trade-offs acknowledged
☐ Alternative patterns considered

## Common Patterns to Validate

### Monolithic Architecture
☐ Justified for project size and complexity
☐ Modularization strategy defined
☐ Scaling strategy documented
☐ Migration path considered (if applicable)

### Microservices Architecture
☐ Service boundaries follow business domains
☐ Services are independently deployable
☐ Database per service enforced
☐ Service communication patterns defined
☐ Service discovery mechanism specified
☐ Circuit breakers and resilience patterns included
☐ Distributed tracing implemented
☐ Saga pattern for transactions (if needed)

### Event-Driven Architecture
☐ Event schemas defined
☐ Event sourcing strategy documented
☐ Event ordering guarantees defined
☐ Dead letter queue handling specified
☐ Event replay capability considered
☐ Message broker selected appropriately

### Serverless Architecture
☐ Function boundaries appropriate
☐ Cold start impact assessed
☐ Stateless design enforced
☐ Vendor lock-in considerations documented
☐ Cost model validated

### Layered Architecture
☐ Layer responsibilities clearly defined
☐ Layer dependencies unidirectional
☐ Cross-cutting concerns addressed
☐ Layer coupling minimized
```

**2. Design Principles Assessment**

```markdown
# Design Principles Checklist

## SOLID Principles
☐ Single Responsibility: Components have single purpose
☐ Open/Closed: Design extensible without modification
☐ Liskov Substitution: Subtypes are substitutable
☐ Interface Segregation: Focused interfaces
☐ Dependency Inversion: Depend on abstractions

## General Principles
☐ Separation of Concerns: Distinct responsibilities
☐ DRY (Don't Repeat Yourself): No duplication
☐ KISS (Keep It Simple): Simplicity favored
☐ YAGNI (You Aren't Gonna Need It): No over-engineering
☐ Loose Coupling: Minimal dependencies
☐ High Cohesion: Related functionality together
☐ Fail Fast: Early error detection
☐ Defensive Programming: Input validation

## Domain-Driven Design (if applicable)
☐ Bounded contexts identified
☐ Ubiquitous language defined
☐ Aggregates properly modeled
☐ Domain events identified
☐ Anti-corruption layers for legacy integration
```

### Phase 3: Component and Service Review

**1. System Decomposition**

```markdown
# System Decomposition Review

## Component Organization
☐ Components organized by business capability
☐ Component boundaries clear and logical
☐ Component responsibilities well-defined
☐ Component size appropriate (not too large/small)
☐ Component reusability considered

## Service Boundaries (Microservices)
☐ Services align with business domains
☐ Services can be developed independently
☐ Services can be deployed independently
☐ Services can be scaled independently
☐ Service dependencies minimized
☐ Circular dependencies avoided

## API Design
☐ API contracts well-defined
☐ Versioning strategy specified
☐ Error handling standardized
☐ Rate limiting considered
☐ Authentication/authorization defined
☐ API documentation complete (OpenAPI/Swagger)

## Data Management
☐ Data ownership clearly defined
☐ Database per service (if microservices)
☐ Data consistency strategy defined
☐ Caching strategy documented
☐ Data migration plan specified
```

**2. Integration Patterns**

```markdown
# Integration Review Checklist

## Synchronous Communication
☐ REST/GraphQL/gRPC choice justified
☐ API Gateway pattern used appropriately
☐ Timeouts defined
☐ Retry logic specified
☐ Circuit breakers implemented
☐ Fallback mechanisms defined

## Asynchronous Communication
☐ Message broker selected appropriately
☐ Message patterns defined (pub/sub, queue)
☐ Message schemas versioned
☐ Dead letter queues configured
☐ Message ordering guaranteed (if needed)
☐ Idempotency handled
☐ Message replay capability (if needed)

## Event-Driven Patterns
☐ Event schemas well-defined
☐ Event sourcing considered
☐ CQRS pattern applied appropriately
☐ Saga orchestration vs choreography chosen
☐ Eventual consistency acceptable

## External Integrations
☐ Third-party APIs properly abstracted
☐ Anti-corruption layer for legacy systems
☐ Integration resilience patterns applied
☐ API rate limits handled
☐ Webhook security implemented
```

### Phase 4: Quality Attributes Assessment

**1. Scalability Review**

```markdown
# Scalability Assessment

## Horizontal Scalability
☐ Services/components stateless
☐ Session management externalized
☐ Load balancer configured
☐ Auto-scaling policies defined
☐ Database read replicas planned
☐ CDN for static content

## Vertical Scalability
☐ Resource limits defined
☐ Upgrade path documented
☐ Cost implications assessed

## Data Scalability
☐ Database sharding strategy
☐ Partitioning approach defined
☐ Archive/purge strategy for old data
☐ Read/write separation (CQRS)

## Caching Strategy
☐ Cache layers identified
☐ Cache invalidation strategy
☐ Cache eviction policies
☐ Cache warming strategy
☐ Distributed caching for stateless

## Performance Targets
☐ Response time requirements defined
☐ Throughput requirements specified
☐ Concurrent user load defined
☐ Performance testing planned
☐ Performance monitoring implemented
```

**2. Security Architecture Review**

```markdown
# Security Assessment

## Authentication & Authorization
☐ Authentication mechanism appropriate (OAuth, JWT, etc.)
☐ Multi-factor authentication considered
☐ Authorization model defined (RBAC, ABAC)
☐ Token management secure
☐ Session management secure
☐ Password policies enforced

## Data Security
☐ Data encryption at rest
☐ Data encryption in transit (TLS/SSL)
☐ Sensitive data identified and protected
☐ PII/PHI handling compliant
☐ Database encryption keys managed
☐ Secrets management (Vault, KMS)

## Network Security
☐ Network segmentation (VPC, subnets)
☐ Security groups/firewall rules defined
☐ API Gateway with WAF
☐ DDoS protection considered
☐ VPN/Private links for sensitive communication

## Application Security
☐ Input validation on all inputs
☐ SQL injection prevention
☐ XSS prevention
☐ CSRF protection
☐ Dependency vulnerabilities scanned
☐ Security headers configured
☐ API rate limiting
☐ OWASP Top 10 addressed

## Compliance
☐ GDPR compliance (if applicable)
☐ HIPAA compliance (if applicable)
☐ PCI-DSS compliance (if applicable)
☐ SOC 2 requirements met
☐ Data residency requirements
☐ Audit logging implemented
```

**3. Reliability & Availability Review**

```markdown
# Reliability Assessment

## High Availability Design
☐ Multi-AZ/multi-region deployment
☐ Load balancing configured
☐ Health checks implemented
☐ Automatic failover configured
☐ Database replication setup
☐ SLA targets defined and achievable

## Fault Tolerance
☐ Single points of failure identified and addressed
☐ Graceful degradation defined
☐ Circuit breakers implemented
☐ Bulkhead pattern for isolation
☐ Retry with exponential backoff
☐ Timeout policies defined

## Disaster Recovery
☐ Backup strategy defined
☐ Backup frequency appropriate
☐ Backup testing planned
☐ Recovery Time Objective (RTO) defined
☐ Recovery Point Objective (RPO) defined
☐ DR runbooks created
☐ DR testing scheduled

## Monitoring & Observability
☐ Logging strategy defined
☐ Metrics collection configured
☐ Distributed tracing implemented
☐ Alerting rules defined
☐ Dashboards for key metrics
☐ On-call procedures documented
```

**4. Maintainability Assessment**

```markdown
# Maintainability Review

## Code Organization
☐ Clear folder structure
☐ Separation of concerns
☐ Modularity and reusability
☐ Configuration externalized
☐ Infrastructure as Code

## Documentation
☐ Architecture documentation complete
☐ API documentation available
☐ Deployment procedures documented
☐ Runbooks for common operations
☐ ADRs for key decisions
☐ README files comprehensive

## Testing Strategy
☐ Unit testing approach defined
☐ Integration testing planned
☐ E2E testing strategy
☐ Performance testing planned
☐ Security testing included
☐ Test coverage targets set

## Development Practices
☐ Version control strategy
☐ Branching strategy defined
☐ Code review process
☐ CI/CD pipeline configured
☐ Automated testing in pipeline
☐ Deployment automation
```

### Phase 5: Technology Stack Review

**1. Technology Selection Assessment**

```markdown
# Technology Stack Review

## Backend Technologies
☐ Language choice justified
☐ Framework appropriate for requirements
☐ Framework maturity and support
☐ Team expertise with technology
☐ Community and ecosystem
☐ Long-term viability

## Frontend Technologies
☐ Framework choice justified
☐ Mobile vs web considerations
☐ SEO requirements addressed
☐ Performance characteristics
☐ Browser compatibility
☐ Accessibility support

## Database Selection
☐ Database type appropriate (SQL vs NoSQL)
☐ Consistency requirements met
☐ Query patterns supported
☐ Scalability characteristics
☐ Backup and recovery features
☐ Cost implications

## Infrastructure
☐ Cloud provider choice justified
☐ Vendor lock-in risks assessed
☐ Multi-cloud strategy (if applicable)
☐ Container orchestration (K8s, ECS, etc.)
☐ Serverless considerations
☐ Cost optimization strategies

## Third-Party Services
☐ Build vs buy decisions justified
☐ Vendor reliability assessed
☐ SLA agreements reviewed
☐ Integration complexity evaluated
☐ Cost analysis performed
☐ Exit strategy defined
```

### Phase 6: Data Architecture Review

**1. Data Model Assessment**

```markdown
# Data Architecture Review

## Data Modeling
☐ Data models well-designed
☐ Normalization appropriate
☐ Relationships properly defined
☐ Indexes planned effectively
☐ Query patterns optimized
☐ Data integrity constraints

## Data Flow
☐ Data flow diagrams clear
☐ Data transformation documented
☐ ETL/ELT processes defined
☐ Data validation at boundaries
☐ Data lineage tracked

## Data Storage Strategy
☐ Hot vs cold storage defined
☐ Data retention policies
☐ Archival strategy
☐ Data purging procedures
☐ Backup and restore tested

## Data Consistency
☐ Consistency model defined (strong vs eventual)
☐ Transaction boundaries clear
☐ Distributed transaction handling
☐ Conflict resolution strategy
☐ Data synchronization approach

## Data Migration
☐ Migration strategy defined
☐ Data migration tools selected
☐ Rollback plan documented
☐ Validation procedures
☐ Downtime requirements
```

### Phase 7: Deployment and Operations Review

**1. Deployment Architecture**

```markdown
# Deployment Review

## Infrastructure
☐ Environment strategy (dev, staging, prod)
☐ Infrastructure as Code (Terraform, CloudFormation)
☐ Resource provisioning automated
☐ Configuration management
☐ Secrets management

## CI/CD Pipeline
☐ Build automation configured
☐ Automated testing in pipeline
☐ Deployment automation
☐ Blue-green or canary deployments
☐ Rollback procedures
☐ Pipeline security scanned

## Container Strategy
☐ Container images optimized
☐ Image vulnerability scanning
☐ Image registry secured
☐ Orchestration configured properly
☐ Resource limits defined
☐ Health checks configured

## Operational Readiness
☐ Monitoring configured
☐ Logging centralized
☐ Alerting rules defined
☐ Incident response procedures
☐ Escalation paths documented
☐ On-call rotation defined
```

### Phase 8: Cost and Performance Review

**1. Cost Analysis**

```markdown
# Cost Review

## Infrastructure Costs
☐ Compute costs estimated
☐ Storage costs calculated
☐ Network/bandwidth costs
☐ Database costs projected
☐ Third-party service costs
☐ Cost optimization strategies identified

## Cost Efficiency
☐ Right-sizing of resources
☐ Reserved instances considered
☐ Spot instances where appropriate
☐ Auto-scaling to optimize costs
☐ Cost monitoring and alerts
☐ Budget vs actual tracking

## Performance Budget
☐ Performance requirements defined
☐ Performance testing planned
☐ Performance monitoring
☐ Performance optimization strategies
☐ Trade-offs documented
```

## Review Report Template

```markdown
# Architecture Design Review Report

## Executive Summary
- Project: [Name]
- Review Date: [Date]
- Reviewers: [Names]
- Architecture Style: [Monolithic/Microservices/etc.]
- Overall Assessment: [Critical Issues / Concerns / Acceptable / Good / Excellent]

### Key Findings
1. [Critical finding 1]
2. [Important finding 2]
3. [Recommendation 1]

---

## Architecture Overview
- Brief description of the system
- Architecture diagrams (C4 Context, Container)
- Key components and services
- Technology stack summary

---

## Detailed Findings

### 1. Architecture Pattern & Design
**Status**: ⚠️ Concerns / ✅ Acceptable / ✨ Excellent

**Findings**:
- [Finding 1]: [Description]
  - Severity: Critical / High / Medium / Low
  - Impact: [Impact description]
  - Recommendation: [Recommended action]

**Positive Aspects**:
- [What was done well]

---

### 2. Scalability Assessment
**Status**: [Status]

**Findings**:
- Horizontal scaling: [Assessment]
- Vertical scaling: [Assessment]
- Database scaling: [Assessment]
- Caching strategy: [Assessment]

**Recommendations**:
- [Recommendation 1]
- [Recommendation 2]

---

### 3. Security Architecture
**Status**: [Status]

**Findings**:
- Authentication/Authorization: [Assessment]
- Data encryption: [Assessment]
- Network security: [Assessment]
- Application security: [Assessment]
- Compliance: [Assessment]

**Critical Security Issues**:
- [Issue 1]
- [Issue 2]

**Recommendations**:
- [High priority security recommendations]

---

### 4. Reliability & Availability
**Status**: [Status]

**Findings**:
- High availability: [Assessment]
- Fault tolerance: [Assessment]
- Disaster recovery: [Assessment]
- Monitoring: [Assessment]

**Single Points of Failure**:
- [SPOF 1]
- [SPOF 2]

---

### 5. Technology Stack
**Status**: [Status]

**Technology Choices**:
| Component | Technology | Assessment | Notes |
|-----------|------------|------------|-------|
| Backend | [Tech] | ✅ / ⚠️ / ❌ | [Notes] |
| Frontend | [Tech] | ✅ / ⚠️ / ❌ | [Notes] |
| Database | [Tech] | ✅ / ⚠️ / ❌ | [Notes] |
| Caching | [Tech] | ✅ / ⚠️ / ❌ | [Notes] |
| Message Queue | [Tech] | ✅ / ⚠️ / ❌ | [Notes] |

**Concerns**:
- [Concern 1]

---

### 6. Data Architecture
**Status**: [Status]

**Findings**:
- Data modeling: [Assessment]
- Data consistency: [Assessment]
- Data migration: [Assessment]

---

### 7. Integration Architecture
**Status**: [Status]

**Findings**:
- API design: [Assessment]
- Service communication: [Assessment]
- External integrations: [Assessment]

---

### 8. Operational Readiness
**Status**: [Status]

**Findings**:
- CI/CD: [Assessment]
- Monitoring & Logging: [Assessment]
- Deployment strategy: [Assessment]
- Documentation: [Assessment]

**Gaps**:
- [Gap 1]
- [Gap 2]

---

## Risk Assessment

### Critical Risks
| Risk | Impact | Likelihood | Mitigation |
|------|--------|------------|------------|
| [Risk 1] | High | High | [Mitigation] |

### High Priority Risks
| Risk | Impact | Likelihood | Mitigation |
|------|--------|------------|------------|
| [Risk 2] | High | Medium | [Mitigation] |

### Medium Priority Risks
[List medium priority risks]

---

## Recommendations

### Must Fix (Before Production)
1. **[Critical Issue 1]**
   - Current State: [Description]
   - Recommended Solution: [Solution]
   - Effort: [Estimate]
   - Priority: Critical

2. **[Critical Issue 2]**
   - [Details]

### Should Fix (Within 3 Months)
1. **[High Priority Issue]**
   - [Details]

### Nice to Have (Future Improvements)
1. **[Enhancement 1]**
   - [Details]

---

## Alternative Approaches Considered

### Alternative 1: [Name]
- **Description**: [Brief description]
- **Pros**: [Advantages]
- **Cons**: [Disadvantages]
- **Why Not Chosen**: [Reasoning]

---

## Cost Analysis

### Estimated Monthly Costs
| Component | Cost | Notes |
|-----------|------|-------|
| Compute | $X | [Details] |
| Database | $X | [Details] |
| Storage | $X | [Details] |
| Network | $X | [Details] |
| **Total** | **$X** | |

### Cost Optimization Opportunities
- [Opportunity 1]: Potential savings of $X/month

---

## Sign-off

### Review Team
- Lead Architect: [Name] - [Date]
- Security Architect: [Name] - [Date]
- DevOps Lead: [Name] - [Date]

### Action Items
| Item | Owner | Due Date | Status |
|------|-------|----------|--------|
| [Action 1] | [Name] | [Date] | Not Started |
| [Action 2] | [Name] | [Date] | Not Started |

---

## Appendix

### A. Architecture Diagrams
[Include or reference detailed diagrams]

### B. ADRs Reviewed
- ADR-001: [Title]
- ADR-002: [Title]

### C. References
- [Document 1]
- [Document 2]
```

## Review Severity Levels

**Critical (🔴)**
- Security vulnerabilities
- Single points of failure without mitigation
- Data loss risks
- Compliance violations
- Architecture decisions that prevent meeting requirements

**High (🟠)**
- Significant scalability limitations
- Performance bottlenecks
- Operational complexity
- Technology choices with major drawbacks
- Missing critical non-functional requirements

**Medium (🟡)**
- Sub-optimal patterns
- Missing best practices
- Documentation gaps
- Technical debt
- Cost inefficiencies

**Low (🟢)**
- Style improvements
- Future enhancements
- Nice-to-have features
- Minor optimizations

## Best Practices for Architecture Reviews

1. **Be Objective**
   - Focus on design, not people
   - Use data and evidence
   - Avoid personal preferences
   - Consider context and constraints

2. **Be Constructive**
   - Provide alternatives, not just criticism
   - Explain the "why" behind recommendations
   - Acknowledge good decisions
   - Focus on high-impact issues

3. **Be Thorough**
   - Review all aspects systematically
   - Check against requirements
   - Consider long-term implications
   - Document all findings

4. **Be Clear**
   - Use clear, specific language
   - Provide examples
   - Prioritize findings
   - Make recommendations actionable

5. **Consider Context**
   - Understand business constraints
   - Consider team capabilities
   - Factor in timeline pressures
   - Balance perfection with pragmatism

## Common Architecture Anti-Patterns

**Distributed Monolith**
- Microservices with tight coupling
- Shared database across services
- Synchronous communication everywhere
- Cannot deploy independently

**Big Ball of Mud**
- No clear structure
- High coupling
- No separation of concerns
- Difficult to maintain

**God Object/Service**
- Single component doing too much
- Too many responsibilities
- Becomes bottleneck
- Hard to scale and maintain

**Database as Integration Point**
- Multiple services sharing database
- Tight coupling through data
- Cannot evolve independently
- Scaling issues

**Chatty Communication**
- Too many fine-grained service calls
- High network overhead
- Performance issues
- Should aggregate or batch

**Premature Optimization**
- Complex solutions for non-problems
- Over-engineering
- Increased maintenance burden
- Delayed delivery

## Activation Guidelines

This skill should be activated when:
- Conducting architecture design reviews
- Validating architecture decisions
- Assessing system design before implementation
- Reviewing architecture for scalability/security concerns
- Preparing for architecture review boards
- Evaluating vendor-proposed architectures
- Analyzing existing system architectures
- Planning architecture modernization
- Assessing cloud migration architectures
- Reviewing microservices decomposition

The skill provides the most value when given complete architecture documentation including diagrams, ADRs, technical specs, and non-functional requirements.
