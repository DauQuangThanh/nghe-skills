---
name: backend-code-review
description: Conducts comprehensive backend code reviews including API design (REST/GraphQL/gRPC), database patterns, authentication/authorization, caching strategies, message queues, microservices architecture, security vulnerabilities, and performance optimization for Node.js, Python, Java, Go, and C#. Produces detailed review reports with specific issues, severity ratings, and actionable recommendations. Use when reviewing server-side code, analyzing API implementations, checking database queries, validating authentication flows, assessing microservices architecture, or when users mention "review backend code", "check API design", "analyze server code", "validate database patterns", "security audit", "performance review", or "backend code quality".
---

# Backend Code Review

## Purpose

Conduct systematic and thorough reviews of backend codebases to identify issues, improve quality, ensure security, optimize performance, and validate architectural decisions. Produces actionable reports with specific recommendations.

## When to Use This Skill

Trigger this skill when:
- Reviewing server-side code in Node.js, Python, Java, Go, C#, or other backend languages
- Analyzing API implementations (REST, GraphQL, gRPC)
- Checking database query patterns and optimizations
- Validating authentication and authorization mechanisms
- Assessing microservices architecture and design patterns
- Conducting security audits of backend systems
- Evaluating performance and scalability
- Reviewing caching strategies and message queue implementations
- User mentions backend code review, API review, server code analysis, or security audit

## Review Workflow

### Step 1: Initial Assessment

**Gather Context:**
- Technology stack (languages, frameworks, databases)
- Architecture style (monolith, microservices, serverless)
- Deployment environment (cloud provider, containerization)
- Code scope (files, modules, services to review)

**Read Project Structure:**
```bash
# Common patterns to identify
- /src or /app - Application code
- /tests - Test files
- /config - Configuration
- /docs - Documentation
- package.json, requirements.txt, pom.xml - Dependencies
- docker-compose.yml, Dockerfile - Containerization
```

**Identify Critical Areas:**
- Authentication/authorization flows
- Payment processing
- Data access layers
- External API integrations
- Background jobs/workers

### Step 2: Code Quality Analysis

**Review Focus:**
- Code organization and structure
- Design patterns and principles (SOLID, DRY, KISS)
- Error handling and logging
- Type safety (TypeScript, type hints, generics)
- Dependency injection and modularity
- Code duplication and complexity

**Detailed guidance:** See [code-quality-checklist.md](references/code-quality-checklist.md)

**Key Checks:**
- [ ] Proper separation of concerns (controllers, services, repositories)
- [ ] Consistent error handling patterns
- [ ] Appropriate use of async/await or promises
- [ ] No hardcoded secrets or credentials
- [ ] Proper use of environment variables
- [ ] Logging with appropriate levels (debug, info, warn, error)

### Step 3: API Design Review

**REST APIs:**
- Resource naming and URI structure
- HTTP method usage (GET, POST, PUT, PATCH, DELETE)
- Status code appropriateness
- Request/response formats
- Pagination and filtering
- API versioning strategy

**GraphQL:**
- Schema design and type definitions
- Query complexity and depth limiting
- N+1 query prevention (DataLoader)
- Error handling
- Authentication/authorization per field

**gRPC:**
- Protocol buffer definitions
- Service method design
- Streaming patterns (unary, server, client, bidirectional)
- Error handling with status codes

**Detailed guidance:** See [api-design-checklist.md](references/api-design-checklist.md)

### Step 4: Database Review

**Query Analysis:**
- SQL injection prevention
- Query optimization (indexes, joins, subqueries)
- N+1 query problems
- Connection pooling
- Transaction handling

**Schema Review:**
- Normalization vs denormalization decisions
- Indexing strategy
- Foreign key constraints
- Data types and sizes
- Migration strategy

**ORM/Query Builder Usage:**
- Proper use of ORM features
- Raw query safety
- Lazy vs eager loading
- Caching strategies

**Detailed guidance:** See [database-checklist.md](references/database-checklist.md)

### Step 5: Security Assessment

**Authentication:**
- Password hashing (bcrypt, argon2)
- JWT implementation (signing, expiration, refresh)
- OAuth/OIDC integration
- Session management

**Authorization:**
- Role-Based Access Control (RBAC)
- Attribute-Based Access Control (ABAC)
- Permission checking
- Resource ownership validation

**Input Validation:**
- Request validation (schema, types, ranges)
- SQL injection prevention
- NoSQL injection prevention
- Command injection prevention
- Path traversal prevention

**Data Protection:**
- Encryption at rest and in transit
- Sensitive data handling (PII, PCI)
- Secret management
- CORS configuration

**Detailed guidance:** See [security-checklist.md](references/security-checklist.md)

### Step 6: Performance Review

**Application Performance:**
- Response time analysis
- Memory usage patterns
- CPU utilization
- Asynchronous processing
- Caching strategies (Redis, Memcached)

**Database Performance:**
- Query execution plans
- Index utilization
- Connection pooling
- Query caching
- Database connection patterns

**API Performance:**
- Rate limiting
- Request batching
- Response compression
- HTTP/2 or HTTP/3 usage
- CDN integration

**Scalability:**
- Horizontal scaling readiness
- Stateless design
- Load balancing considerations
- Background job processing
- Message queue usage

**Detailed guidance:** See [performance-checklist.md](references/performance-checklist.md)

### Step 7: Architecture Review

**Design Patterns:**
- Repository pattern
- Service layer pattern
- Factory pattern
- Strategy pattern
- Dependency injection

**Microservices (if applicable):**
- Service boundaries and responsibilities
- Inter-service communication
- Data consistency patterns (Saga, Event Sourcing)
- Service discovery
- Circuit breakers and resilience

**Code Organization:**
- Layered architecture
- Clean architecture
- Hexagonal architecture
- Domain-Driven Design (DDD)

**Detailed guidance:** See [architecture-checklist.md](references/architecture-checklist.md)

### Step 8: Testing Coverage

**Unit Tests:**
- Test coverage percentage
- Critical path coverage
- Edge case testing
- Mock/stub usage
- Test independence

**Integration Tests:**
- Database integration
- External API integration
- End-to-end flow testing
- Contract testing

**API Tests:**
- Endpoint testing
- Authentication/authorization testing
- Error case testing
- Load testing

**Test Quality:**
- Test readability
- Test maintainability
- Test performance
- Flaky test identification

### Step 9: Generate Review Report

Use appropriate template from [report-templates.md](references/report-templates.md).

**Report Structure:**
1. **Executive Summary**
   - Overall assessment
   - Critical issues count
   - Priority recommendations

2. **Detailed Findings**
   - Code quality issues
   - Security vulnerabilities
   - Performance bottlenecks
   - Architecture concerns

3. **Recommendations**
   - Immediate actions (critical)
   - Short-term improvements
   - Long-term enhancements

4. **Code Examples**
   - Problematic code with location
   - Recommended fixes
   - Explanation of impact

## Best Practices

### Review Timing

**Pre-commit Review:**
- Focus on immediate issues
- Quick feedback cycle
- Automated checks

**Pull Request Review:**
- Comprehensive analysis
- Feature-level assessment
- Integration concerns

**Periodic Audit:**
- Codebase-wide patterns
- Technical debt assessment
- Architecture evolution

### Review Techniques

**Automated Scanning:**
- Linters (ESLint, Pylint, Checkstyle)
- Security scanners (Snyk, SonarQube)
- Code coverage tools
- Performance profilers

**Manual Review:**
- Business logic validation
- Design pattern appropriateness
- Edge case handling
- Security-critical sections

**Collaborative Review:**
- Pair programming insights
- Team knowledge sharing
- Architecture discussions

## Anti-Patterns to Flag

### Code Smells

- **God classes/functions** - Doing too much
- **Shotgun surgery** - Changes affect many files
- **Feature envy** - Classes using other classes' data excessively
- **Dead code** - Unused functions or classes
- **Magic numbers** - Hardcoded values without constants

### Architecture Smells

- **Circular dependencies** - Modules depending on each other
- **Big ball of mud** - No clear structure
- **Spaghetti code** - Complex control flow
- **Tight coupling** - Components too dependent
- **Hidden dependencies** - Implicit requirements

### Security Smells

- **Hardcoded credentials**
- **Weak password policies**
- **Missing input validation**
- **Insufficient logging**
- **Overly permissive CORS**

## Quick Reference

### Common Issues Checklist

**Critical (Must Fix):**
- [ ] SQL injection vulnerabilities
- [ ] Hardcoded secrets
- [ ] Authentication bypass
- [ ] Data exposure
- [ ] Memory leaks

**High Priority (Should Fix):**
- [ ] Missing input validation
- [ ] Poor error handling
- [ ] N+1 queries
- [ ] Missing indexes
- [ ] Insufficient logging

**Medium Priority (Nice to Fix):**
- [ ] Code duplication
- [ ] Inconsistent naming
- [ ] Missing tests
- [ ] TODO comments
- [ ] Unused imports

### Technology-Specific Concerns

**Node.js:**
- Callback hell / promise chains
- Unhandled promise rejections
- Blocking event loop
- Memory leaks in closures

**Python:**
- Global interpreter lock (GIL) awareness
- Exception handling patterns
- Generator usage
- Type hints usage

**Java:**
- Exception handling (checked vs unchecked)
- Resource management (try-with-resources)
- Stream API usage
- Null pointer handling

**Go:**
- Error handling patterns
- Goroutine leaks
- Channel usage
- Context propagation

**C#:**
- Async/await patterns
- IDisposable implementation
- LINQ query optimization
- Exception handling

## Additional Resources

**Detailed Checklists:**
- [Code Quality Checklist](references/code-quality-checklist.md) - Design patterns, SOLID principles, error handling
- [API Design Checklist](references/api-design-checklist.md) - REST, GraphQL, gRPC best practices
- [Database Checklist](references/database-checklist.md) - Query optimization, schema design, transactions
- [Security Checklist](references/security-checklist.md) - Authentication, authorization, input validation
- [Performance Checklist](references/performance-checklist.md) - Optimization, caching, scalability
- [Architecture Checklist](references/architecture-checklist.md) - Patterns, microservices, design principles

**Report Templates:**
- [Report Templates](references/report-templates.md) - Executive summaries, detailed reports, issue tracking

## Output Format

Structure findings as:

```markdown
# Backend Code Review Report

## Executive Summary
[Overall assessment and key findings]

## Critical Issues (Must Fix)
### Issue 1: [Title]
- **Location:** file.js:123
- **Severity:** Critical
- **Description:** [What's wrong]
- **Impact:** [Consequences]
- **Recommendation:** [How to fix]

## High Priority Issues
[Similar structure]

## Medium Priority Issues
[Similar structure]

## Code Quality Assessment
[Scores and analysis]

## Performance Analysis
[Metrics and recommendations]

## Security Assessment
[Vulnerabilities and fixes]

## Recommendations
### Immediate Actions
### Short-term Improvements
### Long-term Enhancements
```

Provide specific file locations, line numbers, code examples, and actionable recommendations for each finding.
