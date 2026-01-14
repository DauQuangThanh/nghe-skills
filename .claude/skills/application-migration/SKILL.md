---
name: application-migration
description: Guides comprehensive application migration projects including legacy system modernization, cloud migration, technology stack upgrades, database migration, and architecture transformation. Covers assessment, planning, migration strategies (strangler fig, big bang, phased), risk management, data migration, testing, and cutover. Use when migrating applications, modernizing legacy systems, moving to cloud, changing technology stacks, or when users mention "migration", "modernization", "replatform", "lift and shift", "refactor", "strangler pattern", or "legacy transformation".
---

# Application Migration

## Overview

This skill provides comprehensive guidance for planning and executing application migration projects, from legacy system modernization to cloud migration and technology stack transformation. It covers proven strategies, risk mitigation, and best practices for successful migrations.

## Migration Assessment

### 1. Current State Analysis

**Inventory Existing Systems:**

```markdown
Application: Legacy CRM System
Technology Stack:
- Language: COBOL, JCL
- Database: DB2 on mainframe
- Infrastructure: IBM z/OS mainframe
- Integration: Batch file transfers, MQ
- Users: 500 concurrent, 2000 total
- Data Volume: 5TB
- Transaction Volume: 10K/day

Business Criticality: High
Uptime Requirement: 99.9%
Compliance: PCI-DSS, SOX
```

**Assess Application Characteristics:**

- **Architecture**: Monolith, microservices, n-tier, batch processing
- **Dependencies**: External systems, APIs, databases, file shares
- **Data Complexity**: Schema complexity, data volume, referential integrity
- **Customizations**: Extent of custom code vs. standard functionality
- **Integration Points**: Number and complexity of integrations
- **Technical Debt**: Code quality, outdated frameworks, security issues
- **Documentation**: Quality and completeness of existing docs

**Identify Migration Drivers:**

- **Cost Reduction**: Reduce licensing, infrastructure, or maintenance costs
- **Performance**: Improve response times, throughput, scalability
- **Modernization**: Adopt modern tech stack, architecture patterns
- **Cloud Benefits**: Scalability, reliability, geographic distribution
- **Compliance**: Meet new regulatory requirements
- **End of Support**: Vendor discontinuing platform/language
- **Business Agility**: Enable faster feature delivery

### 2. Target State Definition

**Define Target Architecture:**

```markdown
Target: Cloud-Native CRM

Technology Stack:
- Frontend: React, TypeScript
- Backend: Node.js microservices, Java Spring Boot
- Database: PostgreSQL (Aurora), MongoDB (Atlas)
- Infrastructure: AWS (ECS, Lambda, RDS)
- Integration: REST APIs, Event-driven (Kafka)
- Authentication: Auth0, SSO
- Monitoring: CloudWatch, DataDog

Architecture Pattern: Microservices with API Gateway
Deployment: Containers (ECS), Serverless (Lambda)
Data Strategy: Polyglot persistence
```

**Success Criteria:**

- **Performance**: 95th percentile response time < 200ms
- **Availability**: 99.95% uptime SLA
- **Scalability**: Support 2x current load without degradation
- **Cost**: Reduce total cost of ownership by 40%
- **Time-to-Market**: Reduce feature delivery from months to weeks
- **Security**: Pass security audit, achieve SOC 2 compliance

### 3. Gap Analysis

**Technical Gaps:**

```markdown
| Capability | Current | Target | Gap | Priority |
|------------|---------|--------|-----|----------|
| API Layer | None | REST/GraphQL | Need to build | High |
| Authentication | Custom | OAuth 2.0/SAML | Need integration | High |
| Monitoring | Basic logs | APM, distributed tracing | Tooling needed | Medium |
| CI/CD | Manual | Automated pipelines | DevOps setup | High |
| Testing | Manual | Automated test suite | Test automation | Medium |
| Database | DB2 | PostgreSQL | Migration + conversion | High |
```

**Skill Gaps:**

- Current team: COBOL, mainframe
- Target needs: JavaScript/Node.js, React, AWS, containers
- Training required: 3-6 months for upskilling
- Hiring needs: 2 senior cloud engineers, 1 DevOps engineer

## Migration Strategies

### 1. Rehost (Lift and Shift)

**When to Use:**
- Quick migration needed
- Minimal changes acceptable
- Infrastructure cost reduction primary goal
- Low risk tolerance

**Approach:**
```markdown
Steps:
1. Provision equivalent cloud infrastructure
2. Set up networking and security
3. Migrate applications as-is
4. Migrate data with minimal transformation
5. Update DNS and routing
6. Verify functionality

Example: Move VM-based app to AWS EC2
- Same OS, same runtime, same configuration
- Benefits: Fast, low risk
- Drawbacks: Doesn't leverage cloud benefits
```

**Effort**: Low | **Risk**: Low | **Value**: Low

### 2. Replatform (Lift and Reshape)

**When to Use:**
- Want some cloud benefits without full rewrite
- Database or runtime modernization beneficial
- Balanced approach needed

**Approach:**
```markdown
Steps:
1. Identify platform upgrades (e.g., DB2 → PostgreSQL)
2. Update configurations for target platform
3. Modify data layer for new database
4. Test compatibility
5. Migrate and validate

Example: Migrate app to managed services
- Keep application code mostly unchanged
- Move database to RDS or Aurora
- Use managed services (Redis, S3)
- Benefits: Better scalability, reduced ops overhead
- Drawbacks: Some application changes needed
```

**Effort**: Medium | **Risk**: Medium | **Value**: Medium

### 3. Refactor (Re-architect)

**When to Use:**
- Significant business value from modernization
- Current architecture limiting business
- Long-term investment warranted

**Approach:**
```markdown
Steps:
1. Design target microservices architecture
2. Identify service boundaries
3. Rewrite services incrementally
4. Implement API contracts
5. Migrate data to appropriate stores
6. Decompose monolith gradually

Example: Monolith to microservices
- Break into bounded contexts
- Build new services with modern stack
- Use API gateway for routing
- Benefits: Scalability, independent deployment, tech flexibility
- Drawbacks: High effort, complex, long timeline
```

**Effort**: High | **Risk**: High | **Value**: High

### 4. Strangler Fig Pattern (Recommended)

**When to Use:**
- Minimize risk during large migrations
- Maintain business continuity essential
- Gradual migration preferred

**Approach:**
```markdown
Phase 1: Setup Infrastructure
- Deploy API gateway/proxy
- Route all traffic through proxy
- Legacy system continues serving requests

Phase 2: Incremental Replacement
- Identify high-value, low-risk functionality
- Build new service for that functionality
- Route specific requests to new service
- Legacy handles remaining requests

Phase 3: Gradual Migration
- Continue replacing functionality piece by piece
- Data synchronized between old and new
- Monitor and validate each migration

Phase 4: Decommission Legacy
- When all functionality migrated
- Redirect all traffic to new services
- Decommission legacy system

Example: E-commerce Migration
Week 1-4: Product catalog service (read-only)
Week 5-8: Product search service
Week 9-12: Shopping cart service
Week 13-16: Order processing service
Week 17-20: Payment processing
Week 21-24: Legacy decommissioned
```

**Effort**: High | **Risk**: Low | **Value**: High | **Recommended**: ✓

### 5. Big Bang Migration

**When to Use:**
- Small, simple application
- Short maintenance window acceptable
- Testing fully validates migration

**Approach:**
```markdown
Preparation:
- Build complete target system
- Migrate and validate all data
- Test thoroughly in staging

Cutover:
- Schedule maintenance window
- Final data sync
- Switch DNS/routing to new system
- Rollback plan ready

Example: Weekend cutover
Fri 6pm: Begin final data migration
Sat 2am: Complete data sync
Sat 4am: Switch traffic to new system
Sat 6am: Validation complete
Mon 8am: Users on new system
```

**Effort**: Medium | **Risk**: Very High | **Value**: Medium | **Use Sparingly**

## Migration Planning

### 1. Create Migration Roadmap

**Phased Approach:**

```markdown
Phase 1: Foundation (Months 1-3)
- Set up cloud infrastructure
- Establish CI/CD pipelines
- Deploy monitoring and logging
- Create API gateway
- Migrate reference data
- Build authentication service
- Deliverable: Core infrastructure ready

Phase 2: Read-Only Services (Months 4-6)
- Migrate product catalog (read)
- Migrate customer profiles (read)
- Migrate reporting/analytics
- Keep writes to legacy system
- Deliverable: 30% traffic on new platform

Phase 3: Write Services (Months 7-9)
- Migrate customer updates
- Migrate order creation
- Migrate inventory management
- Bi-directional data sync
- Deliverable: 60% traffic on new platform

Phase 4: Complex Workflows (Months 10-12)
- Migrate payment processing
- Migrate fulfillment workflows
- Migrate integrations
- Deliverable: 90% traffic on new platform

Phase 5: Decommission (Month 13)
- Final data migration
- Legacy system retired
- Deliverable: 100% on new platform
```

### 2. Data Migration Strategy

**Data Migration Phases:**

```markdown
Phase 1: Data Analysis
- Profile source data (quality, volume, structure)
- Map source to target schema
- Identify transformations needed
- Estimate migration duration

Phase 2: Data Cleansing
- Remove duplicates
- Fix data quality issues
- Standardize formats
- Archive obsolete data

Phase 3: Initial Load
- Migrate historical data
- Validate data integrity
- Run reconciliation reports
- Fix discrepancies

Phase 4: Delta Sync
- Replicate ongoing changes
- Minimize cutover data lag
- Use CDC (Change Data Capture) or batch sync

Phase 5: Final Cutover
- Final data sync
- Validate completeness
- Lock source system
- Switch to target system
```

**Data Transformation Example:**

```sql
-- Legacy: Single customer table with embedded address
-- Target: Normalized customer and address tables

-- Transformation Logic
INSERT INTO customers (id, first_name, last_name, email)
SELECT 
  customer_id,
  SUBSTRING(customer_name, 1, POSITION(' ' IN customer_name)-1) as first_name,
  SUBSTRING(customer_name, POSITION(' ' IN customer_name)+1) as last_name,
  customer_email
FROM legacy_customers
WHERE active_flag = 'Y';

INSERT INTO addresses (customer_id, street, city, state, zip)
SELECT
  customer_id,
  customer_address,
  customer_city,
  customer_state,
  customer_zip
FROM legacy_customers
WHERE active_flag = 'Y'
  AND customer_address IS NOT NULL;
```

### 3. Testing Strategy

**Test Types:**

```markdown
1. Unit Tests
- Test individual services
- Mock external dependencies
- Target: >80% code coverage

2. Integration Tests
- Test service interactions
- Test API contracts
- Test database operations
- Validate data transformations

3. End-to-End Tests
- Test complete business workflows
- User journey validation
- Cross-service scenarios

4. Performance Tests
- Load testing: Expected volume
- Stress testing: 2x expected volume
- Soak testing: Sustained load over time
- Target: Meet performance SLAs

5. Compatibility Tests
- Legacy system integration
- Third-party API compatibility
- Browser/device compatibility

6. Data Validation Tests
- Record count reconciliation
- Data integrity checks
- Business rule validation
- Before/after comparison

7. User Acceptance Testing
- Real users test real scenarios
- Validate business processes
- Identify usability issues
```

**Test Data Strategy:**

```markdown
Production Copy:
- Full copy of production data (anonymized)
- Use: Final validation, performance testing
- Refresh: Weekly during migration

Synthetic Data:
- Generated test data
- Use: Development, integration testing
- Volume: 10% of production

Subset:
- Representative sample from production
- Use: Functional testing, debugging
- Size: 1000 customers, 10K orders
```

### 4. Risk Management

**Common Migration Risks:**

```markdown
Risk: Data Loss During Migration
Probability: Medium | Impact: Critical
Mitigation:
- Multiple backups before cutover
- Incremental migration with checkpoints
- Automated data validation
- Rollback procedures tested
Contingency: Restore from backup, revert to legacy

Risk: Performance Degradation
Probability: High | Impact: High
Mitigation:
- Load testing before cutover
- Gradual traffic increase
- Auto-scaling configured
- Performance monitoring active
Contingency: Roll back traffic to legacy, optimize

Risk: Integration Failures
Probability: Medium | Impact: High
Mitigation:
- Test all integrations in staging
- Keep legacy integration active during transition
- Circuit breakers implemented
Contingency: Fallback to legacy integrations

Risk: User Adoption Issues
Probability: Medium | Impact: Medium
Mitigation:
- Training before launch
- Documentation prepared
- Support team ready
- Gradual user migration
Contingency: Extended dual-running period

Risk: Extended Downtime
Probability: Low | Impact: Critical
Mitigation:
- Practice cutover in staging
- Detailed runbook
- Rollback plan ready
- 24/7 team during cutover
Contingency: Rollback to legacy system
```

## Execution Playbook

### 1. Pre-Migration Checklist

```markdown
Infrastructure:
- [ ] Target environment provisioned
- [ ] Network connectivity configured
- [ ] Security groups and firewalls configured
- [ ] SSL certificates installed
- [ ] DNS records prepared (not activated)
- [ ] Load balancers configured

Application:
- [ ] Code deployed to target environment
- [ ] Configuration externalized
- [ ] Environment variables set
- [ ] Database migrations tested
- [ ] Static assets uploaded (S3, CDN)

Data:
- [ ] Initial data load completed
- [ ] Data validation passed
- [ ] Delta sync process tested
- [ ] Final sync runbook prepared

Testing:
- [ ] All test phases completed
- [ ] Performance benchmarks met
- [ ] UAT sign-off received
- [ ] Security scan passed

Operations:
- [ ] Monitoring dashboards created
- [ ] Alerts configured
- [ ] Runbooks documented
- [ ] Support team trained
- [ ] Escalation paths defined

Communication:
- [ ] Stakeholders notified
- [ ] Users informed of changes
- [ ] Support documentation published
- [ ] Training completed

Rollback:
- [ ] Rollback procedure documented
- [ ] Rollback tested in staging
- [ ] Decision criteria defined
- [ ] Rollback team identified
```

### 2. Cutover Execution

**Cutover Runbook Template:**

```markdown
Migration Date: [Date]
Start Time: [Time]
Expected Duration: [Hours]
Go/No-Go Decision Time: [Time]

Team Members:
- Migration Lead: [Name]
- Technical Lead: [Name]
- DBA: [Name]
- Network Engineer: [Name]
- Application Support: [Name]
- Business Owner: [Name]

T-24h: Final Go/No-Go Decision
- [ ] Review system health
- [ ] Confirm all prerequisites met
- [ ] Verify team availability

T-4h: Begin Cutover
- [ ] Announce maintenance window
- [ ] Redirect users to maintenance page
- [ ] Stop background jobs
- [ ] Create final backup
- [ ] Begin final data sync

T-2h: Application Deployment
- [ ] Deploy new application
- [ ] Run database migrations
- [ ] Verify deployment successful
- [ ] Run smoke tests

T-1h: Data Validation
- [ ] Compare record counts
- [ ] Validate critical data
- [ ] Run reconciliation reports
- [ ] Resolve discrepancies

T-30m: Final Checks
- [ ] Health checks passing
- [ ] All services running
- [ ] Logs clean
- [ ] Performance metrics normal

T-15m: Traffic Switch
- [ ] Update DNS records
- [ ] Update load balancer
- [ ] Monitor traffic flow
- [ ] Verify requests succeeding

T-0h: Go Live
- [ ] Announce system available
- [ ] Monitor actively for 4 hours
- [ ] Document any issues
- [ ] Collect feedback

T+4h: Post-Launch Review
- [ ] Verify all functionality
- [ ] Review error rates
- [ ] Check performance metrics
- [ ] Confirm integrations working
- [ ] Declare success or initiate rollback
```

### 3. Rollback Procedure

**When to Rollback:**
- Critical functionality not working
- Data corruption detected
- Performance below acceptable thresholds
- Security vulnerability discovered
- Cannot resolve issue within cutover window

**Rollback Steps:**

```markdown
1. Declare Rollback Decision (5 minutes)
   - Migration Lead makes decision
   - Notify all stakeholders
   - Begin rollback procedure

2. Redirect Traffic (10 minutes)
   - Update DNS to legacy system
   - Update load balancer rules
   - Stop new application

3. Verify Legacy System (15 minutes)
   - Check legacy system health
   - Verify data synchronized back
   - Test critical functions
   - Confirm users can access

4. Communication (Ongoing)
   - Notify users of restoration
   - Inform stakeholders
   - Document rollback reason

5. Post-Rollback Analysis (24 hours)
   - Root cause analysis
   - Update migration plan
   - Set new cutover date
```

## Post-Migration Activities

### 1. Validation and Stabilization

**Immediate (First 24 Hours):**
```markdown
- Monitor error rates continuously
- Watch performance metrics
- Track user feedback
- Address critical issues immediately
- Keep rollback option available
- Extra support staff on duty
```

**First Week:**
```markdown
- Daily review of metrics
- Resolve high-priority issues
- Fine-tune performance
- Adjust scaling policies
- Update documentation
- Gather user feedback
```

**First Month:**
```markdown
- Weekly reviews
- Optimize costs
- Address medium-priority issues
- Complete training gaps
- Refine operational procedures
- Plan next phase (if phased migration)
```

### 2. Decommissioning Legacy

**Decommission Checklist:**

```markdown
Phase 1: Parallel Running (30-90 days)
- [ ] Keep legacy system operational
- [ ] Monitor new system stability
- [ ] Validate all functionality migrated
- [ ] Confirm no legacy dependencies

Phase 2: Archive Legacy Data
- [ ] Export legacy data for compliance
- [ ] Store in accessible format
- [ ] Document retention period
- [ ] Test data retrieval

Phase 3: Shutdown Legacy Infrastructure
- [ ] Cancel licenses
- [ ] Terminate servers
- [ ] Remove integrations
- [ ] Update network configs
- [ ] Document decommission date

Phase 4: Cost Validation
- [ ] Verify license cancellations
- [ ] Confirm infrastructure terminated
- [ ] Calculate actual cost savings
- [ ] Report to stakeholders
```

### 3. Knowledge Transfer

```markdown
Documentation:
- Architecture diagrams
- API documentation
- Database schemas
- Deployment procedures
- Troubleshooting guides
- Operational runbooks

Training:
- Development team training
- Operations team training
- Support team training
- Business user training

Handover:
- Code repository access
- Infrastructure access
- Monitoring dashboard access
- Support tool access
- Contact information
```

## Best Practices

### Do's

✅ **Start Small**: Pilot with non-critical functionality first
✅ **Automate**: Infrastructure, deployment, testing, data migration
✅ **Monitor Continuously**: Metrics, logs, user feedback
✅ **Test Thoroughly**: All test types, especially performance and data validation
✅ **Communicate Often**: Keep stakeholders informed throughout
✅ **Plan for Rollback**: Always have a backup plan
✅ **Document Everything**: Architecture, procedures, decisions
✅ **Involve Users Early**: Get feedback during UAT
✅ **Use Feature Flags**: Enable gradual rollout and easy rollback
✅ **Celebrate Milestones**: Recognize team achievements

### Don'ts

❌ **Don't Rush**: Allow adequate time for testing and validation
❌ **Don't Skip Testing**: Every shortcut increases risk
❌ **Don't Migrate Everything**: Retire unused functionality
❌ **Don't Over-Engineer**: Start simple, add complexity as needed
❌ **Don't Ignore Data Quality**: Fix issues before migration
❌ **Don't Forget Compliance**: Security, privacy, regulatory requirements
❌ **Don't Underestimate Complexity**: Build in buffer for unknowns
❌ **Don't Neglect Training**: Users and support team need preparation
❌ **Don't Lose Legacy Knowledge**: Document before experts leave
❌ **Don't Declare Victory Too Early**: Stabilization takes time

## Migration Patterns by Application Type

### Mainframe Applications

```markdown
Challenges:
- COBOL, JCL, CICS code
- Tightly coupled architecture
- Batch processing dependencies
- Complex business logic

Strategy:
- Strangler fig pattern
- Extract business logic to services
- Replace batch with event-driven
- Use messaging for integration
- Maintain data sync during transition

Tools:
- Micro Focus for COBOL compilation
- AWS Mainframe Modernization
- Data replication tools (Precisely, Qlik)
```

### Monolithic Web Applications

```markdown
Challenges:
- Single deployment unit
- Shared database
- Tight coupling

Strategy:
- Identify bounded contexts
- Extract services incrementally
- Use API gateway for routing
- Decompose database gradually

Example Sequence:
1. Extract authentication service
2. Extract notification service
3. Extract reporting service
4. Extract core business services
5. Decompose remaining monolith
```

### On-Premise to Cloud

```markdown
Phases:
1. Rehost (Lift and Shift)
   - Quick migration to cloud VMs
   
2. Replatform
   - Use managed databases (RDS)
   - Use object storage (S3)
   - Use managed caching (ElastiCache)
   
3. Refactor
   - Containerize applications
   - Adopt serverless where appropriate
   - Implement auto-scaling
   
4. Optimize
   - Right-size resources
   - Implement cost management
   - Optimize performance
```

## Metrics and Success Criteria

**Track Throughout Migration:**

```markdown
Technical Metrics:
- Error rate: < 0.1%
- Response time: 95th percentile < target
- Availability: Meet SLA (e.g., 99.9%)
- Throughput: Handle expected load
- Resource utilization: CPU, memory, database

Business Metrics:
- User adoption rate
- Feature usage
- Customer satisfaction (CSAT, NPS)
- Revenue impact
- Cost savings

Project Metrics:
- On-time delivery
- Budget adherence
- Scope control
- Issue resolution time
- Team velocity
```

## Notes

- Every migration is unique; adapt these guidelines to your context
- Start with a pilot to validate approach and build confidence
- Prioritize business continuity and risk mitigation
- Invest in automation to reduce manual errors and speed up process
- Plan for 1.5-2x your initial estimate; migrations often take longer
- Keep stakeholders informed throughout the journey
