---
name: project-planning
description: Guides comprehensive software project planning including task breakdown, estimation, sprint planning, backlog management, resource allocation, milestone tracking, and risk management. Creates user stories, acceptance criteria, project roadmaps, and work breakdown structures. Use when planning software projects, organizing sprints, breaking down features into tasks, estimating effort, managing backlogs, or when users mention "project planning", "sprint planning", "task breakdown", "roadmap", "backlog management", "user stories", or "project estimation".
---

# Software Project Planning and Task Management

## Overview

This skill provides comprehensive guidance for planning and managing software projects using industry best practices. It covers breaking down requirements into manageable tasks, estimating effort, organizing sprints, managing backlogs, and tracking progress effectively.

## Core Planning Workflow

### 1. Initial Project Setup

**Define Project Scope:**
- **Vision Statement**: Clear project purpose and objectives
- **Success Criteria**: Measurable outcomes that define project success
- **Stakeholders**: Identify all stakeholders and their roles
- **Constraints**: Budget, timeline, resources, technical limitations
- **Assumptions**: Document all assumptions being made

**Example Vision Statement:**
```
Build a customer relationship management (CRM) system that enables 
sales teams to track leads, manage customer interactions, and generate 
reports. Success means 100 active users within 3 months and 30% 
improvement in lead conversion rates.
```

**Identify Deliverables:**
- Core features and functionality
- Documentation requirements
- Training materials
- Deployment artifacts
- Support handover materials

### 2. Requirements Breakdown

**Transform Requirements into Epics:**

Organize high-level requirements into epics (large bodies of work):

```markdown
Epic: User Management
Description: Complete user authentication, authorization, and profile management
Business Value: Enables secure access and personalized experience
Estimated Effort: 3-4 sprints

Epic: Lead Tracking
Description: Capture, track, and manage sales leads through pipeline
Business Value: Core CRM functionality for sales team
Estimated Effort: 5-6 sprints
```

**Break Epics into User Stories:**

Use the format: "As a [role], I want [feature] so that [benefit]"

```markdown
**User Story**: User Registration
As a new user
I want to create an account with email and password
So that I can access the CRM system

Acceptance Criteria:
- User can enter email, password, and confirm password
- Email validation ensures proper format
- Password must be 8+ characters with 1 uppercase, 1 lowercase, 1 number
- System sends verification email
- User receives confirmation message
- Duplicate emails are rejected with clear error

Priority: High
Estimated Effort: 5 story points
Dependencies: None
```

**Create Technical Tasks:**

Break user stories into implementation tasks:

```markdown
Story: User Registration

Tasks:
1. Design database schema for user table (2h)
   - Email, password hash, created_at, verified fields
   - Add unique constraint on email
   
2. Implement backend API endpoint POST /api/auth/register (4h)
   - Input validation
   - Password hashing
   - Database insert
   - Error handling
   
3. Create email verification service (3h)
   - Generate verification token
   - Send email via provider
   - Token expiry logic
   
4. Build frontend registration form (4h)
   - Form validation
   - Password strength indicator
   - Error display
   - Success confirmation
   
5. Write unit tests (3h)
6. Write integration tests (2h)
7. Update API documentation (1h)

Total Estimate: 19 hours (~2.5 days)
```

### 3. Estimation Techniques

**Story Point Estimation:**

Use relative sizing (Fibonacci: 1, 2, 3, 5, 8, 13, 21):

- **1 point**: Trivial change, < 2 hours, no complexity
- **2 points**: Simple task, 2-4 hours, well understood
- **3 points**: Small feature, 4-8 hours, some complexity
- **5 points**: Medium feature, 1-2 days, moderate complexity
- **8 points**: Large feature, 2-3 days, significant complexity
- **13 points**: Very large, 3-5 days, high complexity (consider splitting)
- **21+ points**: Too large, must be broken down

**Planning Poker Process:**

1. Present user story to team
2. Discuss requirements and acceptance criteria
3. Each team member privately selects estimate
4. Reveal estimates simultaneously
5. Discuss differences (especially outliers)
6. Re-estimate until consensus

**T-Shirt Sizing (High-Level):**

For early estimation:
- **XS**: 1-2 days
- **S**: 3-5 days
- **M**: 1-2 weeks
- **L**: 2-4 weeks
- **XL**: 1-2 months (break down further)

**Three-Point Estimation:**

For critical or uncertain tasks:

```
Optimistic (O): Best-case scenario
Most Likely (M): Expected duration
Pessimistic (P): Worst-case scenario

Expected Time = (O + 4M + P) / 6

Example:
O = 2 days, M = 4 days, P = 10 days
Expected = (2 + 16 + 10) / 6 = 4.67 days
```

### 4. Sprint Planning

**Sprint Structure:**

```markdown
Sprint: Sprint 15
Duration: 2 weeks (Jan 15 - Jan 28)
Team Capacity: 80 story points (4 developers × 20 points each)
Sprint Goal: Complete user authentication and basic profile management

Stories Committed:
1. User Registration (5 pts) - High Priority
2. User Login (3 pts) - High Priority
3. Password Reset (5 pts) - High Priority
4. Email Verification (3 pts) - High Priority
5. User Profile View (5 pts) - Medium Priority
6. Profile Edit (8 pts) - Medium Priority

Total: 29 story points
Buffer: 51 story points remaining for discoveries and bugs
```

**Calculate Team Velocity:**

Track completed story points per sprint:

```
Sprint 12: 32 points
Sprint 13: 28 points
Sprint 14: 35 points

Average Velocity: 31.7 points
Use for planning: ~30 points per sprint
```

**Sprint Ceremonies:**

1. **Sprint Planning** (4 hours for 2-week sprint)
   - Review and refine backlog items
   - Select stories for sprint
   - Break stories into tasks
   - Assign ownership

2. **Daily Standup** (15 minutes)
   - What did I complete yesterday?
   - What will I work on today?
   - Any blockers?

3. **Sprint Review** (2 hours)
   - Demo completed work to stakeholders
   - Gather feedback
   - Update product backlog

4. **Sprint Retrospective** (1.5 hours)
   - What went well?
   - What could improve?
   - Action items for next sprint

### 5. Backlog Management

**Backlog Structure:**

```markdown
Product Backlog (Prioritized):

## Now (Sprint Ready)
- [ ] User Registration (5 pts) - Acceptance criteria defined
- [ ] User Login (3 pts) - Design mockups approved
- [ ] Password Reset (5 pts) - API contract agreed

## Next (2-3 sprints)
- [ ] Role-Based Access Control (13 pts) - Needs refinement
- [ ] Lead Import (8 pts) - Requirements gathering
- [ ] Dashboard Overview (8 pts) - Design in progress

## Later (Future)
- [ ] Advanced Reporting (21 pts) - High-level requirement only
- [ ] Mobile App (XL) - Needs discovery
- [ ] Integration with external CRM (?) - Spike needed

## Backlog (Not prioritized)
- [ ] Dark mode support
- [ ] Email templates customization
- [ ] Audit logging
```

**Definition of Ready (DoR):**

Story is ready for sprint when it has:
- [ ] Clear user story format
- [ ] Acceptance criteria defined
- [ ] Dependencies identified and resolved
- [ ] Estimated by the team
- [ ] Design mockups (if UI work)
- [ ] Technical approach discussed
- [ ] Small enough to complete in one sprint

**Definition of Done (DoD):**

Story is complete when:
- [ ] Code written and reviewed
- [ ] Unit tests written (>80% coverage)
- [ ] Integration tests written
- [ ] Acceptance criteria validated
- [ ] Documentation updated
- [ ] No critical bugs
- [ ] Deployed to staging environment
- [ ] Accepted by Product Owner

### 6. Prioritization Frameworks

**MoSCoW Method:**

- **Must Have**: Critical, project fails without it
- **Should Have**: Important, significant value but not critical
- **Could Have**: Nice to have, improves user experience
- **Won't Have**: Not in this release, but maybe future

**Value vs. Effort Matrix:**

```
High Value, Low Effort  → Do First (Quick Wins)
High Value, High Effort → Do Next (Major Projects)
Low Value, Low Effort   → Do Later (Fill-ins)
Low Value, High Effort  → Don't Do (Time Wasters)
```

**RICE Scoring:**

```
RICE = (Reach × Impact × Confidence) / Effort

Reach: Number of users affected per time period
Impact: 0.25 (minimal), 0.5 (low), 1 (medium), 2 (high), 3 (massive)
Confidence: 0-100% (how certain are estimates)
Effort: Person-months required

Example:
Feature A: (1000 × 2 × 80%) / 2 = 800
Feature B: (500 × 3 × 60%) / 1 = 900

Feature B has higher RICE score → prioritize first
```

**Kano Model:**

- **Basic**: Expected features, dissatisfaction if missing
- **Performance**: Satisfaction increases with quality
- **Excitement**: Unexpected delighters, differentiation
- **Indifferent**: Users don't care either way
- **Reverse**: Some users prefer absence

### 7. Resource Allocation

**Team Capacity Planning:**

```markdown
Sprint 15 Capacity Analysis:

Developer 1 (Senior): 
- Available: 10 days (80 hours)
- Meetings: 10 hours
- Code reviews: 5 hours
- Technical debt: 5 hours
- Development capacity: 60 hours (30 points)

Developer 2 (Mid-level):
- Available: 9 days (72 hours) - 1 day PTO
- Meetings: 10 hours
- Development capacity: 62 hours (25 points)

Developer 3 (Junior):
- Available: 10 days (80 hours)
- Meetings: 10 hours
- Mentoring: 8 hours
- Development capacity: 62 hours (20 points)

Total Team Capacity: 75 story points

Allocation:
- Committed work: 60 points (80%)
- Buffer for bugs/discoveries: 15 points (20%)
```

**Skill Matrix Mapping:**

```markdown
Feature: Payment Integration

Required Skills:
- Backend (Node.js): 16 hours → Developer 1, 2
- Frontend (React): 8 hours → Developer 3
- DevOps (CI/CD): 4 hours → Developer 1
- Testing (E2E): 4 hours → Developer 2

Assignments:
- Developer 1: Backend API + DevOps setup
- Developer 2: Backend integration + Testing
- Developer 3: Frontend UI components
```

### 8. Risk Management

**Risk Identification:**

```markdown
Risk Register:

## Technical Risks

Risk: Third-party API has rate limits
Probability: High | Impact: Medium
Mitigation: Implement caching, request batching
Contingency: Build fallback to alternative provider
Owner: Lead Developer

Risk: Database scalability under load
Probability: Medium | Impact: High
Mitigation: Performance testing, query optimization
Contingency: Database sharding plan ready
Owner: Backend Team Lead

## Schedule Risks

Risk: Key developer vacation during critical sprint
Probability: High | Impact: Medium
Mitigation: Knowledge sharing, pair programming
Contingency: Adjust sprint scope, extend timeline
Owner: Project Manager

## External Dependencies

Risk: Design approval delays from stakeholders
Probability: Medium | Impact: Medium
Mitigation: Regular check-ins, early feedback loops
Contingency: Proceed with MVP design
Owner: Product Manager
```

**Risk Response Strategies:**

1. **Avoid**: Change plan to eliminate risk
2. **Mitigate**: Reduce probability or impact
3. **Transfer**: Outsource or insure
4. **Accept**: Acknowledge and monitor

### 9. Milestone and Timeline Planning

**Project Roadmap:**

```markdown
Q1 2026: Foundation (Jan-Mar)
- ✓ Project setup and architecture
- ✓ User authentication system
- ⏳ User management and profiles
- ⏳ Basic lead capture

Q2 2026: Core Features (Apr-Jun)
- Lead tracking and pipeline
- Contact management
- Activity logging
- Basic reporting

Q3 2026: Advanced Features (Jul-Sep)
- Email integration
- Advanced reporting
- Dashboard analytics
- Mobile responsive UI

Q4 2026: Polish and Launch (Oct-Dec)
- Performance optimization
- Security audit
- User acceptance testing
- Production launch
- Training and onboarding

Milestones:
🎯 Alpha Release: End of Q1 (Mar 31)
🎯 Beta Release: End of Q2 (Jun 30)
🎯 Production Launch: Mid Q4 (Nov 15)
```

**Gantt Chart Planning:**

```
Task                    Jan  Feb  Mar  Apr  May  Jun
─────────────────────  ────────────────────────────
Architecture           [██]
User Authentication    [████]
User Management             [███]
Lead Capture                  [██]
Lead Pipeline                      [████]
Contact Management                   [████]
Testing (ongoing)      [══════════════════════════]
Documentation                    [══════════]
```

### 10. Progress Tracking

**Burndown Chart:**

```
Story Points
100 │\
    │ \
 75 │  \___
    │      \___
 50 │          \___
    │              \___
 25 │                  \___
    │                      \___
  0 └─────────────────────────────> Days
    1   3   5   7   9   11  13

Ideal: Straight diagonal line
Actual: Track daily remaining points
```

**Cumulative Flow Diagram:**

```
Work Items
100 │
    │          [Done]
 75 │      [In Review]
    │    [In Progress]
 50 │  [Ready]
    │[Backlog]
 25 │
  0 └─────────────────────> Time

Monitor for:
- Bottlenecks (widening bands)
- Work starvation (shrinking bands)
- Healthy flow (parallel bands)
```

**Key Metrics:**

1. **Velocity**: Average story points per sprint
2. **Cycle Time**: Time from start to done
3. **Lead Time**: Time from request to delivery
4. **Throughput**: Stories completed per time period
5. **Escaped Defects**: Bugs found in production
6. **Technical Debt Ratio**: Debt work vs. feature work

## Task Management Best Practices

### Task Organization

**Task Attributes:**

```markdown
Task: Implement User Login API

ID: TASK-142
Type: Development
Status: In Progress
Priority: High
Assignee: Developer 1
Story: User Login (STORY-28)
Sprint: Sprint 15

Estimated: 4 hours
Actual: 3 hours
Remaining: 1 hour

Tags: backend, authentication, api
Blocked: No
Dependencies: TASK-141 (Database schema)

Description:
Implement POST /api/auth/login endpoint with email/password validation,
JWT token generation, and error handling.

Acceptance:
- [ ] Validates email format
- [ ] Checks password against hash
- [ ] Returns JWT token on success
- [ ] Returns appropriate errors
- [ ] Includes refresh token
- [ ] Unit tests written
```

### Dependency Management

**Dependency Types:**

1. **Finish-to-Start (FS)**: Task B can't start until Task A finishes
2. **Start-to-Start (SS)**: Task B can't start until Task A starts
3. **Finish-to-Finish (FF)**: Task B can't finish until Task A finishes
4. **Start-to-Finish (SF)**: Task B can't finish until Task A starts (rare)

**Critical Path Identification:**

```
Authentication Flow:
[DB Schema] → [API Endpoint] → [Frontend Form] → [Integration Test]
   2h            4h               4h                2h
Critical Path: 12 hours (blocks other work)

Profile Management:
[UI Mockup] → [Frontend] → [API] → [Test]
   4h          6h          3h       2h
Non-critical: Can proceed in parallel with auth
```

### Communication and Documentation

**Status Updates:**

```markdown
Weekly Status Report - Week of Jan 15

Completed This Week:
- ✅ User registration API and UI (STORY-27)
- ✅ Email verification service (STORY-29)
- ✅ Database migration for user tables

In Progress:
- ⏳ User login implementation (STORY-28) - 80% complete
- ⏳ Password reset flow (STORY-30) - Design review pending

Blocked:
- 🚫 OAuth integration (STORY-31) - Waiting for API keys from Google

Risks/Issues:
- Email service rate limits causing delays in testing
- One developer out sick, may impact sprint velocity

Next Week Plan:
- Complete login and password reset
- Begin profile management features
- Address technical debt in authentication module
```

**Decision Log:**

```markdown
Decision: Use JWT for Authentication

Date: Jan 10, 2026
Status: Approved
Participants: Tech Lead, Backend Team, Security Consultant

Context:
Need to choose authentication mechanism for API

Options Considered:
1. Session-based authentication
2. JWT tokens
3. OAuth only

Decision: JWT with refresh tokens

Reasoning:
- Stateless, scalable for microservices
- Works well for mobile and SPA
- Industry standard with good library support
- Refresh tokens mitigate security concerns

Consequences:
- Must implement token refresh logic
- Need secure storage on client side
- Will require careful key management
```

## Templates and Checklists

### Project Kickoff Checklist

- [ ] Vision and objectives defined
- [ ] Stakeholders identified
- [ ] Success criteria established
- [ ] Budget and timeline approved
- [ ] Team assembled and roles assigned
- [ ] Development environment setup
- [ ] Version control repository created
- [ ] Project management tool configured
- [ ] Communication channels established
- [ ] Initial backlog created
- [ ] First sprint planned

### Story Template

```markdown
**Title**: [Action] as [Role]

**As a** [user type]
**I want** [goal/feature]
**So that** [business value/benefit]

**Acceptance Criteria:**
- [ ] [Testable criterion 1]
- [ ] [Testable criterion 2]
- [ ] [Testable criterion 3]

**Notes:**
[Additional context, constraints, or considerations]

**Attachments:**
- Design mockup: [link]
- API contract: [link]
- Technical spike: [link]
```

### Sprint Planning Template

```markdown
Sprint: [Number]
Dates: [Start] to [End]
Sprint Goal: [One sentence objective]

Team Capacity: [X story points]

Stories:
1. [Story name] ([points]) - [Priority]
2. [Story name] ([points]) - [Priority]

Total Committed: [X points]

Risks:
- [Risk description and mitigation]

Notes:
- [Any important considerations]
```

## Integration with Development Workflow

### Continuous Planning

- **Daily**: Update task status, log time, identify blockers
- **Weekly**: Review progress, adjust priorities, update estimates
- **Bi-weekly**: Sprint planning and retrospective
- **Monthly**: Review roadmap, adjust timelines, stakeholder updates
- **Quarterly**: Strategic planning, goal setting, resource allocation

### Tool Integration

**Common Project Management Tools:**
- Jira, Azure DevOps, Linear
- GitHub Projects, GitLab Issues
- Trello, Asana, Monday.com
- Notion, Confluence

**Key Integrations:**
- Link commits to stories: `git commit -m "STORY-28: Add login validation"`
- Automated status updates from CI/CD
- Pull requests linked to tasks
- Deployment tracking per story

## Anti-Patterns to Avoid

- ❌ **Planning without team input**: Estimating and planning alone
- ❌ **Overly detailed upfront planning**: Waterfall approach in agile
- ❌ **Ignoring technical debt**: Only focusing on features
- ❌ **Unrealistic deadlines**: Pressure without regard for capacity
- ❌ **Skipping retrospectives**: Missing improvement opportunities
- ❌ **Analysis paralysis**: Over-planning instead of starting
- ❌ **No slack time**: 100% capacity utilization
- ❌ **Gold plating**: Adding unnecessary features
- ❌ **Poor story sizing**: All stories are large or undefined
- ❌ **Lack of prioritization**: Everything is high priority

## Notes

- Adapt practices to team size and project complexity
- Start simple, add complexity as needed
- Focus on delivering value, not following process
- Regular inspection and adaptation are key
- Maintain sustainable pace, avoid burnout
- Celebrate successes and learn from failures
