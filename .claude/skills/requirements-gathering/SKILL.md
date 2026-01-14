---
name: requirements-gathering
description: Guides comprehensive requirements gathering and analysis including stakeholder interviews, user story creation, use case documentation, acceptance criteria, requirements prioritization, and traceability. Produces requirements documents, user stories, use cases, and development roadmaps. Use when gathering requirements, writing user stories, creating acceptance criteria, analyzing stakeholder needs, prioritizing features, or when users mention requirements analysis, business analysis, user stories, use cases, or requirements documentation.
license: MIT
metadata:
  author: Dau Quang Thanh
  version: "1.0"
  category: development
---

# Requirements Gathering & Development Skill

This skill guides you through comprehensive requirements gathering and analysis, from initial stakeholder engagement through requirements documentation, user story creation, and development planning.

## Core Capabilities

When activated, this skill enables you to:

1. **Stakeholder Analysis & Engagement**
   - Identify and categorize stakeholders
   - Plan stakeholder interviews and workshops
   - Facilitate requirements elicitation sessions
   - Manage stakeholder expectations
   - Build stakeholder communication plans

2. **Requirements Elicitation**
   - Conduct stakeholder interviews
   - Run requirements workshops
   - Create surveys and questionnaires
   - Perform document analysis
   - Observe user workflows
   - Analyze competitive products

3. **Requirements Analysis**
   - Classify requirements (functional, non-functional)
   - Identify gaps and conflicts
   - Prioritize requirements (MoSCoW, RICE, Kano)
   - Analyze feasibility and impact
   - Define acceptance criteria
   - Create requirements traceability matrix

4. **User Stories & Use Cases**
   - Write effective user stories
   - Create detailed use cases
   - Define personas and user journeys
   - Document user workflows
   - Create story maps
   - Define epics and features

5. **Requirements Documentation**
   - Create Business Requirements Documents (BRD)
   - Write Functional Specifications
   - Document technical requirements
   - Create Software Requirements Specifications (SRS)
   - Maintain requirements repositories
   - Version control requirements

6. **Agile Requirements Management**
   - Manage product backlogs
   - Conduct backlog refinement
   - Write acceptance criteria
   - Estimate user stories
   - Plan sprints and releases
   - Track requirements changes

## Requirements Gathering Process

Follow this systematic approach for gathering and managing requirements:

### Phase 1: Project Initiation & Stakeholder Analysis

**1. Project Context Analysis**

```markdown
# Project Context Document

## Project Overview
- **Project Name**: [Name]
- **Business Problem**: [What problem are we solving?]
- **Project Goals**: [What are we trying to achieve?]
- **Success Criteria**: [How will we measure success?]
- **Constraints**: [Budget, timeline, resources, technology]
- **Assumptions**: [What are we assuming to be true?]
- **Risks**: [What could go wrong?]

## Business Case
- **Business Objectives**: [Align with organizational goals]
- **Expected Benefits**: [ROI, cost savings, efficiency gains]
- **Strategic Alignment**: [How does this fit company strategy?]
- **Market Opportunity**: [Market size, competition, timing]

## Project Scope
**In Scope**:
- Feature A
- Feature B
- Integration with System X

**Out of Scope**:
- Feature C (future phase)
- Integration with System Y
- Mobile app (separate project)

## High-Level Timeline
- Discovery: 2 weeks
- Design: 4 weeks
- Development: 12 weeks
- Testing: 3 weeks
- Deployment: 1 week
```

**2. Stakeholder Identification & Analysis**

```markdown
# Stakeholder Analysis Matrix

| Stakeholder | Role | Interest | Influence | Engagement Strategy |
|------------|------|----------|-----------|---------------------|
| CEO | Executive Sponsor | High | High | Weekly status updates, quarterly reviews |
| CTO | Technical Approver | High | High | Architecture reviews, technical decisions |
| VP Sales | End User Champion | High | Medium | Demo sessions, feedback loops |
| Sales Team | Primary Users | High | Low | User testing, training sessions |
| IT Operations | System Support | Medium | Medium | Technical requirements, deployment planning |
| Customers | External Users | High | Medium | User research, beta testing |
| Marketing | Secondary Users | Medium | Low | Feature input, launch planning |

## Stakeholder Categories

**Power/Interest Grid**:
```
High Power │ MANAGE CLOSELY        │ KEEP SATISFIED
           │ CEO, CTO              │ CFO, Legal
           │                       │
───────────┼───────────────────────┼─────────────────
Low Power  │ KEEP INFORMED         │ MONITOR
           │ Sales Team, Support   │ External Vendors
           │                       │
           Low Interest           High Interest
```

**Engagement Plan**:
1. **Manage Closely** (High Power/High Interest)
   - Regular one-on-one meetings
   - Involved in key decisions
   - Early visibility of issues

2. **Keep Satisfied** (High Power/Low Interest)
   - Periodic status reports
   - Informed of major milestones
   - Escalation point for issues

3. **Keep Informed** (Low Power/High Interest)
   - Regular communications
   - Opportunities for feedback
   - User testing participants

4. **Monitor** (Low Power/Low Interest)
   - Minimal communication
   - Standard project updates
   - Available documentation
```

**3. Stakeholder Interview Planning**

```markdown
# Interview Guide Template

## Interview Details
- **Stakeholder**: [Name, Role]
- **Date/Time**: [Schedule]
- **Duration**: 60 minutes
- **Method**: [In-person, Video call, Phone]
- **Attendees**: [BA, PM, others]

## Interview Objectives
1. Understand current workflow and pain points
2. Identify desired features and functionality
3. Gather constraints and requirements
4. Understand success criteria

## Interview Questions

### Opening (5 min)
- Describe your role and daily responsibilities
- How do you currently accomplish [task]?
- What tools/systems do you use?

### Current State (15 min)
- Walk me through your typical workflow for [process]
- What works well in the current process?
- What are the main pain points or frustrations?
- How much time do you spend on [task]?
- What workarounds have you created?

### Future State (20 min)
- What would an ideal solution look like?
- What features or capabilities are must-haves?
- What would make your job easier/faster?
- How would you measure success?
- What business outcomes are you hoping for?

### Requirements Deep-Dive (15 min)
- Specific scenarios or use cases
- Data requirements and sources
- Integration needs
- Reporting and analytics needs
- Security and compliance requirements

### Closing (5 min)
- Any other requirements we haven't discussed?
- Who else should we talk to?
- Can we follow up if we have questions?
- Would you be available for user testing?

## Follow-up Actions
- Send meeting notes within 24 hours
- Schedule follow-up if needed
- Add requirements to backlog
- Share with project team
```

### Phase 2: Requirements Elicitation

**1. Interview Execution & Documentation**

```markdown
# Interview Notes Template

## Interview Summary
- **Date**: January 14, 2026
- **Stakeholder**: John Smith, Sales Director
- **Interviewer**: Sarah Johnson, Business Analyst
- **Duration**: 60 minutes

## Key Takeaways
1. Sales team spends 30% of time on manual data entry
2. Current CRM lacks mobile access - major pain point
3. Quote generation takes 2-3 days - needs to be < 1 hour
4. Integration with ERP is critical for inventory visibility
5. Sales dashboard with real-time metrics is high priority

## Current State Challenges
- Manual data entry from multiple sources (email, calls, meetings)
- No mobile access - can't update CRM in the field
- Quote approval process requires 4-5 email exchanges
- No visibility into inventory or pricing changes
- Reporting is manual and time-consuming

## Desired Future State
- Mobile-first CRM with offline capability
- Automated quote generation with approval workflow
- Real-time inventory and pricing integration
- Customizable dashboard for sales metrics
- AI-powered lead scoring and recommendations

## Requirements Identified

### Functional Requirements
- FR-001: Mobile app for iOS and Android
- FR-002: Offline data sync capability
- FR-003: Automated quote generation from product catalog
- FR-004: Multi-level approval workflow (configurable)
- FR-005: Real-time ERP integration for inventory/pricing
- FR-006: Customizable sales dashboard
- FR-007: Lead scoring algorithm
- FR-008: Email and calendar integration

### Non-Functional Requirements
- NFR-001: Response time < 2 seconds for all operations
- NFR-002: Support 500 concurrent users
- NFR-003: 99.9% uptime SLA
- NFR-004: Data sync within 5 seconds of connectivity
- NFR-005: GDPR and SOC2 compliance

## Priority Assessment (Initial)
**Must Have**:
- Mobile app
- Quote generation
- ERP integration

**Should Have**:
- Lead scoring
- Advanced dashboard

**Could Have**:
- AI recommendations

**Won't Have (This Release)**:
- Social media integration
- Marketing automation

## Follow-up Needed
- Technical deep-dive on ERP integration with IT team
- Review quote approval workflow with Finance
- Validate mobile requirements with field sales team
- Discuss data migration with previous CRM vendor

## Action Items
- [ ] Schedule ERP integration workshop
- [ ] Create draft user stories for mobile app
- [ ] Document quote workflow in BPMN
- [ ] Prepare prototype for validation session
```

**2. Requirements Workshop Facilitation**

```markdown
# Requirements Workshop Plan

## Workshop Details
- **Date**: [Date]
- **Duration**: 3 hours
- **Participants**: 8-12 stakeholders
- **Location**: Conference Room A / Virtual
- **Facilitator**: Business Analyst
- **Scribe**: Project Manager

## Workshop Objectives
1. Align stakeholders on project vision and scope
2. Identify and prioritize key requirements
3. Resolve conflicts and dependencies
4. Create shared understanding of success criteria

## Agenda

### 1. Introduction (15 min)
- Workshop objectives
- Ground rules
- Participant introductions
- Review project context

### 2. Vision & Goals Exercise (30 min)
**Activity**: Product Vision Board
- Target Group: Who are we building for?
- Needs: What problems are we solving?
- Product: What is the solution?
- Business Goals: What business value will we create?

### 3. User Journey Mapping (45 min)
**Activity**: Map current vs. future state journey
- Identify pain points in current process
- Design ideal future experience
- Highlight moments that matter
- Identify requirements from journey

**Break** (15 min)

### 4. Feature Brainstorming (45 min)
**Activity**: Silent brainstorming + affinity grouping
- Each participant writes features on sticky notes
- Post and cluster similar items
- Name each cluster
- Discuss and refine

### 5. Prioritization Exercise (30 min)
**Activity**: MoSCoW Method
- Must Have: Non-negotiable, project fails without these
- Should Have: Important but not critical
- Could Have: Nice to have if time/budget allows
- Won't Have: Out of scope for this release

### 6. Dependencies & Risks (20 min)
- Identify technical dependencies
- Highlight integration requirements
- Surface risks and constraints
- Discuss mitigation strategies

### 7. Next Steps (10 min)
- Review decisions and priorities
- Assign action items
- Schedule follow-up sessions
- Feedback on workshop

## Workshop Materials Needed
- Whiteboard or digital collaboration tool (Miro, Mural)
- Sticky notes (physical or virtual)
- Workshop templates
- Project context document
- Stakeholder list
- Parking lot for off-topic items

## Workshop Outputs
- Product vision statement
- Prioritized requirements list
- User journey maps
- Identified risks and dependencies
- Action items with owners
```

**3. Additional Elicitation Techniques**

```markdown
# Elicitation Techniques Toolkit

## 1. Document Analysis
**Purpose**: Extract requirements from existing documentation

**Documents to Review**:
- Current system documentation
- Business process documents
- Industry regulations and standards
- Competitor analysis reports
- Customer support tickets
- Previous project documentation

**Analysis Checklist**:
- [ ] Identify current business rules
- [ ] Extract data models and entities
- [ ] Document existing workflows
- [ ] Note compliance requirements
- [ ] Identify integration points
- [ ] List current system limitations

## 2. Observation & Job Shadowing
**Purpose**: Understand actual user behavior and workflows

**Observation Plan**:
- Schedule: 2-4 hours with different user types
- Observe: Users performing real tasks
- Note: Workarounds, pain points, frequency
- Ask: Why they do things certain ways
- Capture: Screenshots, workflow diagrams

**Questions During Observation**:
- Why did you do that?
- How often does this happen?
- What would make this easier?
- What happens when X occurs?
- How do you handle exceptions?

## 3. Prototyping
**Purpose**: Validate understanding and gather feedback

**Prototype Types**:
- **Paper Prototypes**: Quick, low-fidelity sketches
- **Wireframes**: Basic UI layout and navigation
- **Clickable Mockups**: Interactive prototype
- **Working Prototype**: Limited functionality demo

**Validation Sessions**:
1. Show prototype to stakeholders
2. Ask them to complete tasks
3. Observe interactions and confusion
4. Gather feedback
5. Iterate and refine

## 4. Competitive Analysis
**Purpose**: Understand industry standards and opportunities

**Analysis Framework**:
| Feature | Competitor A | Competitor B | Our Product | Priority |
|---------|-------------|-------------|-------------|----------|
| Mobile App | ✅ | ✅ | Must Have | High |
| API Integration | ✅ | ❌ | Should Have | Medium |
| AI Features | ❌ | ✅ | Could Have | Low |

## 5. Surveys & Questionnaires
**Purpose**: Gather requirements from large user base

**Survey Design Tips**:
- Keep it short (< 10 minutes)
- Mix question types (multiple choice, rating, open-ended)
- Use clear, unbiased language
- Test before distributing
- Incentivize completion

**Sample Questions**:
- How often do you use [feature]? (Daily/Weekly/Monthly/Never)
- Rate your satisfaction with [feature] (1-5 scale)
- What is your biggest pain point?
- What feature would you add if you could?
```

### Phase 3: User Stories & Use Cases

**1. User Story Creation**

```markdown
# User Story Template

## Epic: Sales Process Automation

### User Story: Quick Quote Generation
**As a** Sales Representative
**I want to** generate quotes in under 5 minutes
**So that** I can respond to customers faster and close more deals

**Acceptance Criteria**:
1. Given I am logged into the CRM
   When I select "Create Quote" for a customer
   Then I should see a quote form pre-filled with customer data

2. Given I am creating a quote
   When I add products from the catalog
   Then pricing should automatically update based on quantity and current pricing rules

3. Given I have completed the quote
   When I submit for approval
   Then the appropriate approver should be notified within 1 minute

4. Given my quote is approved
   When I generate the PDF
   Then it should include all customer details, line items, pricing, and terms

**Priority**: Must Have
**Story Points**: 8
**Dependencies**: 
- Product catalog must be integrated
- Pricing engine must be available
- Approval workflow must be defined

**Non-Functional Requirements**:
- Quote generation should complete in < 5 seconds
- PDF generation should complete in < 3 seconds
- Support up to 100 line items per quote

**Notes**:
- Requires integration with ERP for real-time pricing
- Need to handle multi-currency quotes
- Should support discount rules and special pricing

**Wireframes**: [Link to designs]
**Technical Notes**: [Link to technical spec]

---

### User Story: Mobile CRM Access
**As a** Field Sales Representative
**I want to** access CRM on my mobile device
**So that** I can update customer information while on-site

**Acceptance Criteria**:
1. Given I have the mobile app installed
   When I log in with my credentials
   Then I should see my customer list and today's appointments

2. Given I am viewing a customer record
   When I tap to edit
   Then I should be able to update all key fields (contact info, notes, status)

3. Given I lose network connectivity
   When I make changes to customer records
   Then changes should be queued and synced when connectivity returns

4. Given I regain connectivity
   When my queued changes sync
   Then I should receive confirmation of successful sync

**Priority**: Must Have
**Story Points**: 13
**Dependencies**: 
- API endpoints for mobile access
- Authentication system
- Offline storage design

**Non-Functional Requirements**:
- App should work offline for up to 8 hours
- Sync should complete within 30 seconds
- Support iOS 15+ and Android 12+

---

### INVEST Checklist
✅ **Independent**: Can be developed without other stories
✅ **Negotiable**: Details can be discussed with team
✅ **Valuable**: Delivers value to users/business
✅ **Estimable**: Team can estimate effort
✅ **Small**: Can be completed in one sprint
✅ **Testable**: Clear acceptance criteria for testing
```

**2. Use Case Documentation**

```markdown
# Use Case: Generate Customer Quote

## Use Case Overview
- **ID**: UC-001
- **Name**: Generate Customer Quote
- **Actor**: Sales Representative
- **Goal**: Create and submit a quote for customer approval
- **Preconditions**: 
  - User is authenticated
  - Customer exists in CRM
  - Product catalog is available
- **Postconditions**: 
  - Quote is created and saved
  - Approval request is submitted
  - Customer is notified
- **Priority**: High
- **Frequency**: 50-100 times per day across all users

## Main Success Scenario (Basic Flow)

1. Sales Rep navigates to customer record
2. System displays customer details and history
3. Sales Rep clicks "Create Quote" button
4. System displays quote creation form with:
   - Customer information (pre-filled)
   - Product selection interface
   - Pricing and discount fields
   - Terms and conditions
5. Sales Rep searches for products
6. System displays matching products with current prices
7. Sales Rep adds products to quote
8. System calculates total based on:
   - Quantity
   - Unit price
   - Volume discounts
   - Special pricing rules
9. Sales Rep reviews and adjusts quote
10. System validates quote completeness
11. Sales Rep clicks "Submit for Approval"
12. System determines appropriate approver based on:
    - Quote value
    - Discount percentage
    - Customer type
13. System sends approval request to approver
14. System displays confirmation to Sales Rep
15. System sends quote preview to customer

## Alternative Flows

### 3a. Product Not in Catalog
3a1. Sales Rep searches for product
3a2. System returns no results
3a3. Sales Rep clicks "Request New Product"
3a4. System opens product request form
3a5. Sales Rep submits product request
3a6. System notifies product team
3a7. Sales Rep can save quote as draft and return later
→ Use case continues at step 9

### 8a. Special Pricing Required
8a1. Sales Rep applies discount > standard threshold
8a2. System flags quote as requiring special approval
8a3. System requires justification from Sales Rep
8a4. Sales Rep enters discount justification
→ Use case continues at step 9

### 10a. Quote Validation Fails
10a1. System identifies missing required fields
10a2. System displays error messages
10a3. Sales Rep corrects errors
→ Use case continues at step 10

### 13a. Auto-Approval
13a1. System determines quote meets auto-approval criteria:
     - Total value < $10,000
     - Discount < 10%
     - Standard customer terms
13a2. System auto-approves quote
13a3. System notifies Sales Rep
13a4. System generates final quote PDF
→ Use case ends successfully

## Exception Flows

### E1. System Unavailable
- System displays error message
- Changes are saved locally
- User can retry when system is available

### E2. Pricing Service Fails
- System displays warning message
- Sales Rep can use last known pricing
- Quote is flagged for pricing verification

### E3. Customer Credit Check Fails
- System displays credit hold warning
- Quote can be created but not submitted
- Sales Rep must contact Finance team

## Business Rules

**BR-001**: Quote Approval Thresholds
- < $10K and < 10% discount: Auto-approve
- $10K - $50K or 10-20% discount: Manager approval
- $50K - $100K or 20-30% discount: Director approval
- > $100K or > 30% discount: VP approval

**BR-002**: Pricing Rules
- Volume discounts apply automatically
- Customer-specific pricing overrides standard pricing
- Promotional pricing requires valid promo code
- Minimum margin rules enforced (20%)

**BR-003**: Quote Validity
- Standard quotes valid for 30 days
- Custom quotes valid for 15 days
- Pricing locked when quote is approved
- Expired quotes require pricing refresh

## Data Requirements

**Input Data**:
- Customer ID (required)
- Product IDs (1-100 items)
- Quantities (integer > 0)
- Discount percentage (0-100%)
- Special terms (optional)
- Delivery date (optional)

**Output Data**:
- Quote ID
- Quote number (auto-generated)
- Line items with pricing
- Total amount
- Applicable taxes
- Approval status
- PDF document

## UI Requirements
- Mobile-responsive design
- Auto-save every 30 seconds
- Real-time pricing updates
- Product search with autocomplete
- Drag-and-drop line item reordering
- Keyboard shortcuts for power users

## Technical Requirements
- API: POST /api/v1/quotes
- Response time: < 2 seconds
- Concurrent users: 500
- Data validation on client and server
- Transaction support for multi-step operations
- Audit logging for all quote changes

## Related Use Cases
- UC-002: Approve Quote
- UC-003: Revise Quote
- UC-004: Convert Quote to Order
- UC-005: View Quote History
```

**3. Persona Development**

```markdown
# User Persona: Field Sales Representative

## Demographics
- **Name**: Alex Martinez
- **Age**: 32
- **Location**: Regional territory (75% travel)
- **Education**: Bachelor's in Business
- **Experience**: 7 years in B2B sales
- **Tech Savviness**: Medium (comfortable with mobile apps)

## Professional Context
**Role**: Field Sales Representative
**Company**: Mid-size manufacturing company
**Team**: Part of 15-person regional sales team
**Reports To**: Regional Sales Manager
**Customers**: 50-75 active accounts, mix of small and medium businesses

## Goals & Motivations
**Primary Goals**:
- Meet quarterly sales quota ($750K)
- Build long-term customer relationships
- Reduce administrative time
- Close deals faster

**Motivations**:
- Commission-based compensation
- Career advancement to Senior Rep
- Recognition as top performer
- Work-life balance

## Pain Points & Challenges
**Current Frustrations**:
- Spends 30% of time on data entry (should be selling)
- Can't access CRM while on customer sites
- Quote generation takes 2-3 days (loses deals)
- Difficult to find customer history during calls
- Manual expense reporting is tedious

**Technology Challenges**:
- Weak cellular coverage in some territories
- CRM only works on desktop
- No integration between CRM and email
- Multiple systems for different tasks

## Behavioral Patterns
**Daily Routine**:
- 7:00 AM: Check email, review day's schedule
- 8:00 AM - 5:00 PM: Customer meetings (3-5 per day)
- 5:00 PM - 6:30 PM: Update CRM, respond to emails
- Evening: Prepare for next day's meetings

**Tools Used**:
- CRM (desktop only)
- Email (mobile and desktop)
- Calendar (Outlook)
- Product catalog (PDF)
- Quote template (Excel)
- GPS navigation

**Communication Preferences**:
- Mobile phone for quick questions
- Email for detailed information
- Video calls for remote meetings
- In-person for important deals

## Needs & Requirements
**Must Have**:
- Mobile CRM with offline access
- Quick quote generation (< 5 minutes)
- Customer history at fingertips
- Real-time inventory visibility
- Voice-to-text for notes

**Nice to Have**:
- AI-powered lead scoring
- Automated follow-up reminders
- Integration with LinkedIn
- Competitive intelligence
- Route optimization

## Success Metrics
**How Alex Measures Success**:
- Sales quota attainment
- Number of new customers
- Average deal size
- Customer satisfaction scores
- Time spent on administrative tasks

**What Would Make Alex's Job Easier**:
- Mobile-first tools
- Automated data entry
- Instant pricing and availability
- Smart recommendations
- Seamless communication

## Quote
> "I need to spend more time selling and less time entering data. If I could access everything from my phone and generate quotes on-site, I'd close 30% more deals."

## User Journey Touchpoints
1. Morning prep: Review schedule and customer data
2. Travel to customer: Research competitive info
3. Customer meeting: Present products, answer questions
4. On-site: Generate quote, check inventory
5. Post-meeting: Update CRM, set follow-ups
6. Evening: Plan next day, review pipeline

## Design Implications
- Mobile-first design is critical
- Offline functionality required
- Minimize data entry (auto-fill, voice input)
- Quick access to key information
- One-tap actions for common tasks
- Large touch targets for in-car use
```

### Phase 4: Requirements Documentation

**1. Business Requirements Document (BRD)**

```markdown
# Business Requirements Document

## Document Information
- **Project**: CRM Modernization
- **Version**: 1.0
- **Date**: January 14, 2026
- **Author**: Business Analyst Team
- **Status**: Approved
- **Approvers**: CTO, VP Sales, CFO

## Executive Summary

### Business Problem
Our current CRM system is desktop-only and lacks modern capabilities, resulting in:
- 30% of sales time spent on manual data entry
- 2-3 day quote turnaround (competitors: < 1 hour)
- No mobile access for field sales team
- Limited visibility into customer interactions
- Declining customer satisfaction scores

### Proposed Solution
Implement a modern, mobile-first CRM with:
- Native iOS and Android apps
- Offline capabilities
- Automated quote generation
- Real-time ERP integration
- AI-powered insights

### Expected Benefits
**Quantitative**:
- 25% increase in sales productivity
- 50% reduction in quote turnaround time
- 20% increase in deal closure rate
- $2.5M additional annual revenue
- ROI: 180% in first year

**Qualitative**:
- Improved customer experience
- Higher employee satisfaction
- Better data quality
- Competitive advantage

### Investment Required
- Software licenses: $250K/year
- Implementation: $500K (one-time)
- Training: $50K
- Ongoing support: $100K/year

## Business Context

### Strategic Alignment
This project aligns with our 2026 strategic priorities:
1. Digital transformation initiative
2. Customer experience improvement
3. Sales force enablement
4. Operational efficiency

### Market Analysis
**Industry Trends**:
- 78% of sales organizations use mobile CRM
- AI-powered CRM market growing at 25% CAGR
- Customer expectations for instant responses

**Competitive Pressure**:
- Competitor A: Responds to quote requests in < 1 hour
- Competitor B: Has mobile app with 4.8 star rating
- Competitor C: Uses AI for lead scoring

### Current State Assessment
**System Limitations**:
- 15-year-old desktop application
- No API for integrations
- Limited reporting capabilities
- No mobile access
- Manual quote generation

**Process Inefficiencies**:
- Sales reps spend 6 hours/week on data entry
- Quote approval requires 4-5 email exchanges
- No visibility into inventory during sales calls
- Customer history scattered across systems

## Business Requirements

### BR-1: Sales Force Enablement
**Objective**: Increase sales team productivity by 25%

**Requirements**:
- BR-1.1: Mobile access to CRM for iOS and Android
- BR-1.2: Offline functionality for minimum 8 hours
- BR-1.3: Voice-to-text for quick note entry
- BR-1.4: One-tap access to customer history
- BR-1.5: Quick quote generation (< 5 minutes)

**Success Metrics**:
- Time spent on data entry reduced by 50%
- Sales calls increased by 30%
- User satisfaction score > 4.5/5
- App usage rate > 90% of sales team

### BR-2: Quote Process Automation
**Objective**: Reduce quote turnaround from 2-3 days to < 1 hour

**Requirements**:
- BR-2.1: Automated quote generation from product catalog
- BR-2.2: Real-time pricing from ERP system
- BR-2.3: Automated approval workflow based on business rules
- BR-2.4: Electronic signature capture
- BR-2.5: PDF generation with company branding

**Success Metrics**:
- 90% of quotes generated in < 30 minutes
- 50% of quotes auto-approved
- Quote acceptance rate increased by 15%
- Customer satisfaction with quote process > 4.5/5

### BR-3: Customer 360° View
**Objective**: Provide complete customer context in one place

**Requirements**:
- BR-3.1: Unified customer profile with all interactions
- BR-3.2: Integration with email and calendar
- BR-3.3: Integration with support ticket system
- BR-3.4: Purchase history and order status
- BR-3.5: Customer health score and renewal likelihood

**Success Metrics**:
- Customer data accuracy > 95%
- Average time to find customer information < 10 seconds
- Complete interaction history for 100% of customers

### BR-4: Analytics & Insights
**Objective**: Enable data-driven sales decisions

**Requirements**:
- BR-4.1: Real-time sales dashboard
- BR-4.2: Pipeline visibility and forecasting
- BR-4.3: Win/loss analysis
- BR-4.4: AI-powered lead scoring
- BR-4.5: Predictive analytics for churn risk

**Success Metrics**:
- Forecast accuracy improved by 20%
- Lead conversion rate increased by 15%
- Churn rate reduced by 10%

### BR-5: Integration & Data Flow
**Objective**: Eliminate manual data entry through automation

**Requirements**:
- BR-5.1: Bi-directional ERP integration
- BR-5.2: Email and calendar sync
- BR-5.3: Marketing automation integration
- BR-5.4: Support system integration
- BR-5.5: Accounting system integration

**Success Metrics**:
- 95% reduction in manual data entry
- Data sync latency < 5 seconds
- Integration uptime > 99.9%

## Constraints & Assumptions

### Constraints
**Budget**: $500K implementation, $350K annual operating
**Timeline**: Must launch by Q3 2026 for fiscal year planning
**Technology**: Must integrate with existing ERP (Oracle)
**Compliance**: Must meet GDPR, SOC2, and industry regulations
**Resources**: 2 full-time developers, 1 BA, 1 PM

### Assumptions
- Sales team will adopt new system (change management required)
- ERP APIs are available and documented
- Mobile devices provided to all field sales reps
- Executive sponsorship will continue
- Current data can be migrated successfully

### Risks
| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| Low user adoption | High | Medium | Comprehensive training, change management |
| ERP integration complexity | High | High | Technical spike, vendor engagement |
| Data migration issues | Medium | Medium | Phased approach, extensive testing |
| Budget overrun | Medium | Low | Regular budget reviews, scope management |

## Dependencies
- ERP API documentation and access
- Mobile device procurement
- Data cleansing project completion
- Network infrastructure upgrades
- Vendor selection and contracting

## Approval & Sign-off

**Approved By**:
- ________________ VP Sales, Date: __________
- ________________ CTO, Date: __________
- ________________ CFO, Date: __________
```

## Prioritization Frameworks

### MoSCoW Method
```markdown
# MoSCoW Prioritization

## Must Have (Non-negotiable, 60%)
Critical features without which the project fails:
- Mobile app (iOS & Android)
- Offline data sync
- Quote generation
- ERP integration (pricing & inventory)
- Basic customer management
- User authentication & security

## Should Have (Important, 20%)
Important but not critical for launch:
- AI lead scoring
- Advanced analytics dashboard
- Email integration
- Calendar sync
- Approval workflow automation

## Could Have (Nice to have, 15%)
Desirable if time/budget allows:
- Voice-to-text notes
- LinkedIn integration
- Route optimization
- Competitive intelligence
- Advanced forecasting

## Won't Have (Out of scope, 5%)
Not in this release:
- Marketing automation
- Social media integration
- Custom mobile app branding
- Multi-language support (only English v1)
```

### RICE Scoring
```markdown
# RICE Prioritization Framework
Score = (Reach × Impact × Confidence) / Effort

| Feature | Reach | Impact | Confidence | Effort | RICE Score | Priority |
|---------|-------|--------|------------|--------|------------|----------|
| Mobile App | 100 | 3 | 80% | 8 | 30.0 | 1 |
| Quote Generation | 100 | 3 | 95% | 5 | 57.0 | 2 |
| ERP Integration | 90 | 3 | 70% | 13 | 14.8 | 3 |
| Lead Scoring | 100 | 2 | 60% | 5 | 24.0 | 4 |
| Analytics Dashboard | 80 | 2 | 80% | 3 | 42.7 | 5 |

**Scoring Guide**:
- **Reach**: How many users affected per quarter (%)
- **Impact**: 3=Massive, 2=High, 1=Medium, 0.5=Low
- **Confidence**: 100%=High, 80%=Medium, 50%=Low
- **Effort**: Person-months
```

## Requirements Traceability Matrix

```markdown
# Requirements Traceability Matrix

| Req ID | Description | Source | Priority | User Story | Use Case | Test Case | Status |
|--------|-------------|--------|----------|------------|----------|-----------|--------|
| BR-1.1 | Mobile CRM Access | Stakeholder Interview | Must Have | US-001 | UC-001 | TC-001-010 | Approved |
| BR-1.2 | Offline Capability | Workshop | Must Have | US-002 | UC-001 | TC-011-015 | Approved |
| BR-2.1 | Quote Generation | Interview, Observation | Must Have | US-003 | UC-002 | TC-016-025 | Approved |
| BR-2.2 | Real-time Pricing | ERP Team | Must Have | US-004 | UC-002 | TC-026-030 | In Review |
| BR-3.1 | Customer 360 | Multiple Sources | Should Have | US-005 | UC-003 | TC-031-040 | Draft |
| BR-4.1 | Sales Dashboard | Sales Leadership | Should Have | US-006 | UC-004 | TC-041-050 | Draft |
```

## Best Practices

### Requirements Quality Checklist
- [ ] **Clear**: Unambiguous and easy to understand
- [ ] **Complete**: All necessary information included
- [ ] **Consistent**: No contradictions with other requirements
- [ ] **Testable**: Can verify when implemented correctly
- [ ] **Feasible**: Technically and economically possible
- [ ] **Necessary**: Directly supports business goals
- [ ] **Prioritized**: Classified by importance
- [ ] **Traceable**: Linked to source and downstream artifacts

### Common Pitfalls to Avoid
1. **Solution-Focused**: Describing HOW instead of WHAT
   - ❌ "System must use React framework"
   - ✅ "System must provide responsive web interface"

2. **Vague Language**: Using ambiguous terms
   - ❌ "System should be fast"
   - ✅ "System must respond within 2 seconds"

3. **Gold Plating**: Including unnecessary features
   - Focus on business value
   - Apply 80/20 rule
   - Challenge "nice to have" features

4. **Assuming Knowledge**: Not documenting obvious things
   - Document all assumptions
   - Define acronyms and terminology
   - Provide context

5. **Skipping Validation**: Not confirming understanding
   - Review requirements with stakeholders
   - Create prototypes for complex features
   - Get sign-off before development

## Activation Guidelines

This skill should be activated when:
- Gathering requirements for new projects
- Conducting stakeholder interviews
- Writing user stories and acceptance criteria
- Creating use case documentation
- Developing product requirements documents
- Prioritizing features and requirements
- Analyzing business needs
- Creating requirements traceability
- Facilitating requirements workshops
- Managing product backlogs
- Defining project scope
- Creating business cases

The skill provides the most value when given project context, stakeholder information, and clear business objectives.
