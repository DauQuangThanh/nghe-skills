---
name: requirement-review
description: Conducts comprehensive requirements review including completeness validation, clarity assessment, consistency checking, testability evaluation, and standards compliance. Produces detailed review reports with findings, gaps, conflicts, and improvement recommendations. Use when reviewing requirements documents (BRD, SRS, user stories), validating acceptance criteria, assessing requirements quality, identifying gaps and conflicts, or ensuring standards compliance (IEEE 830, INVEST criteria). Trigger when users mention "review requirements", "validate requirements", "check requirements quality", "find requirement issues", or "assess BRD/SRS quality".
---

# Requirement Review

Systematically review requirements to validate quality and identify issues before design and development begins.

## Purpose

Ensure requirements are complete, clear, consistent, testable, and traceable. Identify gaps, conflicts, and ambiguities early to reduce costly rework later.

## When to Use This Skill

- Review BRD, SRS, or user stories before approval
- Assess requirements quality and identify improvement areas
- Validate requirements before design phase begins (quality gate)
- Prepare review reports for stakeholder meetings
- Verify compliance with standards (IEEE 830, INVEST, industry regulations)
- Find and resolve gaps, conflicts, and ambiguities

## Review Workflow

### 1. Preparation

Gather all relevant documentation:
- Requirements documents (BRD, SRS, user stories)
- Non-functional requirements
- Requirements Traceability Matrix (if available)
- Project context (charter, goals, constraints)
- Acceptance criteria

### 2. Quality Assessment

Evaluate each requirement using the **six quality attributes**:

1. **Completeness**: All needs captured, no missing details or TBDs, dependencies identified
2. **Clarity**: Unambiguous, specific, no vague terms ("user-friendly", "fast", "easy")
3. **Consistency**: No conflicts, consistent terminology and priorities
4. **Testability**: Verifiable with measurable acceptance criteria and clear pass/fail
5. **Traceability**: Links to business goals, design, and tests
6. **Feasibility**: Technically achievable within constraints (budget, timeline, capability)

> **See references/quality-checklists.md** for detailed assessment criteria and document-specific checklists (BRD, SRS, User Stories)

### 3. Issue Identification

Common requirement problems to look for:

**Ambiguous**: "The system shall be fast" → Should specify measurable criteria like "< 2s response for 95% of requests"

**Untestable**: "The system shall be secure" → Should define specific controls like password complexity, encryption standards, audit logging

**Incomplete**: "The system shall send notifications" → Should specify channel (email/SMS), timing, content, recipients

**Conflicting**: REQ-012 "phone required" vs REQ-089 "phone optional" → Needs stakeholder resolution

> **See references/examples.md** for extensive before/after examples of common issues and improvements

### 4. Quality Scoring

Calculate scores for each quality attribute (0-100%):

- **90-100%**: Excellent - minimal issues
- **75-89%**: Good - minor improvements needed
- **60-74%**: Fair - notable gaps exist
- **Below 60%**: Poor - major rework required

**Overall Quality** = Average of all six attribute scores

Target: ≥ 90% for production-readiness

### 5. Report Generation

Create structured review report containing:

**Executive Summary**
- Overall quality rating (Excellent/Good/Fair/Poor)
- Recommendation (Approve / Approve with Conditions / Major Revision / Reject)
- Key strengths (2-3 bullet points)
- Critical issues (2-3 bullet points)
- Top 3-5 recommended actions

**Detailed Findings**
- Quality scores by attribute
- Issues categorized by severity (Critical/Major/Minor)
- Specific examples with requirement IDs
- Recommendations for each issue

**Action Items**
- Prioritized list with owners, due dates, effort estimates
- Conditions for approval (if conditional approval)
- Next steps and timeline

> **See references/report-templates.md** for full report template, quick summary template, and issue tracking template

### 6. Recommendations

Provide clear decision with rationale:

- **Approve**: Requirements meet quality thresholds (≥90%)
- **Approve with Conditions**: Good foundation but critical issues must be resolved before design
- **Major Revision Needed**: Significant quality gaps (60-74%) require substantial rework
- **Reject**: Poor quality (<60%) or fundamental issues; complete redesign needed

## Document-Specific Guidance

### For Business Requirements Documents (BRD)
Focus on: Business objectives, stakeholder needs, scope definition, business case, constraints

### For Software Requirements Specifications (SRS)
Focus on: Functional requirements, non-functional requirements (performance, security, scalability), system behaviors, integration points, IEEE 830 compliance

### For User Story Backlogs
Focus on: INVEST criteria (Independent, Negotiable, Valuable, Estimable, Small, Testable), acceptance criteria format (Given-When-Then), story format, prioritization

## Best Practices

**Review Timing**
- **Initial Review**: After first draft completion
- **Iteration Reviews**: After major updates
- **Quality Gate**: Before design phase starts (required)
- **Continuous Review**: During sprint planning (Agile)

**Review Techniques**
- **Individual**: Reviewer examines alone with checklist
- **Peer Review**: Colleagues review each other's work
- **Walkthrough**: Author presents to team for feedback
- **Inspection**: Formal structured review with defined roles

**Severity Definitions**
- **Critical**: Blocks progress, must fix immediately (security gaps, fundamental conflicts, missing scope)
- **Major**: Significant quality impact, should fix before approval (ambiguous criteria, missing acceptance criteria, testability issues)
- **Minor**: Quality improvements, nice to have (formatting, examples, diagram enhancements)

## Anti-Patterns to Avoid

- **Gold Plating**: Adding unrequested features
- **Solution-Focused**: Specifying "how" instead of "what" (requirements should be technology-agnostic)
- **Scope Creep**: Accepting changes without change control process
- **Analysis Paralysis**: Over-perfecting requirements indefinitely
- **Ignoring Non-Functionals**: Missing performance, security, scalability requirements
- **Weak Acceptance Criteria**: Vague or missing test conditions
- **No Traceability**: Requirements disconnected from business goals and downstream artifacts

## Quick Reference

### Quality Assessment Checklist
- [ ] Read all requirements thoroughly
- [ ] Score each quality attribute (completeness, clarity, consistency, testability, traceability, feasibility)
- [ ] Document issues with specific requirement IDs
- [ ] Categorize issues by severity (Critical/Major/Minor)
- [ ] Calculate overall quality score
- [ ] Generate review report
- [ ] Provide approval recommendation with rationale

### Common Vague Terms to Flag
Replace these with measurable criteria:
- "user-friendly", "intuitive", "easy to use" → Define usability metrics
- "fast", "quick", "responsive" → Specify response time targets
- "secure" → Define specific security controls
- "scalable" → Define capacity and load targets
- "reliable" → Define uptime percentage and recovery time

### Standards Compliance
- **IEEE 830**: SRS structure and content standards
- **INVEST**: User story quality criteria
- **ISO/IEC 25010**: Systems and software quality models
- **Industry-Specific**: HIPAA (healthcare), PCI-DSS (payments), SOC 2 (SaaS), FDA (medical devices)

## Additional Resources

- **references/quality-checklists.md**: Detailed assessment criteria, INVEST criteria, scoring guidelines, standards compliance
- **references/examples.md**: Extensive before/after examples showing common issues and improvements
- **references/report-templates.md**: Full review report template, quick summary, issue tracking formats

## Output Format

Generate review reports in this structure:

1. **Executive Summary** (1 page): Overall assessment, key findings, recommendation
2. **Quality Metrics** (0.5 page): Scores by attribute with targets
3. **Detailed Findings** (2-5 pages): Issues grouped by attribute, with examples and recommendations
4. **Action Items** (1 page): Prioritized list with owners and due dates
5. **Approval Decision** (0.5 page): Recommendation with rationale and next steps

Keep reports factual, specific, and actionable. Always cite specific requirement IDs when identifying issues.
