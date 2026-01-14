# Frontend Design Review Skill

```yaml
name: frontend-design-review
description: >
  Conducts comprehensive frontend design reviews covering UI/UX design quality, design system validation, accessibility compliance, responsive design patterns, component library architecture, and visual design consistency. Evaluates design specifications, Figma/Sketch files, design tokens, interaction patterns, and user experience flows. Identifies usability issues, accessibility violations, design system deviations, and provides actionable recommendations for improvement. Produces detailed design review reports with severity-rated findings, visual examples, and implementation guidelines. Use when reviewing frontend designs, validating design systems, ensuring accessibility compliance, evaluating component libraries, assessing responsive designs, or when users mention design review, UI/UX review, Figma review, design system validation, accessibility audit, or frontend design quality.
license: MIT
author: Dau Quang Thanh
version: "1.0"
category: review
```

## Overview

This skill provides expert guidance for conducting thorough frontend design reviews, covering all aspects of UI/UX design quality, design system consistency, accessibility compliance, and responsive design patterns. The skill helps identify design issues early in the development lifecycle, ensuring designs meet quality standards, accessibility requirements, and business objectives before implementation.

## Core Capabilities

### 1. UI/UX Design Quality Review

- **Visual Design Assessment**: Evaluate typography, color usage, spacing, layout, visual hierarchy, and overall aesthetic quality
- **User Experience Evaluation**: Assess user flows, interaction patterns, navigation structure, and usability
- **Design Consistency**: Verify consistency across screens, components, and user journeys
- **Brand Alignment**: Ensure designs align with brand guidelines, style guides, and visual identity
- **Cognitive Load Analysis**: Evaluate information density, clarity, and ease of understanding

### 2. Design System Validation

- **Component Library Review**: Assess component design, variants, states, and reusability
- **Design Tokens Validation**: Review color tokens, typography tokens, spacing tokens, and other design variables
- **Pattern Library Assessment**: Evaluate design patterns for consistency and appropriateness
- **Documentation Quality**: Review design system documentation, usage guidelines, and examples
- **Design System Governance**: Assess governance processes, versioning, and change management

### 3. Accessibility Compliance

- **WCAG 2.1 AA Compliance**: Verify conformance with Web Content Accessibility Guidelines
- **Color Contrast Validation**: Check color contrast ratios meet minimum requirements (4.5:1 for text, 3:1 for UI components)
- **Keyboard Navigation**: Ensure designs support keyboard-only navigation and proper focus management
- **Screen Reader Support**: Validate designs include proper semantic structure, labels, and alternative text
- **Accessible Interactions**: Review interactive elements for accessibility (buttons, forms, modals, dropdowns)

### 4. Responsive Design Review

- **Breakpoint Strategy**: Evaluate breakpoint selection and responsive behavior across device sizes
- **Mobile-First Approach**: Assess mobile design quality and progressive enhancement strategy
- **Touch Target Sizing**: Verify interactive elements meet minimum touch target sizes (44x44px)
- **Content Adaptation**: Review how content adapts across different viewport sizes
- **Performance Considerations**: Identify potential performance issues in responsive designs

### 5. Component Architecture Assessment

- **Component Hierarchy**: Evaluate component organization, composition patterns, and relationships
- **Component Reusability**: Assess component design for maximum reusability and flexibility
- **State Management**: Review component states (default, hover, active, disabled, error, loading)
- **Component Variants**: Evaluate variant design and prop-based customization approaches
- **Component Documentation**: Review component specifications, usage guidelines, and examples

## Design Review Process

### Phase 1: Pre-Review Preparation

**Activities:**
1. **Gather Design Assets**
   - Collect Figma/Sketch files, design specifications, and mockups
   - Obtain design system documentation and component library
   - Review brand guidelines and style guides
   - Gather user research findings and personas

2. **Understand Context**
   - Review project requirements and business objectives
   - Understand target users and use cases
   - Identify key user journeys and critical flows
   - Note technical constraints and platform requirements

3. **Define Review Scope**
   - Identify screens/flows to review
   - Determine review depth (high-level vs. detailed)
   - Set review priorities based on importance
   - Establish timeline and deliverables

**Deliverables:**
- Review scope document
- Asset inventory
- Context summary

### Phase 2: Visual Design Review

**Review Areas:**

**Typography Assessment:**
- [ ] Font selection appropriate for brand and platform
- [ ] Type scale follows consistent progression (1.2x, 1.5x, etc.)
- [ ] Line heights optimal for readability (1.4-1.6 for body text)
- [ ] Letter spacing appropriate for font size
- [ ] Font weights used consistently across hierarchy levels
- [ ] Text truncation and overflow handled properly
- [ ] Typography responsive behavior defined

**Color System Review:**
- [ ] Color palette aligned with brand guidelines
- [ ] Sufficient color contrast for accessibility (WCAG AA)
- [ ] Color usage semantic and consistent (success=green, error=red)
- [ ] Dark mode support if required
- [ ] Color tokens defined and documented
- [ ] Color combinations tested for color blindness
- [ ] Gradient usage appropriate and consistent

**Spacing & Layout:**
- [ ] Spacing system follows consistent scale (4px, 8px, 16px, 24px, 32px, etc.)
- [ ] Margins and padding applied consistently
- [ ] Grid system used appropriately
- [ ] White space usage enhances readability
- [ ] Alignment precise and consistent
- [ ] Layout adapts well to different content lengths

**Visual Hierarchy:**
- [ ] Important elements stand out clearly
- [ ] Heading levels create clear information structure
- [ ] Visual weight guides user attention appropriately
- [ ] Related content grouped visually
- [ ] Consistent visual patterns across screens

**Severity Ratings:**
- 🔴 **Critical**: Severe visual issues affecting usability or brand
- 🟠 **High**: Significant inconsistencies or quality problems
- 🟡 **Medium**: Minor visual issues or improvement opportunities
- 🟢 **Low**: Polish suggestions or nice-to-have improvements

### Phase 3: User Experience Review

**Review Areas:**

**User Flows:**
- [ ] Primary user journeys clearly defined
- [ ] Flow steps logical and efficient
- [ ] Decision points clear and well-labeled
- [ ] Error paths and edge cases considered
- [ ] Exit points and escape hatches available
- [ ] Success states celebrated appropriately

**Navigation:**
- [ ] Navigation structure clear and intuitive
- [ ] Current location always clear to users
- [ ] Navigation consistent across sections
- [ ] Deep linking supported where appropriate
- [ ] Breadcrumbs used for complex hierarchies
- [ ] Back button behavior predictable

**Interaction Patterns:**
- [ ] Interactions follow platform conventions
- [ ] Feedback immediate for all actions
- [ ] Loading states designed for all async operations
- [ ] Error messages helpful and actionable
- [ ] Form validation inline and clear
- [ ] Confirmation dialogs for destructive actions

**Information Architecture:**
- [ ] Content organized logically
- [ ] Labels clear and understandable
- [ ] Search functionality accessible if needed
- [ ] Filters and sorting options appropriate
- [ ] Content density appropriate for use case

**Cognitive Load:**
- [ ] Information presented in digestible chunks
- [ ] Progressive disclosure used appropriately
- [ ] Defaults sensible and commonly used
- [ ] Choices limited to prevent decision paralysis
- [ ] Complex tasks broken into steps

### Phase 4: Design System Compliance

**Review Areas:**

**Component Usage:**
- [ ] Existing components used where appropriate
- [ ] No duplicate components created unnecessarily
- [ ] Component usage follows documentation
- [ ] Custom components justified and documented
- [ ] Component composition patterns followed

**Design Tokens:**
- [ ] Colors reference design tokens, not hardcoded values
- [ ] Spacing uses token values consistently
- [ ] Typography references token values
- [ ] Border radius, shadows use tokens
- [ ] Tokens applied correctly in all contexts

**Pattern Consistency:**
- [ ] Common patterns used consistently (cards, lists, modals)
- [ ] Form patterns follow established conventions
- [ ] Data display patterns consistent (tables, charts)
- [ ] Navigation patterns uniform across app
- [ ] Feedback patterns applied consistently

**Documentation Quality:**
- [ ] New components documented with usage guidelines
- [ ] Props/variants clearly specified
- [ ] Examples provided for common use cases
- [ ] Do's and don'ts documented
- [ ] Accessibility notes included

### Phase 5: Accessibility Audit

**Review Areas:**

**WCAG 2.1 AA Compliance:**

**Perceivable:**
- [ ] All images have alt text or are decorative
- [ ] Color not the only means of conveying information
- [ ] Text contrast meets 4.5:1 ratio (normal text) or 3:1 (large text)
- [ ] UI components meet 3:1 contrast ratio
- [ ] Content structured with proper headings
- [ ] Content readable and understandable without CSS

**Operable:**
- [ ] All functionality available via keyboard
- [ ] No keyboard traps exist
- [ ] Tab order logical and intuitive
- [ ] Focus indicators clearly visible
- [ ] Skip links provided for main content
- [ ] No time limits on interactions (or can be extended)
- [ ] No content flashes more than 3 times per second

**Understandable:**
- [ ] Language of page specified
- [ ] Navigation consistent across pages
- [ ] Labels and instructions clear
- [ ] Error messages specific and helpful
- [ ] Form fields have visible labels
- [ ] Required fields clearly indicated

**Robust:**
- [ ] Semantic HTML elements used appropriately
- [ ] ARIA labels used where needed
- [ ] Status messages announced to screen readers
- [ ] Custom controls have proper roles and states

**Touch Target Sizing:**
- [ ] Interactive elements minimum 44x44px
- [ ] Adequate spacing between touch targets
- [ ] Touch targets don't overlap

### Phase 6: Responsive Design Assessment

**Review Areas:**

**Mobile Design (320px - 767px):**
- [ ] Critical content prioritized and visible
- [ ] Touch targets appropriately sized (44x44px minimum)
- [ ] Forms optimized for mobile input
- [ ] Navigation collapsed appropriately (hamburger menu)
- [ ] Images optimized for mobile bandwidth
- [ ] Horizontal scrolling avoided
- [ ] Typography readable without zooming

**Tablet Design (768px - 1023px):**
- [ ] Layout takes advantage of medium screen size
- [ ] Navigation appropriate for tablet interaction
- [ ] Touch and mouse interactions supported
- [ ] Content density balanced for tablet viewing
- [ ] Portrait and landscape orientations considered

**Desktop Design (1024px+):**
- [ ] Layout scales appropriately for large screens
- [ ] Line lengths optimal for readability (45-75 characters)
- [ ] Hover states designed for mouse interaction
- [ ] Multiple columns used effectively
- [ ] Whitespace prevents content sprawl on wide screens

**Breakpoint Strategy:**
- [ ] Breakpoints chosen based on content, not devices
- [ ] Major layout shifts at appropriate breakpoints
- [ ] Smooth transitions between breakpoints
- [ ] Content readable at all viewport sizes
- [ ] No unnecessary breakpoints

### Phase 7: Component Architecture Review

**Review Areas:**

**Component Structure:**
- [ ] Components atomic, focused, single-purpose
- [ ] Component hierarchy clear and logical
- [ ] Composition preferred over inheritance
- [ ] Props interface intuitive and complete
- [ ] Component naming clear and consistent

**Component States:**
- [ ] Default state clearly defined
- [ ] Hover state provides visual feedback
- [ ] Active/pressed state distinct from hover
- [ ] Focus state highly visible for keyboard users
- [ ] Disabled state clearly indicates unavailability
- [ ] Loading state shown for async operations
- [ ] Error state visible and actionable
- [ ] Success state celebrates completion

**Component Variants:**
- [ ] Variants serve clear purposes
- [ ] Size variants follow consistent scale (sm, md, lg)
- [ ] Style variants limited and purposeful
- [ ] Variant combinations tested and supported
- [ ] Default variant appropriate for most use cases

**Component Documentation:**
- [ ] Component purpose clearly described
- [ ] All props documented with types
- [ ] Usage examples provided
- [ ] Accessibility considerations noted
- [ ] Related components cross-referenced

### Phase 8: Reporting & Recommendations

**Activities:**
1. **Consolidate Findings**
   - Categorize issues by severity and area
   - Document each finding with screenshots
   - Provide specific locations in design files
   - Estimate effort required to address

2. **Prioritize Recommendations**
   - Critical issues must be fixed before launch
   - High-priority issues should be fixed soon
   - Medium issues addressed in next iteration
   - Low issues tracked for future improvements

3. **Create Action Items**
   - Assign ownership for each issue
   - Set realistic timelines for fixes
   - Track progress on recommendations
   - Schedule follow-up review

## Design Review Report Template

```markdown
# Frontend Design Review Report

## Executive Summary
- **Project**: [Project name]
- **Review Date**: [Date]
- **Reviewer**: [Name]
- **Design Files**: [Links to Figma/Sketch files]
- **Overall Assessment**: [Summary of design quality]

## Review Scope
- Screens reviewed: [List]
- Flows analyzed: [List]
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

### Visual Design

#### Issue: [Issue Title]
- **Severity**: 🔴/🟠/🟡/🟢
- **Category**: Visual Design > Typography/Color/Spacing/Layout
- **Location**: [Screen name, component name]
- **Description**: [Detailed description of the issue]
- **Current State**: [Screenshot or description]
- **Impact**: [How this affects users/brand/usability]
- **Recommendation**: [Specific steps to resolve]
- **Effort**: [Low/Medium/High]

### User Experience

#### Issue: [Issue Title]
- **Severity**: 🔴/🟠/🟡/🟢
- **Category**: UX > Navigation/Flow/Interaction/Information Architecture
- **Location**: [Screen name, flow name]
- **Description**: [Detailed description]
- **User Impact**: [How users are affected]
- **Recommendation**: [Solution proposal]

### Design System Compliance

#### Issue: [Issue Title]
- **Severity**: 🔴/🟠/🟡/🟢
- **Category**: Design System > Components/Tokens/Patterns
- **Description**: [Inconsistency or deviation found]
- **Recommendation**: [How to align with design system]

### Accessibility

#### Issue: [Issue Title]
- **Severity**: 🔴/🟠/🟡/🟢
- **Category**: Accessibility > WCAG Criteria
- **WCAG Criterion**: [e.g., 1.4.3 Contrast (Minimum)]
- **Description**: [Accessibility violation]
- **Impact**: [Which users are affected]
- **Recommendation**: [How to achieve compliance]

### Responsive Design

#### Issue: [Issue Title]
- **Severity**: 🔴/🟠/🟡/🟢
- **Category**: Responsive > Mobile/Tablet/Desktop
- **Device/Breakpoint**: [Affected viewport sizes]
- **Description**: [Responsive behavior issue]
- **Recommendation**: [Responsive solution]

### Component Architecture

#### Issue: [Issue Title]
- **Severity**: 🔴/🟠/🟡/🟢
- **Category**: Components > Structure/States/Variants
- **Component**: [Component name]
- **Description**: [Component design issue]
- **Recommendation**: [Improvement suggestion]

## Positive Observations

### Strengths
- [What the design does well]
- [Effective patterns or approaches]
- [Particularly strong areas]

### Best Practices Followed
- [Good practices observed]
- [Design system usage]
- [Accessibility considerations]

## Recommendations Summary

### Must Fix Before Launch (🔴 Critical)
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
- WCAG 2.1 AA Guidelines
- [Company] Design System v[version]
- [Brand] Brand Guidelines
- [Platform] Human Interface Guidelines

### Tools Used
- Figma/Sketch for design review
- Contrast checkers for accessibility
- Device emulators for responsive testing
```

## Severity Levels

### 🔴 Critical
- **Definition**: Issues that severely impact usability, accessibility, or brand
- **Examples**:
  - WCAG AA violations blocking users with disabilities
  - Brand guideline violations affecting brand identity
  - Critical user flows broken or confusing
  - Interactive elements too small to use (< 44px)
- **Action Required**: Must be fixed before launch

### 🟠 High
- **Definition**: Significant issues affecting user experience or consistency
- **Examples**:
  - Navigation confusing or inconsistent
  - Major design system deviations
  - Responsive design breaks at important breakpoints
  - Poor visual hierarchy impacting comprehension
- **Action Required**: Should be fixed before launch or very soon after

### 🟡 Medium
- **Definition**: Moderate issues or improvement opportunities
- **Examples**:
  - Minor inconsistencies in spacing or typography
  - Suboptimal component variants
  - Missing edge case designs
  - Documentation gaps
- **Action Required**: Address in next iteration

### 🟢 Low
- **Definition**: Polish suggestions or nice-to-have improvements
- **Examples**:
  - Animation polish opportunities
  - Additional component states
  - Enhanced visual details
  - Future feature suggestions
- **Action Required**: Track for future improvements

## Best Practices for Design Reviews

### 1. **Be Objective and Evidence-Based**
- Base feedback on established principles (WCAG, usability heuristics, design system rules)
- Provide specific examples and screenshots
- Avoid subjective opinions without rationale
- Reference guidelines and standards

### 2. **Be Constructive and Solution-Oriented**
- Focus on problems and solutions, not personal preferences
- Provide specific, actionable recommendations
- Offer alternatives when criticizing designs
- Balance critique with recognition of strengths

### 3. **Consider Context**
- Understand business constraints and priorities
- Consider technical feasibility
- Respect timeline and resource limitations
- Align feedback with project goals

### 4. **Prioritize User Impact**
- Focus first on issues affecting real users
- Prioritize accessibility and usability over aesthetics
- Consider different user types and scenarios
- Think about edge cases and error states

### 5. **Collaborate with Designers**
- Involve designers in the review process
- Encourage discussion and questions
- Be open to designer perspectives
- Work together to find best solutions

### 6. **Document Thoroughly**
- Capture all findings with clear descriptions
- Include visual examples for clarity
- Provide specific locations in design files
- Track recommendations and their status

### 7. **Follow Up**
- Schedule review of design iterations
- Verify critical issues resolved
- Track progress on recommendations
- Celebrate improvements

## Common Design Issues to Watch For

### Visual Design
- Inconsistent typography scales
- Insufficient color contrast
- Spacing not following system
- Poor visual hierarchy
- Inconsistent icon styles
- Misaligned elements
- Inconsistent border radius
- Overuse of colors or styles

### User Experience
- Unclear navigation structure
- Missing feedback for actions
- Confusing user flows
- Too many clicks to complete tasks
- Missing error states
- No loading indicators
- Unclear form validation
- Poor error messages

### Design System
- Duplicating existing components
- Not using design tokens
- Inventing one-off styles
- Ignoring pattern library
- Poor component documentation
- Inconsistent naming conventions

### Accessibility
- Insufficient color contrast
- Missing alt text on images
- No keyboard navigation support
- Tiny touch targets (< 44px)
- Color-only information conveyance
- Missing form labels
- Poor focus indicators
- Inaccessible custom controls

### Responsive Design
- Fixed widths breaking layouts
- Horizontal scrolling on mobile
- Tiny text requiring zoom
- Touch targets too small
- Poor breakpoint choices
- Desktop-only features on mobile
- Poor mobile navigation

### Component Architecture
- Overly complex components
- Too many variants
- Missing common states
- Poor prop design
- Unclear component boundaries
- Tight coupling between components

## Activation Guidelines

Use this skill when:

### Design Review Scenarios
- Reviewing high-fidelity mockups before development
- Validating Figma/Sketch designs against requirements
- Conducting accessibility audits on designs
- Evaluating responsive design strategies
- Assessing component library designs

### Design System Work
- Validating new components for design system
- Reviewing design system documentation
- Ensuring design token usage
- Auditing existing designs for system compliance

### Quality Assurance
- Pre-implementation design validation
- Design QA before stakeholder presentation
- Brand guideline compliance checks
- Accessibility compliance verification

### Collaboration Contexts
- Design critique sessions
- Designer-developer handoff reviews
- Stakeholder design presentations
- User testing preparation

### User Mentions
- "Review this design"
- "Check accessibility of this UI"
- "Validate against design system"
- "Is this design responsive?"
- "Critique this Figma file"
- "Check WCAG compliance"
- "Review component designs"
- "Assess UI/UX quality"

## Related Skills

- **frontend-ui-ux-design**: For creating designs that will be reviewed
- **frontend-coding**: For implementing reviewed designs
- **architecture-design-review**: For system-level architecture review
- **code-quality-security-review**: For reviewing implemented frontend code
- **integration-testing**: For testing implemented designs

## Additional Resources

- WCAG 2.1 Guidelines: https://www.w3.org/WAI/WCAG21/quickref/
- Material Design Guidelines: https://material.io/design
- Apple Human Interface Guidelines: https://developer.apple.com/design/human-interface-guidelines/
- Inclusive Design Principles: https://inclusivedesignprinciples.org/
- Web Content Accessibility Guidelines: https://www.w3.org/WAI/standards-guidelines/wcag/
