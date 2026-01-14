---
name: frontend-code-review
description: Conducts comprehensive frontend code reviews including React/Vue/Angular component analysis, TypeScript/JavaScript quality assessment, CSS/styling review, performance optimization, accessibility compliance, security vulnerabilities, and best practices validation. Produces detailed review reports with specific issues, severity ratings, and actionable recommendations. Use when reviewing frontend code, analyzing React/Vue/Angular components, checking JavaScript/TypeScript quality, validating CSS/SCSS, assessing web performance, or when users mention "review frontend code", "check React components", "analyze JavaScript", "review TypeScript", "validate accessibility", or "frontend code quality".
---

# Frontend Code Review

Systematically review frontend code to identify issues, ensure quality, and provide actionable improvement recommendations.

## Purpose

Evaluate frontend code for quality, maintainability, performance, accessibility, and security. Identify code smells, anti-patterns, and potential bugs before they reach production.

## When to Use This Skill

- Review React, Vue, Angular, or vanilla JavaScript code
- Assess component architecture and design patterns
- Validate TypeScript types and JavaScript code quality
- Review CSS/SCSS/styled-components/Tailwind implementations
- Check accessibility compliance (WCAG 2.1 AA/AAA)
- Identify performance bottlenecks and optimization opportunities
- Detect security vulnerabilities (XSS, CSRF, injection attacks)
- Pre-merge PR reviews and code quality gates

## Review Workflow

### 1. Initial Assessment

**Gather Context:**
- Project type (React, Vue, Angular, vanilla JS)
- Framework versions and dependencies
- Build tools (Webpack, Vite, Rollup)
- Target browsers and devices
- Accessibility requirements (WCAG level)
- Performance targets (LCP, FID, CLS)

**Identify Review Scope:**
- New features vs. refactoring
- Component complexity (simple, medium, complex)
- Critical user paths
- Security-sensitive areas

### 2. Code Quality Analysis

**Component Structure:**
- Single Responsibility Principle adherence
- Component composition vs. inheritance
- Props/state management appropriateness
- Lifecycle methods usage (React) or hooks pattern
- Computed properties and watchers (Vue)
- Component communication patterns

**JavaScript/TypeScript Quality:**
- Type safety and inference (TypeScript)
- ESLint/Prettier compliance
- Variable naming conventions
- Function complexity (cyclomatic complexity < 10)
- Error handling and edge cases
- Code duplication (DRY principle)

**State Management:**
- Local vs. global state decisions
- Redux/Vuex/Pinia/Context API usage
- Immutability patterns
- Side effect handling
- State normalization

> **See references/code-quality-checklist.md** for detailed quality criteria and common issues

### 3. Performance Review

**Rendering Performance:**
- Unnecessary re-renders (React.memo, useMemo, useCallback)
- Virtual DOM optimization
- List rendering with keys
- Lazy loading and code splitting
- Bundle size analysis

**Resource Loading:**
- Image optimization (format, size, lazy loading)
- Font loading strategies
- Third-party script impact
- Asset compression
- CDN usage

**Web Vitals:**
- Largest Contentful Paint (LCP < 2.5s)
- First Input Delay (FID < 100ms)
- Cumulative Layout Shift (CLS < 0.1)

> **See references/performance-checklist.md** for detailed performance optimization guidelines

### 4. Accessibility Assessment

**Semantic HTML:**
- Proper heading hierarchy (h1-h6)
- Landmark elements (header, nav, main, footer)
- Lists for list content
- Buttons vs. links usage

**ARIA Implementation:**
- ARIA labels and descriptions
- Role attributes
- Live regions
- Focus management

**Keyboard Navigation:**
- Tab order logical
- Focus indicators visible
- Skip links present
- Keyboard shortcuts

**Screen Reader Support:**
- Alt text for images
- Form labels associated
- Error messages announced
- Dynamic content updates

> **See references/accessibility-checklist.md** for WCAG 2.1 AA/AAA compliance criteria

### 5. Security Review

**Common Vulnerabilities:**
- XSS prevention (sanitize user input)
- CSRF protection
- Authentication token handling
- Sensitive data exposure
- Third-party dependency vulnerabilities

**Best Practices:**
- Content Security Policy (CSP)
- HTTPS enforcement
- Secure cookie attributes
- Input validation
- Output encoding

> **See references/security-checklist.md** for frontend security guidelines

### 6. CSS/Styling Review

**Architecture:**
- CSS methodology (BEM, OOCSS, SMACSS)
- Naming conventions consistency
- Specificity management
- CSS-in-JS best practices (styled-components, Emotion)
- Utility-first frameworks (Tailwind CSS)

**Responsive Design:**
- Mobile-first approach
- Breakpoint strategy
- Flexible layouts (Flexbox, Grid)
- Media queries organization
- Responsive images

**Maintainability:**
- Variable/token usage for colors, spacing
- Avoid magic numbers
- Reusable utility classes
- CSS selector performance
- Unused CSS detection

### 7. Testing Coverage

**Unit Tests:**
- Component logic testing
- Utility function coverage
- Edge case handling
- Mock strategy

**Integration Tests:**
- Component interaction
- API integration
- State management flows
- Routing behavior

**E2E Tests:**
- Critical user journeys
- Cross-browser compatibility
- Accessibility automated tests

### 8. Generate Review Report

Create structured report containing:

**Executive Summary:**
- Overall code quality rating (Excellent/Good/Fair/Poor)
- Critical issues count
- Recommendation (Approve/Approve with Changes/Major Revision Needed)

**Detailed Findings by Category:**
- Code Quality (issues with severity: Critical/Major/Minor)
- Performance (specific metrics and recommendations)
- Accessibility (WCAG violations)
- Security (vulnerabilities identified)
- Styling (CSS/design issues)
- Testing (coverage gaps)

**Action Items:**
- Prioritized list with file/line references
- Specific fix recommendations
- Estimated effort

> **See references/report-templates.md** for full review report formats

## Best Practices

**Review Timing:**
- **Pre-Commit**: Automated linting and formatting
- **Pre-PR**: Self-review using this checklist
- **PR Review**: Peer review with this skill
- **Pre-Deployment**: Final quality gate

**Review Techniques:**
- **Line-by-Line**: Thorough examination of changes
- **Checklist-Based**: Systematic quality verification
- **Automated Tools**: ESLint, Prettier, Lighthouse, axe DevTools
- **Manual Testing**: Run code locally, test user flows

**Severity Definitions:**
- **Critical**: Blocks deployment (security issues, breaking bugs, accessibility blockers)
- **Major**: Should fix before merge (performance issues, poor patterns, maintainability concerns)
- **Minor**: Nice to improve (style suggestions, micro-optimizations, documentation)

## Anti-Patterns to Flag

**React/Component Patterns:**
- Prop drilling (pass props through multiple levels)
- God components (components doing too much)
- Premature optimization
- Missing error boundaries
- Mutating state directly
- Inline functions in JSX causing re-renders

**JavaScript/TypeScript:**
- Any types in TypeScript
- Console.log in production code
- Deeply nested callbacks (callback hell)
- Global variables
- Ignoring promises (not awaiting or catching)

**CSS/Styling:**
- !important overuse
- Inline styles instead of classes
- Fixed pixel values instead of relative units
- No design system/tokens
- CSS specificity wars

**Performance:**
- Synchronous expensive operations in render
- Large bundle sizes (>500KB uncompressed)
- No code splitting
- Unoptimized images
- Missing lazy loading

**Accessibility:**
- Div/span buttons
- Missing alt attributes
- Color-only information conveyance
- Keyboard traps
- No focus management in modals/dialogs

## Quick Reference

### Code Quality Checklist
- [ ] Components follow single responsibility
- [ ] Functions < 50 lines, cyclomatic complexity < 10
- [ ] TypeScript strict mode enabled
- [ ] No ESLint warnings
- [ ] Meaningful variable/function names
- [ ] Error handling present
- [ ] No code duplication
- [ ] Comments for complex logic only

### Performance Checklist
- [ ] React.memo/useMemo/useCallback used appropriately
- [ ] Images optimized (WebP/AVIF, proper sizing)
- [ ] Code splitting implemented
- [ ] Lazy loading for routes/components
- [ ] Bundle size analyzed
- [ ] Web Vitals within targets
- [ ] No memory leaks (cleanup in useEffect)

### Accessibility Checklist
- [ ] Semantic HTML used
- [ ] All images have alt text
- [ ] Form inputs have labels
- [ ] Keyboard navigable
- [ ] Focus indicators visible
- [ ] Color contrast ratio ≥ 4.5:1 (AA) or 7:1 (AAA)
- [ ] ARIA used correctly (or not at all if semantic HTML sufficient)

### Security Checklist
- [ ] User input sanitized
- [ ] XSS prevention (DOMPurify for dangerous HTML)
- [ ] Authentication tokens secure
- [ ] No sensitive data in localStorage
- [ ] Dependencies up to date (no critical vulnerabilities)
- [ ] CSP headers configured

## Additional Resources

- **references/code-quality-checklist.md**: Comprehensive quality criteria, common issues, and fixes
- **references/performance-checklist.md**: Performance optimization techniques and Web Vitals guidelines
- **references/accessibility-checklist.md**: WCAG 2.1 compliance criteria with examples
- **references/security-checklist.md**: Frontend security best practices and vulnerability prevention
- **references/report-templates.md**: Review report templates and examples

## Output Format

Generate review reports with:

1. **Executive Summary** (0.5 page): Overall assessment, critical issues, recommendation
2. **Code Quality Analysis** (1-2 pages): Component structure, JavaScript/TypeScript issues, patterns
3. **Performance Review** (0.5-1 page): Metrics, bottlenecks, optimization recommendations
4. **Accessibility Assessment** (0.5-1 page): WCAG violations, keyboard navigation, screen reader issues
5. **Security Analysis** (0.5 page): Vulnerabilities identified, risk level, remediation steps
6. **CSS/Styling Review** (0.5 page): Architecture issues, responsiveness, maintainability
7. **Action Items** (1 page): Prioritized list with file references and specific fixes

Keep findings specific with file names, line numbers, and code examples. Provide actionable recommendations with concrete fixes.
