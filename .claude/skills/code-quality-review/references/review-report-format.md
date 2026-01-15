# Review Report Format


### Executive Summary
```
Code Quality Score: [X/100]
Maintainability Index: [X]
Technical Debt Ratio: [X%]
Critical Issues: [N]
Major Issues: [N]
Minor Issues: [N]

Overall Assessment: [Brief summary]
Top Priorities: [Top 3-5 issues to address]
```

### Detailed Findings

For each issue, provide:

```markdown
#### Issue [N]: [Brief Title]

**Severity:** Critical | Major | Minor | Info
**Category:** Code Smell | Complexity | Naming | Duplication | Design | Performance | Security | Documentation
**Location:** [file.ext#LX-LY]

**Description:**
[Clear explanation of the issue]

**Current Code:**
```[language]
[problematic code snippet]
```

**Impact:**
- Maintainability: [impact level]
- Readability: [impact level]
- Performance: [impact level if relevant]
- Risk: [potential problems]

**Recommendation:**
[Specific improvement suggestion]

**Improved Code:**
```[language]
[suggested improvement]
```

**Effort:** Low | Medium | High
**Priority:** P0 | P1 | P2 | P3
```

### Metrics Summary

```markdown
| Metric | Current | Target | Status |
|--------|---------|--------|--------|
| Cyclomatic Complexity (avg) | X | <10 | ⚠️ |
| Lines per Method (avg) | X | <50 | ✅ |
| Code Duplication | X% | <5% | ⚠️ |
| Test Coverage | X% | >80% | ❌ |
| Documentation Coverage | X% | >70% | ✅ |
| Technical Debt Ratio | X% | <5% | ⚠️ |
```

### Recommendations by Priority

**P0 - Critical (Fix Immediately):**
1. [Issue with major impact]
2. [Issue with major impact]

**P1 - High (Fix Soon):**
1. [Important issue]
2. [Important issue]

**P2 - Medium (Plan for Next Sprint):**
1. [Moderate issue]
2. [Moderate issue]

**P3 - Low (Technical Debt Backlog):**
1. [Minor improvement]
2. [Minor improvement]

### Positive Observations

Acknowledge good practices:
- Well-structured modules
- Excellent test coverage in module X
- Clear naming conventions
- Good error handling in component Y
- Effective use of design patterns

### Technical Debt Summary

```
Total Estimated Effort: [X] person-days
- P0 Issues: [X] days
- P1 Issues: [X] days
- P2 Issues: [X] days
- P3 Issues: [X] days

ROI Analysis:
- Reduced maintenance time: [X] hours/month
- Improved developer productivity: [X]%
- Reduced bug rate: [estimated reduction]
```
