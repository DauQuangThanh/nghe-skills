---
name: bug-analysis
description: Analyzes software bugs including root cause identification, severity assessment, impact analysis, reproduction steps validation, and fix recommendations. Performs bug triage, categorization, duplicate detection, and regression analysis. Use when investigating bugs, analyzing crash reports, triaging issues, debugging problems, reviewing error logs, or when users mention "analyze bug", "investigate issue", "debug problem", "bug report", "crash analysis", "root cause analysis", or "fix recommendation".
---

# Bug Analysis

## Overview

This skill provides comprehensive bug analysis capabilities to help identify, categorize, and resolve software defects efficiently. It assists with root cause analysis, impact assessment, severity classification, and generates actionable fix recommendations.

## Core Analysis Workflow

### 1. Initial Bug Assessment

**Collect Information:**
- Bug description and symptoms
- Steps to reproduce
- Expected vs actual behavior
- Environment details (OS, browser, version, configuration)
- Error messages and stack traces
- Logs and screenshots
- Affected users/frequency

**Quick Triage:**
- Assess severity (Critical, High, Medium, Low)
- Determine priority based on impact and urgency
- Check for security implications
- Identify affected components/modules

### 2. Bug Categorization

Classify the bug into appropriate categories:

**By Type:**
- **Functional**: Feature not working as specified
- **Performance**: Slow response, timeouts, resource issues
- **Security**: Vulnerabilities, unauthorized access, data exposure
- **UI/UX**: Visual glitches, layout issues, usability problems
- **Data**: Data corruption, loss, or incorrect processing
- **Integration**: API failures, third-party service issues
- **Configuration**: Environment or deployment issues
- **Regression**: Previously working feature broken

**By Severity:**
- **Critical**: System crash, data loss, security breach, complete feature failure
- **High**: Major feature broken, significant user impact, workaround difficult
- **Medium**: Feature partially broken, moderate impact, workaround available
- **Low**: Minor issue, cosmetic problem, minimal impact

**By Root Cause Pattern:**
- Logic errors
- Race conditions
- Memory leaks
- Null pointer/reference errors
- Boundary conditions
- Configuration issues
- Dependency problems
- Integration failures

### 3. Root Cause Analysis

**Analyze the Evidence:**

1. **Review Error Messages**
   - Parse stack traces to identify failure point
   - Map error codes to known issues
   - Identify exception types and contexts

2. **Examine Code Context**
   - Review relevant code sections
   - Check recent changes (git blame, commit history)
   - Identify potentially related modules
   - Look for similar patterns elsewhere

3. **Reproduce the Issue**
   - Validate reproduction steps
   - Test in different environments
   - Vary inputs to identify boundaries
   - Document consistent reproduction method

4. **Trace Execution Flow**
   - Follow code execution path
   - Identify where actual diverges from expected
   - Check data transformations
   - Review control flow logic

5. **Check Dependencies**
   - Verify library versions
   - Check for known issues in dependencies
   - Test with different dependency versions
   - Review integration points

### 4. Impact Analysis

**Assess the Impact:**

- **User Impact**: Number of affected users, user workflows disrupted
- **Business Impact**: Revenue loss, SLA violations, customer satisfaction
- **System Impact**: Performance degradation, resource consumption, cascading failures
- **Data Impact**: Data integrity, loss, or corruption risks
- **Security Impact**: Exposure level, exploit potential, compliance violations

**Determine Scope:**
- Affected versions/releases
- Affected platforms/browsers
- Affected user segments
- Affected features/workflows
- Potential regression scope

### 5. Duplicate Detection

**Check for Duplicates:**

Search existing bug reports for:
- Similar symptoms or error messages
- Same component/module affected
- Related stack traces
- Matching reproduction steps
- Similar user reports

If duplicate found:
- Link to original issue
- Add additional context if valuable
- Close as duplicate with reference

### 6. Fix Recommendation

**Generate Fix Strategy:**

1. **Immediate Mitigation**
   - Workarounds available to users
   - Configuration changes to reduce impact
   - Feature flags to disable problematic code
   - Rollback options if recent regression

2. **Root Cause Fix**
   - Specific code changes needed
   - Design changes required
   - Data migration or cleanup needed
   - Configuration updates required

3. **Testing Strategy**
   - Unit tests to add
   - Integration tests needed
   - Regression tests to prevent recurrence
   - Performance tests if applicable

4. **Preventive Measures**
   - Code review focus areas
   - Additional validation needed
   - Monitoring/alerting to add
   - Documentation to update

## Analysis Output Format

Provide structured analysis following this template:

```markdown
## Bug Analysis Report

### Summary
- **Bug ID**: [Reference number]
- **Title**: [Concise description]
- **Severity**: [Critical/High/Medium/Low]
- **Priority**: [P0/P1/P2/P3]
- **Type**: [Bug category]
- **Status**: [New/Confirmed/In Progress/Fixed]

### Description
[Clear description of the bug]

### Environment
- **OS**: [Operating system]
- **Browser/Client**: [Browser/app version]
- **Version**: [Software version]
- **Configuration**: [Relevant settings]

### Reproduction Steps
1. [Step 1]
2. [Step 2]
3. [Observe result]

**Expected Behavior**: [What should happen]
**Actual Behavior**: [What actually happens]
**Reproducibility**: [Always/Sometimes/Rare]

### Root Cause Analysis

**Primary Cause**: [Main cause identified]

**Technical Details**:
- [Specific code/logic issue]
- [Contributing factors]
- [Why it occurs]

**Evidence**:
- Error messages/stack traces
- Log entries
- Code references

### Impact Assessment

**Users Affected**: [Number/percentage]
**Business Impact**: [Description]
**Workaround Available**: [Yes/No - describe if yes]

### Recommended Fix

**Immediate Actions**:
1. [Short-term mitigation]
2. [User communication]

**Permanent Solution**:
- [Code changes needed]
- [Files to modify]
- [Design considerations]

**Testing Requirements**:
- [Test cases to add]
- [Regression tests needed]

**Prevention**:
- [How to prevent similar issues]
- [Monitoring to add]

### Related Issues
- [Links to similar/duplicate bugs]
- [Related feature requests]

### Estimated Effort
[Time estimate for fix implementation]
```

## Special Analysis Scenarios

### Security Bug Analysis

For security vulnerabilities:

1. **Assess Severity Using CVSS**
   - Attack vector (Network/Adjacent/Local/Physical)
   - Attack complexity (Low/High)
   - Privileges required (None/Low/High)
   - User interaction (None/Required)
   - Impact on confidentiality, integrity, availability

2. **Identify Exploit Potential**
   - Can it be exploited remotely?
   - Authentication required?
   - Known exploits exist?
   - Attack surface size

3. **Containment Strategy**
   - Immediate security patches
   - Access restrictions
   - Monitoring for exploitation
   - User notification requirements

4. **Disclosure Plan**
   - Responsible disclosure timeline
   - Customer communication
   - Public disclosure considerations
   - Compliance requirements (CVE, etc.)

### Performance Bug Analysis

For performance issues:

1. **Establish Baseline**
   - Expected performance metrics
   - Acceptable thresholds
   - Previous performance data

2. **Identify Bottlenecks**
   - CPU profiling results
   - Memory usage patterns
   - I/O operations
   - Database query performance
   - Network latency

3. **Quantify Degradation**
   - Response time increase
   - Throughput reduction
   - Resource consumption growth
   - User experience impact

4. **Optimization Strategy**
   - Code optimization opportunities
   - Caching strategies
   - Database indexing
   - Architecture improvements

### Crash Analysis

For application crashes:

1. **Analyze Crash Dump**
   - Exception type and message
   - Stack trace analysis
   - Thread states
   - Memory state at crash

2. **Identify Trigger**
   - Specific user action
   - System condition
   - Data input
   - Timing/race condition

3. **Assess Stability Impact**
   - Crash frequency
   - Affected scenarios
   - Data loss risk
   - Recovery capability

## Best Practices

### Information Gathering

- **Ask Clarifying Questions**: Don't assume; verify details
- **Request Logs**: System logs, application logs, browser console
- **Get Screenshots/Videos**: Visual evidence is invaluable
- **Check Monitoring Data**: APM tools, error tracking services
- **Review Recent Changes**: Deployments, configuration updates, dependency changes

### Analysis Principles

- **Start with Evidence**: Base conclusions on concrete data
- **Consider Multiple Hypotheses**: Don't fixate on first theory
- **Test Assumptions**: Verify each assumption made
- **Document Reasoning**: Explain how you reached conclusions
- **Be Systematic**: Follow logical investigation process

### Communication

- **Use Clear Language**: Avoid jargon when communicating with users
- **Provide Context**: Explain why the bug matters
- **Set Expectations**: Realistic timelines and fix complexity
- **Offer Workarounds**: Help users immediately if possible
- **Follow Up**: Update stakeholders on progress

### Prevention Focus

- **Identify Patterns**: Common causes across multiple bugs
- **Suggest Improvements**: Process, testing, code review enhancements
- **Update Documentation**: Lessons learned, common pitfalls
- **Enhance Monitoring**: Add alerts for similar issues
- **Improve Testing**: Add test coverage for bug scenarios

## Quick Reference

### Severity Guidelines

**Critical (P0)**:
- Complete system outage
- Data loss or corruption
- Security breach
- No workaround available
- **Response**: Immediate (< 1 hour)

**High (P1)**:
- Major feature broken
- Significant user impact (>25%)
- Security vulnerability (low risk)
- Difficult workaround
- **Response**: Same day

**Medium (P2)**:
- Feature partially broken
- Moderate user impact
- Workaround available
- **Response**: Within 1 week

**Low (P3)**:
- Minor issue
- Cosmetic problem
- Minimal impact
- **Response**: Backlog/next release

### Common Root Causes Checklist

- [ ] Null/undefined reference
- [ ] Off-by-one error
- [ ] Race condition
- [ ] Memory leak
- [ ] Incorrect validation
- [ ] Missing error handling
- [ ] Configuration issue
- [ ] Dependency version mismatch
- [ ] API contract change
- [ ] Database schema mismatch
- [ ] Incorrect permissions
- [ ] Resource exhaustion
- [ ] Caching issue
- [ ] Timezone/date handling
- [ ] Character encoding problem

### Investigation Commands

```bash
# Check recent changes
git log --since="1 week ago" --oneline -- path/to/file

# Find similar errors in logs
grep -r "error_message" /var/log/app/

# Check dependency versions
npm ls package-name  # Node.js
pip show package-name  # Python
mvn dependency:tree  # Java

# Monitor resource usage
top  # Linux/Mac
Get-Process | Sort-Object CPU -Descending | Select -First 10  # PowerShell

# Analyze heap dump
jmap -dump:format=b,file=heap.bin <pid>  # Java
node --inspect --heap-prof app.js  # Node.js

# Check network issues
curl -v https://api.example.com
ping example.com
traceroute example.com
```

## Integration with Development Workflow

### Bug Lifecycle

1. **New** → Initial report received
2. **Triage** → Analysis and prioritization (use this skill)
3. **Confirmed** → Reproduced and root cause identified
4. **Assigned** → Developer assigned to fix
5. **In Progress** → Fix being implemented
6. **Code Review** → Fix under review
7. **Testing** → QA validation
8. **Fixed** → Deployed to production
9. **Closed** → Verified resolved

### Documentation Requirements

- **Link to Code**: Reference specific files and line numbers
- **Link to Tests**: Reference test cases that verify the fix
- **Link to Monitoring**: Dashboard or alerts for tracking
- **Link to Documentation**: Updated docs if user-facing
- **Link to Related Issues**: Connected bugs, features, or tech debt

## Notes

- Always verify bug is reproducible before deep analysis
- Document all findings even if inconclusive
- Consider both technical and business context
- Engage relevant experts when needed (security, performance, domain)
- Update bug tracking system with analysis results
- Use this analysis to improve prevention strategies
