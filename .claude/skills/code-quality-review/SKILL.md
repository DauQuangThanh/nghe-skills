---
name: code-quality-review
description: Conducts comprehensive code quality reviews including code smells detection, maintainability assessment, complexity analysis, design pattern evaluation, naming conventions, code duplication, technical debt identification, and best practices validation. Produces detailed review reports with specific issues, severity ratings, metrics analysis, and actionable improvement recommendations. Use when reviewing code quality, analyzing code maintainability, detecting code smells, checking coding standards, measuring code complexity, identifying technical debt, or when users mention "code quality review", "code quality check", "maintainability analysis", "code smells", "clean code", "refactoring candidates", or "technical debt assessment".
---

# Code Quality Review

## Overview

This skill provides systematic code quality analysis across multiple dimensions including maintainability, readability, complexity, design patterns, and adherence to best practices. It produces actionable feedback with severity levels and specific improvement recommendations.

## Review Workflow

### Step 1: Initial Assessment

**Gather Context:**
- Identify programming language and framework
- Understand project type (web app, API, library, CLI, etc.)
- Note any existing coding standards or style guides
- Check for configuration files (.eslintrc, .pylintrc, checkstyle.xml, etc.)

**Read the Code:**
- Start with entry points (main files, index files)
- Review module/package organization
- Check dependency management
- Examine test files if available

### Step 2: Quality Dimensions Analysis

Analyze code across these key dimensions:

#### 2.1 Code Smells Detection

**Common Code Smells to Identify:**

**Bloaters:**
- Long Method (>50 lines)
- Large Class (>300 lines or >10 methods)
- Primitive Obsession (overuse of primitives instead of objects)
- Long Parameter List (>3-4 parameters)
- Data Clumps (groups of variables passed together)

**Object-Orientation Abusers:**
- Switch/Case statements (should use polymorphism)
- Temporary Field (fields used only in certain cases)
- Refused Bequest (subclass doesn't use inherited methods)
- Alternative Classes with Different Interfaces

**Change Preventers:**
- Divergent Change (one class changes for multiple reasons)
- Shotgun Surgery (one change requires many small changes)
- Parallel Inheritance Hierarchies

**Dispensables:**
- Comments (excessive or outdated comments)
- Duplicate Code
- Lazy Class (class doing too little)
- Data Class (class with only getters/setters)
- Dead Code (unused code)
- Speculative Generality (unused abstractions)

**Couplers:**
- Feature Envy (method using more features from another class)
- Inappropriate Intimacy (excessive coupling between classes)
- Message Chains (a.getB().getC().getD())
- Middle Man (class delegating all work)

#### 2.2 Complexity Analysis

**Cyclomatic Complexity:**
- Calculate decision points (if, for, while, case, &&, ||)
- **Low Risk**: Complexity 1-10
- **Moderate Risk**: Complexity 11-20
- **High Risk**: Complexity 21-50
- **Very High Risk**: Complexity >50

**Cognitive Complexity:**
- Assess how difficult code is to understand
- Identify nested conditions, loops, recursion
- Flag methods with high cognitive load

**Example Analysis:**
```python
# High Cyclomatic Complexity (>15)
def process_order(order):
    if order.type == 'standard':
        if order.priority == 'high':
            if order.value > 1000:
                # ... nested logic
            elif order.value > 500:
                # ... more conditions
        elif order.priority == 'normal':
            # ... more branches
    elif order.type == 'express':
        # ... even more conditions
    # Recommendation: Extract to smaller methods or use strategy pattern
```

#### 2.3 Maintainability Assessment

**Maintainability Index (MI):**
- Calculate based on: MI = 171 - 5.2 * ln(Halstead Volume) - 0.23 * (Cyclomatic Complexity) - 16.2 * ln(Lines of Code)
- **Good**: MI > 85
- **Moderate**: MI 65-85
- **Difficult**: MI < 65

**Key Factors:**
- Code readability (clear naming, logical structure)
- Modularity (separation of concerns)
- Documentation quality
- Test coverage
- Dependency management

#### 2.4 Naming Conventions

**Check for:**
- Consistent naming style (camelCase, snake_case, PascalCase)
- Descriptive names (avoid single letters except loop counters)
- Appropriate length (not too short, not too long)
- Meaningful abbreviations only
- Boolean names starting with is/has/can/should
- Function names as verbs, class names as nouns

**Examples:**

❌ **Poor Naming:**
```javascript
function proc(d) {  // Unclear function name and parameter
    let x = d * 2;
    return x;
}

class mgr {  // Non-descriptive class name
    // ...
}
```

✅ **Good Naming:**
```javascript
function calculateDiscountedPrice(originalPrice) {
    const discountMultiplier = 2;
    return originalPrice * discountMultiplier;
}

class OrderManager {
    // ...
}
```

#### 2.5 Code Duplication

**Detection:**
- Identify duplicate code blocks (>6 lines)
- Look for similar logic with minor variations
- Check for copy-paste patterns

**Metrics:**
- Calculate duplication percentage
- Identify duplication hotspots

**Recommendation Template:**
```
Found: 3 instances of duplicate code (45 lines total)
Location 1: [file1.js#L120-L165]
Location 2: [file2.js#L89-L134]
Location 3: [file3.js#L201-L246]

Recommendation: Extract common logic to shared utility function
Potential name: formatAndValidateUserInput()
Expected reduction: 135 lines → 45 lines (67% reduction)
```

#### 2.6 Design Patterns and Architecture

**Evaluate:**
- Appropriate use of design patterns (Strategy, Factory, Observer, etc.)
- SOLID principles adherence:
  - Single Responsibility Principle
  - Open/Closed Principle
  - Liskov Substitution Principle
  - Interface Segregation Principle
  - Dependency Inversion Principle
- Separation of concerns
- Dependency injection usage
- Layer separation (presentation, business, data)

**Anti-patterns to Flag:**
- God Object (class doing everything)
- Spaghetti Code (tangled dependencies)
- Golden Hammer (overusing one pattern)
- Cargo Cult Programming (code without understanding)
- Hard Coding (values that should be configurable)

#### 2.7 Error Handling

**Check for:**
- Consistent error handling strategy
- Appropriate exception types
- Error messages quality (descriptive, actionable)
- Resource cleanup (try-finally, context managers)
- Graceful degradation
- Logging at appropriate levels

**Examples:**

❌ **Poor Error Handling:**
```python
def read_file(filename):
    f = open(filename)  # No error handling, resource leak
    data = f.read()
    return data
```

✅ **Good Error Handling:**
```python
def read_file(filename):
    try:
        with open(filename, 'r') as f:
            return f.read()
    except FileNotFoundError:
        logger.error(f"File not found: {filename}")
        raise
    except PermissionError:
        logger.error(f"Permission denied: {filename}")
        raise
    except Exception as e:
        logger.error(f"Unexpected error reading {filename}: {e}")
        raise
```

#### 2.8 Performance Considerations

**Review:**
- Algorithm efficiency (O(n), O(n²), etc.)
- Database query optimization (N+1 queries, missing indexes)
- Memory usage patterns
- Unnecessary computations
- Caching opportunities
- Resource pooling

**Common Issues:**
```javascript
// ❌ O(n²) - inefficient
users.forEach(user => {
    orders.forEach(order => {
        if (order.userId === user.id) {
            // process
        }
    });
});

// ✅ O(n) - efficient with Map
const ordersByUser = new Map();
orders.forEach(order => {
    if (!ordersByUser.has(order.userId)) {
        ordersByUser.set(order.userId, []);
    }
    ordersByUser.get(order.userId).push(order);
});
users.forEach(user => {
    const userOrders = ordersByUser.get(user.id) || [];
    // process
});
```

### Step 3: Language-Specific Analysis

Apply language-specific best practices:

**JavaScript/TypeScript:**
- Use strict mode
- Avoid var, prefer const/let
- Use async/await over callbacks
- Proper promise error handling
- Type safety (TypeScript)
- Avoid implicit any

**Python:**
- PEP 8 compliance
- Type hints usage
- List comprehensions appropriately
- Context managers for resources
- Avoid mutable default arguments
- Virtual environment usage

**Java:**
- Proper use of collections
- Stream API usage
- Exception hierarchy
- Thread safety
- Resource management (try-with-resources)
- Immutability where appropriate

**Go:**
- Error handling patterns
- Goroutine management
- Defer usage
- Interface design
- Package organization
- Effective Go guidelines

**C#:**
- LINQ usage
- Async/await patterns
- IDisposable implementation
- Nullable reference types
- Dependency injection
- .NET conventions

### Step 4: Documentation Quality

**Assess:**
- Code comments (when needed, not excessive)
- Function/method documentation
- Class/module documentation
- API documentation
- README quality
- Inline documentation for complex logic

**Guidelines:**
- Comments explain WHY, not WHAT
- Public APIs fully documented
- Complex algorithms explained
- TODOs tracked and dated
- No commented-out code

### Step 5: Test Quality Assessment

**Review:**
- Test coverage percentage
- Test organization (unit, integration, e2e)
- Test naming conventions
- Assertion quality
- Test data management
- Mock usage appropriateness
- Test maintainability

### Step 6: Generate Review Report

## Review Report Format

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

## Quality Metrics Reference

### Cyclomatic Complexity Thresholds
- 1-10: Simple, low risk
- 11-20: Moderate complexity, moderate risk
- 21-50: Complex, high risk
- >50: Very complex, very high risk

### Maintainability Index
- 85-100: Good maintainability
- 65-84: Moderate maintainability
- 0-64: Difficult to maintain

### Code Duplication
- <3%: Excellent
- 3-5%: Good
- 5-10%: Acceptable
- >10%: Needs attention

### Test Coverage
- >80%: Excellent
- 60-80%: Good
- 40-60%: Moderate
- <40%: Insufficient

## Best Practices Checklist

Use this checklist for consistent reviews:

**Code Organization:**
- [ ] Logical directory structure
- [ ] Clear module boundaries
- [ ] Appropriate file sizes (<500 lines)
- [ ] Single responsibility per file

**Code Style:**
- [ ] Consistent formatting
- [ ] Style guide compliance
- [ ] Meaningful names throughout
- [ ] No magic numbers

**Complexity:**
- [ ] Methods under 50 lines
- [ ] Cyclomatic complexity <15
- [ ] Maximum nesting depth <4
- [ ] No god objects

**Design:**
- [ ] SOLID principles followed
- [ ] Appropriate design patterns
- [ ] Low coupling, high cohesion
- [ ] Interface-based design

**Error Handling:**
- [ ] Consistent error strategy
- [ ] Appropriate exception types
- [ ] Proper resource cleanup
- [ ] Meaningful error messages

**Testing:**
- [ ] Adequate test coverage
- [ ] Test quality (not just coverage)
- [ ] Tests are maintainable
- [ ] Edge cases covered

**Documentation:**
- [ ] Public API documented
- [ ] Complex logic explained
- [ ] README is comprehensive
- [ ] Examples provided

**Performance:**
- [ ] Efficient algorithms
- [ ] No obvious bottlenecks
- [ ] Appropriate caching
- [ ] Resource management

## Common Pitfalls to Avoid

**During Review:**
- Don't focus only on style issues - prioritize substantive problems
- Don't be overly critical without actionable suggestions
- Don't ignore context - understand business constraints
- Don't nitpick minor issues in critical code reviews
- Don't overwhelm with too many issues at once

**In Reports:**
- Don't use vague terms like "bad" or "poor" without explanation
- Don't suggest refactoring without clear benefit
- Don't ignore existing conventions without good reason
- Don't recommend changes without considering backward compatibility
- Don't forget to acknowledge good practices

## Review Scope Guidelines

**Small Change (<100 lines):**
- Focus on correctness and immediate issues
- Quick turnaround (15-30 minutes)

**Medium Change (100-500 lines):**
- Full quality analysis
- Design pattern review
- Typical turnaround (1-2 hours)

**Large Change (>500 lines):**
- Architectural review
- Break into smaller reviews if possible
- Multiple passes (2-4 hours)

**Refactoring:**
- Ensure behavior preservation
- Check test coverage
- Validate performance impact

## Output Guidelines

**Be Specific:**
- Reference exact line numbers
- Quote problematic code
- Provide concrete examples

**Be Constructive:**
- Explain WHY something is an issue
- Suggest HOW to fix it
- Provide improved code examples

**Be Balanced:**
- Acknowledge good practices
- Prioritize issues appropriately
- Consider effort vs. benefit

**Be Professional:**
- Focus on code, not developer
- Use objective criteria
- Provide educational context

## Example Review Output

```markdown
# Code Quality Review Report

**Project:** User Authentication Service
**Reviewed Files:** 15 files, 2,341 lines
**Review Date:** 2026-01-14
**Reviewer:** GitHub Copilot

## Executive Summary

**Overall Quality Score:** 72/100 (Moderate)
**Maintainability Index:** 68 (Moderate)
**Technical Debt Ratio:** 8.5%
**Critical Issues:** 2
**Major Issues:** 5
**Minor Issues:** 12

The codebase shows good structure and clear intent, but suffers from high complexity in core authentication logic and insufficient error handling. Priority focus should be on reducing cyclomatic complexity in AuthenticationManager and implementing consistent error handling throughout.

**Top 3 Priorities:**
1. Reduce complexity in AuthenticationManager.authenticate() (Complexity: 28)
2. Implement consistent error handling strategy
3. Address code duplication in validation logic (18% duplication)

---

## Critical Issues

### Issue 1: Excessive Cyclomatic Complexity

**Severity:** Critical
**Category:** Complexity
**Location:** [src/auth/AuthenticationManager.java#L45-L156]

**Description:**
The `authenticate()` method has a cyclomatic complexity of 28, significantly exceeding the recommended threshold of 15. This makes the code difficult to test, maintain, and understand.

**Current Code:**
```java
public AuthResult authenticate(Credentials creds) {
    if (creds == null) {
        if (allowAnonymous) {
            if (isAnonymousAllowedForEndpoint()) {
                // ... 20 more levels of nesting
            }
        }
    } else if (creds.getType() == AuthType.BASIC) {
        if (validateBasic(creds)) {
            // ... more branching
        }
    } // ... continues for 111 lines
}
```

**Impact:**
- Maintainability: High negative impact - difficult to modify
- Testability: Requires 28+ test cases for full coverage
- Bug Risk: High probability of edge case bugs

**Recommendation:**
Extract authentication logic into strategy pattern with separate validators for each auth type.

**Improved Code:**
```java
public AuthResult authenticate(Credentials creds) {
    AuthenticationStrategy strategy = strategyFactory.getStrategy(creds);
    return strategy.authenticate(creds);
}

// Separate classes: BasicAuthStrategy, OAuth2Strategy, AnonymousStrategy
// Each with complexity < 8
```

**Effort:** High (2-3 days)
**Priority:** P0

---

### Issue 2: No Error Handling in Database Operations

**Severity:** Critical
**Category:** Error Handling
**Location:** [src/db/UserRepository.java#L67-L89]

**Description:**
Database operations lack try-catch blocks, potentially causing application crashes on connection failures.

**Current Code:**
```java
public User findById(String id) {
    Connection conn = dataSource.getConnection();
    PreparedStatement stmt = conn.prepareStatement("SELECT * FROM users WHERE id = ?");
    stmt.setString(1, id);
    ResultSet rs = stmt.executeQuery();
    return mapToUser(rs);
    // No resource cleanup, no error handling
}
```

**Recommendation:**
```java
public User findById(String id) {
    try (Connection conn = dataSource.getConnection();
         PreparedStatement stmt = conn.prepareStatement("SELECT * FROM users WHERE id = ?")) {
        
        stmt.setString(1, id);
        try (ResultSet rs = stmt.executeQuery()) {
            return mapToUser(rs);
        }
    } catch (SQLException e) {
        logger.error("Database error finding user: " + id, e);
        throw new DataAccessException("Failed to retrieve user", e);
    }
}
```

**Effort:** Medium (1 day)
**Priority:** P0

---

## Positive Observations

✅ **Excellent Test Coverage:** 87% overall coverage with quality assertions
✅ **Clear Naming:** Consistent and descriptive naming throughout
✅ **Good Package Structure:** Clean separation of concerns
✅ **Effective Logging:** Comprehensive logging at appropriate levels
✅ **Modern Java Features:** Good use of streams, optional, and try-with-resources

---

## Recommendations Summary

**Immediate Actions (P0):**
- Refactor AuthenticationManager.authenticate() using strategy pattern
- Add comprehensive error handling to all database operations

**Near Term (P1):**
- Extract duplicate validation logic to shared utilities
- Add input validation to public API methods
- Document public interfaces

**Planned Improvements (P2):**
- Reduce average method length from 42 to <30 lines
- Increase documentation coverage from 45% to 70%
- Implement caching for frequent queries

**Technical Debt (P3):**
- Replace deprecated Spring Security methods
- Migrate to Java 17 features
- Update third-party dependencies

**Estimated Total Effort:** 8-10 person-days
**Expected Quality Improvement:** +15-20 points
```

This comprehensive review provides actionable insights while maintaining professional, constructive feedback focused on improving code quality.
