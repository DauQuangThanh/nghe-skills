---
name: git-commit
description: Generates well-structured git commit messages following conventional commit standards and best practices. Creates clear, descriptive commits with proper type prefixes (feat, fix, docs, refactor, etc.), concise subjects, and detailed bodies when needed. Use when committing code changes, creating git commits, writing commit messages, or when users mention "commit", "git commit", "commit message", "conventional commits", "changelog", or need help structuring version control messages. Ensures commits are atomic, descriptive, and follow team conventions.
---

# Git Commit Message Generator

Generate professional, conventional commit messages that are clear, descriptive, and follow industry best practices.

## Overview

This skill helps create high-quality git commit messages that:
- Follow conventional commit format
- Are clear and descriptive
- Provide appropriate context
- Enable automated changelog generation
- Support semantic versioning
- Improve team collaboration

## Commit Message Structure

### Basic Format

```
<type>(<scope>): <subject>

<body>

<footer>
```

**Components:**
- **type**: Category of change (required)
- **scope**: Area affected (optional)
- **subject**: Brief description (required, ≤50 chars)
- **body**: Detailed explanation (optional, wrap at 72 chars)
- **footer**: Breaking changes, issue refs (optional)

## Commit Types

### Primary Types

**feat**: New feature for users
```
feat(auth): add OAuth2 authentication
feat(api): implement user profile endpoint
feat: add dark mode toggle
```

**fix**: Bug fix for users
```
fix(login): resolve session timeout issue
fix(api): handle null response from database
fix: prevent memory leak in image processing
```

**docs**: Documentation changes
```
docs(readme): update installation instructions
docs(api): add authentication examples
docs: fix typos in contributing guide
```

**refactor**: Code changes that neither fix bugs nor add features
```
refactor(auth): simplify token validation logic
refactor(db): extract query builder to separate module
refactor: convert callbacks to async/await
```

**perf**: Performance improvements
```
perf(search): optimize query with database indexing
perf(render): reduce component re-renders with memoization
perf: implement lazy loading for images
```

**test**: Adding or updating tests
```
test(auth): add unit tests for login flow
test(api): increase coverage for error scenarios
test: add integration tests for checkout process
```

**build**: Changes to build system or dependencies
```
build(deps): upgrade react to v18.2.0
build(webpack): optimize bundle size configuration
build: add npm script for production build
```

**ci**: Changes to CI/CD configuration
```
ci(github): add automated testing workflow
ci(deploy): configure staging environment
ci: update deployment pipeline timeout
```

**chore**: Other changes that don't modify src or test files
```
chore(deps): update development dependencies
chore: remove unused configuration files
chore(release): bump version to 2.1.0
```

**style**: Code style changes (formatting, missing semicolons, etc.)
```
style(eslint): fix linting errors
style: format code with prettier
style(css): organize stylesheet properties
```

**revert**: Reverting a previous commit
```
revert: revert "feat(api): add user endpoint"

This reverts commit abc123def456
```

## Writing Guidelines

### Subject Line Rules

**DO:**
- ✅ Use imperative mood: "add" not "added" or "adds"
- ✅ Keep under 50 characters
- ✅ Don't end with period
- ✅ Capitalize first letter after type/scope
- ✅ Be specific and descriptive

**Examples:**
```
✅ feat(auth): add password reset functionality
✅ fix(api): resolve race condition in user updates
✅ refactor(db): simplify connection pooling logic
```

**DON'T:**
```
❌ feat: added stuff
❌ fix: fixes
❌ update: updated some files
❌ misc changes
❌ WIP
```

### Scope Selection

**Common Scopes by Project Type:**

**Web Application:**
- `auth`, `api`, `ui`, `db`, `config`, `router`, `state`, `validation`

**Library/SDK:**
- `core`, `utils`, `types`, `docs`, `examples`, `tests`

**Mobile App:**
- `ios`, `android`, `ui`, `navigation`, `storage`, `network`

**Microservices:**
- `user-service`, `payment-service`, `gateway`, `auth`, `logging`

**Omit scope when change affects multiple areas or entire project.**

### Body Content

**When to include a body:**
- Change requires explanation
- Multiple related changes
- Context about "why" not obvious
- Breaking changes need documentation
- Complex bug fixes

**Body Format:**
```
<type>(<scope>): <subject>
[blank line]
- Explain what changed
- Explain why it changed
- Reference relevant context
- Note any side effects
[blank line]
<footer>
```

**Example:**
```
fix(auth): prevent token expiry race condition

- Add mutex lock around token refresh operations
- Implement exponential backoff for retry attempts
- Update token validation to check expiry before use

This fixes an issue where concurrent requests could cause
multiple token refresh attempts, leading to authentication
failures.

Closes #456
```

### Footer Usage

**Breaking Changes:**
```
feat(api): change response format to JSON:API spec

BREAKING CHANGE: API responses now follow JSON:API format.
Update client code to parse data from `data` key instead
of root level. See migration guide in docs/MIGRATION.md
```

**Issue References:**
```
Closes #123
Fixes #456, #789
Resolves #234
Related to #567
```

**Multiple References:**
```
This change affects multiple systems and resolves several issues.

Closes #123, #456
Related to #789
See also: #234
```

## Commit Workflow

### 1. Review Changes

Before committing, understand what changed:
```bash
git status          # See modified files
git diff            # See specific changes
git diff --staged   # See staged changes
```

### 2. Stage Appropriately

Create atomic commits (one logical change per commit):
```bash
git add <specific-files>        # Stage specific files
git add -p                      # Stage specific hunks interactively
```

### 3. Generate Message

**For simple changes:**
```bash
git commit -m "type(scope): subject"
```

**For complex changes:**
```bash
git commit
# Opens editor for multi-line message
```

### 4. Verify Message

Check commit before pushing:
```bash
git log -1          # View last commit
git show            # View last commit with diff
git commit --amend  # Modify last commit message
```

## Common Scenarios

### Feature Addition

```
feat(checkout): add coupon code validation

- Implement coupon validation API endpoint
- Add coupon input field to checkout form
- Display discount amount in order summary
- Handle expired and invalid coupon errors

Closes #234
```

### Bug Fix

```
fix(payment): resolve duplicate charge issue

Race condition in payment processing caused some
transactions to be charged twice. Added transaction
locking to prevent concurrent processing of same order.

- Add distributed lock with 30s timeout
- Implement idempotency key validation
- Add retry logic for lock acquisition failures

Fixes #567
```

### Refactoring

```
refactor(user): extract validation to separate module

Move user validation logic from controller to dedicated
validator class for better testability and reuse.

- Create UserValidator class
- Move validation rules from UserController
- Add comprehensive validation tests
- Update documentation
```

### Documentation

```
docs(api): add authentication flow examples

Add code examples showing:
- OAuth2 authorization flow
- JWT token refresh process
- Error handling patterns

Related to #123
```

### Performance Improvement

```
perf(search): implement query result caching

Add Redis caching layer for search queries to reduce
database load and improve response times.

Results:
- Average response time: 450ms → 45ms
- Database queries reduced by 80%
- Cache hit rate: 92%

Closes #789
```

### Breaking Change

```
feat(api)!: migrate to GraphQL schema v2

BREAKING CHANGE: GraphQL schema updated to v2 with
field naming changes and deprecated field removals.

Changes:
- Rename `userId` to `id` in User type
- Remove deprecated `fullName` field (use `firstName` + `lastName`)
- Change `createdAt` format to ISO 8601

Migration guide: docs/migration/v1-to-v2.md

Closes #456
```

### Multiple Related Changes

```
refactor(auth): modernize authentication system

Comprehensive authentication refactor including:

- Replace session-based auth with JWT tokens
- Implement refresh token rotation
- Add OAuth2 provider support (Google, GitHub)
- Migrate password hashing to bcrypt
- Update security headers and CORS config

This improves security, performance, and enables SSO.
Session data migration script provided in scripts/migrate-sessions.js

Closes #234, #567, #890
```

## Best Practices

### Atomic Commits

**DO:** One logical change per commit
```
✅ Commit 1: feat(auth): add login endpoint
✅ Commit 2: feat(auth): add registration endpoint
✅ Commit 3: test(auth): add authentication tests
```

**DON'T:** Multiple unrelated changes
```
❌ Commit: feat(auth): add login, fix button styling, update docs, refactor utils
```

### Meaningful Messages

**DO:** Describe what and why
```
✅ fix(api): handle null values in user preferences

User preferences API crashed when optional fields were null.
Added null checks and default values.
```

**DON'T:** Vague descriptions
```
❌ fix: bug fix
❌ update: changes
❌ misc: various updates
```

### Present Tense, Imperative Mood

**DO:**
```
✅ add feature
✅ fix bug
✅ update documentation
✅ remove deprecated code
```

**DON'T:**
```
❌ added feature
❌ fixing bug
❌ updated documentation
❌ removes deprecated code
```

### Reference Issues

**Always link to issue tracker:**
```
Closes #123
Fixes #456
Resolves #789
Related to #234
```

### Breaking Changes Visibility

**Use exclamation mark for breaking changes:**
```
feat(api)!: change authentication method

BREAKING CHANGE: detailed description
```

**Or in footer:**
```
feat(api): change authentication method

BREAKING CHANGE: detailed description
```

## Team Conventions

### Establish Project Standards

Document in `CONTRIBUTING.md`:
- Required commit types
- Scope naming conventions
- When to include body
- Issue reference format
- Breaking change policy

### Example Team Convention

```markdown
## Commit Message Convention

All commits must follow conventional commit format:

**Required Types:** feat, fix, docs, refactor, test, chore
**Optional Scopes:** frontend, backend, api, db, auth, ui
**Subject Length:** Maximum 50 characters
**Body:** Required for breaking changes and complex fixes
**Footer:** Must reference issue number when applicable
```

### Enforce with Tools

**commitlint:**
```json
{
  "extends": ["@commitlint/config-conventional"],
  "rules": {
    "type-enum": [2, "always", [
      "feat", "fix", "docs", "refactor", "test", "chore", "perf", "ci", "build"
    ]],
    "subject-max-length": [2, "always", 50],
    "scope-enum": [2, "always", [
      "auth", "api", "ui", "db", "config"
    ]]
  }
}
```

**Git hooks (husky):**
```json
{
  "husky": {
    "hooks": {
      "commit-msg": "commitlint -E HUSKY_GIT_PARAMS"
    }
  }
}
```

## Commit Message Templates

### Template Setup

Create `.gitmessage` template:
```
# <type>(<scope>): <subject> (max 50 chars)

# Why is this change necessary?


# How does it address the issue?


# What side effects does this change have?


# Issue references (Closes #123, Fixes #456):

```

Configure git to use template:
```bash
git config --global commit.template ~/.gitmessage
```

## Advanced Patterns

### Co-authored Commits

For pair programming:
```
feat(payment): implement Stripe integration

Co-authored-by: Jane Doe <jane@example.com>
Co-authored-by: John Smith <john@example.com>
```

### Signed Commits

For verified commits:
```bash
git commit -S -m "feat(auth): add 2FA support"
```

Message includes:
```
feat(auth): add 2FA support

Signed-off-by: Developer Name <dev@example.com>
```

### Fixup and Squash

For cleaner history:
```bash
git commit --fixup=<commit-hash>   # Mark as fixup
git rebase -i --autosquash         # Squash fixups
```

## Common Mistakes to Avoid

### Vague Messages
```
❌ fix: fixed issue
❌ update: updates
❌ changes
❌ WIP
❌ asdf
```

### Too Much Detail in Subject
```
❌ feat(auth): add new authentication system with OAuth2, JWT tokens, refresh token rotation, and password hashing
```
Better:
```
✅ feat(auth): implement OAuth2 authentication

- Add JWT token generation and validation
- Implement refresh token rotation
- Upgrade password hashing to bcrypt
```

### Missing Context
```
❌ fix(api): fix bug
```
Better:
```
✅ fix(api): prevent null pointer in user lookup

API crashed when querying users with deleted accounts.
Added null check and return 404 for deleted users.

Fixes #456
```

### Multiple Unrelated Changes
```
❌ feat: add login, update button styles, fix typo, refactor utils
```
Better: Split into 4 separate commits

### Wrong Tense/Mood
```
❌ fixed the login bug
❌ adding new feature
❌ updated documentation
```
Better:
```
✅ fix(auth): resolve login bug
✅ feat(api): add new endpoint
✅ docs(readme): update installation guide
```

## Quick Reference

### Type Decision Tree

```
Does it add new user-facing functionality? → feat
Does it fix a user-facing bug? → fix
Is it only documentation? → docs
Does it change code structure without changing behavior? → refactor
Does it improve performance? → perf
Is it adding/updating tests? → test
Is it dependency/build/CI related? → build/ci/chore
Is it code formatting/style? → style
Are you reverting a commit? → revert
```

### Subject Line Checklist

- [ ] Type and optional scope present: `type(scope):`
- [ ] Imperative mood: "add" not "added"
- [ ] Under 50 characters
- [ ] No period at end
- [ ] Describes what the commit does
- [ ] Specific and meaningful

### Body Checklist

- [ ] Blank line after subject
- [ ] Explains "why" not just "what"
- [ ] Lines wrapped at 72 characters
- [ ] Bullets or paragraphs for multiple points
- [ ] Blank line before footer

### Footer Checklist

- [ ] Breaking changes clearly marked
- [ ] Issue references included
- [ ] Co-author credits if applicable

## Examples by Project Type

### Web Application

```
feat(dashboard): add real-time analytics widget
fix(auth): resolve session expiry notification
refactor(api): simplify error handling middleware
perf(search): optimize full-text search queries
docs(readme): add deployment instructions
```

### Library/Package

```
feat(core): add async/await support
fix(parser): handle malformed JSON gracefully
refactor(types): improve TypeScript definitions
test(utils): increase coverage to 95%
build(deps): update peer dependencies
```

### Mobile App

```
feat(ios): implement biometric authentication
fix(android): resolve crash on API 29+
perf(ui): optimize list rendering performance
refactor(navigation): migrate to React Navigation v6
ci(fastlane): automate beta deployment
```

### Microservices

```
feat(user-service): add email verification
fix(payment-service): handle webhook retries
refactor(gateway): extract rate limiting logic
perf(db): add indexes for common queries
ci(k8s): update deployment manifests
```

---

## Key Principles

1. **Be specific**: Describe exactly what changed
2. **Be consistent**: Follow team conventions
3. **Be atomic**: One logical change per commit
4. **Be clear**: Write for others (including future you)
5. **Be complete**: Include why and context when needed
6. **Be conventional**: Follow standard format for automation

Good commit messages are documentation for your code's evolution. Invest time in writing them well.
