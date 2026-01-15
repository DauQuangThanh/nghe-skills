---
name: technical-writing
description: Creates high-quality technical documentation including API documentation, user guides, tutorials, architecture documents, README files, release notes, and technical specifications. Produces clear, structured, and comprehensive documentation following industry best practices. Use when writing technical documentation, creating API docs, developing user guides, documenting architecture, writing tutorials, preparing release notes, or when users mention "technical writing", "documentation", "API docs", "user guide", "tutorial", "README", "technical specification", "architecture document", or "developer documentation".
---

# Technical Writing

Creates professional technical documentation with clear structure, appropriate detail level, and user-focused content.

## Workflow

### 1. Identify Documentation Type

**Common types:**
- API Documentation (REST, GraphQL, webhooks)
- User Guides (features, how-tos, troubleshooting)
- Tutorials (learning-focused with hands-on examples)
- Architecture Documents (system design, technical decisions)
- README Files (project overview, quick start)
- Release Notes (changes, migrations, breaking changes)
- Technical Specifications (requirements, constraints)

**Load reference:** For detailed patterns, structure, and examples for each type, see [documentation-types-and-workflows.md](references/documentation-types-and-workflows.md)

### 2. Gather Context

**Essential information:**
- Target audience (developers, end-users, managers)
- Technical depth (beginner, intermediate, advanced)
- Existing codebase/APIs/systems to document
- Style guides or organizational standards
- Related documentation

### 3. Structure Content

**Organization principles:**
- Start with overview/introduction
- Use descriptive heading hierarchy (H1 → H2 → H3)
- Add table of contents for documents >3 sections
- Group related information logically
- Place examples immediately after explanations

### 4. Write Clear Content

**Core principles:**
- Use active voice: "The API returns..." not "The response is returned..."
- Be specific: "Response time < 200ms" not "Fast response"
- Define acronyms on first use: "API (Application Programming Interface)"
- Use consistent terminology throughout
- Write imperative instructions: "Run the command" not "You should run..."
- Show examples for every concept

**Load reference:** For comprehensive writing guidelines, see [writing-guidelines.md](references/writing-guidelines.md)

### 5. Add Code Examples

**Requirements:**
- Specify language in code blocks (```python, ```javascript, etc.)
- Show complete, runnable examples
- Include input/output pairs
- Add comments for complex logic
- Test all code before publishing

### 6. Review and Validate

**Quality checks:**
- Verify technical accuracy
- Test all code examples
- Check clarity and completeness
- Ensure consistent terminology
- Validate all links

## Quick Reference by Type

### API Documentation
**When to load reference:** [documentation-types-and-workflows.md](references/documentation-types-and-workflows.md) for REST, GraphQL, webhooks, authentication flows

**Key components:** Authentication, endpoints, parameters, responses, errors, rate limits

### User Guides
**When to load reference:** [documentation-types-and-workflows.md](references/documentation-types-and-workflows.md) for feature docs, how-tos, troubleshooting

**Key components:** Overview, prerequisites, step-by-step instructions, configuration, troubleshooting

### Tutorials
**When to load reference:** [documentation-types-and-workflows.md](references/documentation-types-and-workflows.md) for learning-focused content

**Key components:** Learning objectives, time estimate, prerequisites, progressive steps, working examples, next steps

### README Files
**Key components:** Description, features, installation, quick start, configuration, license

**Template:**
```markdown
# Project Name
Brief description

## Features
- Feature 1
- Feature 2

## Installation
[commands]

## Quick Start
[working example]

## License
[license]
```

### Release Notes
**Key components:** Version/date, summary, new features, bug fixes, breaking changes, migration guide

**Template:**
```markdown
# vX.X.X - Date

## Summary
[overview]

## New Features
- Feature (#issue)

## Bug Fixes
- Fix (#issue)

## Breaking Changes
⚠️ **Change description**
Migration: [steps]
```

### Architecture Documents
**When to load reference:** [documentation-types-and-workflows.md](references/documentation-types-and-workflows.md) for system design, technical decisions

**Key components:** Executive summary, system context, diagrams, components, data flow, technology stack, design decisions

## Quality Checklist

Before publishing documentation:

- [ ] Accuracy: All technical details correct
- [ ] Completeness: All necessary topics covered
- [ ] Clarity: Target audience can understand
- [ ] Examples: Working code included and tested
- [ ] Structure: Logical organization with clear headings
- [ ] Consistency: Terminology and formatting consistent
- [ ] Links: All hyperlinks valid
- [ ] Grammar: No spelling/grammatical errors
- [ ] Current: Version numbers and dates up-to-date

## Common Pitfalls

1. Assuming knowledge - Define acronyms and technical terms
2. Vague instructions - Be specific with examples
3. Missing error scenarios - Document errors and solutions
4. Outdated code - Test and update examples regularly
5. Inconsistent terms - Use same terminology throughout
6. Missing prerequisites - List all requirements upfront
7. Poor formatting - Use headings, lists, code blocks properly
8. No examples - Always include working code samples
9. Wrong audience level - Match technical depth to readers
10. Dense text - Break into scannable sections with headings

## References

- **[documentation-types-and-workflows.md](references/documentation-types-and-workflows.md)** - Comprehensive patterns and templates for API docs, user guides, tutorials, architecture docs, and technical specifications
- **[writing-guidelines.md](references/writing-guidelines.md)** - Detailed style rules for clarity, active voice, specificity, consistency, heading hierarchy, code formatting, and lists

