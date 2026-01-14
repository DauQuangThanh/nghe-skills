---
name: technical-writing
description: Creates high-quality technical documentation including API documentation, user guides, tutorials, architecture documents, README files, release notes, and technical specifications. Produces clear, structured, and comprehensive documentation following industry best practices. Use when writing technical documentation, creating API docs, developing user guides, documenting architecture, writing tutorials, preparing release notes, or when users mention "technical writing", "documentation", "API docs", "user guide", "tutorial", "README", "technical specification", "architecture document", or "developer documentation".
---

# Technical Writing

## Overview

This skill guides the creation of professional technical documentation that is clear, comprehensive, and user-focused. It covers various documentation types from API references to user guides, following industry standards and best practices.

## Core Principles

**1. Know Your Audience**
- Identify reader's technical level (beginner, intermediate, expert)
- Adjust terminology, depth, and examples accordingly
- Consider reader's goals and use cases

**2. Clarity Over Cleverness**
- Use simple, direct language
- Avoid jargon unless necessary (define when used)
- One idea per sentence, one topic per paragraph
- Active voice preferred over passive voice

**3. Structure and Hierarchy**
- Logical organization with clear headings
- Progressive disclosure (simple → complex)
- Consistent formatting and terminology
- Scannable content with visual hierarchy

**4. Show, Don't Just Tell**
- Include concrete examples
- Provide code samples with explanations
- Use diagrams for complex concepts
- Real-world use cases and scenarios

## Documentation Types and Workflows

### API Documentation

**Purpose:** Enable developers to integrate and use your API effectively

**Structure:**
```markdown
# API Name

## Overview
Brief description, key capabilities, authentication method

## Quick Start
Minimal working example in 5 minutes or less

## Authentication
How to obtain and use credentials

## Endpoints

### GET /api/resource
- Description: What this endpoint does
- Parameters:
  - `param1` (string, required): Description
  - `param2` (integer, optional): Description
- Request Example:
  ```http
  GET /api/resource?param1=value HTTP/1.1
  Authorization: Bearer <token>
  ```
- Response Example:
  ```json
  {
    "status": "success",
    "data": {...}
  }
  ```
- Error Responses:
  - 400: Bad Request - Invalid parameters
  - 401: Unauthorized - Missing or invalid token
  - 404: Not Found - Resource does not exist

## Rate Limits
Requests per minute/hour, how to handle rate limiting

## Error Handling
Common errors, error codes, troubleshooting

## Pagination
How to paginate large result sets

## Changelog
Version history and breaking changes
```

**Best Practices:**
- List all required and optional parameters
- Show complete request/response examples
- Document all possible error codes
- Include rate limits and quotas
- Provide SDKs or client library examples
- Keep examples up-to-date with API changes

### User Guides

**Purpose:** Help users accomplish specific tasks with your product

**Structure:**
```markdown
# Feature/Product User Guide

## Introduction
What this feature does, who it's for, when to use it

## Prerequisites
- Required access, tools, or setup
- Assumed knowledge

## Step-by-Step Instructions

### Task 1: [Action-Oriented Title]
1. Navigate to [location]
2. Click [button/link]
3. Enter [information]:
   - Field 1: Description and example
   - Field 2: Description and example
4. Click "Save"

**Expected Result:** What you should see after completing this step

**Troubleshooting:**
- **Problem:** Common issue
  **Solution:** How to fix it

### Task 2: [Action-Oriented Title]
[Instructions]

## Tips and Best Practices
- Tip 1: Optimize performance by...
- Tip 2: Avoid common mistake...

## FAQ
**Q: Common question?**
A: Clear answer with examples

## Related Resources
- Link to related guide
- Link to API documentation
```

**Best Practices:**
- Use task-oriented headings ("How to..." or "Create a...")
- Number sequential steps
- Include screenshots for UI-heavy tasks
- Show expected results after each major step
- Provide troubleshooting for common issues
- Use consistent terminology throughout

### Tutorials

**Purpose:** Teach concepts while building something real

**Structure:**
```markdown
# Tutorial: Building [Project Name]

## What You'll Build
Brief description and screenshot/demo of final result

## What You'll Learn
- Concept 1
- Concept 2
- Concept 3

## Prerequisites
- Required knowledge (link to resources if needed)
- Required tools and versions
- Estimated time: X minutes

## Step 1: Setup
```bash
# Commands to set up environment
npm install package-name
```

**What we're doing:** Explain the purpose of this step

## Step 2: [Milestone Title]
```javascript
// Code with inline comments
function example() {
  // Explain complex logic
  return result;
}
```

**Explanation:**
- Why we structured it this way
- Key concepts at work
- Alternative approaches

## Step 3: [Next Milestone]
[Continue building incrementally]

## Testing Your Work
How to verify everything works correctly

## Next Steps
- Extend functionality with...
- Optimize by...
- Deploy to production

## Complete Code
Link to GitHub repo or provide full code listing

## Troubleshooting
Common errors and solutions
```

**Best Practices:**
- Build incrementally (working code at each step)
- Explain WHY, not just WHAT
- Include complete, runnable code
- Test tutorial with fresh users
- Keep scope focused and achievable
- Celebrate milestones along the way

### Architecture Documentation

**Purpose:** Communicate system design decisions and structure

**Structure:**
```markdown
# System Architecture

## Executive Summary
High-level overview in 2-3 paragraphs

## System Context
- Business objectives
- Key stakeholders
- Constraints and assumptions

## Architecture Overview
[System diagram showing major components]

Brief description of how components interact

## Component Details

### Component 1: [Name]
- **Purpose:** What it does
- **Technology:** Stack used
- **Responsibilities:**
  - Responsibility 1
  - Responsibility 2
- **Interfaces:** How other components interact with it
- **Data:** Key data structures
- **Scaling:** How it scales

### Component 2: [Name]
[Same structure]

## Data Flow
[Sequence diagrams or flow charts]

1. User action triggers...
2. Component A processes...
3. Component B receives...

## Key Design Decisions

### Decision 1: [Topic]
- **Context:** What problem we faced
- **Options Considered:**
  - Option A: Pros and cons
  - Option B: Pros and cons
- **Decision:** What we chose and why
- **Consequences:** Trade-offs accepted

### Decision 2: [Topic]
[Same structure]

## Security Architecture
- Authentication and authorization
- Data encryption
- Network security
- Compliance requirements

## Deployment Architecture
- Infrastructure components
- Environments (dev, staging, prod)
- CI/CD pipeline
- Monitoring and logging

## Performance Considerations
- Expected load and scaling strategy
- Caching strategy
- Database optimization
- Bottlenecks and mitigations

## Disaster Recovery
- Backup strategy
- Recovery procedures
- RTO and RPO targets

## Future Considerations
- Known limitations
- Planned improvements
- Technical debt
```

**Best Practices:**
- Use diagrams extensively (C4 model, UML, etc.)
- Document the "why" behind decisions
- Keep diagrams up-to-date with code
- Include both logical and physical architecture
- Address non-functional requirements
- Update as system evolves

### README Files

**Purpose:** Provide essential information for getting started with a project

**Structure:**
```markdown
# Project Name

Brief description (1-2 sentences)

![Project Screenshot or Logo](image.png)

## Features
- Feature 1
- Feature 2
- Feature 3

## Quick Start

### Installation
```bash
npm install project-name
# or
pip install project-name
```

### Basic Usage
```javascript
// Minimal working example
const project = require('project-name');
const result = project.doSomething();
```

## Documentation
- [User Guide](docs/user-guide.md)
- [API Reference](docs/api-reference.md)
- [Examples](examples/)

## Requirements
- Node.js >= 16.0.0
- PostgreSQL >= 13

## Configuration
```yaml
# config.yml example
setting1: value1
setting2: value2
```

## Development

### Setup
```bash
git clone https://github.com/user/repo.git
cd repo
npm install
npm run dev
```

### Running Tests
```bash
npm test
```

### Contributing
See [CONTRIBUTING.md](CONTRIBUTING.md)

## License
[License Type](LICENSE)

## Support
- Documentation: https://docs.example.com
- Issues: https://github.com/user/repo/issues
- Discord: https://discord.gg/example
```

**Best Practices:**
- Put most important information first
- Include working code examples
- Keep it concise (link to detailed docs)
- Add badges for build status, coverage, etc.
- Include screenshots or demos
- Specify version requirements clearly

### Release Notes

**Purpose:** Communicate changes in new releases to users

**Structure:**
```markdown
# Release Notes

## Version 2.1.0 (2026-01-14)

### 🎉 New Features
- **Feature Name:** Description of what's new and why users should care
  ```javascript
  // Example usage
  newFeature.doSomething();
  ```

### ⚡ Improvements
- **Performance:** API response times improved by 40%
- **UX:** Simplified authentication flow

### 🐛 Bug Fixes
- Fixed issue where X would fail when Y ([#123](link))
- Resolved memory leak in Z component ([#456](link))

### ⚠️ Breaking Changes
- **Removed deprecated `oldMethod()`**
  - Migration: Use `newMethod()` instead
  ```javascript
  // Before
  oldMethod(param);
  
  // After
  newMethod(param);
  ```

### 📚 Documentation
- Added tutorial for advanced use case
- Updated API reference for new endpoints

### 🔒 Security
- Patched vulnerability CVE-2026-XXXXX
- Updated dependencies to fix security issues

### ⬆️ Upgrade Instructions
1. Backup your data
2. Run migration script: `npm run migrate`
3. Update configuration: Add `new_setting` to config file
4. Restart application

### 📦 Dependencies
- Updated `package-name` from 1.0.0 to 2.0.0
- Added `new-dependency` 1.5.0

## Version 2.0.0 (2025-12-01)
[Previous release notes]
```

**Best Practices:**
- Group changes by type (features, fixes, breaking changes)
- Use emoji for visual scanning (optional)
- Explain impact on users, not just technical changes
- Provide migration guides for breaking changes
- Link to relevant issues/PRs
- Include upgrade instructions
- Publish release notes with every release

### Technical Specifications

**Purpose:** Define detailed technical requirements and behaviors

**Structure:**
```markdown
# Technical Specification: [Feature Name]

## Metadata
- **Author:** Name
- **Created:** 2026-01-14
- **Status:** Draft | Review | Approved | Implemented
- **Reviewers:** Names

## Executive Summary
2-3 paragraph overview of what's being specified and why

## Background

### Problem Statement
Clear description of the problem being solved

### Goals
- Goal 1: Specific, measurable outcome
- Goal 2: Specific, measurable outcome

### Non-Goals
- What this spec explicitly does NOT cover

## Proposal

### Overview
High-level description of the solution

### Detailed Design

#### Component Architecture
[Diagrams and descriptions]

#### Data Models
```typescript
interface DataModel {
  field1: string;  // Description
  field2: number;  // Description
}
```

#### API Contract
```http
POST /api/endpoint
Content-Type: application/json

{
  "field": "value"
}

Response: 200 OK
{
  "result": "success"
}
```

#### Algorithms
Pseudocode or detailed description of key algorithms

#### Error Handling
How errors are detected, reported, and recovered

### User Experience
How users interact with this feature

### Performance Requirements
- Latency: < 100ms for 95th percentile
- Throughput: 1000 requests/second
- Storage: < 1GB per 10,000 users

### Security Considerations
- Authentication requirements
- Authorization model
- Data encryption
- Input validation

### Testing Strategy
- Unit tests: Coverage of...
- Integration tests: Scenarios...
- Load tests: Targets...

## Alternatives Considered

### Alternative 1: [Approach]
- **Pros:** Benefits
- **Cons:** Drawbacks
- **Why not chosen:** Reasoning

### Alternative 2: [Approach]
[Same structure]

## Implementation Plan

### Phase 1: Foundation (Week 1-2)
- Task 1
- Task 2

### Phase 2: Core Features (Week 3-4)
- Task 3
- Task 4

### Phase 3: Polish (Week 5)
- Task 5
- Task 6

## Risks and Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Technical risk | Medium | High | Strategy |
| Resource risk | Low | Medium | Strategy |

## Open Questions
- Question 1: What needs to be decided
- Question 2: Pending information

## References
- Link to related docs
- Research papers
- Prior art
```

**Best Practices:**
- Be specific and unambiguous
- Include diagrams for complex flows
- Define acceptance criteria clearly
- Consider edge cases and error scenarios
- Review with stakeholders before implementation
- Update spec as design evolves

## Writing Guidelines

### Clarity and Conciseness

**Use Active Voice:**
✅ "The system processes the request"
❌ "The request is processed by the system"

**Be Direct:**
✅ "Click Save to store your changes"
❌ "Your changes can be stored by clicking on the Save button"

**Avoid Redundancy:**
✅ "Configure the settings"
❌ "Configure the settings and parameters"

**Define Acronyms:**
✅ "API (Application Programming Interface)"
❌ Assume everyone knows "API"

### Consistent Terminology

**Create a glossary** for your project and use terms consistently:
- Don't alternate between "user", "customer", "client" for the same concept
- Choose one term and stick with it
- Document your glossary in a reference file

**Example:**
```markdown
## Glossary
- **Workspace:** A container for projects and team members
- **Project:** A collection of tasks and resources
- **Task:** A unit of work with assignees and due dates
```

### Code Examples

**Complete and Runnable:**
```javascript
// ✅ Good: Complete, runnable example
const express = require('express');
const app = express();

app.get('/api/users', (req, res) => {
  res.json({ users: [] });
});

app.listen(3000, () => {
  console.log('Server running on port 3000');
});
```

```javascript
// ❌ Bad: Incomplete, won't run
app.get('/api/users', (req, res) => {
  // ... do something
});
```

**Include Context:**
```python
# ✅ Good: Shows file structure and imports
# file: src/models/user.py
from dataclasses import dataclass
from datetime import datetime

@dataclass
class User:
    id: int
    email: str
    created_at: datetime
```

```python
# ❌ Bad: No context about where this goes
@dataclass
class User:
    # ...
```

**Explain Non-Obvious Code:**
```javascript
// ✅ Good: Explains the why
// Use exponential backoff to avoid overwhelming the server
// during transient failures
const delay = Math.min(1000 * Math.pow(2, attempt), 30000);
await sleep(delay);
```

```javascript
// ❌ Bad: No explanation of complex logic
const delay = Math.min(1000 * Math.pow(2, attempt), 30000);
await sleep(delay);
```

### Formatting Standards

**Headings:**
- H1 (#): Document title only
- H2 (##): Major sections
- H3 (###): Subsections
- H4 (####): Sub-subsections (use sparingly)

**Lists:**
- Use bullets for unordered items
- Use numbers for sequential steps
- Keep list items parallel in structure

**Code Blocks:**
- Always specify language: ```javascript, ```python, ```bash
- Include file paths when relevant: `// file: src/app.js`
- Keep examples under 50 lines (split into multiple if longer)

**Tables:**
- Use for structured data comparison
- Keep columns to 5 or fewer
- Use markdown tables for simple data
- Consider alternative formats for complex data

**Callouts:**
```markdown
**Note:** For additional information that's helpful but not critical

**Warning:** For important cautions about potential issues

**Tip:** For best practices and optimization suggestions

**Important:** For critical information that must not be missed
```

### Accessibility

**Alt Text for Images:**
```markdown
![Dashboard showing user statistics with bar chart](dashboard.png)
```

**Descriptive Links:**
✅ "See the [API authentication guide](link) for details"
❌ "Click [here](link) for more information"

**Semantic HTML in Docs:**
Use proper heading hierarchy, lists, and structure

## Common Patterns

### Before/After Examples

When documenting changes or migrations:

```markdown
### Migration Example

**Before (v1.x):**
```javascript
const result = api.oldMethod(param1, param2);
```

**After (v2.x):**
```javascript
const result = api.newMethod({
  param1: value1,
  param2: value2
});
```

**Why this change:** Improved flexibility and clarity with named parameters
```

### Decision Trees

For helping users choose between options:

```markdown
## Choosing the Right Approach

**Do you need real-time updates?**
- **Yes** → Use WebSocket connection (see WebSocket Guide)
- **No** → Continue below

**Is your data larger than 100MB?**
- **Yes** → Use streaming API (see Streaming Guide)
- **No** → Use standard REST API (see REST Guide)
```

### Comparison Tables

For comparing features or options:

```markdown
| Feature | Option A | Option B | Option C |
|---------|----------|----------|----------|
| Speed | Fast | Medium | Slow |
| Cost | High | Medium | Low |
| Ease of Use | Medium | Easy | Easy |
| Best For | Large scale | General use | Small projects |
```

## Quality Checklist

Before publishing documentation:

- [ ] **Accuracy:** All information correct and up-to-date
- [ ] **Completeness:** Covers all necessary topics
- [ ] **Clarity:** Understandable by target audience
- [ ] **Examples:** Includes working code examples
- [ ] **Structure:** Logical organization with clear headings
- [ ] **Consistency:** Terminology and formatting consistent
- [ ] **Tested:** Code examples actually run
- [ ] **Reviewed:** Has been reviewed by someone else
- [ ] **Accessible:** Links, alt text, heading hierarchy proper
- [ ] **Updated:** Date and version information current

## Tools and Resources

### Documentation Generators
- **JSDoc/TSDoc:** JavaScript/TypeScript code comments → API docs
- **Sphinx:** Python documentation generator
- **Javadoc:** Java API documentation
- **Swagger/OpenAPI:** API specification and documentation
- **Docusaurus/VitePress:** Static site generators for docs

### Diagramming Tools
- **Mermaid:** Text-based diagrams in Markdown
- **PlantUML:** UML diagrams from text
- **Draw.io/Excalidraw:** Visual diagramming
- **Lucidchart:** Professional diagramming

### Style Guides
- **Google Developer Documentation Style Guide**
- **Microsoft Writing Style Guide**
- **Red Hat Technical Writing Style Guide**
- **Write the Docs community resources**

## Examples Repository

For comprehensive examples of each documentation type, see:
- [Example API Documentation](references/examples-api.md)
- [Example User Guide](references/examples-user-guide.md)
- [Example Tutorial](references/examples-tutorial.md)
- [Example Architecture Doc](references/examples-architecture.md)

## Maintenance

**Keep Documentation Fresh:**
- Update docs when code changes
- Review quarterly for accuracy
- Solicit user feedback regularly
- Track documentation coverage
- Deprecate outdated content

**Documentation Debt:**
Just like technical debt, documentation debt accumulates. Schedule regular maintenance sprints.

**Metrics to Track:**
- Documentation coverage (% of code/features documented)
- User feedback and satisfaction scores
- Support ticket reduction after doc updates
- Time to onboard new developers

---

**Remember:** Great documentation is iterative. Ship early, gather feedback, and continuously improve based on how users actually use your docs.
