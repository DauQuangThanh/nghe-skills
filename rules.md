# Agent Skills: Guidelines and Rules for Creation

This document provides comprehensive guidelines and rules for creating high-quality agent skills based on the [Agent Skills specification](https://agentskills.io/specification) and best practices from [Anthropic's skills repository](https://github.com/anthropics/skills).

## Table of Contents

- [Core Principles](#core-principles)
- [Directory Structure](#directory-structure)
- [SKILL.md Format](#skillmd-format)
- [Frontmatter Specifications](#frontmatter-specifications)
- [Body Content Guidelines](#body-content-guidelines)
- [Progressive Disclosure](#progressive-disclosure)
- [Bundled Resources](#bundled-resources)
- [Writing Style Guidelines](#writing-style-guidelines)
- [Quality Standards](#quality-standards)
- [Common Pitfalls to Avoid](#common-pitfalls-to-avoid)
- [Validation and Testing](#validation-and-testing)

---

## Core Principles

### 1. Concise is Key

**Context window is a shared resource.** Every token in a skill competes for space with the agent's working context.

- **Keep SKILL.md under 500 lines** as a hard target
- **Frontmatter (name + description) should be ~100 tokens** - always loaded for all skills
- **SKILL.md body should be < 5000 tokens** - loaded when skill triggers
- **Move detailed content to reference files** - loaded only when needed

**Why this matters:**
- Skills are loaded alongside user requests, code, and other context
- Bloated skills reduce available space for the actual work
- Agents can load reference files on demand, but can't unload the main SKILL.md

### 2. Set Appropriate Degrees of Freedom

Match the level of specificity to the task's fragility and variability:

**High Freedom (Text Instructions)**
- Use when: Multiple approaches are valid, decisions depend on context
- Example: "Analyze the data and identify trends"
- Best for: Creative tasks, context-dependent decisions

**Medium Freedom (Pseudocode/Scripts with Parameters)**
- Use when: Preferred pattern exists but some variation acceptable
- Example: Workflow with configurable steps
- Best for: Standard processes with variation points

**Low Freedom (Specific Scripts, Few Parameters)**
- Use when: Operations are fragile and error-prone, consistency critical
- Example: OOXML manipulation, complex file format handling
- Best for: Technical operations where errors corrupt output

**Think of it as:** A narrow bridge with cliffs needs specific guardrails (low freedom), while an open field allows many routes (high freedom).

### 3. Progressive Disclosure Design

Skills use a three-level loading system:

1. **Metadata (name + description)** - Always in context (~100 words)
2. **SKILL.md body** - When skill triggers (<5k words recommended)
3. **Bundled resources** - As needed by agent (scripts can be executed without reading into context)

**Key principle:** Information should live in either SKILL.md or reference files, not both. Prefer reference files for detailed information unless it's truly core to the skill.

---

## Directory Structure

### Minimum Required Structure

```
skill-name/
└── SKILL.md          # Required
```

### Full Structure with Optional Resources

```
skill-name/
├── SKILL.md                    # Required: Main skill file
├── LICENSE.txt                 # Optional: License terms
├── scripts/                    # Optional: Executable code
│   ├── example.py
│   └── helper.sh
├── references/                 # Optional: Documentation loaded on demand
│   ├── api_reference.md
│   ├── workflows.md
│   └── examples.md
└── assets/                     # Optional: Files used in output
    ├── template.docx
    ├── logo.png
    └── config_template.json
```

### Rules

- **Name must match directory name**: If directory is `pdf-processing/`, frontmatter name must be `pdf-processing`
- **SKILL.md is required**: This is the only mandatory file
- **Optional directories are lowercase**: `scripts/`, `references/`, `assets/`
- **No auxiliary documentation**: NO README.md, INSTALLATION_GUIDE.md, CHANGELOG.md, etc.

**Rationale:** Skills are for agents, not humans. Documentation files add clutter without helping the agent perform tasks.

---

## SKILL.md Format

Every SKILL.md consists of:

1. **YAML Frontmatter** (required) - Enclosed in `---` markers
2. **Markdown Body** (required) - Instructions and guidelines

### Basic Template

```markdown
---
name: skill-name
description: A complete description of what this skill does and when to use it. Include specific triggers and scenarios.
---

# Skill Name

[Main instructions and workflow]

## Section 1

[Content]

## Section 2

[Content]
```

---

## Frontmatter Specifications

### Required Fields

#### `name`

**Rules:**
- **Required**: Yes
- **Length**: 1-64 characters
- **Format**: Lowercase alphanumeric and hyphens only (`a-z`, `0-9`, `-`)
- **Constraints**:
  - Must NOT start or end with hyphen
  - Must NOT contain consecutive hyphens (`--`)
  - Must match parent directory name

**Valid Examples:**
```yaml
name: pdf-processing
name: data-analysis
name: code-review
name: requirements-gathering
```

**Invalid Examples:**
```yaml
name: PDF-Processing        # uppercase not allowed
name: -pdf                  # cannot start with hyphen
name: pdf-                  # cannot end with hyphen
name: pdf--processing       # consecutive hyphens not allowed
name: pdf_processing        # underscores not allowed
```

#### `description`

**Rules:**
- **Required**: Yes
- **Length**: 1-1024 characters
- **Purpose**: Primary triggering mechanism for the skill

**What to Include:**
1. **What the skill does** - Core capabilities and functionality
2. **When to use it** - Specific scenarios, file types, tasks, or keywords
3. **Trigger phrases** - Words/phrases users might mention

**Cannot contain:** Angle brackets (`<` or `>`)

**Good Example:**
```yaml
description: Conducts comprehensive requirements review including completeness validation, clarity assessment, consistency checking, testability evaluation, and standards compliance. Produces detailed review reports with findings, gaps, conflicts, and improvement recommendations. Use when reviewing requirements documents (BRD, SRS, user stories), validating acceptance criteria, assessing requirements quality, identifying gaps and conflicts, or ensuring standards compliance (IEEE 830, INVEST criteria). Trigger when users mention "review requirements", "validate requirements", "check requirements quality", "find requirement issues", or "assess BRD/SRS quality".
```

**Poor Example:**
```yaml
description: Helps with requirements.  # Too vague, no triggers, no context
```

**Critical Rule:** Include ALL "when to use" information in the description, NOT in the body. The body is only loaded after triggering, so "When to Use This Skill" sections in the body are useless for triggering.

### Optional Fields

#### `license`

**Rules:**
- **Optional**: Yes
- **Format**: License name or reference to bundled file
- **Keep it short**: Either SPDX identifier or reference to LICENSE.txt

**Examples:**
```yaml
license: MIT
license: Apache-2.0
license: Proprietary. LICENSE.txt has complete terms
license: Complete terms in LICENSE.txt
```

#### `compatibility`

**Rules:**
- **Optional**: Yes (only include if specific requirements exist)
- **Length**: 1-500 characters if provided
- **Purpose**: Indicate environment requirements

**Examples:**
```yaml
compatibility: Designed for Claude Code (or similar products)
compatibility: Requires git, docker, jq, and access to the internet
compatibility: Requires Python 3.9+ with pandas and numpy
```

**Note:** Most skills do NOT need this field.

#### `metadata`

**Rules:**
- **Optional**: Yes
- **Format**: Key-value map (strings only)
- **Purpose**: Client-specific additional properties

**Example:**
```yaml
metadata:
  author: example-org
  version: "1.0"
  category: development
```

**Warning:** Some implementations may not support metadata beyond name/description. Use sparingly.

#### `allowed-tools`

**Rules:**
- **Optional**: Yes
- **Format**: Space-delimited list of pre-approved tools
- **Status**: Experimental - support varies

**Example:**
```yaml
allowed-tools: Bash(git:*) Bash(jq:*) Read
```

### Frontmatter Examples

**Minimal (Required Only):**
```yaml
---
name: pdf-processing
description: Extracts text and tables from PDF files, fills PDF forms, and merges multiple PDFs. Use when working with PDF documents or when the user mentions PDFs, forms, or document extraction.
---
```

**With Optional Fields:**
```yaml
---
name: pdf-processing
description: Extracts text and tables from PDF files, fills PDF forms, and merges multiple PDFs. Use when working with PDF documents or when the user mentions PDFs, forms, or document extraction.
license: Apache-2.0
metadata:
  author: example-org
  version: "1.0"
---
```

### Validation Rules Summary

| Field | Required | Max Length | Allowed Characters | Notes |
|-------|----------|------------|-------------------|-------|
| `name` | Yes | 64 | `a-z`, `0-9`, `-` | No consecutive `--`, no start/end with `-` |
| `description` | Yes | 1024 | Any except `<>` | Must include what and when |
| `license` | No | - | Any | Keep short |
| `compatibility` | No | 500 | Any | Only if needed |
| `metadata` | No | - | String keys/values | Use sparingly |
| `allowed-tools` | No | - | Space-delimited | Experimental |

---

## Body Content Guidelines

### Structure and Organization

#### Choose the Right Pattern

**1. Workflow-Based** (best for sequential processes)
```markdown
# Skill Name

## Overview
Brief introduction

## Workflow Decision Tree
Guide for choosing path

## Step 1: [Action]
Detailed instructions

## Step 2: [Action]
Detailed instructions
```
**Use when:** Clear step-by-step procedures exist
**Example:** DOCX skill with reading → creating → editing workflows

**2. Task-Based** (best for tool collections)
```markdown
# Skill Name

## Overview
Brief introduction

## Quick Start
Getting started guide

## Task Category 1
### Subtask A
### Subtask B

## Task Category 2
### Subtask C
```
**Use when:** Skill offers different operations/capabilities
**Example:** PDF skill with merge, split, extract operations

**3. Reference/Guidelines** (best for standards)
```markdown
# Skill Name

## Overview
Purpose and scope

## Guidelines
Core principles

## Specifications
Detailed specs

## Usage
Implementation guidance
```
**Use when:** Providing standards, specifications, or guidelines
**Example:** Brand styling, coding standards

**4. Capabilities-Based** (best for integrated systems)
```markdown
# Skill Name

## Overview
System description

## Core Capabilities

### 1. Capability Name
Description and usage

### 2. Capability Name
Description and usage
```
**Use when:** Multiple interrelated features
**Example:** Product management system

### Recommended Sections

While there are no format restrictions, effective skills typically include:

- **Overview/Purpose**: 1-2 sentences on what the skill enables
- **Workflow/Process**: Step-by-step guidance
- **Examples**: Input/output pairs showing expected results
- **Edge Cases**: Common problems and solutions
- **Resources**: References to bundled scripts/references

### Writing Guidelines

**Use Imperative/Infinitive Form:**
✅ "Create a new document"
✅ "Extract text from PDF"
✅ "Validate the schema"

❌ "You should create a document"
❌ "The agent needs to extract text"

**Be Specific:**
✅ "Sort by relevance (best match first)"
✅ "Response time < 2 seconds for 95% of requests"

❌ "Sort appropriately"
❌ "Fast performance"

**Provide Examples:**
```markdown
## Commit Message Format

Generate commit messages following these examples:

**Example 1:**
Input: Added user authentication with JWT tokens
Output:
```
feat(auth): implement JWT-based authentication

Add login endpoint and token validation middleware
```

Follow this style: type(scope): brief description, then detailed explanation.
```

---

## Progressive Disclosure

### The Three-Level System

**Level 1: Metadata (Always Loaded)**
- Name + description (~100 tokens)
- Loaded for ALL skills at agent startup
- Used for skill selection and triggering

**Level 2: SKILL.md Body (Loaded on Trigger)**
- Full instructions and workflow (<5000 tokens recommended)
- Loaded when agent decides to use the skill
- Should be complete but concise

**Level 3: Bundled Resources (Loaded As Needed)**
- Scripts, references, assets (unlimited size)
- Loaded only when agent determines they're needed
- Scripts can be executed without reading into context

### Progressive Disclosure Patterns

#### Pattern 1: High-Level Guide with References

```markdown
# PDF Processing

## Quick Start
Extract text with pdfplumber:
[code example]

## Advanced Features
- **Form filling**: See [FORMS.md](references/FORMS.md) for complete guide
- **API reference**: See [REFERENCE.md](references/REFERENCE.md) for all methods
- **Examples**: See [EXAMPLES.md](references/EXAMPLES.md) for common patterns
```

Agent loads FORMS.md, REFERENCE.md, or EXAMPLES.md only when needed.

#### Pattern 2: Domain-Specific Organization

For skills with multiple domains:

```
bigquery-skill/
├── SKILL.md (overview and navigation)
└── references/
    ├── finance.md (revenue, billing metrics)
    ├── sales.md (opportunities, pipeline)
    ├── product.md (API usage, features)
    └── marketing.md (campaigns, attribution)
```

When user asks about sales metrics, agent only reads sales.md.

#### Pattern 3: Framework/Variant Organization

For skills supporting multiple frameworks:

```
cloud-deploy/
├── SKILL.md (workflow + provider selection)
└── references/
    ├── aws.md (AWS deployment patterns)
    ├── gcp.md (GCP deployment patterns)
    └── azure.md (Azure deployment patterns)
```

When user chooses AWS, agent only reads aws.md.

#### Pattern 4: Conditional Details

```markdown
# DOCX Processing

## Creating Documents
Use docx-js for new documents. See [DOCX-JS.md](DOCX-JS.md).

## Editing Documents
For simple edits, modify the XML directly.

**For tracked changes**: See [REDLINING.md](REDLINING.md)
**For OOXML details**: See [OOXML.md](OOXML.md)
```

Agent reads REDLINING.md or OOXML.md only when user needs those features.

### Progressive Disclosure Best Practices

1. **Keep SKILL.md under 500 lines** - Split content when approaching this limit
2. **Reference files from SKILL.md** - Make them discoverable
3. **Describe when to load each reference** - Guide the agent's decisions
4. **Avoid duplication** - Information lives in ONE place
5. **Keep references one level deep** - Don't chain references (references/file1.md → references/file2.md)
6. **Structure long references** - Include table of contents for files >100 lines
7. **No deeply nested chains** - All reference files link directly from SKILL.md

---

## Bundled Resources

### scripts/

**Purpose:** Executable code agents can run directly

**Characteristics:**
- Code that performs specific operations (Python, Bash, JavaScript, etc.)
- Can be executed WITHOUT loading into context window
- Should be self-contained or clearly document dependencies

**When to Use:**
- Complex operations that would be error-prone if written by agent
- Operations requiring specific libraries or tools
- Repeatable automation tasks

**Examples:**
```
scripts/
├── extract_form_fields.py    # Extract fields from PDF form
├── merge_pdfs.py              # Merge multiple PDF files
└── validate_schema.sh         # Validate JSON schema
```

**Best Practices:**
- Be self-contained or document dependencies clearly
- Include helpful error messages
- Handle edge cases gracefully
- Make scripts executable (`chmod +x`)
- Add shebang line (`#!/usr/bin/env python3`)

**Script Guidelines:**
```python
#!/usr/bin/env python3
"""
Brief description of what this script does.

Usage:
    script_name.py <arg1> <arg2>

Example:
    script_name.py input.pdf output.txt
"""
import sys

def main():
    if len(sys.argv) < 3:
        print("Error: Missing required arguments", file=sys.stderr)
        print(__doc__)
        sys.exit(1)
    
    # Implementation
    pass

if __name__ == "__main__":
    main()
```

### references/

**Purpose:** Documentation intended to be loaded into context as needed

**Characteristics:**
- Markdown documentation
- Loaded on demand when agent determines it's needed
- Should be focused and specific

**When to Use:**
- API documentation
- Detailed workflow guides
- Domain knowledge
- Company policies
- Database schemas
- Technical specifications

**Examples:**
```
references/
├── api_reference.md         # Complete API documentation
├── workflows.md             # Detailed workflow patterns
├── examples.md              # Extensive example collection
├── finance.md               # Financial domain schemas
└── ooxml.md                 # Office Open XML technical reference
```

**Best Practices:**
- Keep files focused on specific topics
- Include table of contents for long files (>100 lines)
- Use clear, descriptive filenames
- Avoid duplicating content from SKILL.md
- Structure for easy reference (headings, examples, code blocks)

**Reference File Template:**
```markdown
# API Reference

## Table of Contents
- [Overview](#overview)
- [Authentication](#authentication)
- [Endpoints](#endpoints)
- [Error Codes](#error-codes)

## Overview
Brief introduction to the API

## Authentication
How to authenticate

## Endpoints

### GET /api/resource
Description and examples

### POST /api/resource
Description and examples

## Error Codes
Common error codes and meanings
```

### assets/

**Purpose:** Static resources used in output (NOT loaded into context)

**Characteristics:**
- Files that agents reference or include in their output
- Templates, images, data files
- NOT intended to be read into context

**When to Use:**
- Document templates (DOCX, PPTX, etc.)
- Image assets (logos, diagrams, icons)
- Configuration templates
- Data files (lookup tables, schemas)

**Examples:**
```
assets/
├── template.docx            # Document template
├── logo.png                 # Company logo
├── presentation.pptx        # Slide deck template
└── config_template.json     # Configuration template
```

**Best Practices:**
- Use clear, descriptive filenames
- Keep assets relevant to the skill
- Document how to use each asset in SKILL.md
- Prefer common formats

### What NOT to Include

**Do NOT create these files:**
- ❌ README.md - Skill is for agents, not humans
- ❌ INSTALLATION_GUIDE.md - Environment setup is external
- ❌ CHANGELOG.md - Version control handles history
- ❌ QUICK_REFERENCE.md - Belongs in SKILL.md or references/
- ❌ CONTRIBUTING.md - Not relevant for agent skills
- ❌ TESTING.md - Testing is external to skill

**Rationale:** Skills should contain ONLY information needed for an agent to do the job. Auxiliary documentation adds clutter without helping task completion.

---

## Writing Style Guidelines

### Tone and Voice

- **Imperative mood**: Direct commands ("Create", "Extract", "Validate")
- **Active voice**: "Run the script" not "The script should be run"
- **Present tense**: "The tool generates" not "The tool will generate"
- **Professional but clear**: Avoid jargon, define technical terms

### Formatting

**Use Markdown Effectively:**

```markdown
# Main Heading (H1) - Skill name or major sections

## Section Heading (H2) - Primary divisions

### Subsection (H3) - Detailed topics

**Bold** for emphasis and important terms
*Italic* for slight emphasis
`Code` for inline code, commands, filenames
```

**Code Blocks:**
```markdown
```python
# Always specify the language
def example():
    return "Clear, runnable code"
```
```

**Lists:**
```markdown
Unordered lists for non-sequential items:
- Item 1
- Item 2
- Item 3

Ordered lists for sequential steps:
1. First step
2. Second step
3. Third step
```

**Examples Pattern:**

Show input/output pairs for clarity:

```markdown
## Format Examples

**Example 1:**
Input: User registration data
Output:
```json
{
  "name": "John Doe",
  "email": "john@example.com"
}
```

**Example 2:**
Input: Error condition
Output: Clear error message with resolution steps
```

### Specificity Over Vagueness

**Be Specific:**
✅ "Response time < 2 seconds for 95th percentile"
✅ "Retry up to 3 times with exponential backoff"
✅ "Password must be 12+ characters with 1 uppercase, 1 lowercase, 1 number, 1 special"

**Avoid Vagueness:**
❌ "Fast performance"
❌ "Retry a few times"
❌ "Strong password"

### Providing Context

**Good Context:**
```markdown
## Authentication

Use OAuth 2.0 for API access. The token expires after 1 hour.

To authenticate:
1. Obtain client credentials from dashboard
2. Request token: POST /oauth/token
3. Include token in Authorization header: `Bearer <token>`

If token expires during operation, request a new token automatically.
```

**Poor Context:**
```markdown
## Authentication

Use OAuth. Get a token and use it.
```

---

## Quality Standards

### Completeness Checklist

For every skill, ensure:

- [ ] **Frontmatter valid**: name and description present and properly formatted
- [ ] **Description comprehensive**: Includes what, when, and trigger keywords
- [ ] **Instructions clear**: Step-by-step guidance provided
- [ ] **Examples included**: Input/output examples show expected behavior
- [ ] **Edge cases covered**: Common problems and solutions documented
- [ ] **Resources referenced**: Scripts/references mentioned in SKILL.md
- [ ] **File structure correct**: Directories named properly (`scripts/`, `references/`, `assets/`)
- [ ] **Under 500 lines**: Main SKILL.md is concise
- [ ] **No auxiliary files**: No README, CHANGELOG, etc.

### Testability

Skills should be testable through:

1. **Validation**: Run `skills-ref validate ./skill-name`
2. **Real usage**: Test with actual agent on representative tasks
3. **Edge cases**: Verify handling of error conditions
4. **Reference loading**: Confirm references load correctly when needed

### Maintainability

Good skills are:

- **Self-documenting**: Clear structure and naming
- **Modular**: Separate concerns into appropriate files
- **Versioned**: Use metadata for version tracking if needed
- **Consistent**: Follow established patterns from other skills

---

## Common Pitfalls to Avoid

### 1. Bloated SKILL.md

**Problem:** Including every detail in the main file
**Solution:** Use progressive disclosure - move details to reference files

**Bad:**
```markdown
# API Skill (2000+ lines)

## Complete API Documentation
[Pages and pages of endpoint documentation]

## Every Possible Example
[Hundreds of examples]

## All Error Codes
[Complete error code reference]
```

**Good:**
```markdown
# API Skill (200 lines)

## Quick Start
Basic usage example

## Making Requests
Core workflow

## Advanced Topics
- **Complete API Reference**: See [API.md](references/API.md)
- **Examples**: See [EXAMPLES.md](references/EXAMPLES.md)
- **Error Handling**: See [ERRORS.md](references/ERRORS.md)
```

### 2. Vague Descriptions

**Problem:** Description doesn't help with skill selection
**Solution:** Include specific scenarios and trigger keywords

**Bad:**
```yaml
description: Helps with documents.
```

**Good:**
```yaml
description: Creates, edits, and analyzes Word documents (.docx) with support for tracked changes, comments, formatting, tables, and images. Use when working with professional documents, creating reports, modifying existing .docx files, or when users mention Word documents, DOCX files, or document editing.
```

### 3. Solution-Focused Requirements

**Problem:** Specifying HOW instead of WHAT
**Impact:** Limits agent flexibility and creativity

**Bad:**
```markdown
System must use React framework with Redux for state management.
```

**Good:**
```markdown
System must provide responsive web interface with persistent client-side state.
```

### 4. Missing "When to Use" in Description

**Problem:** Putting triggers in body instead of description
**Impact:** Skill won't trigger because body isn't loaded until after trigger

**Bad:**
```yaml
description: Analyzes code quality and provides recommendations.
```

**Good:**
```yaml
description: Analyzes code quality, detects code smells, suggests refactoring, and enforces coding standards. Use when reviewing code, checking code quality, identifying technical debt, or when users mention code review, refactoring, static analysis, or quality checks.
```

### 5. Deeply Nested References

**Problem:** References that reference other references
**Impact:** Agent must load multiple files, wasting context

**Bad:**
```
SKILL.md references:
  → references/workflow.md references:
    → references/advanced.md references:
      → references/details.md
```

**Good:**
```
SKILL.md references:
  → references/basic_workflow.md
  → references/advanced_workflow.md
  → references/detailed_specs.md
```

### 6. Duplicate Content

**Problem:** Same information in both SKILL.md and reference files
**Impact:** Wastes context window space

**Solution:** Information lives in ONE place. If it's in a reference, don't repeat it in SKILL.md.

### 7. Auxiliary Documentation

**Problem:** Creating README.md, CONTRIBUTING.md, etc.
**Impact:** Clutters skill, confuses agent

**Solution:** Skills are for agents. No human-focused documentation needed.

### 8. Assuming Knowledge

**Problem:** Not documenting obvious things or using unexplained jargon
**Solution:** Document assumptions, define acronyms, provide context

**Bad:**
```markdown
Configure the RTM using the TPS report from the SME via the API.
```

**Good:**
```markdown
Configure the Requirements Traceability Matrix (RTM) using the Technical Process Specification (TPS) report from the Subject Matter Expert (SME) via the API endpoint: POST /api/rtm/configure
```

### 9. Ignoring File Naming Conventions

**Problem:** Using uppercase, underscores, or inconsistent naming
**Impact:** Validation fails, looks unprofessional

**Bad:**
```
PDF_Processing/
Skill.md
PDF-processing/
pdfProcessing/
```

**Good:**
```
pdf-processing/
SKILL.md (always uppercase)
```

### 10. Over-Specification

**Problem:** Providing too much freedom when precision is needed
**Impact:** Agent generates incorrect or invalid output

**Bad (for fragile XML operations):**
```markdown
Modify the XML as needed to add the content.
```

**Good:**
```markdown
Insert the content using this exact XML structure:
```xml
<w:p>
  <w:r><w:t>Your text here</w:t></w:r>
</w:p>
```
Preserve all existing attributes and maintain element ordering.
```

---

## Validation and Testing

### Automated Validation

Use the [skills-ref](https://github.com/agentskills/agentskills/tree/main/skills-ref) tool:

```bash
# Validate skill structure and frontmatter
skills-ref validate ./my-skill

# Validates:
# - YAML frontmatter format
# - Required fields present (name, description)
# - Name follows conventions (lowercase, hyphens, no consecutive --)
# - Name matches directory
# - Description length (1-1024 characters)
# - No angle brackets in description
```

### Manual Testing Checklist

- [ ] **Load test**: Can agent load and understand the skill?
- [ ] **Trigger test**: Does description trigger skill appropriately?
- [ ] **Workflow test**: Can agent follow instructions successfully?
- [ ] **Example test**: Do examples produce expected outputs?
- [ ] **Edge case test**: Are error conditions handled correctly?
- [ ] **Reference test**: Do reference files load when needed?
- [ ] **Script test**: Do scripts execute correctly with proper inputs?

### Validation Errors and Fixes

**Error:** "Name must be lowercase with hyphens only"
**Fix:** Change `My_Skill` or `MySkill` to `my-skill`

**Error:** "Name cannot start or end with hyphen"
**Fix:** Change `-my-skill` or `my-skill-` to `my-skill`

**Error:** "Description is too long (1500 characters)"
**Fix:** Condense description to essential information, remove redundancy

**Error:** "Description cannot contain angle brackets"
**Fix:** Remove `<` and `>` characters from description

**Error:** "Missing 'description' in frontmatter"
**Fix:** Add `description: [your description]` to YAML frontmatter

### Iteration Based on Real Usage

After initial creation:

1. **Deploy to test environment**: Let agent use skill on real tasks
2. **Monitor usage**: Which reference files are loaded? Are scripts working?
3. **Gather feedback**: What's confusing? What's missing?
4. **Refine**: Improve based on actual usage patterns
5. **Repeat**: Skills improve through iteration

---

## Quick Reference Card

### Frontmatter Template

```yaml
---
name: skill-name
description: [What it does]. Use when [scenarios, triggers]. Trigger when users mention [keywords].
license: MIT
---
```

### Structure Checklist

- [ ] Directory name matches frontmatter name
- [ ] SKILL.md exists with valid frontmatter
- [ ] Description includes what + when + triggers
- [ ] Body uses imperative form
- [ ] SKILL.md under 500 lines
- [ ] Detailed content in references/
- [ ] Scripts in scripts/ (if any)
- [ ] Assets in assets/ (if any)
- [ ] No README or auxiliary files

### File Organization

```
skill-name/
├── SKILL.md              # Core: <500 lines
├── scripts/              # Optional: Executable code
│   └── *.py, *.sh
├── references/           # Optional: Docs loaded on demand
│   └── *.md
└── assets/               # Optional: Templates, images
    └── *.*
```

### Progressive Disclosure Pattern

1. **SKILL.md**: Core workflow + references to detailed docs
2. **references/**: Detailed docs loaded when needed
3. **scripts/**: Executed without loading into context

### Common Patterns

**Workflow-Based:**
Overview → Decision Tree → Step 1 → Step 2 → Step N

**Task-Based:**
Overview → Quick Start → Task 1 → Task 2 → Task N

**Reference/Guidelines:**
Overview → Guidelines → Specifications → Usage

**Capabilities-Based:**
Overview → Capability 1 → Capability 2 → Capability N

### Writing Rules

- ✅ Use imperative form: "Create", "Extract", "Validate"
- ✅ Be specific: "< 2s for 95%" not "fast"
- ✅ Include examples: input/output pairs
- ✅ Cover edge cases: errors, boundaries
- ✅ Reference other files: "See [FILE.md](references/FILE.md)"
- ❌ Don't duplicate: info lives in one place
- ❌ Don't nest deeply: one level of references
- ❌ Don't create auxiliary docs: no README, CHANGELOG

---

## Examples from Anthropic's Repository

### Excellent Description Examples

**frontend-design:**
```yaml
description: Create distinctive, production-grade frontend interfaces with high design quality. Use this skill when the user asks to build web components, pages, artifacts, posters, or applications (examples include websites, landing pages, dashboards, React components, HTML/CSS layouts, or when styling/beautifying any web UI). Generates creative, polished code and UI design that avoids generic AI aesthetics.
```

**skill-creator:**
```yaml
description: Guide for creating effective skills. This skill should be used when users want to create a new skill (or update an existing skill) that extends Claude's capabilities with specialized knowledge, workflows, or tool integrations.
```

**docx:**
```yaml
description: Comprehensive document creation, editing, and analysis with support for tracked changes, comments, formatting preservation, and text extraction. Use when Claude needs to work with professional documents (.docx files) for: (1) Creating new documents, (2) Modifying or editing content, (3) Working with tracked changes, (4) Adding comments, or any other document tasks.
```

### Progressive Disclosure in Practice

**From MCP Builder skill:**
```markdown
# Reference Files

## 📚 Documentation Library

### Core MCP Documentation (Load First)
- **MCP Protocol**: Start with sitemap at `https://modelcontextprotocol.io/sitemap.xml`
- [📋 MCP Best Practices](./reference/mcp_best_practices.md)

### SDK Documentation (Load During Phase 1/2)
- **Python SDK**: Fetch from `https://raw.githubusercontent.com/modelcontextprotocol/python-sdk/main/README.md`
- **TypeScript SDK**: Fetch from `https://raw.githubusercontent.com/modelcontextprotocol/typescript-sdk/main/README.md`
```

**From Internal Comms skill:**
```markdown
## How to use this skill

1. **Identify the communication type** from the request
2. **Load the appropriate guideline file** from the `examples/` directory:
    - `examples/3p-updates.md` - For Progress/Plans/Problems team updates
    - `examples/company-newsletter.md` - For company-wide newsletters
    - `examples/faq-answers.md` - For answering frequently asked questions
3. **Follow the specific instructions** in that file
```

---

## Conclusion

Creating effective agent skills requires balancing completeness with conciseness. Follow these core principles:

1. **Concise is key** - Keep SKILL.md under 500 lines
2. **Progressive disclosure** - Load details only when needed
3. **Clear descriptions** - Include what, when, and triggers
4. **Appropriate freedom** - Match specificity to task fragility
5. **No auxiliary docs** - Skills are for agents, not humans

The best skills are discovered through iteration. Start with the essentials, deploy to real usage, gather feedback, and refine based on how agents actually use them.

For more information:
- **Specification**: https://agentskills.io/specification
- **Examples**: https://github.com/anthropics/skills
- **Validator**: https://github.com/agentskills/agentskills/tree/main/skills-ref

---

**Document Version:** 1.0  
**Last Updated:** January 14, 2026  
**Based On:** Agent Skills Specification + Anthropic's Skills Repository Best Practices
