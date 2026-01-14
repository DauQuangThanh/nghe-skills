# Task List: Update All Skills Based on rules.md

**Generated Date:** January 14, 2026  
**Purpose:** Systematically update all 30 agent skills to comply with Agent Skills specification and best practices from rules.md

---

## Overview

This document provides a comprehensive action plan to update all skills in `.claude/skills/` directory to ensure they comply with the Agent Skills specification and best practices outlined in `rules.md`.

**Total Skills:** 30  
**Skills with Issues:** Multiple (detailed analysis below)

---

## Phase 1: Analysis and Validation

### Task 1.1: Run Automated Validation
**Priority:** High  
**Estimated Time:** 30 minutes

#### Actions:
1. Install `skills-ref` validator tool (if not already installed):
   ```bash
   npm install -g @agentskills/skills-ref
   # or
   cargo install skills-ref
   ```

2. Validate each skill:
   ```bash
   for skill_dir in .claude/skills/*/; do
     echo "Validating: $skill_dir"
     skills-ref validate "$skill_dir"
   done
   ```

3. Create validation report:
   ```bash
   for skill_dir in .claude/skills/*/; do
     echo "=== $(basename $skill_dir) ===" >> validation-report.txt
     skills-ref validate "$skill_dir" 2>&1 >> validation-report.txt
     echo "" >> validation-report.txt
   done
   ```

**Checklist:**
- [ ] Install skills-ref validator
- [ ] Run validation on all 30 skills
- [ ] Generate validation-report.txt
- [ ] Review and categorize validation errors
- [ ] Create priority list based on validation results

---

### Task 1.2: Line Count Analysis
**Priority:** High  
**Estimated Time:** 15 minutes

Current line counts show several skills exceeding recommended limits:

| Skill | Lines | Status | Action Required |
|-------|-------|--------|-----------------|
| devops | 1441 | ❌ **CRITICAL** | Split into references |
| code-security-review | 1381 | ❌ **CRITICAL** | Split into references |
| frontend-coding | 1369 | ❌ **CRITICAL** | Split into references |
| backend-coding | 1317 | ❌ **CRITICAL** | Split into references |
| requirements-gathering | 1184 | ❌ **CRITICAL** | Split into references |
| backend-design | 1017 | ⚠️ **WARNING** | Consider splitting |
| architecture-design-review | 958 | ⚠️ **WARNING** | Consider splitting |
| application-migration | 856 | ✅ OK | Monitor |
| cobol-migration-analyzer | 126 | ✅ EXCELLENT | Use as model |
| jcl-migration-analyzer | 184 | ✅ EXCELLENT | Use as model |
| pli-migration-analyzer | 122 | ✅ EXCELLENT | Use as model |
| rpg-migration-analyzer | 124 | ✅ EXCELLENT | Use as model |

**Target:** Keep SKILL.md under 500 lines (hard limit), ideally under 300 lines

#### Actions:
1. Identify skills exceeding 500 lines (7 skills marked CRITICAL)
2. For each oversized skill:
   - Analyze content structure
   - Identify sections that can be moved to references/
   - Plan reference file structure
   - Determine conditional loading criteria

**Checklist:**
- [ ] Analyze content of 7 critical skills
- [ ] Create content migration plan for each
- [ ] Design reference/ structure for each skill
- [ ] Document conditional loading rules

---

### Task 1.3: Description Quality Audit
**Priority:** High  
**Estimated Time:** 1 hour

Audit all skill descriptions to ensure they include:
- **What:** Core capabilities and functionality
- **When:** Specific scenarios and use cases  
- **Triggers:** Keywords and phrases users might mention

#### Actions:
1. Create description audit spreadsheet
2. Review each description against criteria:
   - Does it clearly state WHAT the skill does?
   - Does it specify WHEN to use it?
   - Does it include trigger keywords/phrases?
   - Is it under 1024 characters?
   - Does it avoid angle brackets?

3. Rewrite inadequate descriptions

**Template for Good Descriptions:**
```
[Core capabilities]. [Output/deliverables]. Use when [scenarios], [file types], [tasks]. 
Trigger when users mention "[keyword1]", "[keyword2]", "[keyword3]", or "[keyword4]".
```

**Checklist:**
- [ ] Audit all 30 skill descriptions
- [ ] Identify descriptions lacking triggers
- [ ] Identify descriptions lacking scenarios
- [ ] Rewrite inadequate descriptions
- [ ] Verify no description exceeds 1024 characters

---

## Phase 2: SKILL.md Optimization

### Task 2.1: Split Oversized SKILL.md Files
**Priority:** Critical  
**Estimated Time:** 3-4 hours

Focus on 7 critical skills that exceed 500 lines.

#### For Each Oversized Skill:

**1. backend-coding (1317 lines)**

**Current Structure Analysis:**
- Contains extensive framework-specific code examples
- Multiple language implementations (Node.js, Python, Java, Go)
- Detailed patterns for each capability

**Proposed References:**
```
backend-coding/
├── SKILL.md (target: <300 lines)
└── references/
    ├── nodejs-patterns.md      # Express, NestJS, Fastify examples
    ├── python-patterns.md      # Django, Flask, FastAPI examples
    ├── java-patterns.md        # Spring Boot examples
    ├── go-patterns.md          # Go/Gin/Echo examples
    ├── api-design.md           # REST, GraphQL, gRPC details
    ├── auth-patterns.md        # JWT, OAuth, RBAC detailed patterns
    ├── database-patterns.md    # SQL, NoSQL, ORM patterns
    ├── caching-patterns.md     # Redis, cache strategies
    ├── messaging-patterns.md   # RabbitMQ, Kafka patterns
    └── testing-patterns.md     # Unit, integration, API testing
```

**SKILL.md Content (Keep):**
- Core capabilities overview
- Quick start guide
- Language selection guide
- References to detailed patterns
- Common patterns (brief examples)

**SKILL.md Structure:**
```markdown
# Backend Coding Skill

## Core Capabilities
[Brief list of 8 capabilities]

## Quick Start
[Simple API example]

## Framework Selection
Choose based on project requirements:
- **Node.js**: See [nodejs-patterns.md](references/nodejs-patterns.md)
- **Python**: See [python-patterns.md](references/python-patterns.md)
- **Java**: See [java-patterns.md](references/java-patterns.md)
- **Go**: See [go-patterns.md](references/go-patterns.md)

## Common Patterns
[3-4 essential patterns with brief examples]

## Advanced Topics
- **API Design**: See [api-design.md](references/api-design.md)
- **Authentication**: See [auth-patterns.md](references/auth-patterns.md)
- **Databases**: See [database-patterns.md](references/database-patterns.md)
- **Caching**: See [caching-patterns.md](references/caching-patterns.md)
- **Messaging**: See [messaging-patterns.md](references/messaging-patterns.md)
- **Testing**: See [testing-patterns.md](references/testing-patterns.md)
```

**Loading Conditions:**
- Load `nodejs-patterns.md` when user mentions Node.js, Express, NestJS, Fastify
- Load `python-patterns.md` when user mentions Python, Django, Flask, FastAPI
- Load `java-patterns.md` when user mentions Java, Spring Boot
- Load `go-patterns.md` when user mentions Go, Gin, Echo
- Load `api-design.md` when user needs REST, GraphQL, or gRPC guidance
- Load `auth-patterns.md` when user needs authentication/authorization
- Load `database-patterns.md` when user works with databases
- Load `caching-patterns.md` when user needs caching strategies
- Load `messaging-patterns.md` when user works with message queues
- Load `testing-patterns.md` when user needs testing guidance

---

**2. devops (1441 lines)**

**Proposed References:**
```
devops/
├── SKILL.md (target: <300 lines)
└── references/
    ├── cicd-pipelines.md       # GitHub Actions, GitLab CI, Jenkins
    ├── aws-patterns.md         # AWS-specific IaC and services
    ├── gcp-patterns.md         # GCP-specific IaC and services
    ├── azure-patterns.md       # Azure-specific IaC and services
    ├── kubernetes.md           # K8s deployment and management
    ├── docker-patterns.md      # Containerization best practices
    ├── terraform.md            # Terraform patterns and modules
    ├── monitoring.md           # Prometheus, Grafana, ELK
    ├── deployment-strategies.md # Blue-green, canary, rolling
    └── security-practices.md   # DevSecOps, secrets management
```

**Loading Conditions:**
- Load cloud-specific patterns based on mentioned provider
- Load `kubernetes.md` when user mentions K8s, pods, deployments
- Load `terraform.md` when user works with IaC
- Load `cicd-pipelines.md` when user sets up CI/CD
- Load `monitoring.md` when user needs observability

---

**3. frontend-coding (1369 lines)**

**Proposed References:**
```
frontend-coding/
├── SKILL.md (target: <300 lines)
└── references/
    ├── react-patterns.md       # React hooks, components, state
    ├── vue-patterns.md         # Vue composition API, components
    ├── angular-patterns.md     # Angular modules, services
    ├── typescript-patterns.md  # TypeScript best practices
    ├── state-management.md     # Redux, Zustand, Pinia, NgRx
    ├── styling-patterns.md     # CSS-in-JS, Tailwind, SCSS
    ├── performance.md          # Optimization, lazy loading
    ├── accessibility.md        # WCAG compliance, ARIA
    ├── testing-frontend.md     # Jest, Vitest, Testing Library
    └── build-tools.md          # Vite, Webpack, build optimization
```

**Loading Conditions:**
- Load framework-specific pattern based on user's choice
- Load `state-management.md` when user needs state management
- Load `performance.md` when user mentions optimization
- Load `accessibility.md` when user mentions a11y or WCAG

---

**4. code-security-review (1381 lines)**

**Proposed References:**
```
code-security-review/
├── SKILL.md (target: <300 lines)
└── references/
    ├── owasp-top10.md          # OWASP Top 10 vulnerabilities
    ├── cwe-catalog.md          # Common Weakness Enumeration
    ├── injection-attacks.md    # SQL, XSS, command injection
    ├── auth-vulnerabilities.md # Auth/authz security issues
    ├── crypto-issues.md        # Cryptography vulnerabilities
    ├── api-security.md         # API security testing
    ├── dependency-security.md  # Vulnerable dependencies
    ├── compliance-rules.md     # PCI-DSS, GDPR, HIPAA
    └── remediation-guide.md    # Fix patterns and secure coding
```

**Loading Conditions:**
- Load `owasp-top10.md` for general security review
- Load specific vulnerability types based on code language/framework
- Load `compliance-rules.md` when user mentions compliance standards

---

**5. requirements-gathering (1184 lines)**

**Proposed References:**
```
requirements-gathering/
├── SKILL.md (target: <300 lines)
└── references/
    ├── interview-techniques.md  # Stakeholder interview methods
    ├── user-story-templates.md  # User story formats and examples
    ├── use-case-templates.md    # Use case documentation
    ├── acceptance-criteria.md   # AC writing guidelines
    ├── prioritization.md        # MoSCoW, RICE, Kano methods
    ├── domain-analysis.md       # Domain modeling techniques
    ├── requirements-docs.md     # BRD, SRS templates
    └── traceability.md          # Requirements traceability matrix
```

**Loading Conditions:**
- Load `interview-techniques.md` when gathering requirements
- Load `user-story-templates.md` when writing user stories
- Load `acceptance-criteria.md` when defining AC
- Load `prioritization.md` when prioritizing features

---

**6. backend-design (1017 lines)**

**Proposed References:**
```
backend-design/
├── SKILL.md (target: <300 lines)
└── references/
    ├── api-design-patterns.md   # REST, GraphQL, gRPC design
    ├── database-architecture.md # Schema design, normalization
    ├── microservices-patterns.md # Service decomposition
    ├── auth-design.md           # Authentication/authorization
    ├── caching-architecture.md  # Cache strategies and patterns
    ├── scalability-patterns.md  # Horizontal/vertical scaling
    └── integration-patterns.md  # Service integration patterns
```

---

**7. architecture-design-review (958 lines)**

**Proposed References:**
```
architecture-design-review/
├── SKILL.md (target: <300 lines)
└── references/
    ├── review-checklist.md      # Comprehensive review criteria
    ├── quality-attributes.md    # Scalability, reliability, etc.
    ├── architecture-patterns.md # Pattern validation
    ├── technology-assessment.md # Tech stack evaluation
    └── risk-assessment.md       # Architecture risks
```

**Checklist for Each Skill:**
- [ ] Analyze current content
- [ ] Create references/ directory
- [ ] Split content into logical reference files
- [ ] Add table of contents to reference files >100 lines
- [ ] Update SKILL.md with references
- [ ] Document loading conditions
- [ ] Test reference loading
- [ ] Verify SKILL.md is under 500 lines

---

### Task 2.2: Enhance Descriptions with Triggers
**Priority:** High  
**Estimated Time:** 2 hours

Review and enhance all skill descriptions to include explicit trigger phrases.

#### Actions:

For each skill, ensure description follows this pattern:
```yaml
description: [Core capabilities]. [Deliverables]. Use when [scenarios]. 
Trigger when users mention "[keyword1]", "[keyword2]", "[keyword3]".
```

**Example Improvements:**

**Before (git-commit):**
```yaml
description: Generates well-structured git commit messages following conventional commit 
standards and best practices. Creates clear, descriptive commits with proper type prefixes.
```

**After (git-commit):**
```yaml
description: Generates well-structured git commit messages following conventional commit 
standards and best practices. Creates clear, descriptive commits with proper type prefixes 
(feat, fix, docs, refactor, etc.), concise subjects, and detailed bodies when needed. 
Use when committing code changes, creating git commits, writing commit messages, or when 
users mention "commit", "git commit", "commit message", "conventional commits", "changelog", 
or need help structuring version control messages.
```

**Checklist:**
- [ ] Review all 30 skill descriptions
- [ ] Add trigger keywords to descriptions lacking them
- [ ] Add specific scenarios to descriptions lacking them
- [ ] Ensure deliverables are clearly stated
- [ ] Verify descriptions are under 1024 characters

---

### Task 2.3: Standardize SKILL.md Structure
**Priority:** Medium  
**Estimated Time:** 2 hours

Ensure all SKILL.md files follow consistent structure patterns.

#### Choose Pattern Based on Skill Type:

**1. Workflow-Based Pattern** (for sequential processes):
```markdown
# Skill Name

## Overview
[Brief introduction - 1-2 sentences]

## Workflow Decision Tree
[Guide for choosing path]

## Step 1: [Action]
[Detailed instructions]

## Step 2: [Action]
[Detailed instructions]

## Critical Tips
[Important considerations]

## Output Structure
[Expected deliverables]
```

**Use for:** Migration analyzers, testing skills, migration skills

---

**2. Task-Based Pattern** (for tool collections):
```markdown
# Skill Name

## Overview
[Brief introduction]

## Quick Start
[Getting started guide]

## Task Category 1
### Subtask A
### Subtask B

## Task Category 2
### Subtask C

## Advanced Topics
[References to detailed documentation]
```

**Use for:** Coding skills, design skills, development skills

---

**3. Reference/Guidelines Pattern** (for standards):
```markdown
# Skill Name

## Overview
[Purpose and scope]

## Guidelines
[Core principles]

## Specifications
[Detailed specs]

## Usage
[Implementation guidance]

## Examples
[Input/output examples]
```

**Use for:** Code review skills, quality standards, guidelines

---

**4. Capabilities-Based Pattern** (for integrated systems):
```markdown
# Skill Name

## Overview
[System description]

## Core Capabilities

### 1. Capability Name
[Description and usage]

### 2. Capability Name
[Description and usage]

## Quick Usage Guide
[Common operations]

## Advanced Features
[References to detailed docs]
```

**Use for:** Platform skills, architecture skills

---

**Checklist:**
- [ ] Categorize each skill by pattern type
- [ ] Restructure skills not following patterns
- [ ] Ensure consistent heading hierarchy
- [ ] Add overview sections where missing
- [ ] Add examples where missing

---

## Phase 3: References and Scripts Optimization

### Task 3.1: Create Missing References Directories
**Priority:** High  
**Estimated Time:** 1 hour

Current skills with references:
- ✅ cobol-migration-analyzer (7 reference files)
- ✅ backend-coding (needs expansion)
- ✅ Others (need audit)

Skills needing references directories:
- Several skills with 500+ lines

#### Actions:
1. Create `references/` directories for skills lacking them
2. Plan reference file structure for each skill
3. Move appropriate content from SKILL.md to references

**Checklist:**
- [ ] Create references/ for all 7 critical skills
- [ ] Create references/ for warning-level skills if needed
- [ ] Verify directory naming (lowercase)
- [ ] Add README.md in scripts/ directories (for documentation purposes only)

---

### Task 3.2: Optimize Existing References
**Priority:** Medium  
**Estimated Time:** 2 hours

#### Actions:

1. **Audit existing reference files:**
   - Check for duplicate content with SKILL.md
   - Verify file naming conventions
   - Check file organization and structure
   - Verify table of contents for files >100 lines

2. **For COBOL skill references:**
   - Verify current 7 files are properly structured
   - Add loading conditions in SKILL.md
   - Ensure no duplication

3. **Reference File Standards:**
   - Add table of contents for files >100 lines
   - Use clear, descriptive filenames
   - Structure with proper headings
   - Include examples and code blocks
   - Avoid duplication with SKILL.md

**Reference File Template:**
```markdown
# [Topic Name]

## Table of Contents
- [Section 1](#section-1)
- [Section 2](#section-2)
- [Section 3](#section-3)

## Section 1
[Content with examples]

## Section 2
[Content with examples]

## Section 3
[Content with examples]
```

**Checklist:**
- [ ] Audit all existing reference files
- [ ] Remove duplicate content
- [ ] Add TOC to files >100 lines
- [ ] Standardize file naming
- [ ] Verify proper markdown structure

---

### Task 3.3: Scripts Documentation and Standards
**Priority:** Medium  
**Estimated Time:** 1 hour

Current scripts (from COBOL skill):
- `analyze-dependencies.sh` / `.ps1`
- `estimate-complexity.py`
- `extract-structure.py`
- `generate-java-classes.py`
- `README.md` (for scripts documentation)

#### Actions:

1. **Audit all scripts for standards compliance:**
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

2. **Verify scripts are:**
   - Self-contained or dependencies documented
   - Executable (`chmod +x`)
   - Include shebang line
   - Include usage documentation
   - Handle errors gracefully
   - Provide clear error messages

3. **Document in SKILL.md:**
   - When to use each script
   - How to execute scripts
   - Expected inputs/outputs
   - Dependencies required

**Checklist:**
- [ ] Audit all existing scripts
- [ ] Add docstrings where missing
- [ ] Verify shebang lines
- [ ] Test script execution
- [ ] Document scripts in SKILL.md
- [ ] Verify error handling

---

### Task 3.4: Add Conditional Loading Instructions
**Priority:** Critical  
**Estimated Time:** 2 hours

Add explicit instructions in SKILL.md for when to load each reference file.

#### Pattern for Reference Loading:

```markdown
## Advanced Topics

Load reference files based on specific needs:

- **Node.js Development**: Load [nodejs-patterns.md](references/nodejs-patterns.md) when:
  - User mentions Node.js, Express, NestJS, or Fastify
  - Working on Node.js backend implementation
  - Need Node.js-specific patterns

- **Python Development**: Load [python-patterns.md](references/python-patterns.md) when:
  - User mentions Python, Django, Flask, or FastAPI
  - Working on Python backend implementation
  - Need Python-specific patterns

- **API Design**: Load [api-design.md](references/api-design.md) when:
  - Designing REST, GraphQL, or gRPC APIs
  - Need API versioning strategies
  - Working on API documentation
```

**Checklist:**
- [ ] Add loading conditions for all reference files
- [ ] Use clear conditional triggers
- [ ] Group related references together
- [ ] Ensure conditions are mutually exclusive where appropriate
- [ ] Test with sample user requests

---

## Phase 4: Quality Assurance

### Task 4.1: Frontmatter Validation
**Priority:** High  
**Estimated Time:** 1 hour

Validate all frontmatter fields against specification.

#### Validation Checklist for Each Skill:

**Required Fields:**
- [ ] `name` field present
- [ ] `name` is 1-64 characters
- [ ] `name` uses only lowercase, numbers, hyphens
- [ ] `name` doesn't start/end with hyphen
- [ ] `name` has no consecutive hyphens (`--`)
- [ ] `name` matches directory name
- [ ] `description` field present
- [ ] `description` is 1-1024 characters
- [ ] `description` has no angle brackets (`<` or `>`)

**Optional Fields:**
- [ ] `license` field appropriate if present
- [ ] `compatibility` field appropriate if present (only if specific requirements)
- [ ] `metadata` fields are strings if present
- [ ] `allowed-tools` properly formatted if present

**Actions:**
1. Run automated validation: `skills-ref validate ./skill-name`
2. Manually review frontmatter
3. Fix any validation errors
4. Document any edge cases

---

### Task 4.2: Content Quality Review
**Priority:** High  
**Estimated Time:** 3 hours

Review content quality for all skills.

#### Quality Checklist:

**Completeness:**
- [ ] Frontmatter valid and comprehensive
- [ ] Description includes what, when, triggers
- [ ] Instructions are clear and step-by-step
- [ ] Examples included (input/output)
- [ ] Edge cases covered
- [ ] Resources referenced appropriately
- [ ] File structure correct

**Style:**
- [ ] Uses imperative form ("Create", "Extract", "Validate")
- [ ] Uses active voice
- [ ] Uses present tense
- [ ] Specific, not vague
- [ ] Professional and clear
- [ ] Proper markdown formatting
- [ ] Code blocks have language specified

**Progressive Disclosure:**
- [ ] SKILL.md under 500 lines (ideally under 300)
- [ ] Detailed content in references/
- [ ] References properly linked
- [ ] Loading conditions documented
- [ ] No duplicate content
- [ ] No deeply nested references

**Structure:**
- [ ] Follows appropriate pattern (workflow/task/reference/capabilities)
- [ ] Consistent heading hierarchy
- [ ] Logical organization
- [ ] Easy to navigate

---

### Task 4.3: Remove Auxiliary Documentation
**Priority:** Medium  
**Estimated Time:** 30 minutes

Remove any files that shouldn't be in skill directories.

**Files to Remove:**
- ❌ README.md (except in scripts/ for script documentation)
- ❌ INSTALLATION_GUIDE.md
- ❌ CHANGELOG.md
- ❌ CONTRIBUTING.md
- ❌ TESTING.md
- ❌ Any other documentation not part of the skill

**Exception:** Scripts can have a README.md to document script usage

**Checklist:**
- [ ] Audit all skill directories
- [ ] Remove auxiliary documentation
- [ ] Keep LICENSE.txt if present
- [ ] Keep scripts/README.md if it documents scripts
- [ ] Verify only required files remain

---

### Task 4.4: Testing and Validation
**Priority:** Critical  
**Estimated Time:** 2 hours

Test updated skills with actual usage scenarios.

#### Testing Process:

**1. Validation Tests:**
```bash
# Run validator on all skills
for skill_dir in .claude/skills/*/; do
  echo "Testing: $(basename $skill_dir)"
  skills-ref validate "$skill_dir"
done
```

**2. Trigger Tests:**
Create test prompts for each skill to verify triggering:
```
Test prompts by skill category:

Migration Skills:
- "Analyze this COBOL program for migration to Java"
- "Help me understand this JCL job"
- "Convert this RPG program to Java"

Development Skills:
- "Build a REST API with Node.js"
- "Create a React component for user profile"
- "Design a database schema for e-commerce"

Review Skills:
- "Review this code for security vulnerabilities"
- "Check this requirements document"
- "Review this architecture design"
```

**3. Reference Loading Tests:**
- Verify references load when conditions are met
- Test that references don't load unnecessarily
- Verify scripts execute correctly

**4. Workflow Tests:**
- Test complete workflows for each skill
- Verify step-by-step instructions work
- Test edge case handling

**Testing Checklist:**
- [ ] All skills pass validation
- [ ] All skills trigger appropriately
- [ ] Reference loading works correctly
- [ ] Scripts execute without errors
- [ ] Workflows complete successfully
- [ ] Edge cases handled properly
- [ ] Examples produce expected outputs

---

## Phase 5: Documentation and Finalization

### Task 5.1: Create Skills Catalog
**Priority:** Medium  
**Estimated Time:** 1 hour

Create a comprehensive catalog of all skills with metadata.

**Format:**
```markdown
# Skills Catalog

## Migration Skills

### cobol-migration-analyzer
- **Lines:** 126
- **References:** 7 files
- **Scripts:** 5 files
- **Description:** Analyzes legacy COBOL programs for Java migration
- **Triggers:** COBOL, .cbl files, mainframe, copybooks
- **Status:** ✅ Compliant

### jcl-migration-analyzer
- **Lines:** 184
- **References:** TBD
- **Scripts:** TBD
- **Description:** Analyzes JCL jobs for workflow migration
- **Triggers:** JCL, .jcl files, job steps, batch processing
- **Status:** ✅ Compliant

[Continue for all skills...]
```

**Checklist:**
- [ ] Document all 30 skills
- [ ] Include line counts
- [ ] List references and scripts
- [ ] Note compliance status
- [ ] Add trigger keywords
- [ ] Organize by category

---

### Task 5.2: Update Project Documentation
**Priority:** Medium  
**Estimated Time:** 1 hour

Update README.md and my-skills.md with current information.

**README.md Updates:**
- Update skill list with descriptions
- Add usage instructions
- Document directory structure
- Add development guidelines

**my-skills.md Updates:**
- Ensure consistency with actual skills
- Update descriptions
- Add new skills if any
- Remove deprecated skills

**Checklist:**
- [ ] Update README.md
- [ ] Update my-skills.md
- [ ] Verify consistency
- [ ] Add usage examples
- [ ] Document contribution guidelines

---

### Task 5.3: Create Maintenance Guidelines
**Priority:** Low  
**Estimated Time:** 30 minutes

Create guidelines for maintaining skills going forward.

**Guidelines Document:**
```markdown
# Skill Maintenance Guidelines

## Adding New Skills
1. Choose appropriate pattern
2. Keep SKILL.md under 500 lines
3. Include comprehensive description
4. Add trigger keywords
5. Create references/ if needed
6. Validate with skills-ref
7. Test triggering

## Updating Existing Skills
1. Verify line count
2. Check for outdated content
3. Update references if needed
4. Re-validate frontmatter
5. Test changes

## Review Cycle
- Monthly: Review skill usage metrics
- Quarterly: Update outdated content
- Yearly: Major refactoring if needed
```

**Checklist:**
- [ ] Create maintenance guidelines
- [ ] Document review cycle
- [ ] Add troubleshooting guide
- [ ] Include validation checklist
- [ ] Add contribution process

---

## Phase 6: Implementation Workflow

### Recommended Implementation Order

**Week 1: Critical Issues**
1. Task 1.1: Run automated validation
2. Task 1.2: Line count analysis
3. Task 2.1: Split 7 critical oversized SKILL.md files
4. Task 3.4: Add conditional loading instructions

**Week 2: Quality Improvements**
5. Task 1.3: Description quality audit
6. Task 2.2: Enhance descriptions with triggers
7. Task 3.1: Create missing references directories
8. Task 3.2: Optimize existing references

**Week 3: Standardization**
9. Task 2.3: Standardize SKILL.md structure
10. Task 3.3: Scripts documentation and standards
11. Task 4.1: Frontmatter validation
12. Task 4.3: Remove auxiliary documentation

**Week 4: Testing and Finalization**
13. Task 4.2: Content quality review
14. Task 4.4: Testing and validation
15. Task 5.1: Create skills catalog
16. Task 5.2: Update project documentation
17. Task 5.3: Create maintenance guidelines

---

## Tools and Scripts

### Validation Script
```bash
#!/bin/bash
# validate-all-skills.sh

echo "Validating all skills..."
FAILED=0

for skill_dir in .claude/skills/*/; do
  skill_name=$(basename "$skill_dir")
  echo "Validating: $skill_name"
  
  if ! skills-ref validate "$skill_dir" 2>&1 | tee -a validation.log; then
    echo "❌ FAILED: $skill_name"
    ((FAILED++))
  else
    echo "✅ PASSED: $skill_name"
  fi
  echo "---"
done

echo ""
echo "Validation complete. Failed: $FAILED"
exit $FAILED
```

### Line Count Report Script
```bash
#!/bin/bash
# line-count-report.sh

echo "# Line Count Report"
echo ""
echo "| Skill | Lines | Status |"
echo "|-------|-------|--------|"

for skill_file in .claude/skills/*/SKILL.md; do
  lines=$(wc -l < "$skill_file")
  skill_name=$(basename $(dirname "$skill_file"))
  
  if [ $lines -gt 500 ]; then
    status="❌ CRITICAL"
  elif [ $lines -gt 400 ]; then
    status="⚠️ WARNING"
  else
    status="✅ OK"
  fi
  
  echo "| $skill_name | $lines | $status |"
done | sort -t'|' -k3 -r
```

### Reference File Generator Template
```bash
#!/bin/bash
# generate-reference-structure.sh

SKILL_NAME=$1

if [ -z "$SKILL_NAME" ]; then
  echo "Usage: $0 <skill-name>"
  exit 1
fi

SKILL_DIR=".claude/skills/$SKILL_NAME"
REF_DIR="$SKILL_DIR/references"

if [ ! -d "$SKILL_DIR" ]; then
  echo "Error: Skill directory not found: $SKILL_DIR"
  exit 1
fi

mkdir -p "$REF_DIR"

echo "Created references directory: $REF_DIR"
echo ""
echo "Suggested reference files for $SKILL_NAME:"
echo "- $REF_DIR/patterns.md"
echo "- $REF_DIR/examples.md"
echo "- $REF_DIR/api-reference.md"
echo ""
echo "Update SKILL.md to reference these files with loading conditions."
```

---

## Checklists

### Pre-Implementation Checklist
- [ ] Backup entire `.claude/skills/` directory
- [ ] Install skills-ref validator
- [ ] Review rules.md completely
- [ ] Understand Agent Skills specification
- [ ] Set up version control for tracking changes
- [ ] Create working branch for updates

### Per-Skill Update Checklist
- [ ] Validate current state
- [ ] Check line count
- [ ] Review description
- [ ] Analyze content structure
- [ ] Plan reference files if needed
- [ ] Update frontmatter if needed
- [ ] Split content if oversized
- [ ] Create reference files
- [ ] Add loading conditions
- [ ] Update SKILL.md structure
- [ ] Add/update examples
- [ ] Remove auxiliary docs
- [ ] Validate updated skill
- [ ] Test triggering
- [ ] Test reference loading
- [ ] Document changes

### Post-Implementation Checklist
- [ ] All skills validated successfully
- [ ] All skills under 500 lines
- [ ] All descriptions include triggers
- [ ] All references properly structured
- [ ] All scripts documented
- [ ] No auxiliary documentation
- [ ] Catalog created
- [ ] Documentation updated
- [ ] Maintenance guidelines created
- [ ] Changes committed to version control

---

## Success Metrics

### Quantitative Metrics
- ✅ 0 validation errors
- ✅ 0 SKILL.md files over 500 lines
- ✅ All descriptions include trigger keywords
- ✅ All reference files have TOC if >100 lines
- ✅ All scripts have proper documentation

### Qualitative Metrics
- ✅ Skills follow consistent patterns
- ✅ Content is clear and actionable
- ✅ Examples are relevant and helpful
- ✅ References load conditionally
- ✅ Scripts execute reliably
- ✅ Skills trigger appropriately

---

## Risk Management

### Identified Risks

**Risk 1: Breaking Existing Functionality**
- **Mitigation:** Create backups, use version control, test thoroughly
- **Rollback Plan:** Revert to backed-up version

**Risk 2: Inconsistent Updates**
- **Mitigation:** Use checklists, follow templates, peer review
- **Prevention:** Clear guidelines and examples

**Risk 3: Loss of Important Content**
- **Mitigation:** Careful content analysis before splitting
- **Prevention:** Review all moved content

**Risk 4: Reference Files Not Loading**
- **Mitigation:** Clear loading conditions, thorough testing
- **Prevention:** Test with multiple scenarios

**Risk 5: Time Overrun**
- **Mitigation:** Prioritize critical issues, implement in phases
- **Prevention:** Realistic time estimates

---

## Templates

### New Reference File Template
```markdown
# [Reference Topic Name]

> **Loading Conditions:** Load this file when [specific conditions]

## Table of Contents
- [Overview](#overview)
- [Section 1](#section-1)
- [Section 2](#section-2)
- [Examples](#examples)

## Overview
Brief description of what this reference covers.

## Section 1
[Detailed content with examples]

### Subsection 1.1
[Content]

### Subsection 1.2
[Content]

## Section 2
[Detailed content with examples]

## Examples

### Example 1: [Scenario]
**Input:**
```
[input example]
```

**Output:**
```
[output example]
```

**Explanation:** [Why this works]

### Example 2: [Scenario]
[Similar structure]

## Best Practices
- Practice 1
- Practice 2
- Practice 3

## Common Pitfalls
- Pitfall 1 and how to avoid it
- Pitfall 2 and how to avoid it

## Related References
- [Related Topic 1](./related-file-1.md)
- [Related Topic 2](./related-file-2.md)
```

### SKILL.md Header Template
```markdown
---
name: skill-name
description: [Core capabilities]. [Deliverables]. Use when [scenarios], [file types], [tasks]. Trigger when users mention "[keyword1]", "[keyword2]", "[keyword3]", or "[keyword4]".
license: MIT
metadata:
  author: [Author Name]
  version: "1.0"
  category: [development/review/migration/design]
---

# [Skill Name]

[One sentence description of what this skill does]

## Core Capabilities

1. **[Capability 1]**
   [Brief description]

2. **[Capability 2]**
   [Brief description]

[Continue...]

## Quick Start

[Simple example to get started]

## [Main Content Section]

[Organized content based on chosen pattern]

## Advanced Topics

Load reference files based on specific needs:

- **[Topic 1]**: See [[file1].md](references/[file1].md) when [conditions]
- **[Topic 2]**: See [[file2].md](references/[file2].md) when [conditions]

## Critical Tips

1. [Important tip 1]
2. [Important tip 2]
3. [Important tip 3]

## Output Structure

[Description of expected deliverables]
```

---

## Appendix

### A. Skill Categories

**Migration Skills (8):**
- application-migration
- cobol-migration-analyzer
- database-migration
- jcl-migration-analyzer
- platform-migration
- pli-migration-analyzer
- rpg-migration-analyzer
- system-migration

**Development Skills (8):**
- backend-coding
- backend-design
- database-design
- devops
- frontend-coding
- frontend-ui-ux-design
- git-commit
- software-solution-architecture

**Review Skills (8):**
- architecture-design-review
- backend-code-review
- backend-design-review
- bug-analysis
- code-quality-review
- code-refactoring
- code-security-review
- frontend-code-review
- frontend-design-review

**Requirements Skills (3):**
- requirement-review
- requirements-gathering
- project-planning

**Testing Skills (1):**
- integration-testing

**Documentation Skills (1):**
- technical-writing

---

### B. Reference File Naming Conventions

**Good Names:**
- `nodejs-patterns.md` (framework-specific)
- `api-design.md` (topic-specific)
- `owasp-top10.md` (standard-specific)
- `auth-patterns.md` (domain-specific)

**Bad Names:**
- `file1.md` (non-descriptive)
- `NodeJS_Patterns.md` (wrong case)
- `api design.md` (spaces)
- `API-DESIGN.MD` (wrong extension case)

---

### C. Loading Condition Examples

**Framework-Specific:**
```markdown
- **React Development**: Load [react-patterns.md](references/react-patterns.md) when:
  - User mentions React, JSX, or React hooks
  - Working on React component implementation
  - Need React-specific patterns or best practices
```

**Domain-Specific:**
```markdown
- **Financial Domain**: Load [finance-schemas.md](references/finance-schemas.md) when:
  - User asks about revenue, billing, or payment data
  - Need financial metrics or reports
  - Working with financial data models
```

**Task-Specific:**
```markdown
- **Form Filling**: Load [forms-guide.md](references/forms-guide.md) when:
  - User needs to fill PDF forms
  - Working with form field extraction
  - Need form validation patterns
```

**Complexity-Specific:**
```markdown
- **Advanced OOXML**: Load [ooxml-details.md](references/ooxml-details.md) when:
  - User needs to modify Word document XML directly
  - Working with tracked changes or complex formatting
  - Need low-level OOXML manipulation
```

---

### D. Quick Reference: Rules.md Key Points

1. **Size Limits:**
   - Frontmatter: ~100 tokens
   - SKILL.md body: <5000 tokens (ideally <500 lines)
   - References: Unlimited (loaded on demand)

2. **Progressive Disclosure:**
   - Level 1: Metadata (always loaded)
   - Level 2: SKILL.md body (loaded on trigger)
   - Level 3: References (loaded as needed)

3. **Description Requirements:**
   - Include WHAT the skill does
   - Include WHEN to use it
   - Include trigger keywords
   - 1-1024 characters
   - No angle brackets

4. **Directory Structure:**
   - `SKILL.md` (required, uppercase)
   - `scripts/` (optional, lowercase)
   - `references/` (optional, lowercase)
   - `assets/` (optional, lowercase)
   - `LICENSE.txt` (optional)
   - NO README, CHANGELOG, etc.

5. **Writing Style:**
   - Imperative mood
   - Active voice
   - Present tense
   - Specific, not vague
   - Include examples

---

**End of Task List**

This comprehensive task list provides a systematic approach to updating all skills based on rules.md guidelines. Follow the phases sequentially, use the checklists, and refer to templates as needed.
