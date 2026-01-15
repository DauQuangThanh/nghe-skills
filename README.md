# Nghệ Skills - Software Development Skills Collection

A comprehensive collection of AI agent skills for software development, covering the complete software development lifecycle from requirements gathering to deployment and migration. Works with multiple AI assistants including GitHub Copilot, Claude Code, Gemini CLI, Cursor, and more.

## What is Nghệ Skills?

**Nghệ Skills** is a curated collection of 40 specialized AI skills designed to enhance your software development workflow across any modern AI coding assistant. The project includes:

- **40 Production-Ready Skills**: Covering cloud platforms, development, code review, testing, and legacy system migration
- **Nghệ CLI Tool**: A command-line interface to bootstrap projects with agent skills tailored for your AI assistant
- **Multi-Agent Support**: Compatible with 16+ AI assistants including GitHub Copilot, Claude Code, Gemini, Cursor, Windsurf, Amazon Q, and more
- **Progressive Disclosure**: Modular reference files loaded on-demand to optimize context usage
- **Agent Skills Compliant**: Following [agentskills.io specification](https://agentskills.io/specification) with all skills under 500 lines

**All 39 skills fully optimized and Agent Skills compliant!**

### Latest Improvements
- ✅ **Specification Compliant** - Removed non-standard frontmatter fields (license, metadata)
- ✅ **Explicit Loading Conditions** - Added "when to load" guidance for all reference files
- ✅ **Concrete Examples** - Added Quick Start examples to all skills
- ✅ **Critical Tips** - Added best practice tips for development skills
- ✅ **Improved Overviews** - Replaced placeholders with actual content
- ✅ **100% compliant** - All skills under 500-line hard limit
- ✅ **90% size reduction** - Average skill reduced from ~700 to ~176 lines
- ✅ **328+ reference files** - Detailed content split into modular references
- ✅ **Better organized** - Core concepts in SKILL.md, details in references/
- ✅ **Nghệ CLI Tool** - Bootstrap projects with multi-agent support (16+ AI assistants)

## Table of Contents

- [What is Nghệ Skills?](#what-is-nghệ-skills)
- [Quick Start](#quick-start)
- [Nghệ CLI Tool](#nghệ-cli-tool)
- [Skills Overview](#skills-overview)
- [Compliance with Agent Skills Specification](#compliance-with-agent-skills-specification)
- [Quick Stats](#quick-stats)
- [Skills by Category](#skills-by-category)
- [Getting Started](#getting-started)
- [Key Features](#key-features)
- [Technologies Supported](#technologies-supported)
- [Project Structure](#project-structure)
- [Use Cases](#use-cases)
- [License](#license)
- [Contributing](#contributing)

## Quick Start

### Installation

Install the Nghệ CLI tool directly from GitHub using `uv`:

```bash
# Install from GitHub (recommended)
uv tool install nghe-cli --force --from git+https://github.com/dauquangthanh/nghe-skills.git
```

```bash
# Install from GitHub (recommended if you face TLS-related errors)
uv tool install nghe-cli --force --from git+https://github.com/dauquangthanh/nghe-skills.git --native-tls
```

### Initialize a New Project

```bash
# Interactive multi-agent selection (Copilot pre-selected)
nghe init my-project

# Specify AI assistant(s)
nghe init my-project --ai claude
nghe init my-project --ai claude,gemini,copilot

# Initialize in current directory
nghe init .
nghe init --here

# Upgrade existing project
nghe init --upgrade
```

### Supported AI Assistants

The CLI supports 16+ AI assistants:
- **GitHub Copilot** (IDE-based)
- **Claude Code** (requires CLI)
- **Gemini CLI** (requires CLI)
- **Cursor** (IDE-based)
- **Windsurf** (IDE-based)
- **Amazon Q Developer CLI** (requires CLI)
- **Codex CLI** (requires CLI)
- **Qwen Code** (requires CLI)
- **Auggie CLI** (requires CLI)
- **SHAI** (requires CLI)
- **Amp** (requires CLI)
- **CodeBuddy** (requires CLI)
- **opencode** (requires CLI)
- **Kilo Code** (IDE-based)
- **Roo Code** (IDE-based)
- **IBM Bob** (IDE-based)

## Nghệ CLI Tool

The Nghệ CLI (`nghe`) is a command-line tool for bootstrapping projects with AI agent skills.

### Features

- **Multi-Agent Support**: Install skills for multiple AI assistants in one project
- **Flexible Installation**: Initialize new projects or add skills to existing projects
- **Upgrade Capability**: Update existing skill installations with automatic backups
- **Local Development**: Support for local template testing with `--use-local-source`
- **Smart Detection**: Auto-detects git, validates AI assistant tools, and checks for existing installations
- **Interactive Selection**: Multi-select interface for choosing AI assistants (arrow keys + space)
- **GitHub Integration**: Downloads latest templates from GitHub releases with optional token authentication

### CLI Commands

#### `nghe init`

Initialize a new project or upgrade existing skills.

**Basic Usage:**
```bash
# Interactive multi-agent selection
nghe init my-project

# Single AI assistant
nghe init my-project --ai claude

# Multiple AI assistants
nghe init my-project --ai claude,gemini,copilot

# Current directory
nghe init --here
nghe init .

# Upgrade existing installation
nghe init --upgrade
nghe init my-project --upgrade
```

**Advanced Options:**
```bash
# Skip git initialization
nghe init my-project --ai claude --no-git

# Skip AI tool checks
nghe init my-project --ai claude --ignore-agent-tools

# Force merge into existing directory
nghe init existing-project --force

# Use local source (development)
nghe init demo --use-local-source --local-source-path /path/to/nghe-skills

# Skip TLS verification (not recommended)
nghe init my-project --skip-tls

# GitHub authentication
nghe init my-project --github-token ghp_yourtoken
export GITHUB_TOKEN=ghp_yourtoken
nghe init my-project

# Debug mode
nghe init my-project --debug
```

#### `nghe check`

Check installed tools and AI assistants.

```bash
nghe check
```

Output shows:
- Git version control status
- Installed AI assistant CLI tools
- VS Code variants (code, code-insiders)

#### `nghe version`

Display version and system information.

```bash
nghe version
```

Shows:
- CLI version
- Latest template version
- Python version
- Platform and architecture

### Environment Variables

```bash
# GitHub token for API access (increases rate limits)
export GITHUB_TOKEN=ghp_yourtoken
export GH_TOKEN=ghp_yourtoken

# Use local source instead of downloading
export NGHE_USE_LOCAL_INSTALLATION=1
export NGHE_SKILL_PATH=/path/to/nghe-skills/my-skills

# Codex-specific (set before running Codex)
export CODEX_HOME=/path/to/project/.codex
```

### Project Structure Created by CLI

When you run `nghe init my-project --ai copilot,claude`, the CLI creates:

```
my-project/
├── .github/               # GitHub Copilot configuration
│   └── skills/           # 39 agent skills for Copilot
│       ├── aws-cloud/
│       ├── backend-coding/
│       └── ... (39 skills total)
├── .claude/              # Claude Code configuration
│   └── skills/           # 39 agent skills for Claude
│       ├── aws-cloud/
│       ├── backend-coding/
│       └── ... (39 skills total)
└── .git/                 # Git repository (unless --no-git)
```

Each AI assistant gets its own folder with dedicated skill copies optimized for that platform.

## Skills Overview

This repository provides **40 specialized AI skills** organized into multiple categories:
- **Cloud Platforms** (6 skills) - AWS, Azure, GCP, Alibaba, IBM, Oracle Cloud
- **Migration & Legacy** (10 skills) - Application, database, platform, system migrations, COBOL, JCL, PL/I, RPG analyzers, mainframe systems
- **Development** (7 skills) - Backend, frontend, database, DevOps, refactoring, git commits
- **Review & Quality** (8 skills) - Code quality, security, architecture, design reviews
- **Requirements & Planning** (4 skills) - Gathering, review, planning, pseudocode to specification
- **Testing** (1 skill) - Integration and E2E testing
- **Specialized** (4 skills) - Bug analysis, technical writing, KeyCloak administration

Each skill follows the [Agent Skills specification](https://agentskills.io/specification) with:
- Concise SKILL.md files (<500 lines)
- Progressive disclosure with reference files
- Concrete examples and quick starts
- Clear trigger conditions
- Modular and context-efficient design

## Compliance with Agent Skills Specification

✅ **Frontmatter** - Only standard fields (`name`, `description`)  
✅ **Line Count** - All skills under 500 lines (100%)  
✅ **Progressive Disclosure** - Reference files loaded on demand  
✅ **Loading Conditions** - Explicit "when to load" for all references  
✅ **Structure** - Overview, Capabilities, Quick Start, Detailed Topics, Critical Tips  
✅ **Examples** - Concrete code examples in all skills

## Skills by Category

### ☁️ Cloud Platforms (6 skills)

#### 1. AWS Cloud
Provides comprehensive Amazon Web Services (AWS) guidance including EC2, S3, RDS, Lambda, ECS/EKS, CloudFormation, API Gateway, VPC, IAM, security configuration, cost optimization, and infrastructure as code.

**Use Cases:**
- Deploying to AWS and designing AWS infrastructure
- Setting up EC2 instances, S3 buckets, RDS databases
- Building serverless applications with Lambda
- Container orchestration with ECS/EKS
- Infrastructure as code with Terraform/CloudFormation/CDK

#### 2. Azure Cloud
Provides comprehensive Microsoft Azure guidance including Virtual Machines, Azure Storage, Azure SQL Database, App Service, Functions, AKS, Azure DevOps, ARM templates, Bicep, security configuration, and cost optimization.

**Use Cases:**
- Deploying to Azure and designing Azure infrastructure
- Setting up VMs, Storage accounts, Azure SQL
- Kubernetes orchestration with AKS
- Infrastructure as code with Bicep/ARM/Terraform
- Azure DevOps and CI/CD pipelines

#### 3. Google Cloud Platform
Provides comprehensive GCP guidance including Compute Engine, Cloud Storage, Cloud SQL, BigQuery, GKE, Cloud Functions, Cloud Run, VPC networking, IAM, security configuration, and infrastructure as code.

**Use Cases:**
- Deploying to Google Cloud and designing GCP infrastructure
- Setting up GCE instances, Cloud Storage, Cloud SQL
- Data analytics with BigQuery
- Kubernetes with GKE
- Infrastructure as code with Terraform

#### 4. Alibaba Cloud
Provides comprehensive Alibaba Cloud (Aliyun) guidance including ECS, ApsaraDB, OSS, SLB, VPC, RAM, ACK (Kubernetes), Function Compute, API Gateway, CDN, and monitoring services.

**Use Cases:**
- Working with Alibaba Cloud services
- Designing cloud architecture on Aliyun
- Setting up Chinese cloud infrastructure
- Multi-region deployments in China

#### 5. IBM Cloud
Provides comprehensive IBM Cloud platform guidance including compute services (VPC, IKS, Code Engine), storage, databases (Db2, Cloudant, PostgreSQL), AI/ML services (Watson), IAM security, and monitoring.

**Use Cases:**
- Working with IBM Cloud infrastructure
- Deploying applications to IBM Cloud
- Managing cloud resources and services
- Integration with Watson AI services

#### 6. Oracle Cloud Infrastructure
Provides comprehensive OCI guidance including compute instances, networking (VCN, load balancers, VPN), storage, database services (Autonomous Database, MySQL, NoSQL), OKE, IAM, and infrastructure as code.

**Use Cases:**
- Designing OCI architecture
- Provisioning cloud resources
- Setting up Autonomous Database
- Container orchestration with OKE

### 📋 Requirements & Architecture (4 skills)

#### 7. Requirements Gathering
Guides comprehensive requirements gathering and analysis including stakeholder interviews, user story creation, use case documentation, acceptance criteria, requirements prioritization, and traceability.

**Capabilities:**
- Stakeholder analysis and engagement
- Requirements elicitation (interviews, workshops, surveys)
- Requirements analysis and classification
- User stories and use cases creation
- Requirements documentation (BRD, SRS)
- Agile requirements management and backlog refinement

**Use Cases:**
- Gathering and documenting requirements
- Writing user stories and acceptance criteria
- Creating use cases and user journeys
- Requirements prioritization and traceability
- Business analysis and stakeholder management

#### 8. Requirement Review
Conducts comprehensive requirements review including completeness validation, clarity assessment, consistency checking, testability evaluation, and standards compliance.

**Capabilities:**
- Completeness and clarity review
- Consistency and conflict detection
- Testability and measurability validation
- Standards compliance (IEEE 830, etc.)
- Feasibility and risk assessment
- User story quality assessment (INVEST criteria)

**Use Cases:**
- Reviewing requirements documents
- Validating user stories and acceptance criteria
- Assessing requirements quality
- Identifying gaps, conflicts, and ambiguities
- Ensuring standards compliance

#### 9. Architecture Design
Designs comprehensive software solution architectures including system components, technology stacks, integration patterns, scalability strategies, and deployment models.

**Capabilities:**
- Architecture pattern selection (microservices, SOA, event-driven)
- Technology stack evaluation and recommendations
- System component design and integration patterns
- Scalability and performance architecture
- Security architecture and deployment models
- Architecture documentation (Mermaid C4 diagrams, ADRs)

**Use Cases:**
- Planning new software systems
- Modernizing legacy applications
- Designing microservices architectures
- Evaluating technology choices
- Creating architecture documentation

#### 10. Pseudocode to Specification
Reverse engineers and generates comprehensive technical specifications from pseudocode, algorithms, or code snippets. Produces detailed requirements documents, functional specifications, API documentation, data models, and workflow diagrams.

**Capabilities:**
- Pseudocode and algorithm analysis
- Functional and non-functional requirements extraction
- Data model generation from code structures
- API specification creation
- Workflow and sequence diagram generation
- Requirements documentation (BRD, FRS, SRS)

**Use Cases:**
- Analyzing pseudocode and extracting requirements
- Reverse engineering specifications from code
- Documenting undocumented algorithms
- Creating specifications from implementation details
- Converting code snippets to formal specifications
- Generating requirements from proof-of-concepts

### 🛠️ Design & Implementation (7 skills)

#### 11. Backend Design
Designs comprehensive backend systems including RESTful APIs, microservices, database architecture, authentication/authorization, caching strategies, message queues, and scalability patterns.

**Capabilities:**
- API design (REST, GraphQL, gRPC)
- Microservices architecture and patterns
- Database design (SQL, NoSQL, schema design)
- Authentication and authorization (JWT, OAuth2, RBAC)
- Caching strategies and message queues
- Scalability and deployment patterns

**Use Cases:**
- Designing backend services and APIs
- Creating database schemas and data models
- Planning microservices architectures
- Designing authentication flows
- Integration patterns and messaging systems

#### 12. Backend Coding
Expert backend development guidance covering Node.js, Python, Java, Go, API design, database patterns, authentication, caching, message queues, microservices, and testing.

**Capabilities:**
- Backend development in multiple languages
- RESTful API and GraphQL implementation
- Database operations (JDBC, JPA, ORMs)
- Security implementation (authentication, authorization)
- Performance optimization and caching
- Microservices implementation and testing

**Use Cases:**
- Building APIs and backend services
- Implementing business logic
- Database integration and queries
- Authentication and security implementation
- Performance optimization

#### 13. Frontend UI/UX Design
Creates comprehensive frontend UI/UX designs including user interfaces, design systems, component libraries, responsive layouts, and accessibility implementations.

**Capabilities:**
- UI/UX design principles and patterns
- Design system creation and management
- Component library architecture
- Responsive and adaptive design
- Accessibility (WCAG 2.1/2.2 compliance)
- Design tools integration (Figma, Sketch)

**Use Cases:**
- Designing user interfaces
- Creating design systems and component libraries
- Implementing responsive designs
- Ensuring accessibility compliance
- Design specification and prototyping

#### 14. Frontend Coding
Expert frontend development guidance covering React, Vue, Angular, TypeScript, state management, component architecture, performance optimization, accessibility, and testing.

**Capabilities:**
- Frontend frameworks (React, Vue, Angular)
- TypeScript and modern JavaScript
- State management (Redux, Zustand, Pinia, NgRx)
- Component architecture and hooks
- Performance optimization and accessibility
- Testing (Jest, React Testing Library, Vitest)

**Use Cases:**
- Building web applications
- Implementing UI components
- Managing application state
- Performance optimization
- Frontend testing and accessibility

#### 15. Database Design
Designs comprehensive database schemas including relational and NoSQL models, normalization, indexing strategies, relationship modeling, data types, constraints, and performance optimization.

**Capabilities:**
- Relational and NoSQL database design
- Schema design and normalization
- Indexing strategies and query optimization
- Entity-relationship diagrams (Mermaid ERD)
- Data types and constraints
- Database best practices

**Use Cases:**
- Designing database schemas
- Creating data models
- Optimizing database queries
- Schema migrations
- Database architecture planning

#### 16. DevOps
Provides comprehensive DevOps guidance including CI/CD pipelines, infrastructure as code (Terraform, CloudFormation), container orchestration (Docker, Kubernetes), deployment strategies, monitoring, and configuration management.

**Capabilities:**
- CI/CD pipeline design and implementation
- Infrastructure as code (IaC)
- Container orchestration with Kubernetes
- Deployment automation
- Monitoring and observability
- Configuration management

**Use Cases:**
- Building CI/CD pipelines
- Automating infrastructure provisioning
- Container orchestration
- Implementing deployment strategies
- Setting up monitoring and alerting

#### 17. Code Refactoring
Guides systematic code refactoring to improve code quality, maintainability, and design. Identifies code smells, applies refactoring patterns, ensures test coverage, and follows safe refactoring practices.

**Capabilities:**
- Code smell detection
- Refactoring patterns application
- Test coverage maintenance
- Safe refactoring practices
- Legacy code modernization
- Performance and maintainability improvements

**Use Cases:**
- Improving code quality
- Eliminating technical debt
- Preparing for feature additions
- Modernizing legacy code
- Code cleanup and optimization

### 🔍 Review & Quality (8 skills)

#### 18. Backend Code Review
Conducts comprehensive backend code reviews including API design (REST/GraphQL/gRPC), database patterns, authentication/authorization, caching strategies, message queues, microservices architecture, security vulnerabilities, and performance optimization.

**Capabilities:**
- Backend code quality assessment
- API design review
- Database query optimization
- Security vulnerability detection
- Performance analysis
- Best practices validation

**Use Cases:**
- Reviewing backend code
- Assessing API implementations
- Validating database patterns
- Security code review
- Performance optimization review

#### 19. Frontend Code Review
Conducts comprehensive frontend code reviews including React/Vue/Angular component analysis, TypeScript/JavaScript quality assessment, CSS/styling review, performance optimization, accessibility compliance, and security vulnerabilities.

**Capabilities:**
- Frontend code quality assessment
- Component architecture review
- JavaScript/TypeScript quality check
- CSS/styling validation
- Performance analysis
- Accessibility compliance (WCAG)

**Use Cases:**
- Reviewing React/Vue/Angular components
- Analyzing JavaScript/TypeScript code
- Validating CSS/SCSS
- Assessing web performance
- Accessibility audits

#### 20. Code Quality Review
Conducts comprehensive code quality reviews including code smells detection, maintainability assessment, complexity analysis, design pattern evaluation, naming conventions, code duplication, and technical debt identification.

**Capabilities:**
- Code smell detection
- Maintainability assessment
- Complexity analysis
- Design pattern evaluation
- Technical debt identification
- Best practices validation

**Use Cases:**
- Reviewing code quality
- Detecting code smells
- Measuring code complexity
- Identifying technical debt
- Refactoring candidates assessment

#### 21. Code Security Review
Conducts comprehensive security code reviews including vulnerability detection (OWASP Top 10, CWE), authentication/authorization flaws, injection attacks, cryptography issues, sensitive data exposure, API security, and compliance validation.

**Capabilities:**
- Security vulnerability detection
- OWASP Top 10 compliance
- Authentication/authorization review
- Injection attack prevention
- Cryptography assessment
- Security best practices validation

**Use Cases:**
- Security code audits
- Vulnerability scanning
- Security compliance validation
- Penetration testing support
- Secure coding review

#### 22. Architecture Design Review
Conducts comprehensive architecture design reviews including system design validation, architecture pattern assessment, quality attributes evaluation, technology stack review, and scalability analysis.

**Capabilities:**
- Architecture assessment and validation
- Quality attributes review (scalability, performance, security)
- Technology stack evaluation
- Design documentation review (Mermaid C4 diagrams, ADRs)
- Risk identification and recommendations
- Alternative approach suggestions

**Use Cases:**
- Reviewing software architecture designs
- Validating architecture decisions
- Assessing system scalability
- Evaluating technology choices
- Architecture quality gates

#### 23. Backend Design Review
Conducts comprehensive backend design reviews covering API design quality, database architecture validation, microservices patterns assessment, integration strategies evaluation, security design review, and scalability analysis.

**Capabilities:**
- API design review (REST, GraphQL, gRPC)
- Database schema validation
- Microservices architecture assessment
- Security design review
- Performance and scalability analysis
- Integration pattern evaluation

**Use Cases:**
- Reviewing backend system designs
- Validating API specifications
- Assessing database schemas
- Evaluating microservices architectures
- Security and performance reviews

#### 24. Frontend Design Review
Conducts comprehensive frontend design reviews covering UI/UX design quality, design system validation, accessibility compliance, responsive design patterns, component library architecture, and visual design consistency.

**Capabilities:**
- UI/UX design quality assessment
- Design system validation
- Accessibility compliance (WCAG)
- Responsive design evaluation
- Component library architecture review
- Visual design consistency checks

**Use Cases:**
- Reviewing frontend designs
- Validating design systems
- Ensuring accessibility compliance
- Evaluating component libraries
- Assessing responsive designs

#### 25. Project Planning
Guides comprehensive software project planning including task breakdown, estimation, sprint planning, backlog management, resource allocation, milestone tracking, and risk management.

**Capabilities:**
- Project planning and estimation
- Task breakdown and user stories
- Sprint planning and backlog management
- Resource allocation and milestone tracking
- Risk management and mitigation
- Roadmap creation

**Use Cases:**
- Planning software projects
- Organizing sprints and iterations
- Breaking down features into tasks
- Estimating effort and timeline
- Managing product backlogs

### 🧪 Testing (1 skill)

#### 26. Integration Testing
Designs comprehensive integration testing strategies including API testing, database testing, microservices testing, end-to-end testing, and test automation frameworks.

**Capabilities:**
- Integration test strategy design
- API testing (REST, GraphQL, gRPC)
- Database testing and test data management
- Microservices and E2E testing
- Test automation frameworks (JUnit, TestNG, Jest, Playwright)
- CI/CD integration and quality metrics

**Use Cases:**
- Designing test strategies
- Writing integration tests
- Setting up test automation
- Testing APIs and microservices
- Quality assurance and CI/CD integration

### 🔄 Legacy Migration & Mainframe (10 skills)

#### 27. Application Migration
Guides comprehensive application migration projects including legacy system modernization, cloud migration, technology stack upgrades, database migration, and architecture transformation.

**Capabilities:**
- Migration strategy planning
- Legacy system assessment
- Cloud migration patterns
- Technology stack modernization
- Risk management and validation
- Cutover and rollback planning

**Use Cases:**
- Migrating applications to cloud
- Modernizing legacy systems
- Changing technology stacks
- Replatforming and refactoring
- Strangler pattern implementation

#### 28. Database Migration
Guides database migration projects including engine changes (MySQL to PostgreSQL, Oracle to PostgreSQL), version upgrades, cloud migrations, schema migrations, zero-downtime migrations, and replication setup.

**Capabilities:**
- Database engine migration
- Schema migration and transformation
- Zero-downtime migration strategies
- Replication and synchronization
- Data migration validation
- Rollback procedures

**Use Cases:**
- Migrating database engines
- Upgrading database versions
- Moving databases to cloud
- Schema transformations
- Data center migrations

#### 29. Platform Migration
Guides infrastructure and platform migration including cloud-to-cloud migration (AWS to GCP, Azure to AWS), Kubernetes cluster migration, CI/CD platform changes, monitoring stack migration, and network infrastructure transformation.

**Capabilities:**\n- Cloud-to-cloud migration\n- Kubernetes cluster migration\n- CI/CD platform changes\n- Infrastructure as code migration\n- Multi-cloud strategies\n- Service mesh migration\n\n**Use Cases:**\n- Changing cloud providers\n- Migrating Kubernetes clusters\n- Infrastructure platform changes\n- CI/CD tool migration\n- Multi-cloud implementations\n\n#### 30. System Migration\nGuides operating system and hardware platform migrations including Linux distribution changes, Windows to Linux migration, mainframe to x86 modernization, data center migrations, and system consolidation.\n\n**Capabilities:**\n- OS migration and upgrades\n- Hardware platform changes\n- Mainframe to x86 modernization\n- Virtual machine migrations (P2V, V2V)\n- System consolidation\n- Configuration transfer\n\n**Use Cases:**\n- Operating system migrations\n- Hardware platform changes\n- Mainframe modernization\n- Data center migrations\n- System consolidation projects\n\n#### 31. IBM Mainframe\nProvides comprehensive IBM Mainframe administration, development, and modernization guidance including z/OS operations, JCL scripting, COBOL/PL/I programming, CICS/IMS configuration, DB2 administration, and mainframe-to-cloud migration strategies.\n\n**Capabilities:**\n- z/OS system administration\n- JCL scripting and batch processing\n- COBOL/PL/I development\n- CICS/IMS transaction processing\n- DB2 database administration\n- Mainframe modernization strategies\n\n**Use Cases:**\n- Mainframe system administration\n- Batch job development and maintenance\n- COBOL/PL/I programming\n- Transaction processing systems\n- Mainframe to cloud migration\n\n#### 32. Fujitsu Mainframe\nAnalyzes and assists with Fujitsu mainframe systems including FACOM, PRIMERGY, BS2000/OSD, OSIV/MSP, OSIV/XSP, NetCOBOL, PowerCOBOL, and Fujitsu JCL.\n\n**Capabilities:**\n- Fujitsu mainframe system analysis\n- NetCOBOL and PowerCOBOL programming\n- Fujitsu JCL scripting\n- BS2000/OSD and OSIV operations\n- Migration to modern platforms\n- SYMFOWARE database integration\n\n**Use Cases:**\n- Fujitsu mainframe modernization\n- NetCOBOL/PowerCOBOL analysis\n- Fujitsu JCL conversion\n- FACOM system migration\n- BS2000 application modernization\n\n#### 33. JCL Migration Analyzer
Analyzes Job Control Language (JCL) scripts for migration to modern workflow orchestration systems.

**Capabilities:**
- Extract job flows, step sequences, and program invocations
- Map data dependencies and dataset relationships
- Parse procedure definitions and symbolic parameters
- Translate conditional logic (COND, IF/THEN/ELSE) with correct inversion handling
- Generate migration strategies for Spring Batch, Apache Airflow, Kubernetes Jobs, shell scripts, AWS Step Functions, or Azure Logic Apps

**Use Cases:**
- Mainframe job migration
- Batch workflow modernization
- Converting .jcl/.JCL files to modern orchestration
- Analyzing job steps and procedures

#### 34. COBOL Migration Analyzer
Analyzes COBOL programs for migration to Java applications.

**Capabilities:**
- Extract division structure (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
- Parse Working-Storage variables and file definitions
- Analyze business logic (paragraphs, PERFORM statements)
- Process copybooks and extract record layouts
- Generate Java POJOs, service methods, and JDBC/JPA patterns
- Map CALL hierarchies and dependencies

**Use Cases:**
- COBOL to Java conversion
- Analyzing .cbl/.CBL/.cob files
- Processing copybooks (.cpy files)
- Legacy system modernization

#### 35. PL/I Migration Analyzer
Analyzes PL/I (Programming Language One) programs for migration to Java.

**Capabilities:**
- Extract program structure and nested procedures
- Parse data declarations (FIXED DECIMAL, FIXED BINARY, CHARACTER, BIT, POINTER)
- Map file operations and business logic
- Translate exception handling (ON conditions) to try-catch
- Generate Java POJOs and service methods
- Preserve numeric precision in BigDecimal conversions

**Use Cases:**
- PL/I to Java conversion
- Analyzing .pli/.PLI/.pl1 files
- Processing PL/I procedures and structures
- Mainframe application modernization

#### 36. RPG Migration Analyzer
Analyzes RPG (Report Program Generator) programs from AS/400 and IBM i systems for migration to Java.

**Capabilities:**
- Extract RPG III/IV/ILE program specifications (H/F/D/C/P-specs)
- Parse data structures and file definitions
- Map file operations (READ, WRITE, UPDATE, DELETE, CHAIN, SETLL)
- Translate indicators (*IN) and built-in functions (%SUBST, %TRIM, %EOF)
- Generate Java POJOs with proper type mappings
- Convert arrays (DIM) to Java collections

**Use Cases:**
- RPG to Java conversion
- AS/400 or IBM i system migration
- Analyzing .rpg/.RPGLE files
- Processing display and database files

### 🔧 Specialized Skills (4 skills)

#### 37. Bug Analysis
Analyzes software bugs including root cause identification, severity assessment, impact analysis, reproduction steps validation, and fix recommendations.

**Capabilities:**
- Root cause analysis
- Bug triage and prioritization
- Impact assessment
- Reproduction steps validation
- Fix recommendations
- Regression analysis

**Use Cases:**
- Investigating bugs and issues
- Analyzing crash reports
- Debugging production problems
- Root cause analysis
- Bug report validation

#### 38. Git Commit
Generates well-structured git commit messages following conventional commit standards and best practices.

**Capabilities:**
- Conventional commit message generation
- Commit type classification (feat, fix, docs, etc.)
- Clear and descriptive commit messages
- Breaking change documentation
- Changelog-friendly commits
- Team convention adherence

**Use Cases:**
- Writing commit messages
- Following conventional commits
- Creating meaningful git history
- Generating changelogs
- Maintaining version control quality

#### 39. Technical Writing
Creates high-quality technical documentation including API documentation, user guides, tutorials, architecture documents, README files, release notes, and technical specifications.

**Capabilities:**
- API documentation
- User guides and tutorials
- Architecture documentation
- README files
- Release notes
- Technical specifications

**Use Cases:**
- Writing API documentation
- Creating user guides
- Documenting architecture
- Writing tutorials
- Preparing release notes

#### 40. KeyCloak Administration
Provides comprehensive KeyCloak administration guidance including realm management, user/group administration, client configuration, authentication flows, identity brokering, and authorization policies.

**Capabilities:**
- Realm and client configuration
- User federation (LDAP/AD)
- SSO setup (SAML/OIDC)
- Authentication flow customization
- Role-based access control (RBAC)
- Multi-factor authentication (MFA)

**Use Cases:**
- Configuring KeyCloak SSO
- Setting up identity providers
- Managing realms and clients
- Implementing RBAC
- User federation setup

## Getting Started

### Prerequisites

- Python 3.11 or higher
- Git (optional but recommended)
- Your preferred AI coding assistant

### Installation Methods

#### Method 1: Install from GitHub via uv (Recommended)

```bash
# Install directly from GitHub repository
uv tool install nghe-cli --force --from git+https://github.com/dauquangthanh/nghe-skills.git

# Install directly from GitHub repository (if you face TLS-related errors)
uv tool install nghe-cli --force --from git+https://github.com/dauquangthanh/nghe-skills.git --native-tls

# Run directly without installation (one-time use)
uvx --from git+https://github.com/dauquangthanh/nghe-skills.git nghe-cli init my-project
```

#### Method 2: Install from Source

```bash
# Clone the repository
git clone https://github.com/dauquangthanh/nghe-skills.git
cd nghe-skills

# Install in development mode
pip install -e .

# Or build and install
pip install build
python -m build
pip install dist/nghe_cli-*.whl
```

### Quick Start Guide

1. **Install the CLI**
   ```bash
   # Install from GitHub (recommended)
   uv tool install nghe-cli --force --from git+https://github.com/dauquangthanh/nghe-skills.git
   ```

   ```bash
   # Install from GitHub (recommended if you face TLS-related errors)
   uv tool install nghe-cli --force --from git+https://github.com/dauquangthanh/nghe-skills.git --native-tls
   ```

2. **Initialize a New Project**
   ```bash
   # Interactive mode (choose AI assistants)
   nghe init my-awesome-project
   
   # Or specify directly
   nghe init my-awesome-project --ai claude,copilot
   ```

3. **Navigate to Your Project**
   ```bash
   cd my-awesome-project
   ```

4. **Start Coding with AI**
   
   Open your project in your AI coding assistant (VS Code with Copilot, Claude Code, Cursor, etc.) and the skills will be automatically loaded.

5. **Use Skills in Your Workflow**

   The skills are automatically activated when you:
   - Mention relevant tasks or technologies
   - Request specific development activities
   - Work with certain file types
   - Ask about specific methodologies or patterns

### Verifying Installation

Check that everything is set up correctly:

```bash
# Check CLI version
nghe version

# Check installed tools
nghe check
```

### Using the Skills

Each skill is automatically activated when you:
- Mention relevant tasks or technologies
- Request specific development activities
- Work with certain file types
- Ask about specific methodologies or patterns

The skills follow a progressive disclosure model:
1. **Frontmatter (name + description)**: Always loaded for skill discovery (~100 tokens)
2. **SKILL.md body**: Loaded when skill is triggered (<5000 tokens)
3. **Reference files**: Loaded on-demand when detailed information is needed

This design minimizes context usage while maximizing available information.

### Development Workflow with Nghệ Skills

The skills support a complete development workflow:

```
1. Project Setup
   └─> nghe init my-project --ai claude,copilot

2. Requirements Phase
   ├─> /requirements-gathering - Elicit and document requirements
   ├─> /requirement-review - Validate requirements quality
   └─> /architecture-design - Design system architecture

3. Design Phase
   ├─> /backend-design - Design APIs and services
   ├─> /frontend-ui-ux-design - Design user interfaces
   ├─> /database-design - Design data models
   └─> Design reviews (backend/frontend/architecture)

4. Implementation Phase
   ├─> /backend-coding - Implement backend services
   ├─> /frontend-coding - Implement UI components
   ├─> /devops - Set up CI/CD pipelines
   └─> /code-refactoring - Improve code quality

5. Review Phase
   ├─> /code-quality-review - Assess code quality
   ├─> /code-security-review - Security audit
   ├─> /backend-code-review - Backend code review
   └─> /frontend-code-review - Frontend code review

6. Testing Phase
   └─> /integration-testing - Design and implement tests

7. Deployment Phase
   ├─> Cloud skills (aws-cloud, azure-cloud, etc.)
   └─> /devops - Deploy and monitor

8. Maintenance Phase
   ├─> /bug-analysis - Investigate and fix bugs
   ├─> /code-refactoring - Technical debt reduction
   └─> /technical-writing - Documentation updates
```

**Example Prompts by Category:**

**Cloud Platforms:**
- "Deploy a Node.js application to AWS using ECS"
- "Set up Azure Kubernetes Service for microservices"
- "Design a GCP architecture with Cloud Run and Cloud SQL"
- "Configure Alibaba Cloud OSS and CDN"
- "Set up IBM Cloud VPC and IKS cluster"
- "Deploy Oracle Autonomous Database on OCI"

**Requirements & Architecture:**
- "Help me gather requirements for a new e-commerce platform"
- "Review these user stories for quality and completeness"
- "Design a microservices architecture for this system"
- "Create a requirements traceability matrix"

**Design & Implementation:**
- "Design a REST API for user management"
- "Help me implement authentication in Node.js"
- "Create a React component library structure"
- "Design a PostgreSQL schema for e-commerce"
- "Set up a CI/CD pipeline with GitHub Actions"
- "Refactor this code to improve maintainability"

**Review & Quality:**
- "Review this backend API code for best practices"
- "Check this React component for accessibility issues"
- "Analyze code quality and identify technical debt"
- "Perform a security review of this authentication code"
- "Review this architecture design for scalability"
- "Validate this database schema design"
- "Plan the next sprint with these user stories"

**Testing:**
- "Create an integration test strategy for microservices"
- "Write integration tests for this REST API"
- "Set up test automation with Playwright"

**Legacy Migration & Mainframe:**
- "Analyze this JCL file and explain the job flow"
- "Convert this COBOL program to Java"
- "Extract the business logic from this PL/I procedure"
- "Generate a Spring Batch job from this JCL"
- "Map this RPG data structure to a Java POJO"
- "Migrate this IBM mainframe application to AWS"
- "Analyze this Fujitsu COBOL program for modernization"

**Specialized:**
- "Analyze this bug and identify root cause"
- "Generate a conventional commit message for these changes"
- "Write API documentation for this REST service"
- "Configure KeyCloak SSO with SAML integration"

## Key Features

### Comprehensive SDLC Coverage
Complete software development lifecycle support from initial requirements gathering through design, implementation, testing, and quality review.

### Multiple Development Paradigms
- **Modern Development**: Full-stack web development with React, Vue, Angular, Node.js, Python, Java
- **API Design**: REST, GraphQL, gRPC specifications and implementations
- **Architecture Patterns**: Microservices, event-driven, serverless, monolithic
- **Legacy Migration**: Mainframe to modern platform migrations

### Quality & Best Practices
- **Standards Compliance**: IEEE 830, WCAG 2.1/2.2, industry best practices
- **Code Quality**: Refactoring patterns, code smell detection, clean code principles
- **Testing**: Unit, integration, E2E testing strategies and automation
- **Security**: Authentication, authorization, data protection, compliance

### Requirements Management
- **Elicitation**: Stakeholder interviews, workshops, surveys
- **Analysis**: Gap analysis, conflict detection, prioritization (MoSCoW, RICE, Kano)
- **Documentation**: BRD, SRS, user stories, acceptance criteria
- **Validation**: Completeness, clarity, consistency, testability reviews
- **Traceability**: Full requirements traceability matrices

### Architecture & Design
- **Solution Architecture**: System components, technology stacks, integration patterns
- **Backend Design**: APIs, microservices, databases, authentication, caching
- **Frontend Design**: UI/UX, design systems, component libraries, accessibility
- **Design Reviews**: Architecture validation, pattern assessment, quality evaluation

### Implementation Guidance
- **Backend Development**: Node.js, Python, Java, Go with frameworks (Express, Spring Boot, Django, FastAPI)
- **Frontend Development**: React, Vue, Angular with TypeScript, state management, testing
- **Database Design**: SQL, NoSQL, schema design, optimization
- **Security Implementation**: OAuth2, JWT, RBAC, encryption, secure coding

### Testing Strategies
- **Integration Testing**: API testing, database testing, microservices testing
- **Test Automation**: JUnit, TestNG, Jest, Playwright, Cypress
- **Test Data Management**: Test data generation, database seeding
- **CI/CD Integration**: Automated testing pipelines, quality gates

### Legacy System Modernization
**Type Mapping**: Comprehensive type mapping tables for converting legacy data types to modern Java types with proper precision handling.

**Dependency Analysis**: Automatically identifies and maps:
- Program call hierarchies
- File and dataset dependencies
- Copybook/include relationships
- Database table usage
- Shared utilities and procedures

**Migration Strategies**: Generates actionable implementation plans including:
- Java class structures (POJOs, services, repositories)
- Spring Boot integration patterns
- Database access with JDBC/JPA
- Bean Validation annotations
- Exception handling strategies
- Testing recommendations

**Critical Considerations**:
- **JCL COND Logic**: Properly handles the inverted logic of COND parameters
- **Numeric Precision**: Preserves decimal precision using BigDecimal
- **Array Indexing**: Converts 1-based arrays to 0-based Java arrays/collections
- **Character Encoding**: Handles EBCDIC to UTF-8 conversions
- **Date Handling**: Modernizes legacy date formats

## Project Structure

```
nghe-skills/
├── src/
│   └── nghe_cli/                # Nghệ CLI tool source code
│       ├── __init__.py          # Package initialization and main entry
│       ├── __main__.py          # CLI entry point script
│       ├── commands.py          # CLI commands (init, check, version)
│       ├── config.py            # Agent configurations and constants
│       ├── github.py            # GitHub API utilities and releases
│       ├── system_utils.py      # System utilities (git, file operations)
│       ├── templates.py         # Template download and extraction
│       └── ui.py                # Rich UI components and interactive menus
├── my-skills/
│   └── skills/                  # 39 specialized AI skills
│       ├── Cloud (6)            # AWS, Azure, GCP, Alibaba, IBM, Oracle
│       │   ├── aws-cloud/
│       │   │   ├── SKILL.md
│       │   │   └── references/
│       │   ├── azure-cloud/
│       │   └── ...
│       ├── Requirements (3)     # Gathering, review, architecture design
│       ├── Development (7)      # Backend, frontend, database, DevOps, refactoring
│       ├── Review (8)           # Code, security, quality, design reviews
│       ├── Testing (1)          # Integration testing
│       ├── Migration (10)       # App, DB, platform, mainframe, analyzers
│       │   ├── cobol-migration-analyzer/
│       │   │   ├── SKILL.md
│       │   │   ├── assets/
│       │   │   ├── references/
│       │   │   └── scripts/    # Python scripts for code analysis
│       │   ├── jcl-migration-analyzer/
│       │   └── ...
│       └── Specialized (4)      # Bug analysis, git, docs, KeyCloak
├── pyproject.toml               # Python package configuration
├── cspell.json                  # Spell checker configuration
├── LICENSE                      # MIT License
├── README.md                    # This file - complete documentation
└── rules.md                     # Agent Skills guidelines and best practices
```

### CLI Tool Architecture

The `nghe-cli` tool is built with:
- **Typer**: Modern CLI framework with type hints
- **Rich**: Beautiful terminal output with colors, panels, and progress bars
- **httpx**: HTTP client for GitHub API with SSL/TLS support
- **readchar**: Cross-platform keyboard input handling
- **platformdirs**: Standard directory locations
- **truststore**: Enhanced SSL certificate validation

### Skill Structure

Each skill directory contains:
- `SKILL.md` - Core skill documentation with capabilities and examples (<500 lines)
- `references/` - Detailed reference documentation loaded on demand
- `assets/` - Templates and resources (where applicable)
- `scripts/` - Helper scripts and utilities (migration analyzers only)

## Skill Categories Overview

| Category | Skills | Focus Area |
|----------|--------|------------|
| ☁️ **Cloud Platforms** | 6 | AWS, Azure, GCP, Alibaba Cloud, IBM Cloud, Oracle Cloud Infrastructure |
| 📋 **Requirements & Planning** | 3 | Requirements gathering, review, architecture design |
| 🛠️ **Design & Implementation** | 7 | Backend, frontend, database design, DevOps, refactoring |
| 🔍 **Review & Quality** | 8 | Backend/frontend code review, quality, security, architecture/design reviews, project planning |
| 🧪 **Testing** | 1 | Integration testing strategies |
| 🔄 **Migration & Legacy** | 10 | Application, database, platform, system migrations, IBM/Fujitsu mainframes, COBOL, JCL, PL/I, RPG analyzers |
| 🔧 **Specialized** | 4 | Bug analysis, Git commits, technical writing, KeyCloak administration |
| **Total** | **39** | **Complete SDLC + Cloud + Migration + Specialized coverage** |

## Use Cases

### Modern Application Development
1. **Requirements Phase**
   - Use **Requirements Gathering** to elicit and document requirements
   - Use **Requirement Review** to validate quality before design phase

2. **Architecture Phase**
   - Use **Software Solution Architecture** to design system architecture
   - Use **Architecture Design Review** to validate architecture decisions

3. **Design Phase**
   - Use **Backend Design** for API and service design
   - Use **Frontend UI/UX Design** for user interface design
   - Use **Backend/Frontend Design Review** to validate designs

4. **Implementation Phase**
   - Use **Backend Coding** for server-side implementation
   - Use **Frontend Coding** for client-side implementation
   - Use **Code Refactoring** to improve code quality

5. **Testing Phase**
   - Use **Integration Testing** for test strategy and automation

### Legacy System Modernization
1. **Assessment Phase**
   - Use migration analyzers to understand legacy systems
   - Extract business logic and dependencies
   - Generate migration reports

2. **Planning Phase**
   - Review migration strategies
   - Assess technical feasibility
   - Plan modernization roadmap

3. **Implementation Phase**
   - Follow generated migration patterns
   - Implement Java equivalents
   - Validate business logic preservation

4. **Testing Phase**
   - Use **Integration Testing** for validation
   - Compare legacy vs modern behavior
   - Ensure functional equivalence

## Technologies Supported

### Cloud Platforms
- **AWS**: EC2, S3, RDS, Lambda, ECS, EKS, CloudFormation, VPC, IAM
- **Azure**: Virtual Machines, Storage, SQL Database, App Service, Functions, AKS, ARM/Bicep
- **GCP**: Compute Engine, Cloud Storage, Cloud SQL, BigQuery, GKE, Cloud Functions, Cloud Run
- **Alibaba Cloud**: ECS, OSS, ApsaraDB, ACK, Function Compute
- **IBM Cloud**: VPC, IKS, Code Engine, Db2, Cloudant, Watson
- **Oracle Cloud**: Compute, Autonomous Database, OKE, VCN

### Languages & Frameworks
**Modern:**
- **Backend**: Node.js, Python, Java, Go, Express, Spring Boot, Django, FastAPI
- **Frontend**: React, Vue, Angular, TypeScript, JavaScript
- **Testing**: Jest, JUnit, TestNG, Playwright, Cypress

**Legacy:**
- **Mainframe**: IBM z/OS, Fujitsu BS2000/OSIV, COBOL, JCL, PL/I
- **AS/400**: RPG III/IV/ILE

### Architecture Patterns
- Microservices
- Event-Driven Architecture
- Serverless
- Monolithic (modular)
- Service-Oriented Architecture (SOA)

### Databases
- **SQL**: PostgreSQL, MySQL, Oracle, SQL Server
- **NoSQL**: MongoDB, Redis, Cassandra, DynamoDB

### API Styles
- REST
- GraphQL
- gRPC
- WebSocket

### Workflow Orchestration
- Spring Batch
- Apache Airflow
- Kubernetes Jobs & CronJobs
- AWS Step Functions
- Azure Logic Apps
- Shell scripts

### Identity & Access Management
- KeyCloak
- OAuth2 / OpenID Connect
- SAML
- LDAP / Active Directory
- JWT
- RBAC / ABAC

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Author

**Dau Quang Thanh**

## Version

**1.0** - January 2026

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests to improve the skills or add new capabilities.

### How to Contribute

1. **Fork the Repository**
   ```bash
   git clone https://github.com/yourusername/nghe-skills.git
   cd nghe-skills
   ```

2. **Set Up Development Environment**
   ```bash
   # Install in development mode
   pip install -e .
   
   # Or using uv
   uv pip install -e .
   ```

3. **Test Your Changes**
   ```bash
   # Test CLI commands
   nghe check
   nghe version
   
   # Test local source
   nghe init test-project --use-local-source --local-source-path .
   ```

4. **Create a Feature Branch**
   ```bash
   git checkout -b feature/new-skill
   git checkout -b fix/bug-description
   ```

5. **Make Your Changes**
   - Add new skills in `my-skills/skills/`
   - Update CLI code in `src/nghe_cli/`
   - Follow existing patterns and structure

6. **Commit Your Changes**
   ```bash
   git add .
   git commit -m "feat: add new skill for X"
   git commit -m "fix: resolve issue with Y"
   ```

7. **Push and Create Pull Request**
   ```bash
   git push origin feature/new-skill
   ```

### Contribution Guidelines

**For New Skills:**
- Follow the [Agent Skills specification](https://agentskills.io/specification)
- Keep SKILL.md under 500 lines
- Use progressive disclosure with reference files
- Include concrete examples and quick starts
- Add clear trigger conditions
- Follow the structure in `rules.md`

**For CLI Improvements:**
- Maintain Python 3.11+ compatibility
- Use type hints throughout
- Follow existing code patterns
- Update documentation
- Test with multiple AI assistants

**For Documentation:**
- Keep README.md accurate and up-to-date
- Update skill descriptions when adding/modifying skills
- Include usage examples
- Follow markdown best practices

### Code Quality Standards

- ✅ Comprehensive documentation with clear examples
- ✅ Structured templates and checklists
- ✅ Best practices and anti-patterns
- ✅ Real-world use cases and scenarios
- ✅ Industry standards compliance
- ✅ Actionable recommendations
- ✅ Proper metadata (name, description)
- ✅ Type hints in Python code
- ✅ Error handling and validation

## Skill Quality Standards

All skills in this repository follow these quality standards:
- ✅ Comprehensive documentation with clear examples
- ✅ Structured templates and checklists
- ✅ Best practices and anti-patterns
- ✅ Real-world use cases and scenarios
- ✅ Industry standards compliance
- ✅ Actionable recommendations
- ✅ Proper metadata (name, description)
- ✅ Under 500 lines per SKILL.md (Agent Skills spec)
- ✅ Progressive disclosure with reference files
- ✅ Concrete code examples and quick starts

## Technical Architecture

### CLI Tool Design

The Nghệ CLI is built with a modular architecture:

**commands.py** (661 lines): Core CLI commands
- `init`: Project initialization with multi-agent support
  - Interactive multi-select for agent selection
  - Download templates from GitHub releases
  - Merge into existing directories with `--force`
  - Upgrade mode with automatic timestamped backups
  - Local template support for development
  - Git repository initialization
  - Environment variable support
- `check`: Tool detection and validation
  - Checks for git availability
  - Detects installed AI assistant CLI tools
  - Validates VS Code installations
- `version`: Display version and system information
  - Shows CLI and template versions
  - Fetches latest release from GitHub
  - Displays system information
  
**config.py** (100 lines): Agent configurations
- Defines 16+ AI assistant configurations
- Maps agent keys to display names
- Specifies agent folder structures
- Identifies CLI tool requirements
- Provides installation URLs
- Configures GitHub repository details
  
**github.py** (229 lines): GitHub integration
- GitHub API client with SSL/TLS support
- Release fetching with rate limit handling
- Download progress tracking
- GitHub token authentication
- Rate limit error messages with troubleshooting
- Retry-After header parsing
  
**templates.py**: Template management
- Template archive download and extraction
- Multi-agent template support
- Merge capability for existing directories
- Local template mode for development
- ZIP file handling with progress tracking
- Skill folder copying and merging

**ui.py** (322 lines): Rich terminal UI
- `StepTracker`: Progress tracking with live updates
- `select_with_arrows`: Single-select menu with arrow keys
- `multi_select_with_arrows`: Multi-select with space/enter
- `show_banner`: Display ASCII art banner
- Rich console with colors, panels, tables
- Cross-platform keyboard handling
  
**system_utils.py**: System utilities
- Git repository detection with `.git` folder check
- Git initialization with error handling
- Tool availability checks via `shutil.which()`
- Script permission management (`chmod +x`)
- Cross-platform compatibility (Windows, macOS, Linux)

### Dependencies

```toml
[project.dependencies]
typer          # Modern CLI framework with type hints
rich           # Beautiful terminal formatting
httpx[socks]   # HTTP client with SSL support
platformdirs   # Standard directory locations
readchar       # Cross-platform keyboard input
truststore     # Enhanced SSL/TLS certificate validation
```

### Build System

- **Build Backend**: Hatchling (modern, fast, standards-compliant)
- **Python Version**: 3.11+
- **Entry Point**: `nghe` command registered via `project.scripts`

## Roadmap

### Current Status (v1.0)
✅ **39 skills** covering complete SDLC, cloud platforms, code quality, and migration
- Cloud Platforms: 6 skills (AWS, Azure, GCP, Alibaba, IBM, Oracle)
- Requirements & Planning: 3 skills
- Design & Implementation: 7 skills  
- Review & Quality: 8 skills
- Testing: 1 skill
- Migration & Legacy: 10 skills (includes mainframe systems)
- Specialized: 4 skills

### Upcoming Skills (Planned for v2.0)
- **Unit Testing** - Unit test strategies and TDD implementation
- **Performance Testing** - Load, stress, and performance testing
- **Security Testing** - Security testing and vulnerability assessment
- **Mobile App Development** - iOS and Android app development (Swift, Kotlin, React Native, Flutter)
- **Mobile App Design** - Mobile UI/UX design patterns
- **API Documentation** - OpenAPI, Swagger, API documentation generation
- **Cloud Architecture Optimization** - Cost optimization and performance tuning
- **Monitoring & Observability** - Application monitoring, logging, and alerting (Prometheus, Grafana, ELK)
- **Terraform Modules** - Reusable infrastructure modules
- **Kubernetes Operators** - Custom Kubernetes operators development

### Enhancements (Future)
- Interactive examples and templates
- Code generation capabilities with AI
- Integration with popular tools (Jira, Confluence, Figma, GitHub)
- More legacy language support (NATURAL, Assembler, CA-Gen)
- Real-time collaboration features
- AI-powered code analysis and suggestions
- Performance profiling and optimization
- Infrastructure cost analysis and recommendations

## Acknowledgments

Nghệ Skills is designed to:
- **Accelerate Development**: Speed up software development lifecycle
- **Improve Quality**: Ensure high-quality deliverables at each phase
- **Share Knowledge**: Democratize expert-level guidance across teams
- **Enable Migration**: Facilitate legacy system modernization
- **Promote Best Practices**: Encourage industry standards and patterns
- **Multi-Agent Support**: Work seamlessly across 16+ AI coding assistants

### Why "Nghệ"?

"Nghệ" (Vietnamese: /ŋe˦ˀ˥/) means "craft" or "skill" - representing the mastery and artistry in software development. This project embodies the philosophy that software development is both an engineering discipline and a craft requiring skill, practice, and continuous learning.

### Best Used With
- Comprehensive project planning
- Thorough testing and validation
- Subject matter expert review
- Proper quality assurance processes
- Team collaboration and communication
- Continuous integration and deployment
- Code review and pair programming

## Support

For issues, questions, or suggestions:
- � **Report Bugs**: [Open an issue](https://github.com/dauquangthanh/nghe-skills/issues) with detailed reproduction steps
- 💡 **Feature Requests**: [Start a discussion](https://github.com/dauquangthanh/nghe-skills/discussions) for new features or improvements
- 💬 **Questions**: Use [GitHub Discussions](https://github.com/dauquangthanh/nghe-skills/discussions) for questions
- 📧 **Contact**: Reach out via GitHub for other inquiries

### Troubleshooting

**GitHub Rate Limits:**
```bash
# Use GitHub token for higher rate limits (5000/hr vs 60/hr)
export GITHUB_TOKEN=ghp_yourtoken
nghe init my-project
```

**SSL/TLS Issues:**
```bash
# Skip SSL verification (not recommended for production)
nghe init my-project --skip-tls
```

**Debug Mode:**
```bash
# Enable verbose diagnostic output
nghe init my-project --debug
```

**Local Development:**
```bash
# Test with local source
export NGHE_USE_LOCAL_INSTALLATION=1
export NGHE_SKILL_PATH=/path/to/nghe-skills/my-skills
nghe init test-project
```

## Related Resources

### Standards & Guidelines
- IEEE 830 - Software Requirements Specification
- WCAG 2.1/2.2 - Web Content Accessibility Guidelines
- ISO/IEC 25010 - Systems and software Quality Requirements and Evaluation
- OpenAPI Specification - API documentation standard

### Tools & Frameworks
- **Requirements**: Jira, Confluence, Azure DevOps
- **Design**: Figma, Sketch, Adobe XD
- **Development**: VS Code, IntelliJ IDEA, WebStorm
- **Testing**: Jest, JUnit, Playwright, Postman
- **CI/CD**: Jenkins, GitHub Actions, GitLab CI

### Learning Resources
- Clean Code by Robert C. Martin
- Refactoring by Martin Fowler
- Design Patterns by Gang of Four
- Microservices Patterns by Chris Richardson
- RESTful Web API Design by Arnaud Lauret

---

**✨ Crafted by Dau Quang Thanh | 🎯 Focused on Quality | 🚀 Built for Scale**

*Making software development more efficient, one skill at a time.*
