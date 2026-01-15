# Software Development Skills: ✅ Nghệ - Skills

A comprehensive collection of Claude AI skills (and other compatible IDEs) for software development, covering the complete software development lifecycle from requirements gathering to deployment and migration.

## 🎉 Recent Update (January 2026)

**All 39 skills fully optimized and Agent Skills compliant!**

### Latest Improvements (January 15, 2026)
- ✅ **Specification Compliant** - Removed non-standard frontmatter fields (license, metadata)
- ✅ **Explicit Loading Conditions** - Added "when to load" guidance for all reference files
- ✅ **Concrete Examples** - Added Quick Start examples to all skills
- ✅ **Critical Tips** - Added best practice tips for development skills
- ✅ **Improved Overviews** - Replaced placeholders with actual content
- ✅ **100% compliant** - All skills under 500-line hard limit
- ✅ **90% size reduction** - Average skill reduced from ~700 to ~176 lines
- ✅ **328+ reference files** - Detailed content split into modular references
- ✅ **Better organized** - Core concepts in SKILL.md, details in references/

## Overview

This repository provides **39 specialized AI skills** organized into multiple categories:
- **Cloud Platforms** (6 skills) - AWS, Azure, GCP, Alibaba, IBM, Oracle Cloud
- **Migration & Legacy** (10 skills) - Application, database, platform, system migrations, COBOL, JCL, PL/I, RPG analyzers, mainframe systems
- **Development** (7 skills) - Backend, frontend, database, DevOps, refactoring, git commits
- **Review & Quality** (8 skills) - Code quality, security, architecture, design reviews
- **Requirements & Planning** (3 skills) - Gathering, review, planning
- **Testing** (1 skill) - Integration and E2E testing
- **Specialized** (4 skills) - Bug analysis, technical writing, KeyCloak administration

## Compliance with Agent Skills Specification

✅ **Frontmatter** - Only standard fields (`name`, `description`)  
✅ **Line Count** - All skills under 500 lines (100%)  
✅ **Progressive Disclosure** - Reference files loaded on demand  
✅ **Loading Conditions** - Explicit "when to load" for all references  
✅ **Structure** - Overview, Capabilities, Quick Start, Detailed Topics, Critical Tips  
✅ **Examples** - Concrete code examples in all skills

## Quick Stats

| Metric | Value |
|--------|-------|
| Total Skills | 39 |
| Average Lines per Skill | ~176 (down from ~700) |
| Reference Files | 328+ |
| Total Documentation | 3.3MB |
| Skills < 300 lines | 35/39 (90%) |
| Skills < 500 lines | 39/39 (100%) |

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

### 📋 Requirements & Architecture (3 skills)

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
- Architecture documentation (C4 diagrams, ADRs)

**Use Cases:**
- Planning new software systems
- Modernizing legacy applications
- Designing microservices architectures
- Evaluating technology choices
- Creating architecture documentation

### 🛠️ Design & Implementation (7 skills)

#### 10. Backend Design
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

#### 11. Backend Coding
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

#### 12. Frontend UI/UX Design
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

#### 13. Frontend Coding
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

#### 14. Database Design
Designs comprehensive database schemas including relational and NoSQL models, normalization, indexing strategies, relationship modeling, data types, constraints, and performance optimization.

**Capabilities:**
- Relational and NoSQL database design
- Schema design and normalization
- Indexing strategies and query optimization
- Entity-relationship diagrams (ERD)
- Data types and constraints
- Database best practices

**Use Cases:**
- Designing database schemas
- Creating data models
- Optimizing database queries
- Schema migrations
- Database architecture planning

#### 15. DevOps
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

#### 16. Code Refactoring
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

#### 17. Backend Code Review
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

#### 18. Frontend Code Review
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

#### 19. Code Quality Review
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

#### 20. Code Security Review
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

#### 21. Architecture Design Review
Conducts comprehensive architecture design reviews including system design validation, architecture pattern assessment, quality attributes evaluation, technology stack review, and scalability analysis.

**Capabilities:**
- Architecture assessment and validation
- Quality attributes review (scalability, performance, security)
- Technology stack evaluation
- Design documentation review (C4 diagrams, ADRs)
- Risk identification and recommendations
- Alternative approach suggestions

**Use Cases:**
- Reviewing software architecture designs
- Validating architecture decisions
- Assessing system scalability
- Evaluating technology choices
- Architecture quality gates

#### 22. Backend Design Review
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

#### 23. Frontend Design Review
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

#### 24. Project Planning
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

#### 25. Integration Testing
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

#### 26. Application Migration
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

#### 27. Database Migration
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

#### 28. Platform Migration
Guides infrastructure and platform migration including cloud-to-cloud migration (AWS to GCP, Azure to AWS), Kubernetes cluster migration, CI/CD platform changes, monitoring stack migration, and network infrastructure transformation.

**Capabilities:**\n- Cloud-to-cloud migration\n- Kubernetes cluster migration\n- CI/CD platform changes\n- Infrastructure as code migration\n- Multi-cloud strategies\n- Service mesh migration\n\n**Use Cases:**\n- Changing cloud providers\n- Migrating Kubernetes clusters\n- Infrastructure platform changes\n- CI/CD tool migration\n- Multi-cloud implementations\n\n#### 29. System Migration\nGuides operating system and hardware platform migrations including Linux distribution changes, Windows to Linux migration, mainframe to x86 modernization, data center migrations, and system consolidation.\n\n**Capabilities:**\n- OS migration and upgrades\n- Hardware platform changes\n- Mainframe to x86 modernization\n- Virtual machine migrations (P2V, V2V)\n- System consolidation\n- Configuration transfer\n\n**Use Cases:**\n- Operating system migrations\n- Hardware platform changes\n- Mainframe modernization\n- Data center migrations\n- System consolidation projects\n\n#### 30. IBM Mainframe\nProvides comprehensive IBM Mainframe administration, development, and modernization guidance including z/OS operations, JCL scripting, COBOL/PL/I programming, CICS/IMS configuration, DB2 administration, and mainframe-to-cloud migration strategies.\n\n**Capabilities:**\n- z/OS system administration\n- JCL scripting and batch processing\n- COBOL/PL/I development\n- CICS/IMS transaction processing\n- DB2 database administration\n- Mainframe modernization strategies\n\n**Use Cases:**\n- Mainframe system administration\n- Batch job development and maintenance\n- COBOL/PL/I programming\n- Transaction processing systems\n- Mainframe to cloud migration\n\n#### 31. Fujitsu Mainframe\nAnalyzes and assists with Fujitsu mainframe systems including FACOM, PRIMERGY, BS2000/OSD, OSIV/MSP, OSIV/XSP, NetCOBOL, PowerCOBOL, and Fujitsu JCL.\n\n**Capabilities:**\n- Fujitsu mainframe system analysis\n- NetCOBOL and PowerCOBOL programming\n- Fujitsu JCL scripting\n- BS2000/OSD and OSIV operations\n- Migration to modern platforms\n- SYMFOWARE database integration\n\n**Use Cases:**\n- Fujitsu mainframe modernization\n- NetCOBOL/PowerCOBOL analysis\n- Fujitsu JCL conversion\n- FACOM system migration\n- BS2000 application modernization\n\n#### 32. JCL Migration Analyzer
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

#### 33. COBOL Migration Analyzer
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

#### 34. PL/I Migration Analyzer
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

#### 35. RPG Migration Analyzer
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

#### 36. Bug Analysis
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

#### 37. Git Commit
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

#### 38. Technical Writing
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

#### 39. KeyCloak Administration
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

### Installation

These skills are designed to be used with Claude AI assistants that support the Model Context Protocol (MCP) and skill loading.

1. Clone this repository:
```bash
git clone https://github.com/yourusername/nghe-skills.git
cd nghe-skills
```

2. The skills are located in `.claude/skills/` and will be automatically loaded by compatible Claude environments.

### Using the Skills

Each skill is automatically activated when you:
- Mention relevant tasks or technologies
- Request specific development activities
- Work with certain file types
- Ask about specific methodologies or patterns

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
├── .claude/skills/              # 39 specialized AI skills
│   ├── Cloud (6)                # AWS, Azure, GCP, Alibaba, IBM, Oracle
│   ├── Requirements (3)         # Gathering, review, architecture design
│   ├── Development (7)          # Backend, frontend, database, DevOps, refactoring
│   ├── Review (8)               # Code, security, quality, design reviews
│   ├── Testing (1)              # Integration testing
│   ├── Migration (10)           # App, DB, platform, mainframe, analyzers
│   └── Specialized (4)          # Bug analysis, git, docs, KeyCloak
├── .gitignore
├── LICENSE
├── README.md                    # Complete documentation
└── rules.md                     # Agent Skills guidelines
```

### Skill Categories in .claude/skills/

**Cloud Platforms (6):** `alibaba-cloud`, `aws-cloud`, `azure-cloud`, `google-cloud`, `ibm-cloud`, `oracle-cloud`

**Requirements & Planning (3):** `architecture-design`, `requirement-review`, `requirements-gathering`

**Development (7):** `backend-coding`, `backend-design`, `code-refactoring`, `database-design`, `devops`, `frontend-coding`, `frontend-ui-ux-design`

**Review & Quality (8):** `architecture-design-review`, `backend-code-review`, `backend-design-review`, `code-quality-review`, `code-security-review`, `frontend-code-review`, `frontend-design-review`, `project-planning`

**Testing (1):** `integration-testing`

**Migration & Legacy (10):** `application-migration`, `cobol-migration-analyzer`, `database-migration`, `fujitsu-mainframe`, `ibm-mainframe`, `jcl-migration-analyzer`, `platform-migration`, `pli-migration-analyzer`, `rpg-migration-analyzer`, `system-migration`

**Specialized (4):** `bug-analysis`, `git-commit`, `keycloak-administration`, `technical-writing`

Each skill directory contains:
- `SKILL.md` - Core skill documentation with capabilities and examples
- `references/` - Detailed reference documentation loaded on demand
- `assets/` - Templates and resources (where applicable)
- `scripts/` - Helper scripts and utilities (where applicable)

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
1. Fork the repository
2. Create a feature branch (`git checkout -b feature/new-skill`)
3. Commit your changes (`git commit -m 'Add new skill'`)
4. Push to the branch (`git push origin feature/new-skill`)
5. Open a Pull Request

### Contribution Guidelines
- Follow the existing skill structure and format
- Include comprehensive documentation with examples
- Add usage examples and best practices
- Ensure all required metadata is present
- Test the skill thoroughly before submitting

## Skill Quality Standards

All skills in this repository follow these quality standards:
- ✅ Comprehensive documentation with clear examples
- ✅ Structured templates and checklists
- ✅ Best practices and anti-patterns
- ✅ Real-world use cases and scenarios
- ✅ Industry standards compliance
- ✅ Actionable recommendations
- ✅ Proper metadata (name, description, author, version, category, license)

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

These skills are designed to:
- **Accelerate Development**: Speed up software development lifecycle
- **Improve Quality**: Ensure high-quality deliverables at each phase
- **Share Knowledge**: Democratize expert-level guidance
- **Enable Migration**: Facilitate legacy system modernization
- **Promote Best Practices**: Encourage industry standards and patterns

### Best Used With
- Thorough testing and validation
- Subject matter expert review
- Comprehensive project planning
- Proper quality assurance processes
- Team collaboration and communication

## Support

For issues, questions, or suggestions:
- 📧 Open an issue in this repository
- 💬 Start a discussion for questions
- 🐛 Report bugs with detailed reproduction steps
- 💡 Suggest new features or improvements

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
