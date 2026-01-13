# Legacy Mainframe Migration Skills

A collection of Claude AI skills for analyzing and migrating legacy mainframe systems (COBOL, JCL, PL/I, RPG) to modern Java applications and workflow orchestration platforms.

## Overview

This repository provides specialized AI skills that assist with mainframe modernization projects by analyzing legacy code, extracting business logic, mapping dependencies, and generating migration strategies to Java, Spring Batch, Apache Airflow, and other modern technologies.

## Available Skills

### 1. JCL Migration Analyzer
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

### 2. COBOL Migration Analyzer
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

### 3. PL/I Migration Analyzer
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

### 4. RPG Migration Analyzer
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

## Getting Started

### Installation

These skills are designed to be used with Claude AI assistants that support the Model Context Protocol (MCP) and skill loading.

1. Clone this repository:
```bash
git clone <repository-url>
cd nghe-skills
```

2. The skills are located in `.claude/skills/` and will be automatically loaded by compatible Claude environments.

### Using the Skills

Each skill is automatically activated when you:
- Mention the relevant technology (COBOL, JCL, PL/I, RPG)
- Work with files with specific extensions (.cbl, .jcl, .pli, .rpg, etc.)
- Request migration analysis or Java conversion
- Discuss mainframe modernization

Example prompts:
- "Analyze this JCL file and explain the job flow"
- "Convert this COBOL program to Java"
- "Extract the business logic from this PL/I procedure"
- "Generate a Spring Batch job from this JCL"
- "Map this RPG data structure to a Java POJO"

## Key Features

### Type Mapping
Each skill includes comprehensive type mapping tables for converting legacy data types to modern Java types with proper precision handling.

### Dependency Analysis
Automatically identifies and maps:
- Program call hierarchies
- File and dataset dependencies
- Copybook/include relationships
- Database table usage
- Shared utilities and procedures

### Migration Strategies
Generates actionable implementation plans including:
- Java class structures (POJOs, services, repositories)
- Spring Boot integration patterns
- Database access with JDBC/JPA
- Bean Validation annotations
- Exception handling strategies
- Testing recommendations

### Critical Considerations
- **JCL COND Logic**: Properly handles the inverted logic of COND parameters
- **Numeric Precision**: Preserves decimal precision using BigDecimal
- **Array Indexing**: Converts 1-based arrays to 0-based Java arrays/collections
- **Character Encoding**: Handles EBCDIC to UTF-8 conversions
- **Date Handling**: Modernizes legacy date formats

## Project Structure

```
nghe-skills/
├── .claude/
│   └── skills/
│       ├── jcl-migration-analyzer/
│       │   └── SKILL.md
│       ├── cobol-migration-analyzer/
│       │   └── SKILL.md
│       ├── pli-migration-analyzer/
│       │   └── SKILL.md
│       └── rpg-migration-analyzer/
│           └── SKILL.md
├── LICENSE
└── README.md
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Author

Dau Quang Thanh

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests to improve the skills or add new capabilities.

## Acknowledgments

These skills are designed to assist with mainframe migration projects and should be used in conjunction with:
- Thorough testing and validation
- Subject matter expert review
- Comprehensive migration planning
- Proper quality assurance processes

## Support

For issues, questions, or suggestions, please open an issue in this repository.
