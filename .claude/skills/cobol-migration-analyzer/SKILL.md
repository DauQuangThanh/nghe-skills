---
name: cobol-migration-analyzer
description: Analyzes legacy COBOL programs and JCL jobs to assist with migration to modern Java applications. Extracts business logic, identifies dependencies, generates migration reports, and creates Java implementation strategies. Use when working with mainframe migration, COBOL analysis, legacy system modernization, JCL workflows, or when users mention COBOL to Java conversion, analyzing .cbl/.CBL/.cob files, working with copybooks, or planning Java service implementations from COBOL programs.
---

# COBOL Migration Analyzer

Analyzes legacy COBOL programs and JCL scripts for migration to Java, extracting business logic, data structures, dependencies, and generating actionable migration strategies.

## Core Capabilities

### 1. Program Analysis
Extract division structure (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE), Working-Storage variables, file definitions (FD), business logic (paragraphs, PERFORM), CALL statements, embedded SQL, and error handling patterns.

### 2. JCL Job Analysis
Parse job steps, program invocations, data dependencies (DD statements), conditional logic (COND, IF/THEN/ELSE), return codes, and resource requirements.

### 3. Copybook Processing
Extract record layouts (level numbers), REDEFINES, group items, OCCURS clauses, and picture clauses to Java structures.

### 4. Java Migration
Generate POJOs from copybooks, service methods from paragraphs, JDBC/JPA patterns, Bean Validation, and exception handling.

### 5. Dependency Analysis
Map CALL hierarchies, copybook usage, file dependencies, database tables, and shared utilities.

## Quick Usage Guide

### Find Programs
```bash
find . -name "*.cbl" -o -name "*.CBL" -o -name "*.cob"
find . -name "*.jcl" -o -name "*.JCL"
find . -name "*.cpy" -o -name "*.CPY"
```

### Type Mapping
| COBOL Picture | Java Type | Notes |
|---------------|-----------|-------|
| `PIC 9(n)` | `int`, `long`, `BigInteger` | Numeric |
| `PIC S9(n)V9(m)` | `BigDecimal` | Signed decimal |
| `PIC X(n)` | `String` | Alphanumeric |
| `COMP-3` | `BigDecimal` | Packed decimal |
| `OCCURS n` | `List<T>` or `T[]` | Arrays |

### Code Patterns

**Copybook → Class:**
```cobol
01  EMPLOYEE-RECORD.
    05  EMP-ID        PIC 9(6).
    05  EMP-SALARY    PIC S9(7)V99 COMP-3.
```
```java
public class EmployeeRecord {
    private int empId;
    private BigDecimal empSalary;
}
```

**Logic → Method:**
```cobol
CALCULATE-BONUS.
    IF EMP-YEARS-SERVICE > 10
        MULTIPLY EMP-SALARY BY 0.15 GIVING BONUS
    ELSE IF EMP-YEARS-SERVICE > 5
        MULTIPLY EMP-SALARY BY 0.10 GIVING BONUS
    ELSE
        MULTIPLY EMP-SALARY BY 0.05 GIVING BONUS.
```
```java
public BigDecimal calculateBonus(Employee emp) {
    BigDecimal rate = emp.getYearsOfService() > 10 ? new BigDecimal("0.15")
        : emp.getYearsOfService() > 5 ? new BigDecimal("0.10")
        : new BigDecimal("0.05");
    return emp.getSalary().multiply(rate);
}
```

**File I/O → JPA:**
```cobol
READ CUSTOMER-FILE
    INVALID KEY PERFORM ERROR-HANDLING
    NOT INVALID KEY PERFORM PROCESS-CUSTOMER.
```
```java
customerRepository.findByCustId(custId)
    .ifPresentOrElse(this::processCustomer, this::handleCustomerNotFound);
```

## Key Patterns

**File Processing:** `READ...AT END` → `try (BufferedReader...) reader.readLine()`
**Table Lookup:** `SEARCH ALL` → `stream().filter().findFirst()`
**Date Arithmetic:** `FUNCTION INTEGER-OF-DATE` → `LocalDate.parse().plusDays()`
**Level 88:** condition names → `enum` or constants

## Migration Checklist

- [ ] Identify program type, extract divisions, list CALL dependencies, document file/DB operations
- [ ] Convert copybooks to Java classes, handle REDEFINES, map OCCURS to collections
- [ ] Convert paragraphs to methods, PERFORM to method calls, refactor GO TO
- [ ] Map file operations to I/O/database with transaction boundaries, handle errors
- [ ] Create unit/integration tests, plan parallel validation
- [ ] Document business rules and assumptions

## Critical Tips

1. **Computed GO TO** → strategy pattern or switch
2. **ALTER statement** → refactor to normal control flow
3. **REDEFINES** → union types or multiple views
4. **Level 88** → enums or constants
5. **Always use BigDecimal** for COMP-3 and numeric with decimals
6. **Test with production data** samples

## Output Structure

Provide: Program overview, dependencies, data structures, logic summary, Java design, migration estimate, action items.

## Integration

Works with AST parsers, structure files, pseudocode rules (references/pseudocode-cobol-rules.md, pseudocode-jcl-rules.md), version control, modern IDEs, and CI/CD pipelines.
