---
name: pli-migration-analyzer
description: Analyzes legacy PL/I (Programming Language One) programs to assist with migration to modern Java applications. Extracts business logic, data structures, procedure definitions, and file operations from PL/I code. Generates migration reports and creates Java implementation strategies. Use when working with mainframe migration, PL/I analysis, legacy system modernization, or when users mention PL/I to Java conversion, analyzing .pli/.PLI/.pl1 files, working with PL/I procedures, or planning Java service implementations from PL/I programs.
license: MIT
metadata:
  author: Dau Quang Thanh
  version: "1.0"
  category: development
---

# PL/I Migration Analyzer

Analyzes legacy PL/I programs for migration to Java, extracting business logic, data structures, procedures, and generating actionable migration strategies.

## Core Capabilities

### 1. Program Analysis
Extract program structure (PROCEDURE OPTIONS(MAIN), nested procedures), data declarations (DCL statements, FIXED DECIMAL, FIXED BINARY, CHARACTER, BIT, POINTER), file operations, business logic, exception handling (ON conditions), and built-in functions.

### 2. Data Structure Mapping
Convert structures (level-numbered), arrays, pointers, based variables, controlled storage, and picture specifications to Java classes/collections.

### 3. Java Migration
Generate POJOs, service methods, JDBC/JPA patterns, Bean Validation, and exception handling (try-catch from ON conditions).

### 4. Dependency Analysis
Map procedure calls, file dependencies, %INCLUDE directives, external references, and SQL operations.

## Quick Usage Guide

### Find Programs
```bash
find . -name "*.pli" -o -name "*.PLI" -o -name "*.pl1"
```

### Type Mapping

| PL/I Type | Java Type | Notes |
|-----------|-----------|-------|
| `FIXED DECIMAL(n,m)` | `BigDecimal` | **Preserve precision!** |
| `FIXED BINARY(n)` | `int`, `long` | Binary integer |
| `CHARACTER(n)` | `String` | Fixed/variable length |
| `BIT(1)` | `boolean` | Boolean flag |
| `POINTER` | Reference | Object reference |
| Arrays `(n)` | `List<T>` or `T[]` | 1-based → 0-based |

### Code Patterns

**Procedure → Method:**
```pli
CALC_TOTAL: PROCEDURE(qty, price) RETURNS(FIXED DECIMAL(15,2));
    result = qty * price;
    IF result > 1000 THEN result = result * 0.90;
    RETURN(result);
END CALC_TOTAL;
```
```java
public BigDecimal calcTotal(BigDecimal qty, BigDecimal price) {
    BigDecimal result = qty.multiply(price);
    return result.compareTo(new BigDecimal("1000")) > 0 
        ? result.multiply(new BigDecimal("0.90")) : result;
}
```

**File I/O → Java:**
```pli
ON ENDFILE(infile) eof = '1'B;
DO WHILE(¬eof);
    READ FILE(infile) INTO(rec);
    CALL process_record(rec);
END;
```
```java
try (BufferedReader reader = Files.newBufferedReader(path)) {
    reader.lines().forEach(this::processRecord);
}
```

**Structure → Class:**
```pli
DCL 1 EMPLOYEE,
      2 ID CHARACTER(10),
      2 SALARY FIXED DECIMAL(9,2);
```
```java
public class Employee {
    private String id;
    private BigDecimal salary;
}
```

## Key Patterns

**Loops:** `DO i = 1 TO n` → `for (int i = 0; i < n; i++)`
**Strings:** `SUBSTR(s,10,20)` → `s.substring(9,29)` (0-based!)
**SELECT:** PL/I `SELECT/WHEN` → Java `switch/case`
**ON Conditions:** → `try-catch` blocks

## Migration Checklist

- [ ] Identify entry points, declarations, procedures, file operations
- [ ] Convert structures to Java classes with BigDecimal for FIXED DECIMAL
- [ ] Convert procedures to methods, ON conditions to exceptions, refactor GO TO
- [ ] Map file operations to I/O/database with transaction boundaries
- [ ] Create unit tests, validate precision
- [ ] Document business rules and assumptions

## Critical Tips

1. **Always use BigDecimal** for FIXED DECIMAL - never float/double
2. **ON Conditions** map to try-catch systematically
3. **Arrays** are 1-based in PL/I, 0-based in Java
4. **GO TO** must be refactored to structured control flow
5. **Pointers/based variables** redesign with object references

## Output Structure

Provide: Program overview, dependencies, data structures, logic summary, Java design, migration estimate, action items.

## Integration

Works with source analysis tools, structure files, pseudocode rules (references/PSEUDOCODE-PLI-RULES.md), version control, and modern IDEs.
