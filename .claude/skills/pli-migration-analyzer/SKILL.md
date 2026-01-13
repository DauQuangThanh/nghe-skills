---
name: pli-migration-analyzer
description: Analyzes legacy PL/I (Programming Language One) programs to assist with migration to modern Java applications. Extracts business logic, data structures, procedure definitions, and file operations from PL/I code. Generates migration reports and creates Java implementation strategies. Use when working with mainframe migration, PL/I analysis, legacy system modernization, or when users mention PL/I to Java conversion, analyzing .pli/.PLI/.pl1 files, working with PL/I procedures, or planning Java service implementations from PL/I programs.
license: MIT
---

# PL/I Migration Analyzer

This skill helps analyze legacy PL/I (Programming Language One) programs to facilitate migration to modern Java applications. It provides comprehensive analysis of business logic, data structures, procedures, file operations, and generates actionable migration recommendations for PL/I programs typically found on IBM mainframe systems.

## Core Capabilities

### 1. PL/I Program Analysis

Analyze PL/I programs to extract:
- **Program structure**: PROCEDURE OPTIONS(MAIN), nested procedures
- **Data declarations**: DECLARE/DCL statements with detailed attributes
- **Data types**: FIXED DECIMAL, FIXED BINARY, CHARACTER, BIT, POINTER
- **File operations**: FILE declarations, READ, WRITE, OPEN, CLOSE
- **Business logic**: Procedure bodies, control structures, expressions
- **Exception handling**: ON conditions (ENDFILE, ERROR, OVERFLOW, etc.)
- **Built-in functions**: SUBSTR, INDEX, LENGTH, TRIM, MOD, etc.

### 2. Data Structure Mapping

Extract and convert PL/I data structures:
- **Simple declarations**: Scalar variables with attributes
- **Structure declarations**: Level-numbered structures (like COBOL)
- **Arrays**: Multi-dimensional arrays with bounds
- **Pointers**: POINTER data type usage
- **Based variables**: BASED storage and pointer relationships
- **Controlled storage**: CONTROLLED attribute handling
- **Picture specifications**: PIC clause for edited output

### 3. Procedure Analysis

Parse procedure definitions and calls:
- **Main procedures**: OPTIONS(MAIN) entry points
- **Internal procedures**: Nested procedure definitions
- **External procedures**: Separate compilation units
- **Parameters**: Parameter passing (BY REFERENCE, BY VALUE)
- **Return values**: RETURNS attribute and return statements
- **Recursion**: Recursive procedure patterns
- **Entry points**: Multiple ENTRY statements

### 4. Java Migration Strategy

Generate Java equivalents:
- **Class designs**: POJOs from PL/I structures
- **Service methods**: From procedures and functions
- **Data access patterns**: JDBC/JPA from file operations
- **Validation logic**: Bean Validation from PL/I checks
- **Exception handling**: Try-catch from ON conditions
- **Type safety**: Strong typing from PL/I declarations

### 5. Dependency Analysis

Map relationships:
- **Procedure calls**: CALL statements and invocations
- **File dependencies**: Files used by each program
- **Include files**: %INCLUDE directives
- **External references**: EXTERNAL attribute usage
- **Database access**: SQL embedded in PL/I
- **Shared modules**: Common procedures and utilities

## Step-by-Step Usage

### Step 1: Identify Target Programs

First, locate the PL/I programs and related artifacts:

```bash
# Find PL/I programs
find . -name "*.pli" -o -name "*.PLI" -o -name "*.pl1" -o -name "*.PL1"

# Find include files
find . -name "*.inc" -o -name "*.INC"

# Find copybooks or shared declarations
find . -name "*.cpy" -o -name "*.CPY"
```

### Step 2: Analyze Program Structure

Read the PL/I source and identify:

1. **Main procedure**: Entry point with OPTIONS(MAIN)
2. **Declarations**: All DCL statements (variables, files, structures)
3. **Internal procedures**: Nested procedure definitions
4. **Control flow**: DO, IF, SELECT, GO TO statements
5. **ON conditions**: Exception handlers
6. **File operations**: I/O statements

### Step 3: Map Data Types

Convert PL/I data types to Java:

| PL/I Type | Java Type | Notes |
|-----------|-----------|-------|
| `FIXED DECIMAL(n,m)` | `BigDecimal` | **Preserve precision!** |
| `FIXED BINARY(n)` | `int`, `long` | Binary integer |
| `FLOAT DECIMAL(n)` | `BigDecimal` | Avoid float in finance! |
| `CHARACTER(n)` | `String` | Fixed-length string |
| `CHARACTER(n) VARYING` | `String` | Variable-length |
| `BIT(1)` | `boolean` | Boolean flag |
| `BIT(n)` | `BitSet` | Bit string |
| `POINTER` | Reference | Object reference |
| Arrays `(n)` | `List<T>` or `T[]` | 1-based indexing |

### Step 4: Convert Business Logic

Transform PL/I statements to Java:

**PL/I Pattern**:
```pli
CALC_TOTAL: PROCEDURE(qty, price) RETURNS(FIXED DECIMAL(15,2));
    DCL qty FIXED DECIMAL(7,2);
    DCL price FIXED DECIMAL(9,2);
    DCL result FIXED DECIMAL(15,2);
    
    result = qty * price;
    IF result > 1000 THEN
        result = result * 0.90;
    RETURN(result);
END CALC_TOTAL;
```

**Java Equivalent**:
```java
public BigDecimal calcTotal(BigDecimal qty, BigDecimal price) {
    BigDecimal result = qty.multiply(price);
    if (result.compareTo(new BigDecimal("1000")) > 0) {
        result = result.multiply(new BigDecimal("0.90"));
    }
    return result;
}
```

### Step 5: Handle File Operations

Map PL/I file I/O to Java:

**PL/I File Operations**:
```pli
DCL infile FILE INPUT SEQUENTIAL;
DCL rec CHARACTER(80);

ON ENDFILE(infile) eof = '1'B;
OPEN FILE(infile);
DO WHILE(¬eof);
    READ FILE(infile) INTO(rec);
    CALL process_record(rec);
END;
CLOSE FILE(infile);
```

**Java Equivalent**:
```java
try (BufferedReader reader = Files.newBufferedReader(path)) {
    String line;
    while ((line = reader.readLine()) != null) {
        processRecord(line);
    }
} catch (IOException e) {
    handleError(e);
}
```

### Step 6: Convert Exception Handling

Transform ON conditions:

**PL/I ON Conditions**:
```pli
ON ERROR BEGIN;
    PUT SKIP LIST('Error occurred');
    GO TO cleanup;
END;

ON OVERFLOW PUT SKIP LIST('Numeric overflow');

result = numerator / denominator;
```

**Java Exception Handling**:
```java
try {
    result = numerator.divide(denominator);
} catch (ArithmeticException e) {
    System.out.println("Numeric overflow");
    throw e;
} catch (Exception e) {
    System.out.println("Error occurred");
    cleanup();
}
```

### Step 7: Handle Structures

Convert PL/I structures:

**PL/I Structure**:
```pli
DCL 1 EMPLOYEE,
      2 ID CHARACTER(10),
      2 NAME,
        3 FIRST CHARACTER(20),
        3 LAST CHARACTER(30),
      2 SALARY FIXED DECIMAL(9,2),
      2 HIRE_DATE CHARACTER(10);
```

**Java Class**:
```java
public class Employee {
    private String id;
    private EmployeeName name;
    private BigDecimal salary;
    private LocalDate hireDate;
    
    public static class EmployeeName {
        private String first;
        private String last;
    }
}
```

## Examples

### Example 1: Procedure with Parameters

**PL/I Procedure**:
```pli
VALIDATE: PROCEDURE(input) RETURNS(BIT(1));
    DCL input CHARACTER(20);
    DCL valid BIT(1);
    
    valid = '0'B;
    IF LENGTH(TRIM(input)) > 0 THEN
        valid = '1'B;
    RETURN(valid);
END VALIDATE;
```

**Generated Java**:
```java
public boolean validate(String input) {
    boolean valid = false;
    if (input.trim().length() > 0) {
        valid = true;
    }
    return valid;
}
```

### Example 2: Array Processing

**PL/I Array**:
```pli
DCL amounts(100) FIXED DECIMAL(11,2);
DCL total FIXED DECIMAL(15,2);
DCL i FIXED BINARY;

total = 0;
DO i = 1 TO 100;
    total = total + amounts(i);
END;
```

**Java Equivalent**:
```java
BigDecimal[] amounts = new BigDecimal[100];
BigDecimal total = BigDecimal.ZERO;

for (int i = 0; i < 100; i++) {
    total = total.add(amounts[i]);
}
```

### Example 3: SELECT Statement

**PL/I SELECT**:
```pli
SELECT(status);
    WHEN('A') CALL process_active();
    WHEN('I') CALL process_inactive();
    WHEN('P') CALL process_pending();
    OTHERWISE CALL process_unknown();
END;
```

**Java Switch**:
```java
switch (status) {
    case "A":
        processActive();
        break;
    case "I":
        processInactive();
        break;
    case "P":
        processPending();
        break;
    default:
        processUnknown();
        break;
}
```

## Common Patterns and Solutions

### Pattern 1: Iterative Loops

**PL/I Pattern**:
```pli
DO i = 1 TO n BY 2;
    sum = sum + items(i);
END;
```

**Java Solution**:
```java
for (int i = 0; i < n; i += 2) {
    sum = sum.add(items[i]);
}
```

### Pattern 2: String Manipulation

**PL/I Pattern**:
```pli
DCL result CHARACTER(50);
result = SUBSTR(source, 10, 20);
result = TRIM(result);
pos = INDEX(result, 'ABC');
```

**Java Solution**:
```java
String result = source.substring(9, 29); // 0-based
result = result.trim();
int pos = result.indexOf("ABC");
```

### Pattern 3: Controlled Storage

**PL/I Pattern**:
```pli
DCL work_area CONTROLLED;
ALLOCATE work_area;
/* use work_area */
FREE work_area;
```

**Java Solution**:
```java
WorkArea workArea = new WorkArea(); // Auto-managed
// use workArea
// Garbage collected automatically
```

## Migration Checklist

Use this checklist for each program migration:

- [ ] **Source Analysis**
  - [ ] Identify main procedure and entry points
  - [ ] Extract all data declarations
  - [ ] List all procedure calls
  - [ ] Document file operations
  - [ ] Identify external interfaces

- [ ] **Data Structure Mapping**
  - [ ] Convert all structures to Java classes
  - [ ] Handle FIXED DECIMAL with BigDecimal
  - [ ] Map arrays to collections
  - [ ] Handle pointers and based variables

- [ ] **Logic Translation**
  - [ ] Convert procedures to methods
  - [ ] Handle ON conditions as exceptions
  - [ ] Refactor GO TO statements
  - [ ] Translate SELECT to switch

- [ ] **File Operations**
  - [ ] Map file operations to I/O or database
  - [ ] Implement transaction boundaries
  - [ ] Handle ENDFILE and error conditions

- [ ] **Testing Strategy**
  - [ ] Create unit tests for procedures
  - [ ] Test exception handling paths
  - [ ] Validate decimal precision

- [ ] **Documentation**
  - [ ] Document business rules
  - [ ] Map procedures to services
  - [ ] Note any assumptions

## Useful Scripts Reference

This skill includes helper scripts in the `scripts/` directory:

- `scripts/extract-structure.py` - Extract program structure from PL/I source
- `scripts/generate-java-classes.py` - Generate Java POJOs from structures
- `scripts/analyze-dependencies.sh` - Map procedure dependencies
- `scripts/estimate-complexity.py` - Calculate migration complexity score

See [scripts/README.md](scripts/README.md) for detailed usage.

## Advanced Topics

For complex migration scenarios, refer to:
- [references/PERFORMANCE-PATTERNS.md](references/PERFORMANCE-PATTERNS.md) - Optimizing converted code
- [references/TRANSACTION-HANDLING.md](references/TRANSACTION-HANDLING.md) - Transaction conversion
- [references/MESSAGING-INTEGRATION.md](references/MESSAGING-INTEGRATION.md) - Integration patterns
- [references/TESTING-STRATEGY.md](references/TESTING-STRATEGY.md) - Comprehensive testing approach
- [references/PSEUDOCODE-COMMON-RULES.md](references/PSEUDOCODE-COMMON-RULES.md) - Common pseudocode syntax
- [references/PSEUDOCODE-PLI-RULES.md](references/PSEUDOCODE-PLI-RULES.md) - PL/I to pseudocode translation patterns

## Tips for Success

1. **Preserve Precision**: Always use BigDecimal for FIXED DECIMAL
2. **Never Use Float**: PL/I FLOAT DECIMAL must map to BigDecimal for financial data
3. **ON Conditions**: Map systematically to try-catch blocks
4. **GO TO Refactoring**: Restructure into proper control flow
5. **1-based Arrays**: Document indexing differences
6. **Test Thoroughly**: PL/I has subtle behaviors that need validation
7. **Pointer Usage**: Carefully analyze based variables
8. **Modular Design**: Convert procedures to well-defined services

## Troubleshooting

### Issue: GO TO Statements
**Solution**: Refactor into structured control flow using while loops and flags.

### Issue: FLOAT DECIMAL in Financial Code
**Solution**: Convert to BigDecimal and verify precision in all calculations.

### Issue: Complex ON Condition Blocks
**Solution**: Create exception hierarchy matching condition types.

### Issue: Based Variables and Pointers
**Solution**: Redesign using object references and proper data structures.

### Issue: CONTROLLED Storage
**Solution**: Use Java's automatic memory management, no explicit FREE needed.

## Output Format

When using this skill, provide analysis in this structure:

1. **Program Overview**: Purpose, type, complexity
2. **Dependencies**: Procedure calls, includes, files
3. **Data Structures**: Key data elements and mappings
4. **Business Logic Summary**: High-level algorithm
5. **Java Design**: Proposed class structure
6. **Migration Estimate**: Effort and risk
7. **Action Items**: Next steps

## Integration with Existing Tools

This skill works well with:
- **Source Analysis Tools**: Leverage mainframe analysis metadata
- **Structure Files**: Use extracted procedure definitions
- **Pseudocode Rules**: Follow standardized translation patterns
- **Version Control**: Track migration decisions
- **Modern IDEs**: Use refactoring features

## Support and Resources

For additional guidance:
- Review reference documents in `references/` directory for advanced patterns
- Check pseudocode rules for standardized documentation format
- Use helper scripts in `scripts/` directory for automation
- Consult complexity dashboard template in `assets/complexity-dashboard.html`
