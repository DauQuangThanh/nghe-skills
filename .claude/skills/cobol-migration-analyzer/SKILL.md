---
name: cobol-migration-analyzer
description: Analyzes legacy COBOL programs and JCL jobs to assist with migration to modern Java applications. Extracts business logic, identifies dependencies, generates migration reports, and creates Java implementation strategies. Use when working with mainframe migration, COBOL analysis, legacy system modernization, JCL workflows, or when users mention COBOL to Java conversion, analyzing .cbl/.CBL/.cob files, working with copybooks, or planning Java service implementations from COBOL programs.
license: MIT
---

# COBOL Migration Analyzer

This skill helps analyze legacy COBOL programs and JCL (Job Control Language) scripts to facilitate migration to modern Java applications. It provides comprehensive analysis of business logic, data structures, program dependencies, and generates actionable migration recommendations.

## Core Capabilities

### 1. COBOL Program Analysis

Analyze COBOL programs to extract:
- **Division structure**: IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE divisions
- **Working-Storage variables**: Data definitions with pictures, usage, and occurs clauses
- **File definitions**: FD entries, record layouts, access modes
- **Business logic**: Paragraph structure, PERFORM flows, decision points
- **CALL statements**: External program dependencies
- **SQL operations**: Embedded DB2 or other SQL statements
- **Error handling**: ON SIZE ERROR, INVALID KEY, exception handling patterns

### 2. JCL Job Analysis

Parse and understand JCL jobs:
- **Job steps**: Sequence of execution steps
- **Program invocations**: Which programs run in which order
- **Data dependencies**: DD statements, file inputs/outputs
- **Conditional logic**: COND parameters, IF/THEN/ELSE constructs
- **Resource requirements**: Memory, time, priority settings
- **Return code handling**: Error propagation between steps

### 3. Copybook Processing

Extract data structures from COBOL copybooks:
- **Record layouts**: Level numbers, field names, data types
- **Redefines**: Alternative data interpretations
- **Group items**: Hierarchical data structures
- **Occurs clauses**: Arrays and repeating groups
- **Picture clauses**: Data formats (numeric, alphanumeric, edited)

### 4. Java Migration Strategy

Generate Java equivalents:
- **Class designs**: POJOs from copybook structures
- **Service methods**: From COBOL paragraphs and sections
- **Data access patterns**: JDBC/JPA from file operations
- **Validation logic**: Bean Validation from COBOL checks
- **Exception handling**: Try-catch from COBOL error handling

### 5. Dependency Analysis

Map relationships:
- **Program-to-program calls**: Call hierarchies
- **Copybook usage**: Which programs use which copybooks
- **File dependencies**: Data flow between programs
- **Database tables**: Table access patterns
- **Shared resources**: Common utilities and subroutines

## Step-by-Step Usage

### Step 1: Identify Target Programs

First, locate the COBOL programs and related artifacts:

```bash
# Find COBOL programs
find . -name "*.cbl" -o -name "*.CBL" -o -name "*.cob"

# Find JCL jobs
find . -name "*.jcl" -o -name "*.JCL"

# Find copybooks
find . -name "*.cpy" -o -name "*.CPY" -o -name "*.cob"
```

### Step 2: Read Program Structure

Load the program's structural JSON if available:

```bash
# Check for structural metadata (if using external analysis tools)
cat PROGRAM_NAME.structure.json
```

The structure file typically contains:
- Division breakdown
- Variable definitions
- Paragraph/section listings
- CALL dependencies
- File operations

### Step 3: Analyze Business Logic

Read the COBOL source and identify:

1. **Entry points**: Main paragraphs that start processing
2. **Control flow**: PERFORM statements, GO TO usage
3. **Data transformations**: MOVE, COMPUTE, STRING operations
4. **Decision logic**: IF, EVALUATE statements
5. **Loop constructs**: PERFORM VARYING, PERFORM UNTIL
6. **I/O operations**: READ, WRITE, REWRITE, DELETE

### Step 4: Map Data Structures

For each copybook referenced:

1. Parse the record layout
2. Identify the hierarchy (level numbers)
3. Map COBOL data types to Java types:
   - `PIC 9(n)` → `int`, `long`, `BigInteger`
   - `PIC S9(n)V9(m)` → `BigDecimal`
   - `PIC X(n)` → `String`
   - `COMP-3` → `BigDecimal` (packed decimal)
   - `OCCURS n TIMES` → `List<T>` or `T[]`

### Step 5: Generate Java Code Structure

Create corresponding Java artifacts:

```java
// Example transformation from COBOL copybook to Java class
public class CustomerRecord {
    private String customerId;      // PIC X(10)
    private String customerName;    // PIC X(50)
    private BigDecimal balance;     // PIC S9(13)V99 COMP-3
    private List<OrderItem> orders; // OCCURS 10 TIMES
    
    // Getters, setters, validation
}
```

### Step 6: Document Migration Approach

For each COBOL program, create a migration plan:

1. **Complexity assessment**: Simple, Medium, Complex
2. **Estimated effort**: Story points or hours
3. **Risk factors**: Complex logic, external dependencies, performance requirements
4. **Implementation approach**: Direct translation, redesign, API integration
5. **Testing strategy**: Unit tests, integration tests, parallel run requirements

### Step 7: Handle Special Cases

Be aware of COBOL-specific patterns:

- **Computed GO TO**: Use strategy pattern or switch statement
- **ALTER statement**: Refactor to normal control flow
- **REDEFINES**: Use union types or multiple views
- **Level 88 conditions**: Convert to enums or constants
- **COPY REPLACING**: Handle text substitution in analysis

## Examples

### Example 1: Simple Data Structure Migration

**COBOL Copybook**:
```cobol
01  EMPLOYEE-RECORD.
    05  EMP-ID           PIC 9(6).
    05  EMP-NAME.
        10  FIRST-NAME   PIC X(20).
        10  LAST-NAME    PIC X(30).
    05  EMP-SALARY       PIC S9(7)V99 COMP-3.
    05  DEPT-CODE        PIC X(4).
```

**Generated Java**:
```java
public class EmployeeRecord {
    private int empId;
    private EmployeeName empName;
    private BigDecimal empSalary;
    private String deptCode;
    
    public static class EmployeeName {
        private String firstName;
        private String lastName;
    }
}
```

### Example 2: Business Logic Migration

**COBOL Paragraph**:
```cobol
CALCULATE-BONUS.
    IF EMP-YEARS-SERVICE > 10
        MULTIPLY EMP-SALARY BY 0.15 GIVING BONUS-AMOUNT
    ELSE
        IF EMP-YEARS-SERVICE > 5
            MULTIPLY EMP-SALARY BY 0.10 GIVING BONUS-AMOUNT
        ELSE
            MULTIPLY EMP-SALARY BY 0.05 GIVING BONUS-AMOUNT
        END-IF
    END-IF.
```

**Generated Java**:
```java
public BigDecimal calculateBonus(Employee emp) {
    BigDecimal rate;
    if (emp.getYearsOfService() > 10) {
        rate = new BigDecimal("0.15");
    } else if (emp.getYearsOfService() > 5) {
        rate = new BigDecimal("0.10");
    } else {
        rate = new BigDecimal("0.05");
    }
    return emp.getSalary().multiply(rate);
}
```

### Example 3: File Operation Migration

**COBOL File Handling**:
```cobol
SELECT CUSTOMER-FILE
    ASSIGN TO CUSTFILE
    ORGANIZATION IS INDEXED
    ACCESS MODE IS DYNAMIC
    RECORD KEY IS CUST-ID
    FILE STATUS IS WS-FILE-STATUS.

READ CUSTOMER-FILE
    INVALID KEY
        PERFORM ERROR-HANDLING
    NOT INVALID KEY
        PERFORM PROCESS-CUSTOMER
END-READ.
```

**Generated Java (with JPA)**:
```java
@Entity
@Table(name = "CUSTOMER")
public class Customer {
    @Id
    private String custId;
    // other fields...
}

// Repository
public interface CustomerRepository extends JpaRepository<Customer, String> {
    Optional<Customer> findByCustId(String custId);
}

// Service
public void processCustomer(String custId) {
    customerRepository.findByCustId(custId)
        .ifPresentOrElse(
            this::processCustomer,
            this::handleCustomerNotFound
        );
}
```

## Common Patterns and Solutions

### Pattern 1: Sequential File Processing

**COBOL Pattern**:
```cobol
OPEN INPUT INPUT-FILE.
PERFORM UNTIL END-OF-FILE
    READ INPUT-FILE
        AT END SET END-OF-FILE TO TRUE
        NOT AT END PERFORM PROCESS-RECORD
    END-READ
END-PERFORM.
CLOSE INPUT-FILE.
```

**Java Solution**:
```java
try (BufferedReader reader = Files.newBufferedReader(inputPath)) {
    String line;
    while ((line = reader.readLine()) != null) {
        processRecord(line);
    }
} catch (IOException e) {
    handleError(e);
}
```

### Pattern 2: Table Lookup with Binary Search

**COBOL Pattern**:
```cobol
SEARCH ALL RATE-TABLE
    AT END MOVE 'NOT-FOUND' TO ERROR-CODE
    WHEN RATE-CODE (IDX) = WS-SEARCH-CODE
        MOVE RATE-VALUE (IDX) TO WS-RESULT
END-SEARCH.
```

**Java Solution**:
```java
Optional<RateEntry> entry = rateTable.stream()
    .filter(r -> r.getCode().equals(searchCode))
    .findFirst();

String result = entry
    .map(RateEntry::getValue)
    .orElseThrow(() -> new NotFoundException("Rate not found"));
```

### Pattern 3: Date Arithmetic

**COBOL Pattern**:
```cobol
COMPUTE WS-JULIAN-DATE = 
    FUNCTION INTEGER-OF-DATE(WS-GREGORIAN-DATE).
ADD 30 TO WS-JULIAN-DATE.
COMPUTE WS-NEW-DATE = 
    FUNCTION DATE-OF-INTEGER(WS-JULIAN-DATE).
```

**Java Solution**:
```java
LocalDate date = LocalDate.parse(gregorianDate, formatter);
LocalDate newDate = date.plusDays(30);
```

## Migration Checklist

Use this checklist for each program migration:

- [ ] **Source Analysis**
  - [ ] Identify program type (batch, online, utility)
  - [ ] Extract business logic documentation
  - [ ] List all CALL dependencies
  - [ ] Document file/database operations
  - [ ] Identify external interfaces (MQ, CICS, etc.)

- [ ] **Data Structure Mapping**
  - [ ] Convert all copybooks to Java classes
  - [ ] Handle REDEFINES appropriately
  - [ ] Map OCCURS to collections
  - [ ] Define validation rules

- [ ] **Logic Translation**
  - [ ] Convert paragraphs to methods
  - [ ] Translate PERFORM to method calls
  - [ ] Handle GO TO statements (refactor)
  - [ ] Convert EVALUATE to switch or strategy pattern

- [ ] **I/O Operations**
  - [ ] Map file operations to appropriate I/O or database calls
  - [ ] Implement transaction boundaries
  - [ ] Handle error conditions

- [ ] **Testing Strategy**
  - [ ] Create unit tests for each method
  - [ ] Develop integration tests for workflows
  - [ ] Plan parallel run validation

- [ ] **Documentation**
  - [ ] Document business rules
  - [ ] Create architecture diagrams
  - [ ] Write migration notes for maintenance team

## Useful Scripts Reference

This skill includes helper scripts in the `scripts/` directory:

- `scripts/extract-structure.py` - Extract program structure from COBOL source
- `scripts/generate-java-classes.py` - Generate Java POJOs from copybooks
- `scripts/analyze-dependencies.sh` - Map program dependencies
- `scripts/estimate-complexity.py` - Calculate migration complexity score

See [scripts/README.md](scripts/README.md) for detailed usage.

## Advanced Topics

For complex migration scenarios, refer to:
- [references/PERFORMANCE-PATTERNS.md](references/PERFORMANCE-PATTERNS.md) - Optimizing converted code
- [references/TRANSACTION-HANDLING.md](references/TRANSACTION-HANDLING.md) - CICS transaction conversion
- [references/MESSAGING-INTEGRATION.md](references/MESSAGING-INTEGRATION.md) - MQ and middleware patterns
- [references/TESTING-STRATEGY.md](references/TESTING-STRATEGY.md) - Comprehensive testing approach
- [references/PSEUDOCODE-COMMON-RULES.md](references/PSEUDOCODE-COMMON-RULES.md) - Common pseudocode syntax and conventions
- [references/PSEUDOCODE-COBOL-RULES.md](references/PSEUDOCODE-COBOL-RULES.md) - COBOL to pseudocode translation patterns
- [references/PSEUDOCODE-JCL-RULES.md](references/PSEUDOCODE-JCL-RULES.md) - JCL to pseudocode translation patterns

## Tips for Success

1. **Start Small**: Begin with utility programs before tackling complex business logic
2. **Preserve Behavior**: Focus on functional equivalence first, optimization later
3. **Document Assumptions**: COBOL has implicit behaviors - make them explicit
4. **Test Thoroughly**: Use production data samples for validation
5. **Iterate**: Expect to refine Java designs as understanding deepens
6. **Engage SMEs**: Domain experts clarify ambiguous business rules
7. **Consider Performance**: Be mindful of batch processing volumes
8. **Plan for Data Migration**: Structure and data must migrate together

## Troubleshooting

### Issue: Complex Nested PERFORMs
**Solution**: Flatten the call hierarchy, create a clear method structure with meaningful names.

### Issue: Ambiguous Business Logic
**Solution**: Consult with business analysts, review test cases, check production logs.

### Issue: Performance Degradation
**Solution**: Profile Java code, optimize database access, consider batching, use proper indexing.

### Issue: Data Type Mismatches
**Solution**: Use BigDecimal for precision, validate conversions, test edge cases.

### Issue: Missing Documentation
**Solution**: Reverse engineer from code, interview SMEs, analyze production behavior.

## Output Format

When using this skill, provide analysis in this structure:

1. **Program Overview**: Purpose, type, complexity
2. **Dependencies**: Called programs, copybooks, files, databases
3. **Data Structures**: Key data elements and their mappings
4. **Business Logic Summary**: High-level algorithm description
5. **Java Design**: Proposed class structure and method signatures
6. **Migration Estimate**: Effort and risk assessment
7. **Action Items**: Specific next steps for implementation

## Integration with Existing Tools

This skill works well with:
- **AST Parsers**: Leverage existing `ast.json` files for deeper analysis
- **Structure Files**: Use `*.structure.json` for quick navigation
- **Markdown Documentation**: Reference existing `.md` files for program documentation
- **Pseudocode Rules**: Follow standardized translation patterns from `references/PSEUDOCODE-*.md`
- **Version Control**: Track migration decisions and code evolution
- **IDE Features**: Use code generation and refactoring tools

## Support and Resources

For additional guidance:
- Review reference documents in `references/` directory for advanced patterns
- Check pseudocode rules for standardized documentation format
- Use helper scripts in `scripts/` directory for automation
- Consult complexity dashboard template in `assets/complexity-dashboard.html`
