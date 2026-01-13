---
name: rpg-migration-analyzer
description: Analyzes legacy RPG (Report Program Generator) programs to assist with migration to modern Java applications. Extracts business logic from RPG III/IV/ILE, identifies data structures, file operations, and dependencies. Generates migration reports and creates Java implementation strategies. Use when working with AS/400 or IBM i system migration, RPG analysis, legacy system modernization, or when users mention RPG to Java conversion, analyzing .rpg/.RPG/.rpgle files, working with data specifications, or planning Java service implementations from RPG programs.
license: MIT
---

# RPG Migration Analyzer

This skill helps analyze legacy RPG (Report Program Generator) programs to facilitate migration to modern Java applications. It provides comprehensive analysis of business logic, data structures, file operations, and generates actionable migration recommendations for RPG III, RPG IV, and ILE RPG programs.

## Core Capabilities

### 1. RPG Program Analysis

Analyze RPG programs to extract:
- **Specification types**: H-spec (Header), F-spec (Files), D-spec (Data), C-spec (Calculations), P-spec (Procedures)
- **Data structures**: Data structure definitions with subfields and data types
- **File definitions**: File specifications, access methods, record formats
- **Business logic**: Calculation specifications, subroutines, procedures
- **Indicators**: Usage of *IN indicators and their meanings
- **Built-in functions**: RPG BIFs like %SUBST, %TRIM, %CHAR, %EOF
- **Error handling**: %ERROR, %STATUS usage patterns

### 2. Data Structure Mapping

Extract and convert RPG data structures:
- **D-specs**: Standalone fields and data structures
- **Data types**: Packed decimal (P), zoned decimal (S), character (A), indicators (N)
- **Arrays**: DIM keyword and array handling
- **Data structures**: DS definitions with nested subfields
- **External data structures**: EXTNAME keyword references
- **Data structure types**: LIKEDS, QUALIFIED, INZ keywords

### 3. File Operation Analysis

Parse file handling patterns:
- **File types**: Physical files (PF), logical files (LF), display files (DSPF)
- **Access methods**: Sequential (USAGE(*INPUT/*OUTPUT)), keyed (CHAIN, SETLL)
- **I/O operations**: READ, WRITE, UPDATE, DELETE, CHAIN, SETLL, READE
- **File status**: %EOF, %FOUND, %ERROR built-in functions
- **Record-level access**: Record format names and field access

### 4. Java Migration Strategy

Generate Java equivalents:
- **Class designs**: POJOs from data structures
- **Service methods**: From subroutines and procedures
- **Data access patterns**: JDBC/JPA from file operations
- **Validation logic**: Bean Validation from RPG checks
- **Exception handling**: Try-catch from %ERROR handling
- **Collections**: List/Map from RPG arrays and data structures

### 5. Dependency Analysis

Map relationships:
- **Program-to-program calls**: CALLB, CALLP operations
- **Service programs**: BNDDIR, BINDING references
- **File dependencies**: Files used by each program
- **Database tables**: Physical file to table mapping
- **Copy members**: /COPY directives and shared code

## Step-by-Step Usage

### Step 1: Identify Target Programs

First, locate the RPG programs and related artifacts:

```bash
# Find RPG programs
find . -name "*.rpg" -o -name "*.RPG" -o -name "*.rpgle" -o -name "*.RPGLE"

# Find data specifications
find . -name "*.dspf" -o -name "*.DSPF"

# Find physical/logical file definitions
find . -name "*.pf" -o -name "*.lf"
```

### Step 2: Analyze Program Structure

Read the RPG source and identify specification types:

1. **H-spec (Header)**: Control options, activation group, debug settings
2. **F-spec (Files)**: File declarations with USAGE, KEYED attributes
3. **D-spec (Data)**: Variable and data structure definitions
4. **C-spec (Calculations)**: Program logic, operations, indicators
5. **P-spec (Procedures)**: Procedure definitions and prototypes

### Step 3: Map Data Types

Convert RPG data types to Java:

| RPG Type | Java Type | Notes |
|----------|-----------|-------|
| `nP m` (packed) | `BigDecimal` | Preserve precision! |
| `nS m` (zoned) | `BigDecimal` | Decimal with sign |
| `A` (character) | `String` | Character data |
| `D` (date) | `LocalDate` | Date field |
| `N` (indicator) | `boolean` | True/False |
| `I` (integer) | `int` or `long` | Binary integer |
| `DIM(n)` | `List<T>` or `T[]` | Arrays |

### Step 4: Convert Business Logic

Transform RPG operations to Java:

**RPG Pattern**:
```rpg
C                   EVAL      Total = Qty * Price
C                   IF        Total > 1000
C                   EVAL      Discount = Total * 0.10
C                   EVAL      Total = Total - Discount
C                   ENDIF
```

**Java Equivalent**:
```java
BigDecimal total = qty.multiply(price);
if (total.compareTo(new BigDecimal("1000")) > 0) {
    BigDecimal discount = total.multiply(new BigDecimal("0.10"));
    total = total.subtract(discount);
}
```

### Step 5: Handle File Operations

Map RPG file I/O to Java data access:

**RPG File Operations**:
```rpg
F CUSTFILE   IF   E           K DISK
D custRec       E DS                  EXTNAME(CUSTFILE)

C     custId        CHAIN     CUSTFILE
C                   IF        %FOUND(CUSTFILE)
C                   EXSR      ProcessCustomer
C                   ENDIF
```

**Java with JPA**:
```java
@Entity
@Table(name = "CUSTFILE")
public class Customer {
    @Id
    private String custId;
    // other fields...
}

Optional<Customer> customer = customerRepository.findById(custId);
customer.ifPresent(this::processCustomer);
```

### Step 6: Convert Subroutines and Procedures

Transform RPG subroutines:

**RPG Subroutine**:
```rpg
C     CalcTotal     BEGSR
C                   EVAL      Total = 0
C                   FOR       i = 1 TO itemCount
C                   EVAL      Total = Total + itemAmt(i)
C                   ENDFOR
C                   ENDSR
C                   EXSR      CalcTotal
```

**Java Method**:
```java
private BigDecimal calcTotal() {
    BigDecimal total = BigDecimal.ZERO;
    for (int i = 0; i < itemAmt.length; i++) {
        total = total.add(itemAmt[i]);
    }
    return total;
}
```

### Step 7: Handle Indicators

Convert indicators to meaningful boolean variables:

**RPG with Indicators**:
```rpg
C                   SETON                                        LR
C                   IF        *INLR
C                   EVAL      *IN01 = *ON
C                   ENDIF
```

**Java with Named Booleans**:
```java
boolean lastRecord = true;
if (lastRecord) {
    errorCondition = true;
}
```

## Examples

### Example 1: Data Structure Migration

**RPG Data Structure**:
```rpg
D Employee        DS
D   EmpId                  1      6  0
D   EmpName               7     56
D   FirstName             7     26
D   LastName             27     56
D   Salary               57     63  2P
D   DeptCode             64     67
```

**Generated Java**:
```java
public class Employee {
    private int empId;
    private EmployeeName empName;
    private BigDecimal salary;
    private String deptCode;
    
    public static class EmployeeName {
        private String firstName;
        private String lastName;
    }
}
```

### Example 2: File Loop with Processing

**RPG File Processing**:
```rpg
C                   DOW       NOT %EOF(InputFile)
C                   READ      InputFile
C                   IF        NOT %EOF(InputFile)
C                   EXSR      ProcessRecord
C                   EVAL      RecCount = RecCount + 1
C                   ENDIF
C                   ENDDO
```

**Java Equivalent**:
```java
try (BufferedReader reader = Files.newBufferedReader(inputPath)) {
    String line;
    while ((line = reader.readLine()) != null) {
        processRecord(line);
        recCount++;
    }
} catch (IOException e) {
    handleError(e);
}
```

### Example 3: Keyed File Access

**RPG CHAIN Operation**:
```rpg
C     Key           CHAIN     MasterFile
C                   IF        %FOUND(MasterFile)
C                   EVAL      Result = MF_Amount
C                   ELSE
C                   EVAL      Result = 0
C                   ENDIF
```

**Java with Repository**:
```java
MasterRecord record = masterFileRepository.findByKey(key)
    .orElse(null);

BigDecimal result = (record != null) 
    ? record.getAmount() 
    : BigDecimal.ZERO;
```

## Common Patterns and Solutions

### Pattern 1: Array Processing

**RPG Pattern**:
```rpg
D Items           S             10P 2 DIM(100)
C                   FOR       idx = 1 TO %ELEM(Items)
C                   EVAL      Total = Total + Items(idx)
C                   ENDFOR
```

**Java Solution**:
```java
List<BigDecimal> items = new ArrayList<>();
BigDecimal total = items.stream()
    .reduce(BigDecimal.ZERO, BigDecimal::add);
```

### Pattern 2: Date Handling

**RPG Pattern**:
```rpg
D Today           S               D
C                   EVAL      Today = %DATE()
C                   EVAL      Future = Today + %DAYS(30)
```

**Java Solution**:
```java
LocalDate today = LocalDate.now();
LocalDate future = today.plusDays(30);
```

### Pattern 3: String Manipulation

**RPG Pattern**:
```rpg
C                   EVAL      Result = %SUBST(Name:1:10)
C                   EVAL      Result = %TRIM(Result)
C                   EVAL      Len = %LEN(Result)
```

**Java Solution**:
```java
String result = name.substring(0, Math.min(10, name.length()));
result = result.trim();
int len = result.length();
```

## Migration Checklist

Use this checklist for each program migration:

- [ ] **Source Analysis**
  - [ ] Identify program type (batch, interactive, service program)
  - [ ] Extract all specification types (H/F/D/C/P)
  - [ ] List all CALLB/CALLP dependencies
  - [ ] Document file operations and access methods
  - [ ] Identify external interfaces (message queues, data queues)

- [ ] **Data Structure Mapping**
  - [ ] Convert all D-specs to Java classes
  - [ ] Handle packed decimal with BigDecimal
  - [ ] Map arrays to collections
  - [ ] Define validation rules

- [ ] **Logic Translation**
  - [ ] Convert subroutines to methods
  - [ ] Translate procedures to Java methods
  - [ ] Replace indicators with named booleans
  - [ ] Handle %ERROR conditions

- [ ] **File Operations**
  - [ ] Map file operations to database calls
  - [ ] Implement transaction boundaries
  - [ ] Handle file status conditions

- [ ] **Testing Strategy**
  - [ ] Create unit tests for each method
  - [ ] Develop integration tests for workflows
  - [ ] Plan parallel run validation

- [ ] **Documentation**
  - [ ] Document business rules
  - [ ] Create architecture diagrams
  - [ ] Write migration notes

## Useful Scripts Reference

This skill includes helper scripts in the `scripts/` directory:

- `scripts/extract-structure.py` - Extract program structure from RPG source
- `scripts/generate-java-classes.py` - Generate Java POJOs from data structures
- `scripts/analyze-dependencies.sh` - Map program dependencies
- `scripts/estimate-complexity.py` - Calculate migration complexity score

See [scripts/README.md](scripts/README.md) for detailed usage.

## Advanced Topics

For complex migration scenarios, refer to:
- [references/PERFORMANCE-PATTERNS.md](references/PERFORMANCE-PATTERNS.md) - Optimizing converted code
- [references/TRANSACTION-HANDLING.md](references/TRANSACTION-HANDLING.md) - Commitment control conversion
- [references/MESSAGING-INTEGRATION.md](references/MESSAGING-INTEGRATION.md) - Data queue and message queue patterns
- [references/TESTING-STRATEGY.md](references/TESTING-STRATEGY.md) - Comprehensive testing approach
- [references/PSEUDOCODE-COMMON-RULES.md](references/PSEUDOCODE-COMMON-RULES.md) - Common pseudocode syntax
- [references/PSEUDOCODE-RPG-RULES.md](references/PSEUDOCODE-RPG-RULES.md) - RPG to pseudocode translation patterns

## Tips for Success

1. **Understand Indicators**: Map *IN indicators to meaningful boolean variables
2. **Preserve Precision**: Use BigDecimal for packed and zoned decimal fields
3. **File vs Database**: Most file operations map to database access
4. **Free vs Fixed Format**: Handle both RPG IV fixed and free-form syntax
5. **Service Programs**: Consider converting to Spring services or microservices
6. **Test with Real Data**: Use actual AS/400 data samples for validation
7. **Consider Performance**: Be mindful of record locking and batch processing
8. **Engagement**: Work with RPG developers to understand business context

## Troubleshooting

### Issue: Complex Indicator Logic
**Solution**: Create a mapping document for each indicator's meaning, then use named boolean variables.

### Issue: Packed Decimal Precision Loss
**Solution**: Always use BigDecimal, never double or float. Verify precision in unit tests.

### Issue: File Status Handling
**Solution**: Map %EOF, %FOUND, %ERROR to appropriate Java exceptions or Optional.

### Issue: External Descriptions (EXTNAME)
**Solution**: Extract physical file definitions, create corresponding Java entities.

### Issue: Display File Logic
**Solution**: Separate presentation from business logic, consider REST APIs + modern UI.

## Output Format

When using this skill, provide analysis in this structure:

1. **Program Overview**: Purpose, type, complexity
2. **Dependencies**: Called programs, service programs, files
3. **Data Structures**: Key data elements and their mappings
4. **Business Logic Summary**: High-level algorithm description
5. **Java Design**: Proposed class structure and method signatures
6. **Migration Estimate**: Effort and risk assessment
7. **Action Items**: Specific next steps

## Integration with Existing Tools

This skill works well with:
- **Source Analysis Tools**: Leverage metadata from AS/400 analysis tools
- **Structure Files**: Use extracted specifications for quick navigation
- **Pseudocode Rules**: Follow standardized translation patterns
- **Version Control**: Track migration decisions and code evolution
- **Modern IDEs**: Use code generation and refactoring features

## Support and Resources

For additional guidance:
- Review reference documents in `references/` directory for advanced patterns
- Check pseudocode rules for standardized documentation format
- Use helper scripts in `scripts/` directory for automation
- Consult complexity dashboard template in `assets/complexity-dashboard.html`
