---
name: jcl-migration-analyzer
description: Analyzes legacy JCL (Job Control Language) scripts to assist with migration to modern workflow orchestration and batch processing systems. Extracts job flows, step sequences, data dependencies, conditional logic, and program invocations. Generates migration reports and creates implementation strategies for Spring Batch, Apache Airflow, or shell scripts. Use when working with mainframe job migration, JCL analysis, batch workflow modernization, or when users mention JCL conversion, analyzing .jcl/.JCL files, working with job steps, procedures, or planning workflow orchestration from JCL jobs.
license: MIT
---

# JCL Migration Analyzer

This skill helps analyze legacy JCL (Job Control Language) scripts to facilitate migration to modern batch processing and workflow orchestration systems. It provides comprehensive analysis of job structures, step sequences, data dependencies, conditional logic, and generates actionable migration recommendations for converting mainframe batch jobs to modern platforms like Spring Batch, Apache Airflow, Kubernetes Jobs, or shell scripts.

## Core Capabilities

### 1. JCL Job Analysis

Analyze JCL jobs to extract:
- **Job structure**: JOB card parameters, job-level settings
- **Step sequences**: Execution order and step dependencies
- **Program invocations**: EXEC PGM= and EXEC PROC= statements
- **Conditional logic**: COND parameters, IF/THEN/ELSE/ENDIF constructs
- **Return code handling**: Success/failure criteria and propagation
- **Data sets**: DD statements with DSN, DISP, DCB parameters
- **Resource requirements**: REGION, TIME, CLASS specifications
- **Symbolic parameters**: Parameter substitution and overrides

### 2. Data Dependency Mapping

Extract data flow patterns:
- **Input datasets**: Files/tables read by each step
- **Output datasets**: Files/tables written by each step
- **Temporary datasets**: Work files created and deleted within job
- **GDG handling**: Generation Data Group references
- **Concatenation**: Multiple input files for a single DD
- **DISP parameters**: Dataset disposition (NEW, OLD, SHR, MOD)
- **Data passing**: How data flows between steps

### 3. Procedure Analysis

Parse JCL procedures:
- **PROC definitions**: Reusable procedure code
- **Symbolic parameters**: Variables passed to procedures
- **PROC overrides**: SET statements and parameter substitution
- **Nested procedures**: PROC calling other PROCs
- **INCLUDE statements**: External JCL includes
- **JCLLIB references**: Procedure library specifications

### 4. Workflow Migration Strategy

Generate modern equivalents:
- **Spring Batch**: Job definitions with steps and flow control
- **Apache Airflow**: DAG definitions with task dependencies
- **Kubernetes Jobs**: Job and CronJob manifests
- **Shell Scripts**: Bash scripts with error handling
- **AWS Step Functions**: State machine definitions
- **Azure Logic Apps**: Workflow definitions

### 5. Conditional Logic Translation

Map JCL conditionals to modern constructs:
- **COND parameter**: Inverted logic translation
- **IF/THEN/ELSE**: Standard conditional statements
- **Return code checks**: Exit status handling
- **Step bypassing**: Skip conditions and error handling
- **Restart logic**: Checkpoint and recovery patterns

## Step-by-Step Usage

### Step 1: Identify Target Jobs

First, locate the JCL jobs and related artifacts:

```bash
# Find JCL jobs
find . -name "*.jcl" -o -name "*.JCL"

# Find procedures
find . -name "*.proc" -o -name "*.PROC"

# Find include members
find . -name "*.jcl" -o -name "*.inc"
```

### Step 2: Analyze Job Structure

Read the JCL source and identify:

1. **JOB card**: Job name, accounting, CLASS, MSGCLASS
2. **Job-level parameters**: REGION, TIME, NOTIFY
3. **Step sequence**: All EXEC statements in order
4. **Program/Proc names**: What executes in each step
5. **Conditional logic**: COND, IF/THEN/ELSE
6. **DD statements**: Input/output datasets for each step

### Step 3: Map Step Dependencies

Create dependency graph:

- **Sequential flow**: Steps that must run in order
- **Conditional flow**: Steps that run based on return codes
- **Parallel opportunities**: Steps that could run concurrently
- **Data dependencies**: Steps that share datasets
- **Error paths**: What happens on failures

### Step 4: Translate Conditional Logic

**Critical**: COND logic is INVERTED!

**JCL COND (inverted)**:
```jcl
//STEP020  EXEC PGM=PROG2,COND=(0,NE)
```
Means: "Run if previous RC ≠ 0" → Run on ERROR!

**Modern equivalent (normal logic)**:
```bash
if [ $rc -ne 0 ]; then
    run_prog2
fi
```

**JCL IF/THEN (normal logic)**:
```jcl
//IF1      IF RC = 0 THEN
//STEP020  EXEC PGM=PROG2
//ENDIF
```

**Modern equivalent**:
```bash
if [ $rc -eq 0 ]; then
    run_prog2
fi
```

### Step 5: Convert Data Operations

Map DD statements to modern I/O:

**JCL DD Statements**:
```jcl
//INPUT    DD DSN=PROD.DATA.FILE,DISP=SHR
//OUTPUT   DD DSN=PROD.OUTPUT.FILE,DISP=(NEW,CATLG,DELETE)
//WORK     DD DSN=&&TEMP,DISP=(NEW,DELETE,DELETE)
```

**Modern Equivalent (Shell)**:
```bash
INPUT_FILE="/data/prod_data_file"
OUTPUT_FILE="/data/prod_output_file"
WORK_FILE="/tmp/work_${JOB_ID}"

# Process
process_data "$INPUT_FILE" "$OUTPUT_FILE"

# Cleanup
rm -f "$WORK_FILE"
```

### Step 6: Generate Workflow Definition

Create modern workflow representation:

**Spring Batch Job**:
```java
@Bean
public Job dataProcessingJob() {
    return jobBuilderFactory.get("dataProcessingJob")
        .start(step1())
        .next(step2())
        .on("FAILED").to(errorStep())
        .from(step2()).on("*").to(step3())
        .end()
        .build();
}
```

**Apache Airflow DAG**:
```python
with DAG('data_processing', schedule_interval='@daily') as dag:
    step1 = BashOperator(task_id='step1', bash_command='run_prog1.sh')
    step2 = BashOperator(task_id='step2', bash_command='run_prog2.sh')
    step3 = BashOperator(task_id='step3', bash_command='run_prog3.sh')
    
    step1 >> step2 >> step3
```

### Step 7: Handle Procedures

Convert JCL PROCs:

**JCL PROC**:
```jcl
//MYPROC   PROC MEMBER=,INFILE=
//STEP1    EXEC PGM=PROG1
//SYSIN    DD DSN=&MEMBER,DISP=SHR
//INPUT    DD DSN=&INFILE,DISP=SHR
//         PEND
//CALLPROC EXEC MYPROC,MEMBER=TEST.DATA,INFILE=PROD.FILE
```

**Shell Function**:
```bash
function myproc() {
    local member=$1
    local infile=$2
    
    prog1 --sysin="$member" --input="$infile"
}

myproc "test.data" "prod.file"
```

## Examples

### Example 1: Simple Sequential Job

**JCL Job**:
```jcl
//SIMPLEJOB JOB (ACCT),'SIMPLE JOB',CLASS=A
//STEP010  EXEC PGM=PROG1
//SYSOUT   DD SYSOUT=*
//INPUT    DD DSN=INPUT.FILE,DISP=SHR
//OUTPUT   DD DSN=OUTPUT.FILE,DISP=(NEW,CATLG,DELETE)
//STEP020  EXEC PGM=PROG2
//SYSOUT   DD SYSOUT=*
//INPUT    DD DSN=OUTPUT.FILE,DISP=SHR
```

**Shell Script**:
```bash
#!/bin/bash
set -e  # Exit on error

# Step 010
echo "Executing PROG1"
prog1 --input="input.file" --output="output.file" || exit 8

# Step 020
echo "Executing PROG2"
prog2 --input="output.file" || exit 8

echo "Job completed successfully"
```

### Example 2: Conditional Execution

**JCL with COND**:
```jcl
//STEP010  EXEC PGM=VALIDATE
//STEP020  EXEC PGM=PROCESS,COND=(0,NE)
//STEP030  EXEC PGM=CLEANUP
```

**Shell with Normal Logic**:
```bash
# Step 010
validate_data
rc=$?

# Step 020 (INVERTED: runs if rc != 0)
if [ $rc -ne 0 ]; then
    process_data
fi

# Step 030 (always runs)
cleanup
```

### Example 3: Complex Conditional Flow

**JCL IF/THEN/ELSE**:
```jcl
//STEP010  EXEC PGM=VALIDATE
//IF1      IF RC = 0 THEN
//STEP020  EXEC PGM=PROCESSOK
//ELSE
//STEP030  EXEC PGM=PROCESSERR
//ENDIF
//STEP040  EXEC PGM=FINAL
```

**Shell Script**:
```bash
# Step 010
validate_data
rc=$?

# Conditional steps
if [ $rc -eq 0 ]; then
    processok
else
    processerr
fi

# Final step
final_process
```

### Example 4: Procedure with Parameters

**JCL PROC**:
```jcl
//BACKUP   PROC ENV=TEST,SUFFIX=BAK
//STEP1    EXEC PGM=COPYDATA
//INPUT    DD DSN=&ENV..DATA,DISP=SHR
//OUTPUT   DD DSN=&ENV..DATA.&SUFFIX,DISP=(NEW,CATLG)
//         PEND
//RUNJOB   JOB ...
//DOBKUP   EXEC BACKUP,ENV=PROD,SUFFIX=20260113
```

**Shell Function**:
```bash
function backup_data() {
    local env=$1
    local suffix=$2
    
    input_file="${env}.data"
    output_file="${env}.data.${suffix}"
    
    copy_data "$input_file" "$output_file"
}

# Call procedure
backup_data "PROD" "20260113"
```

## Common Patterns and Solutions

### Pattern 1: Error Handling

**JCL Pattern**:
```jcl
//STEP010  EXEC PGM=MAINPROG
//STEP020  EXEC PGM=ERROR,COND=(0,EQ)
//STEP030  EXEC PGM=SUCCESS,COND=(0,NE)
```

**Shell Solution**:
```bash
mainprog
rc=$?

if [ $rc -ne 0 ]; then
    error_handler
else
    success_handler
fi
```

### Pattern 2: GDG Processing

**JCL Pattern**:
```jcl
//INPUT    DD DSN=PROD.DATA.GDG(0),DISP=SHR
//OUTPUT   DD DSN=PROD.DATA.GDG(+1),DISP=(NEW,CATLG)
```

**Modern Solution**:
```bash
# Get latest generation
INPUT_FILE=$(get_latest_generation "prod.data")

# Create new generation
OUTPUT_FILE=$(create_new_generation "prod.data")

process "$INPUT_FILE" "$OUTPUT_FILE"
```

### Pattern 3: Concatenated Input

**JCL Pattern**:
```jcl
//INPUT    DD DSN=FILE1,DISP=SHR
//         DD DSN=FILE2,DISP=SHR
//         DD DSN=FILE3,DISP=SHR
```

**Shell Solution**:
```bash
# Concatenate files
cat file1 file2 file3 | process_data
```

### Pattern 4: Restart Logic

**JCL Pattern**:
```jcl
//STEP010  EXEC PGM=PROG1,COND=(0,NE,STEP010)
//STEP020  EXEC PGM=PROG2,COND=(0,NE,STEP010)
```

**Modern Solution**:
```bash
# Checkpoint-based restart
if [ ! -f ".checkpoint_step010" ]; then
    prog1 && touch ".checkpoint_step010"
fi

if [ ! -f ".checkpoint_step020" ]; then
    prog2 && touch ".checkpoint_step020"
fi
```

## Migration Checklist

Use this checklist for each job migration:

- [ ] **Job Analysis**
  - [ ] Extract job structure and parameters
  - [ ] List all steps in execution order
  - [ ] Identify programs/procedures invoked
  - [ ] Document conditional logic (COND, IF/THEN)
  - [ ] Map return code handling

- [ ] **Data Flow Mapping**
  - [ ] Identify all input datasets
  - [ ] Identify all output datasets
  - [ ] Map temporary datasets
  - [ ] Document GDG usage
  - [ ] Track data dependencies between steps

- [ ] **Logic Translation**
  - [ ] Convert COND to normal logic (invert!)
  - [ ] Translate IF/THEN/ELSE
  - [ ] Handle error paths
  - [ ] Implement restart capability if needed

- [ ] **Target Platform**
  - [ ] Choose migration target (Spring Batch, Airflow, shell)
  - [ ] Define job/workflow structure
  - [ ] Implement step execution
  - [ ] Add monitoring and logging

- [ ] **Testing Strategy**
  - [ ] Test normal execution path
  - [ ] Test error conditions
  - [ ] Validate conditional branches
  - [ ] Test with production-like data

- [ ] **Documentation**
  - [ ] Document job purpose and schedule
  - [ ] Create dependency diagrams
  - [ ] Note any special requirements

## Useful Scripts Reference

This skill includes helper scripts in the `scripts/` directory:

- `scripts/extract-structure.py` - Extract job structure from JCL
- `scripts/analyze-dependencies.sh` - Map job and step dependencies
- `scripts/estimate-complexity.py` - Calculate migration complexity
- `scripts/generate-workflow.py` - Generate workflow definitions

See [scripts/README.md](scripts/README.md) for detailed usage.

## Advanced Topics

For complex migration scenarios, refer to:
- [references/PERFORMANCE-PATTERNS.md](references/PERFORMANCE-PATTERNS.md) - Batch optimization
- [references/TRANSACTION-HANDLING.md](references/TRANSACTION-HANDLING.md) - Job transaction patterns
- [references/MESSAGING-INTEGRATION.md](references/MESSAGING-INTEGRATION.md) - Job coordination
- [references/TESTING-STRATEGY.md](references/TESTING-STRATEGY.md) - Job testing approaches
- [references/PSEUDOCODE-COMMON-RULES.md](references/PSEUDOCODE-COMMON-RULES.md) - Common documentation syntax
- [references/PSEUDOCODE-JCL-RULES.md](references/PSEUDOCODE-JCL-RULES.md) - JCL to pseudocode translation patterns

## Tips for Success

1. **COND is Inverted**: Remember COND logic is backwards - step runs when condition is FALSE!
2. **Return Codes**: 0=success, 4=warning (OK), 8+=error in mainframe convention
3. **Data Dependencies**: Map dataset usage carefully to avoid race conditions
4. **Restart Capability**: Consider if job needs checkpoint/restart
5. **Scheduling**: Map job triggers (time-based, event-based, dependencies)
6. **Monitoring**: Add logging and alerting to modern workflows
7. **Resource Management**: Consider memory, CPU, and I/O requirements
8. **Test Thoroughly**: Use production-like scenarios and data volumes

## Troubleshooting

### Issue: Complex COND Logic
**Solution**: Draw truth table, remember inversion, convert to normal IF logic.

### Issue: Nested Procedures
**Solution**: Flatten or create function hierarchy, track parameter substitution.

### Issue: GDG Dependencies
**Solution**: Implement generation management system or use versioned files.

### Issue: Parallel Step Opportunities
**Solution**: Analyze data dependencies, use modern orchestration for parallelism.

### Issue: Restart Logic
**Solution**: Implement checkpointing mechanism appropriate to target platform.

## Output Format

When using this skill, provide analysis in this structure:

1. **Job Overview**: Purpose, schedule, criticality
2. **Step Sequence**: Execution flow with dependencies
3. **Data Flow**: Input/output datasets and intermediate files
4. **Conditional Logic**: Decision points and error handling
5. **Migration Target**: Recommended platform (Spring Batch, Airflow, etc.)
6. **Workflow Definition**: Generated code for target platform
7. **Migration Estimate**: Effort and risk assessment
8. **Action Items**: Specific next steps

## Integration with Existing Tools

This skill works well with:
- **Job Schedulers**: Integration with modern schedulers (Control-M, AutoSys, cron)
- **Workflow Platforms**: Spring Batch, Apache Airflow, Kubernetes
- **Monitoring Tools**: Integration with observability platforms
- **Pseudocode Rules**: Follow standardized documentation format
- **Version Control**: Track job definitions and changes
- **CI/CD**: Automate job deployment

## Support and Resources

For additional guidance:
- Review reference documents in `references/` directory for advanced patterns
- Check pseudocode rules for standardized documentation format
- Use helper scripts in `scripts/` directory for automation
- Consult complexity dashboard template in `assets/complexity-dashboard.html`
- Study flow diagrams for visualizing job dependencies

## Return Code Reference

Standard mainframe return codes:

| RC | Meaning | Action |
|----|---------|--------|
| 0 | Success | Continue normally |
| 4 | Warning | Continue (informational) |
| 8 | Error | May continue or stop based on COND |
| 12 | Severe Error | Typically stop processing |
| 16 | Fatal Error | Abort job |

Map these appropriately to your target platform's exit codes.
