# Scripts Usage Guide

This directory contains automation scripts for COBOL program analysis and migration. All scripts support both Unix/Linux/macOS and Windows platforms.

## Available Scripts

### 1. analyze-dependencies (.sh / .ps1)
Scans COBOL source files and generates a dependency graph in JSON format.

**Usage (Bash):**
```bash
./analyze-dependencies.sh [source_directory]
```

**Usage (PowerShell):**
```powershell
./analyze-dependencies.ps1 -SourceDir [source_directory]
```

**Output:** `dependencies.json` containing programs, dependencies, copybooks, and file references.

---

### 2. extract-structure.py
Extracts structural information from COBOL source files including divisions, variables, paragraphs, and dependencies.

**Usage:**
```bash
python3 extract-structure.py <source_file> [--output output.json]
```

**Examples:**
```bash
# Output to stdout
python3 extract-structure.py program.cbl

# Save to file
python3 extract-structure.py program.cbl -o structure.json
```

---

### 3. generate-java-classes.py
Generates Java POJO classes from COBOL copybook structures.

**Usage:**
```bash
python3 generate-java-classes.py <copybook_file> [options]
```

**Options:**
- `-p, --package`: Java package name (default: com.example.model)
- `-o, --output-dir`: Output directory for Java files

**Examples:**
```bash
# Print to stdout
python3 generate-java-classes.py CUSTOMER.cob

# Generate to specific package
python3 generate-java-classes.py CUSTOMER.cob -p com.myapp.model -o ./src/main/java
```

---

### 4. estimate-complexity.py
Calculates migration complexity score based on various factors.

**Usage:**
```bash
python3 estimate-complexity.py <cobol_file> [--output report.json]
```

**Factors considered:**
- Lines of code
- Number of paragraphs/sections
- External calls
- File operations
- SQL operations
- Control flow complexity

---

## Platform Support

### Python Scripts
- **Platform**: Cross-platform (Windows, Linux, macOS)
- **Requirements**: Python 3.7+
- **Dependencies**: None (uses only standard library)
- **Path Handling**: Uses `pathlib.Path` for automatic platform-specific path handling

### Bash Scripts (.sh)
- **Platform**: Linux, macOS, Unix-like systems
- **Requirements**: bash, grep, sed, find
- **Optional**: jq (for better JSON formatting)

### PowerShell Scripts (.ps1)
- **Platform**: Windows, Linux (with PowerShell Core), macOS (with PowerShell Core)
- **Requirements**: PowerShell 5.1+ (Windows) or PowerShell 7+ (cross-platform)
- **Features**: Native JSON handling, cross-platform path support

---

## Tips

1. **Choosing Between Bash and PowerShell:**
   - Use `.sh` on Linux/macOS (faster, fewer dependencies)
   - Use `.ps1` on Windows or for consistent cross-platform scripting

2. **Running PowerShell Scripts:**
   - Windows: `powershell.exe -ExecutionPolicy Bypass -File script.ps1`
   - Unix/macOS: `pwsh ./script.ps1`

3. **Python Environment:**
   - Ensure Python 3 is in your PATH
   - No external packages required - all scripts use standard library only

4. **Large Codebases:**
   - Scripts handle large files efficiently
   - Progress messages show which files are being processed
   - JSON output can be piped to other tools for further analysis

---

## Troubleshooting

**"Permission Denied" on .sh files:**
```bash
chmod +x *.sh
```

**"Execution Policy" error on Windows:**
```powershell
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
```

**Python not found:**
- Windows: Install from python.org or Microsoft Store
- Linux: `sudo apt install python3` (Debian/Ubuntu) or `sudo yum install python3` (RHEL/CentOS)
- macOS: `brew install python3`
