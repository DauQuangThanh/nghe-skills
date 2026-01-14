---
name: database-migration
description: Guides database migration projects including engine changes (MySQL to PostgreSQL, Oracle to PostgreSQL, SQL Server to PostgreSQL), version upgrades, cloud migrations (on-premise to RDS/Cloud SQL/Azure Database), schema migrations, zero-downtime migrations, replication setup, and data migration strategies. Covers homogeneous and heterogeneous migrations, ETL processes, cutover procedures, and rollback plans. Use when migrating databases, changing database engines, upgrading database versions, moving databases to cloud, or when users mention "database migration", "DB migration", "PostgreSQL migration", "MySQL to Postgres", "Oracle migration", "database upgrade", or "cloud database migration".
---

# Database Migration

## Overview

This skill provides comprehensive guidance for migrating databases between engines, versions, platforms, and architectures. Covers both schema and data migration with strategies for minimizing downtime and ensuring data integrity.

## Migration Types

### 1. Homogeneous Database Migration

**Definition:** Same database engine, different version or platform

**Examples:**
- PostgreSQL 11 → PostgreSQL 15
- MySQL 5.7 → MySQL 8.0
- On-premise PostgreSQL → AWS RDS PostgreSQL
- Self-managed MySQL → Cloud SQL MySQL

**Migration Strategy:**

```markdown
Approach 1: Dump and Restore
Pros: Simple, reliable, clean database
Cons: Downtime required

Steps:
1. Take backup using native tools
2. Stop application writes
3. Final incremental backup
4. Restore to target database
5. Verify data integrity
6. Update application connection
7. Resume operations

Approach 2: Replication
Pros: Minimal downtime, gradual cutover
Cons: More complex setup

Steps:
1. Set up replication (primary → replica)
2. Monitor replication lag
3. When synchronized, plan cutover
4. Stop writes briefly
5. Promote replica to primary
6. Update application connections
7. Resume operations
```

### 2. Heterogeneous Database Migration

**Definition:** Different database engines

**Common Migrations:**

```markdown
Popular Paths:
- Oracle → PostgreSQL
- MySQL → PostgreSQL
- SQL Server → PostgreSQL
- MongoDB → PostgreSQL
- Oracle → MySQL
- SQL Server → MySQL

Reasons:
- Cost reduction (licensing)
- Open source preference
- Cloud-native features
- Better performance
- Vendor lock-in avoidance
```

**Heterogeneous Migration Challenges:**

```markdown
Schema Differences:
- Data types (Oracle NUMBER → PostgreSQL NUMERIC)
- Stored procedures (PL/SQL → PL/pgSQL)
- Triggers and functions
- Sequences and auto-increment
- Index types
- Constraints and defaults

SQL Dialect Differences:
- Syntax variations
- Function names (NVL vs COALESCE)
- Date/time handling
- String concatenation (|| vs CONCAT)
- LIMIT vs ROWNUM vs TOP

Feature Gaps:
- Packages (Oracle) → Schemas (PostgreSQL)
- Synonyms → Views
- Materialized views
- Partitioning approaches
- Full-text search
```

## Migration Phases

### Phase 1: Assessment and Planning

```markdown
1. Database Inventory
   - Database size (tables, indexes, total GB)
   - Number of objects (tables, views, procedures, functions)
   - Dependencies (foreign keys, triggers)
   - Integration points
   - Peak usage patterns

2. Compatibility Analysis
   - Identify incompatible features
   - Map data types
   - Review stored procedures
   - Check SQL queries
   - Validate application framework

3. Performance Baseline
   - Query response times
   - Transaction throughput
   - Concurrent connections
   - Resource utilization
   - Batch job timings

4. Migration Strategy Selection
   - Downtime tolerance
   - Data volume
   - Budget constraints
   - Technical complexity
   - Risk tolerance

5. Timeline and Resources
   - Duration estimate
   - Team assignment
   - Tool selection
   - Budget allocation
```

### Phase 2: Schema Migration

**Automated Schema Conversion:**

```bash
# Using pgLoader (MySQL to PostgreSQL)
pgloader mysql://user:pass@source-host/dbname \
          postgresql://user:pass@target-host/dbname

# Using AWS Schema Conversion Tool (SCT)
# GUI-based tool for Oracle/SQL Server to PostgreSQL/MySQL

# Using ora2pg (Oracle to PostgreSQL)
ora2pg -c ora2pg.conf -t TABLE -o schema.sql
ora2pg -c ora2pg.conf -t VIEW -o views.sql
ora2pg -c ora2pg.conf -t PROCEDURE -o procedures.sql
```

**Manual Schema Conversion:**

```sql
-- Oracle to PostgreSQL Example

-- Oracle
CREATE TABLE employees (
    emp_id NUMBER(10) PRIMARY KEY,
    emp_name VARCHAR2(100),
    hire_date DATE DEFAULT SYSDATE,
    salary NUMBER(10,2)
);

-- PostgreSQL
CREATE TABLE employees (
    emp_id INTEGER PRIMARY KEY,
    emp_name VARCHAR(100),
    hire_date DATE DEFAULT CURRENT_DATE,
    salary NUMERIC(10,2)
);

-- Oracle Sequence
CREATE SEQUENCE emp_seq START WITH 1;

-- PostgreSQL (using SERIAL or IDENTITY)
CREATE TABLE employees (
    emp_id SERIAL PRIMARY KEY,
    -- or
    emp_id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    ...
);

-- Oracle PL/SQL Function
CREATE OR REPLACE FUNCTION get_employee_count
RETURN NUMBER IS
    v_count NUMBER;
BEGIN
    SELECT COUNT(*) INTO v_count FROM employees;
    RETURN v_count;
END;

-- PostgreSQL PL/pgSQL Function
CREATE OR REPLACE FUNCTION get_employee_count()
RETURNS INTEGER AS $$
DECLARE
    v_count INTEGER;
BEGIN
    SELECT COUNT(*) INTO v_count FROM employees;
    RETURN v_count;
END;
$$ LANGUAGE plpgsql;
```

**Data Type Mapping:**

```markdown
Oracle → PostgreSQL:
- NUMBER → NUMERIC or INTEGER
- VARCHAR2 → VARCHAR
- DATE → DATE or TIMESTAMP
- CLOB → TEXT
- BLOB → BYTEA
- RAW → BYTEA

MySQL → PostgreSQL:
- INT → INTEGER
- TINYINT → SMALLINT
- BIGINT → BIGINT
- VARCHAR → VARCHAR
- TEXT → TEXT
- DATETIME → TIMESTAMP
- ENUM → Custom TYPE or VARCHAR with CHECK

SQL Server → PostgreSQL:
- INT → INTEGER
- NVARCHAR → VARCHAR
- DATETIME → TIMESTAMP
- BIT → BOOLEAN
- UNIQUEIDENTIFIER → UUID
- IMAGE → BYTEA
```

### Phase 3: Data Migration

**Small Database (< 100 GB):**

```bash
# PostgreSQL dump and restore
pg_dump -h source-host -U user -d dbname -F c -f dump.backup
pg_restore -h target-host -U user -d dbname dump.backup

# MySQL dump and restore
mysqldump -h source-host -u user -p dbname > dump.sql
mysql -h target-host -u user -p dbname < dump.sql

# With compression
mysqldump -h source-host -u user -p dbname | gzip > dump.sql.gz
gunzip < dump.sql.gz | mysql -h target-host -u user -p dbname
```

**Large Database (> 100 GB):**

```bash
# Parallel export/import (PostgreSQL)
pg_dump -h source-host -U user -d dbname -F d -j 8 -f dumpdir/
pg_restore -h target-host -U user -d dbname -j 8 dumpdir/

# Table-by-table migration
for table in $(psql -h source -U user -d db -t -c "SELECT tablename FROM pg_tables WHERE schemaname='public'"); do
    pg_dump -h source -U user -d db -t $table -F c -f ${table}.backup
    pg_restore -h target -U user -d db ${table}.backup
done

# Using COPY for fast data transfer
psql -h source -U user -d db -c "COPY table TO STDOUT" | \
psql -h target -U user -d db -c "COPY table FROM STDIN"
```

**Cross-Engine Migration:**

```bash
# MySQL to PostgreSQL using pgLoader
pgloader mysql://user:pass@mysql-host/dbname \
          postgresql://user:pass@pg-host/dbname

# With custom configuration
cat > migration.load <<EOF
LOAD DATABASE
    FROM mysql://user:pass@mysql-host/dbname
    INTO postgresql://user:pass@pg-host/dbname

WITH include drop, create tables, create indexes,
     reset sequences, workers = 8, concurrency = 1

CAST type datetime to timestamp
     drop default drop not null using zero-dates-to-null,
     
     type date drop not null drop default using zero-dates-to-null

EXCLUDING TABLE NAMES MATCHING 'temp_', 'backup_'

BEFORE LOAD DO
    \$\$ DROP SCHEMA IF EXISTS public CASCADE; \$\$,
    \$\$ CREATE SCHEMA public; \$\$;
EOF

pgloader migration.load
```

**AWS Database Migration Service (DMS):**

```markdown
DMS Migration Types:

1. Full Load
   - Migrate all existing data
   - Database stays online
   - No ongoing replication

2. Full Load + CDC (Change Data Capture)
   - Initial full load
   - Continuous replication
   - Minimal downtime cutover

3. CDC Only
   - Replicate changes only
   - Assumes initial data already migrated
   - For validation or sync

DMS Setup:
1. Create replication instance
2. Create source endpoint
3. Create target endpoint
4. Create migration task
5. Start migration
6. Monitor progress
7. Perform cutover
```

### Phase 4: Application Code Migration

```markdown
Code Changes Required:

1. Connection Strings
   # Oracle
   jdbc:oracle:thin:@host:1521:SID
   
   # PostgreSQL
   jdbc:postgresql://host:5432/database

2. SQL Query Syntax
   -- Oracle
   SELECT * FROM employees WHERE ROWNUM <= 10;
   SELECT NVL(column, 'default') FROM table;
   SELECT TO_DATE('2024-01-01', 'YYYY-MM-DD');
   
   -- PostgreSQL
   SELECT * FROM employees LIMIT 10;
   SELECT COALESCE(column, 'default') FROM table;
   SELECT TO_DATE('2024-01-01', 'YYYY-MM-DD');

3. ORM Framework Updates
   # Hibernate
   # Update hibernate.dialect
   # Oracle
   hibernate.dialect=org.hibernate.dialect.Oracle12cDialect
   
   # PostgreSQL
   hibernate.dialect=org.hibernate.dialect.PostgreSQL10Dialect

4. Stored Procedure Calls
   # Update procedure call syntax
   # May need to rewrite in target database language
```

### Phase 5: Testing and Validation

```markdown
Testing Checklist:

Schema Validation:
- [ ] All tables created
- [ ] Columns match (names, types, constraints)
- [ ] Indexes created
- [ ] Foreign keys established
- [ ] Views functional
- [ ] Stored procedures working
- [ ] Triggers active

Data Validation:
- [ ] Row counts match
  SELECT COUNT(*) FROM table;
  
- [ ] Data integrity checks
  SELECT MD5(string_agg(column, '')) FROM 
    (SELECT column FROM table ORDER BY id) t;
  
- [ ] Sample data comparison
- [ ] Referential integrity maintained
- [ ] No data truncation

Functional Testing:
- [ ] All CRUD operations work
- [ ] Queries return correct results
- [ ] Transactions commit/rollback properly
- [ ] Concurrency handling correct
- [ ] Batch jobs complete successfully

Performance Testing:
- [ ] Query response times acceptable
- [ ] Index usage optimal
- [ ] Connection pooling works
- [ ] Resource utilization normal
- [ ] Load testing passed

Application Testing:
- [ ] Application starts successfully
- [ ] All features functional
- [ ] Reports generate correctly
- [ ] APIs respond properly
- [ ] User acceptance testing passed
```

## Zero-Downtime Migration Strategies

### Logical Replication

```sql
-- PostgreSQL Logical Replication Setup

-- On source (publisher)
ALTER SYSTEM SET wal_level = logical;
-- Restart PostgreSQL

CREATE PUBLICATION my_publication FOR ALL TABLES;

-- On target (subscriber)
CREATE SUBSCRIPTION my_subscription
    CONNECTION 'host=source-host port=5432 dbname=mydb user=repuser password=pass'
    PUBLICATION my_publication;

-- Monitor replication
SELECT * FROM pg_stat_subscription;

-- Cutover
-- 1. Stop application writes
-- 2. Wait for replication to catch up
SELECT pg_current_wal_lsn();  -- On source
SELECT latest_end_lsn FROM pg_stat_subscription;  -- On target
-- When they match, replication is current

-- 3. Drop subscription
DROP SUBSCRIPTION my_subscription;

-- 4. Update application connection to target
-- 5. Resume operations
```

### Dual-Write Strategy

```markdown
Approach:
1. Write to both old and new databases
2. Read from old database initially
3. Validate data consistency
4. Switch reads to new database
5. Stop writing to old database

Pros:
- Very low downtime
- Easy rollback

Cons:
- Application complexity
- Requires code changes
- Potential data inconsistency

Implementation:
- Use application middleware
- Queue-based async writes
- Monitoring and reconciliation
```

## Cloud-Specific Migrations

### AWS RDS Migration

```bash
# On-Premise PostgreSQL → AWS RDS PostgreSQL

# Option 1: pg_dump/pg_restore
pg_dump -h source-host -U user -d dbname -F c -f dump.backup
pg_restore -h rds-endpoint -U user -d dbname dump.backup

# Option 2: AWS DMS
# Use AWS Console or CLI to create migration task

# Option 3: RDS Snapshot (for RDS to RDS)
aws rds create-db-snapshot \
  --db-instance-identifier source-db \
  --db-snapshot-identifier migration-snapshot

aws rds restore-db-instance-from-db-snapshot \
  --db-instance-identifier target-db \
  --db-snapshot-identifier migration-snapshot
```

### GCP Cloud SQL Migration

```bash
# MySQL → Cloud SQL MySQL

# Using Database Migration Service (DMS)
gcloud sql connect-db-instances create CONNECTION_PROFILE \
  --source=mysql \
  --host=SOURCE_IP \
  --port=3306 \
  --username=USER

# Or using mysqldump
mysqldump -h source-host -u user -p --databases dbname \
  --single-transaction --set-gtid-purged=OFF > dump.sql

# Import to Cloud SQL
gcloud sql import sql INSTANCE_NAME gs://BUCKET/dump.sql \
  --database=DATABASE_NAME
```

### Azure Database Migration

```bash
# SQL Server → Azure SQL Database

# Using Azure Database Migration Service
# Or using BACPAC export/import

# Export
SqlPackage.exe /Action:Export \
  /SourceServerName:source-server \
  /SourceDatabaseName:mydb \
  /TargetFile:mydb.bacpac

# Upload to Azure Blob Storage
az storage blob upload \
  --account-name mystorageaccount \
  --container-name backups \
  --file mydb.bacpac

# Import to Azure SQL
az sql db import \
  --resource-group mygroup \
  --server target-server \
  --name mydb \
  --storage-key-type StorageAccessKey \
  --storage-key STORAGE_KEY \
  --storage-uri https://mystorageaccount.blob.core.windows.net/backups/mydb.bacpac
```

## Rollback Procedures

```markdown
Rollback Triggers:
- Data corruption detected
- Performance degradation > 50%
- Application failures
- Data inconsistencies
- Unable to resolve issues within SLA

Rollback Steps:

1. Decision Point
   - Assess severity
   - Estimate fix time
   - Make rollback decision
   - Communicate to stakeholders

2. Stop New Database Operations
   - Stop application writes to new DB
   - Prevent further data divergence

3. Revert Application Connections
   - Update connection strings
   - Point to old database
   - Restart application if needed

4. Validate Old Database
   - Check data integrity
   - Verify service functionality
   - Monitor performance

5. Resume Operations
   - Announce rollback complete
   - Normal operations restored

6. Post-Rollback
   - Root cause analysis
   - Fix identified issues
   - Reschedule migration
```

## Best Practices

✅ **Multiple Backups**: Take backups before migration, keep source DB until validated
✅ **Test in Staging**: Practice migration in non-production first
✅ **Monitor Metrics**: Watch lag, throughput, errors during migration
✅ **Incremental Approach**: Migrate in phases if possible
✅ **Validate Thoroughly**: Check schema, data, and functionality
✅ **Plan for Rollback**: Always have a way back
✅ **Document Everything**: Record steps, issues, decisions
✅ **Parallel Run**: Run both databases temporarily when possible
✅ **Performance Tuning**: Optimize indexes and queries in target
✅ **Security**: Encrypt data in transit, rotate credentials

## Common Issues and Solutions

```markdown
Issue: Replication Lag Too High
Solution:
- Increase network bandwidth
- Optimize source DB performance
- Reduce concurrent writes during migration
- Use parallel replication streams

Issue: Data Type Conversion Errors
Solution:
- Review and fix schema mapping
- Handle NULL values appropriately
- Cast incompatible types explicitly
- Test with sample data first

Issue: Foreign Key Constraint Violations
Solution:
- Disable constraints during load
- Migrate in dependency order
- Load parent tables before child tables
- Re-enable and validate constraints after

Issue: Performance Worse on Target
Solution:
- Analyze query plans
- Create missing indexes
- Update database statistics
- Tune database parameters
- Consider partitioning
```

## Tools Reference

```markdown
Cross-Platform Migration:
- pgLoader (MySQL/SQLite → PostgreSQL)
- AWS Database Migration Service (DMS)
- Azure Database Migration Service
- GCP Database Migration Service
- ora2pg (Oracle → PostgreSQL)
- SQLines (multi-database conversion)

Native Tools:
- pg_dump/pg_restore (PostgreSQL)
- mysqldump/mysql (MySQL)
- mongodump/mongorestore (MongoDB)
- SQL Server Management Studio
- Oracle Data Pump (expdp/impdp)

Schema Conversion:
- AWS Schema Conversion Tool (SCT)
- ora2pg
- pgloader (with schema conversion)
- ESF Database Migration Toolkit

Validation:
- data-diff (compare databases)
- Custom SQL scripts
- Checksum comparisons
```

## Notes

- Allow 2-3x estimated time for heterogeneous migrations
- Plan for extended parallel run period (1-4 weeks)
- Database migration often triggers application code changes
- Coordinate with application migration when possible
- Consider phased approach: read replica → read/write split → full cutover
- Cloud provider migration services often simplest for cloud migrations
