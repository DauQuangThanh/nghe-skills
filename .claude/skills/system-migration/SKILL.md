---
name: system-migration
description: Guides operating system and hardware platform migrations including Linux distribution changes, Windows to Linux migration, mainframe to x86 modernization, data center migrations (physical to virtual, P2V, V2V), hardware platform changes, system consolidation, and legacy system decommissioning. Covers OS compatibility, driver migration, system configuration transfer, and cutover procedures. Use when migrating operating systems, changing hardware platforms, moving from mainframes, or when users mention "OS migration", "Linux migration", "mainframe modernization", "P2V migration", "system upgrade", "data center migration", or "legacy system replacement".
---

# System Migration

## Overview

This skill provides comprehensive guidance for system-level migrations including operating system changes, hardware platform transitions, mainframe modernization, virtualization, and data center migrations. Focuses on system infrastructure that underlies applications and platforms.

## Migration Types

### 1. Operating System Migration

#### Linux Distribution Migration

**Common Scenarios:**

```markdown
CentOS 7 → Rocky Linux 9
- Reason: CentOS EOL, compatible replacement

Ubuntu 18.04 → Ubuntu 22.04
- Reason: LTS upgrade, security updates

Red Hat 7 → Red Hat 8/9
- Reason: Modern features, support

Debian 10 → Debian 12
- Reason: Package updates, kernel improvements

Amazon Linux 1 → Amazon Linux 2023
- Reason: Long-term support, performance
```

**In-Place Upgrade vs Fresh Install:**

```markdown
In-Place Upgrade:
Pros:
- Faster migration
- Preserves configuration
- Less downtime

Cons:
- Risk of upgrade failures
- May leave orphaned packages
- Harder to troubleshoot

When to Use:
- Non-critical systems
- Simple configurations
- Limited time window

Fresh Install (Recommended):
Pros:
- Clean, known state
- Remove technical debt
- Better documentation opportunity

Cons:
- More time required
- Configuration re-creation
- Testing overhead

When to Use:
- Production systems
- Complex configurations
- Long-term stability critical
```

**Linux Migration Workflow:**

```markdown
Phase 1: Assessment
1. Document Current State
   - OS version: uname -a
   - Kernel: cat /proc/version
   - Installed packages: rpm -qa or dpkg -l
   - Running services: systemctl list-units
   - Custom configurations: /etc/
   - Cron jobs: crontab -l, /etc/cron.d/
   - Network configuration
   - Mounted filesystems: df -h, /etc/fstab

2. Application Compatibility
   - List all applications
   - Check version compatibility
   - Identify deprecated dependencies
   - Plan application updates

3. Hardware Compatibility
   - Check driver availability
   - Validate kernel modules
   - Test hardware detection

Phase 2: Preparation
1. Backup Everything
   - Full system backup
   - Configuration files tar archive
   - Database dumps
   - Application data
   - Home directories

2. Setup Target System
   - Provision new server/VM
   - Install target OS
   - Configure networking
   - Set up storage

3. Configuration Migration
   # Export users and groups
   getent passwd > users.txt
   getent group > groups.txt
   
   # Export crontabs
   for user in $(cut -f1 -d: /etc/passwd); do
     crontab -u $user -l > cron.$user 2>/dev/null
   done
   
   # Backup configuration
   tar czf etc-backup.tar.gz /etc/

Phase 3: Migration
1. Install Required Packages
   # Document source packages
   rpm -qa > packages-source.txt  # RHEL/CentOS
   dpkg -l > packages-source.txt  # Debian/Ubuntu
   
   # Install on target
   # Review and install needed packages
   # Update package names if changed

2. Restore Configurations
   - Copy /etc/ configurations (carefully)
   - Adjust for new OS differences
   - Update init system (sysvinit → systemd)
   - Migrate network configs

3. Migrate Services
   - Install service packages
   - Restore service configs
   - Enable services: systemctl enable
   - Start services: systemctl start

4. Migrate Data
   - Restore application data
   - Set proper permissions
   - Validate data integrity

Phase 4: Testing
- Verify all services running
- Test application functionality
- Check log files for errors
- Validate network connectivity
- Performance testing
- Security scanning

Phase 5: Cutover
- Schedule maintenance window
- Update DNS/load balancer
- Monitor service health
- Provide rollback option
- Document changes
```

**Package Manager Translation:**

```bash
# RPM (RHEL/CentOS) → DEB (Debian/Ubuntu)

# RHEL/CentOS
yum install httpd
systemctl start httpd

# Debian/Ubuntu equivalent
apt install apache2
systemctl start apache2

# Common package name changes:
# httpd → apache2
# mariadb-server → mariadb-server (same)
# postgresql → postgresql (same)
# openssh-server → openssh-server (same)
```

#### Windows to Linux Migration

```markdown
Use Cases:
- Cost reduction (licensing)
- Stability and security
- Container/cloud-native adoption
- Open source preference

Challenges:
- Application compatibility
- Active Directory integration
- PowerShell → Bash scripts
- Different file systems
- User training

Migration Strategy:

Phase 1: Application Assessment
- Identify all Windows applications
- Find Linux alternatives
  IIS → Apache/Nginx
  MS SQL → PostgreSQL/MySQL
  .NET Framework → .NET Core/Mono
  PowerShell → Bash/Python
  Task Scheduler → cron

Phase 2: Proof of Concept
- Deploy test Linux system
- Install alternative applications
- Migrate sample data
- Validate functionality
- Performance comparison

Phase 3: User Preparation
- Linux training for administrators
- Document differences
- Create runbooks
- Establish support channels

Phase 4: Data Migration
- File shares: Samba on Linux
- Databases: Export/import
- User accounts: LDAP/AD integration
- Permissions mapping

Phase 5: Service Migration
- Migrate services one by one
- Parallel run when possible
- Monitor closely
- Gradual user cutover
```

### 2. Mainframe to x86 Migration

**Mainframe Modernization Strategies:**

```markdown
Strategy 1: Rehost (Lift and Shift)
- Use mainframe emulation on x86
- Tools: Micro Focus, LzLabs
- Fastest migration
- Limited modernization

Strategy 2: Refactor
- Convert COBOL → Java
- Rewrite business logic
- Modernize architecture
- Significant effort

Strategy 3: Replace
- Use COTS software
- SaaS alternatives
- Complete redesign
- Highest risk/reward

Strategy 4: Hybrid
- Keep mainframe for core workloads
- Migrate peripherals to x86
- Gradual transition
- Lower risk
```

**Mainframe Migration Workflow:**

```markdown
Phase 1: Discovery and Assessment
1. Application Inventory
   - COBOL programs
   - JCL jobs
   - CICS transactions
   - DB2 databases
   - Batch processes
   - File dependencies

2. Dependency Mapping
   - Job scheduling
   - Data flows
   - Integration points
   - External interfaces

3. Business Impact Analysis
   - Critical vs non-critical
   - Transaction volumes
   - Performance requirements
   - Compliance needs

Phase 2: Architecture Design
1. Target Platform
   - Cloud vs on-premise
   - Linux vs Windows
   - Database choice (PostgreSQL, Oracle)
   - Application server (Tomcat, WebLogic)

2. Data Architecture
   - VSAM → Relational DB
   - Hierarchical → Relational
   - Data modeling
   - ETL design

3. Integration Design
   - API design for mainframe interfaces
   - Message queues (MQ → Kafka)
   - File transfers (FTP → SFTP/S3)

Phase 3: Data Migration
1. Schema Conversion
   - COBOL copybooks → SQL DDL
   - Data type mapping
   - Key structure preservation

2. Data Extract
   - Export mainframe data
   - Handle EBCDIC → ASCII
   - Decompress packed decimal
   - Convert dates (Julian, etc.)

3. Data Load
   - Bulk load to target DB
   - Validate data integrity
   - Create indexes
   - Gather statistics

Phase 4: Application Migration
1. Code Conversion
   - COBOL → Java (automated tools)
   - Manual refinement
   - Modernize patterns
   - Add logging/monitoring

2. Testing
   - Unit tests
   - Integration tests
   - Performance tests
   - User acceptance testing

Phase 5: Cutover
- Parallel run (mainframe + new system)
- Reconciliation of outputs
- Gradual traffic shift
- Decommission mainframe
```

**COBOL Data Conversion:**

```bash
# Convert EBCDIC to ASCII
dd if=mainframe.dat of=ascii.dat conv=ascii

# Convert packed decimal
# Use specialized tools or custom scripts

# Date conversion (Julian to ISO)
# COBOL: 2024001 (year + day of year)
# ISO: 2024-01-01
```

### 3. Physical to Virtual (P2V) Migration

**P2V Approaches:**

```markdown
Approach 1: Disk Imaging
1. Boot server from live media
2. Create disk image (dd, Clonezilla)
3. Convert to VM format (qemu-img)
4. Import to hypervisor
5. Reconfigure for virtual hardware

Approach 2: Migration Tools
- VMware Converter
- Hyper-V Migration Tool
- Azure Migrate
- Clonezilla

Approach 3: Fresh Install + Data Sync
1. Install OS on new VM
2. Install applications
3. Sync data (rsync)
4. Cutover
```

**P2V Migration Process:**

```bash
# Method 1: Using dd and qemu-img

# 1. Create disk image from physical server
dd if=/dev/sda of=/mnt/backup/server.img bs=64K conv=noerror,sync

# 2. Convert to QCOW2 (for KVM/QEMU)
qemu-img convert -f raw -O qcow2 server.img server.qcow2

# 3. Convert to VMDK (for VMware)
qemu-img convert -f raw -O vmdk server.img server.vmdk

# 4. Create VM with converted disk

# 5. Boot and reconfigure
# - Update network interface names
# - Reinstall VM tools
# - Update drivers
# - Remove physical hardware refs

# Method 2: Using VMware Converter
# 1. Install Converter on physical or separate system
# 2. Run converter wizard
# 3. Select source (powered on machine)
# 4. Select destination (ESXi host)
# 5. Configure VM settings
# 6. Start conversion
# 7. Power on VM after conversion
```

**Post-P2V Configuration:**

```bash
# Remove physical hardware drivers (Linux)
# Update network interface configuration
vim /etc/network/interfaces
# or for NetworkManager
vim /etc/NetworkManager/system-connections/

# Install virtualization guest tools
# VMware
yum install open-vm-tools

# KVM
yum install qemu-guest-agent

# Hyper-V
# Install Linux Integration Services

# Update GRUB for paravirtualized disk
# May need to rebuild initramfs
dracut -f
```

### 4. Virtual to Virtual (V2V) Migration

**V2V Scenarios:**

```markdown
VMware → KVM
VMware → Hyper-V
Hyper-V → VMware
KVM → VMware
On-Premise → Cloud (AWS, Azure, GCP)
```

**V2V Tools:**

```bash
# virt-v2v (libguestfs)
# Converts VMs from foreign hypervisor to KVM

# VMware to KVM
virt-v2v -i libvirtxml -o local -os /var/lib/libvirt/images vm.xml

# VMware ESXi to KVM
virt-v2v -ic vpx://vcenter.example.com/Datacenter/esxi1 \
  -o libvirt -os /var/lib/libvirt/images \
  -of qcow2 vmname

# Convert VMDK to QCOW2
qemu-img convert -f vmdk -O qcow2 source.vmdk target.qcow2

# Import to libvirt
virt-install \
  --name myvm \
  --memory 4096 \
  --vcpus 2 \
  --disk path=/var/lib/libvirt/images/target.qcow2 \
  --import \
  --os-variant rhel8
```

### 5. Data Center Migration

**Migration Scenarios:**

```markdown
Physical DC → Physical DC
- Hardware relocation
- Network reconfiguration
- Minimal downtime strategies

Physical DC → Cloud
- P2V + cloud migration
- Modernization opportunity
- Network connectivity critical

DC Consolidation
- Multiple DCs → Single DC
- Merge environments
- Decommission redundant systems
```

**Data Center Migration Phases:**

```markdown
Phase 1: Planning (Months 1-2)
- Inventory all systems
- Document dependencies
- Map network topology
- Assess bandwidth requirements
- Plan connectivity (VPN, Direct Connect)
- Schedule migration waves

Phase 2: Setup Target DC (Months 2-3)
- Rack and stack hardware
- Configure networking
- Establish connectivity
- Deploy hypervisors
- Test connectivity

Phase 3: Migration Waves (Months 3-6)
Wave 1: Non-critical systems
- File servers
- Development environments
- Testing systems

Wave 2: Supporting systems
- Monitoring
- Logging
- Backup systems

Wave 3: Production systems
- Databases (with replication)
- Application servers
- Web servers

Wave 4: Critical systems
- Core business applications
- Financial systems
- Customer-facing services

Phase 4: Validation and Optimization
- Performance testing
- Network optimization
- Cost analysis
- Documentation updates

Phase 5: Source DC Decommissioning
- Power down systems
- Secure data destruction
- Hardware disposal/repurpose
- Contract termination
```

**Network Connectivity:**

```bash
# Establish VPN between data centers
# IPSec tunnel configuration

# Test bandwidth
iperf3 -s  # On target DC
iperf3 -c target-dc-ip -t 60  # On source DC

# Monitor latency
ping -c 100 target-dc-ip

# Test application connectivity
nc -zv target-dc-ip 3306  # MySQL
nc -zv target-dc-ip 443   # HTTPS
```

## System Configuration Migration

### Configuration Backup and Restore

```bash
# Linux System Configuration Backup

# 1. System files
tar czf system-config.tar.gz \
  /etc/ \
  /var/spool/cron/ \
  /home/*/.ssh/ \
  /root/.ssh/

# 2. Package list
rpm -qa > installed-packages.txt    # RHEL/CentOS
dpkg -l > installed-packages.txt    # Debian/Ubuntu

# 3. Services
systemctl list-unit-files --state=enabled > enabled-services.txt

# 4. Firewall rules
iptables-save > iptables-rules
# or for firewalld
firewall-cmd --list-all-zones > firewall-config.txt

# 5. SELinux/AppArmor
getenforce > selinux-mode.txt
sestatus > selinux-status.txt

# 6. Network configuration
ip addr > ip-config.txt
ip route > routing-table.txt

# Restore on target system (carefully review each file)
```

### Automation Scripts

```bash
# Create migration script
#!/bin/bash
# system-migrate.sh

set -e  # Exit on error

SOURCE_IP="source-server-ip"
TARGET_IP="target-server-ip"

echo "=== System Migration ==="

# 1. Backup source system
echo "Backing up source system..."
ssh root@$SOURCE_IP 'tar czf /tmp/config-backup.tar.gz /etc/'
scp root@$SOURCE_IP:/tmp/config-backup.tar.gz ./

# 2. Copy to target
echo "Copying to target..."
scp config-backup.tar.gz root@$TARGET_IP:/tmp/

# 3. Install packages on target
echo "Installing packages..."
ssh root@$TARGET_IP 'yum install -y httpd mariadb-server'

# 4. Restore configurations
echo "Restoring configurations..."
ssh root@$TARGET_IP 'cd /tmp && tar xzf config-backup.tar.gz'

# 5. Start services
echo "Starting services..."
ssh root@$TARGET_IP 'systemctl start httpd mariadb'

echo "Migration complete!"
```

## Validation and Testing

```markdown
System Level Tests:

1. Boot Test
   - System boots successfully
   - All file systems mount
   - Network interfaces activate

2. Service Test
   - All services start
   - Services respond to requests
   - Inter-service communication works

3. Performance Test
   - CPU utilization normal
   - Memory usage acceptable
   - Disk I/O meets requirements
   - Network throughput adequate

4. Security Test
   - Firewall rules active
   - SELinux/AppArmor enforcing
   - SSH key authentication works
   - No unnecessary open ports

5. Application Test
   - Applications start
   - Database connections work
   - APIs respond correctly
   - UI accessible

6. Monitoring Test
   - Metrics collection working
   - Logs being forwarded
   - Alerts configured
   - Dashboards displaying data
```

## Rollback Procedures

```markdown
Rollback Decision Criteria:
- Critical service failure
- Data corruption detected
- Performance degradation >50%
- Security breach
- Unable to resolve issues in 2 hours

Rollback Steps:

1. Immediate Actions
   - Announce rollback decision
   - Stop new migrations
   - Document reason

2. DNS/Traffic Revert
   - Update DNS to old IPs
   - Revert load balancer
   - Wait for propagation

3. Restart Old Services
   - Power on old systems
   - Start all services
   - Verify functionality

4. Data Sync (if needed)
   - Replicate any new data back
   - Validate data integrity
   - Resume normal operations

5. Post-Rollback
   - Incident analysis
   - Fix issues found
   - Reschedule migration
```

## Best Practices

✅ **Thorough Testing**: Test everything in non-prod first
✅ **Complete Backups**: Multiple backups before migration
✅ **Documentation**: Document every configuration
✅ **Automation**: Script repeatable tasks
✅ **Gradual Migration**: Migrate in phases, not all at once
✅ **Monitoring**: Watch metrics closely during migration
✅ **Communication**: Keep stakeholders informed
✅ **Rollback Plan**: Always have a way back
✅ **Validation**: Verify at each step
✅ **Training**: Ensure team knows new system

## Anti-Patterns

❌ **No Backups**: Migrate without backups
❌ **Big Bang**: Migrate everything simultaneously
❌ **Skip Testing**: Go straight to production
❌ **Manual Only**: No automation or scripts
❌ **Poor Documentation**: Undocumented configurations
❌ **No Rollback Plan**: Can't revert if problems occur
❌ **Ignore Dependencies**: Miss critical integrations
❌ **Insufficient Testing Time**: Rush validation phase
❌ **No Monitoring**: Can't detect issues quickly
❌ **Outdated System**: Migrate old to old (missed opportunity)

## Tools and Resources

```markdown
OS Migration:
- Leapp (RHEL upgrade tool)
- Ubuntu do-release-upgrade
- SystemRescue (live CD for backups)

Mainframe:
- Micro Focus Enterprise Server
- LzLabs Software Defined Mainframe
- AWS Mainframe Modernization

Virtualization:
- VMware vCenter Converter
- virt-v2v (libguestfs)
- Clonezilla
- Azure Migrate
- AWS Application Migration Service

Backup/Restore:
- rsync
- tar
- Clonezilla
- Bacula
- Amanda

Configuration Management:
- Ansible
- Puppet
- Chef
- SaltStack
```

## Notes

- System migrations often involve multiple skills: OS + database + application
- Plan for 30-50% more time than estimated
- Parallel run periods reduce risk but increase cost
- Mainframe migrations are multi-year projects
- Physical to virtual conversions rarely have issues with modern hardware
- Always validate hardware compatibility before OS migrations
- Document everything—configurations, decisions, issues, resolutions
