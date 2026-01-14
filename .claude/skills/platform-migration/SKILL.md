---
name: platform-migration
description: Guides infrastructure and platform migration including cloud-to-cloud migration (AWS to GCP, Azure to AWS), Kubernetes cluster migration, CI/CD platform changes, monitoring stack migration, identity provider migration, and network infrastructure transformation. Covers IaC migration, container orchestration, service mesh, observability tools, and multi-cloud strategies. Use when migrating infrastructure, changing cloud providers, moving Kubernetes clusters, or when users mention "cloud migration", "platform switch", "infrastructure migration", "K8s migration", "multi-cloud", or "cloud provider change".
---

# Platform Migration

## Overview

This skill provides comprehensive guidance for migrating infrastructure platforms, cloud providers, container orchestration, CI/CD systems, and observability stacks. Focuses on infrastructure-level migrations that complement application migration efforts.

## Migration Types

### 1. Cloud-to-Cloud Migration

**Common Scenarios:**
- AWS → GCP
- AWS → Azure
- GCP → AWS
- Azure → GCP
- On-Premise → Any Cloud
- Single Cloud → Multi-Cloud

**Key Considerations:**

```markdown
Service Mapping:

AWS → GCP:
- EC2 → Compute Engine
- S3 → Cloud Storage
- RDS → Cloud SQL
- Lambda → Cloud Functions
- ECS/EKS → GKE
- CloudWatch → Cloud Monitoring
- IAM → Cloud IAM
- Route53 → Cloud DNS
- CloudFront → Cloud CDN

AWS → Azure:
- EC2 → Virtual Machines
- S3 → Blob Storage
- RDS → Azure SQL Database
- Lambda → Azure Functions
- ECS/EKS → AKS
- CloudWatch → Azure Monitor
- IAM → Azure AD
- Route53 → Azure DNS
- CloudFront → Azure CDN

GCP → AWS:
- Compute Engine → EC2
- Cloud Storage → S3
- Cloud SQL → RDS
- Cloud Functions → Lambda
- GKE → EKS
- Cloud Monitoring → CloudWatch
- Cloud IAM → IAM
- Cloud DNS → Route53
```

**Migration Workflow:**

```markdown
Phase 1: Assessment (Weeks 1-2)
- Inventory all resources in source cloud
- Document dependencies and integrations
- Identify proprietary services (no equivalent)
- Estimate costs in target cloud
- Create service mapping document

Phase 2: Design (Weeks 3-4)
- Design target architecture
- Choose equivalent services
- Plan network topology
- Design IAM/security model
- Document migration approach

Phase 3: Foundation (Weeks 5-6)
- Set up target cloud accounts
- Configure networking (VPC, subnets)
- Establish connectivity (VPN, peering)
- Set up IAM and security groups
- Deploy monitoring infrastructure

Phase 4: Pilot Migration (Weeks 7-8)
- Migrate non-critical workload
- Test functionality
- Validate performance
- Measure costs
- Document lessons learned

Phase 5: Bulk Migration (Weeks 9-16)
- Migrate workloads in waves
- Data replication and sync
- DNS cutover per service
- Validate each migration
- Monitor performance

Phase 6: Optimization (Weeks 17-20)
- Right-size resources
- Optimize costs
- Improve performance
- Decommission source resources
```

### 2. Kubernetes Platform Migration

**Migration Scenarios:**

```markdown
Self-Managed → Managed:
- Self-hosted K8s → EKS/GKE/AKS
- Reason: Reduce operational overhead

Managed → Different Managed:
- EKS → GKE
- AKS → EKS
- Reason: Better features, cost, integration

Managed → Self-Managed:
- EKS → Self-hosted
- Reason: More control, cost optimization

Version Upgrade:
- K8s 1.24 → 1.28
- Reason: Security, features, support
```

**Kubernetes Migration Strategy:**

```markdown
Approach 1: Blue-Green Deployment

1. Provision New Cluster:
   - Create target cluster (same version first)
   - Configure networking
   - Set up ingress controllers
   - Install cluster add-ons

2. Deploy Applications:
   - Deploy all applications to new cluster
   - Configure services and ingress
   - Test functionality thoroughly
   - Run load tests

3. Data Migration:
   - Sync stateful data (PV migration)
   - Replicate databases
   - Validate data integrity

4. Traffic Switch:
   - Update DNS to point to new cluster
   - Monitor application health
   - Keep old cluster running temporarily
   - Decommission old cluster after validation

Approach 2: Gradual Migration

1. Set Up Multi-Cluster Mesh:
   - Install service mesh (Istio, Linkerd)
   - Connect both clusters
   - Enable cross-cluster communication

2. Migrate Services Incrementally:
   - Move one service at a time
   - Route traffic across clusters
   - Validate each service
   - Update dependencies

3. Complete Migration:
   - Migrate all services
   - Remove mesh if not needed
   - Decommission old cluster
```

**Kubernetes Migration Checklist:**

```yaml
Infrastructure:
- [ ] Target cluster provisioned
- [ ] Networking configured (VPC, subnets, security groups)
- [ ] Ingress controller installed
- [ ] Load balancers configured
- [ ] DNS records prepared
- [ ] SSL certificates migrated

Cluster Add-ons:
- [ ] Metrics server
- [ ] Cluster autoscaler
- [ ] CoreDNS configuration
- [ ] Network policies
- [ ] Pod security policies/admission controllers

Storage:
- [ ] Storage classes created
- [ ] Persistent volumes migrated
- [ ] Volume snapshots tested
- [ ] Backup solution configured

Workloads:
- [ ] Namespaces created
- [ ] Resource quotas configured
- [ ] Limit ranges set
- [ ] Deployments migrated
- [ ] StatefulSets migrated
- [ ] DaemonSets migrated
- [ ] CronJobs migrated

Configuration:
- [ ] ConfigMaps migrated
- [ ] Secrets migrated (re-encrypt)
- [ ] Service accounts created
- [ ] RBAC policies applied
- [ ] Network policies applied

Observability:
- [ ] Monitoring stack deployed
- [ ] Logging configured
- [ ] Tracing enabled
- [ ] Dashboards created
- [ ] Alerts configured

Testing:
- [ ] Smoke tests passed
- [ ] Integration tests passed
- [ ] Load tests completed
- [ ] Failover tested
```

### 3. CI/CD Platform Migration

**Common Migrations:**

```markdown
Jenkins → GitHub Actions
- Reason: Native GitHub integration, cloud-native

Jenkins → GitLab CI
- Reason: Integrated with GitLab, simpler setup

CircleCI → GitHub Actions
- Reason: Consolidate tools, reduce costs

Travis CI → GitHub Actions
- Reason: Better performance, more features

Azure DevOps → GitLab CI
- Reason: Open source preference, better features
```

**Migration Strategy:**

```markdown
Phase 1: Pipeline Analysis
- Inventory all pipelines
- Document pipeline steps
- Identify dependencies (secrets, integrations)
- List custom scripts and tools
- Map pipeline triggers

Phase 2: Translation
- Convert pipeline syntax
  Jenkins Groovy → GitHub Actions YAML
  CircleCI YAML → GitLab CI YAML
- Migrate build scripts
- Migrate test scripts
- Convert deployment scripts

Phase 3: Setup Target Platform
- Configure organization/projects
- Set up runners/agents
- Migrate secrets and credentials
- Configure integrations (Slack, email)
- Set up artifact repositories

Phase 4: Testing
- Run pipelines in new platform
- Compare outputs with old platform
- Validate deployments
- Test rollback procedures

Phase 5: Cutover
- Update webhook configurations
- Redirect builds to new platform
- Monitor first few runs
- Keep old platform available temporarily
```

**Pipeline Translation Example:**

```yaml
# Jenkins (Groovy)
pipeline {
    agent any
    stages {
        stage('Build') {
            steps {
                sh 'npm install'
                sh 'npm run build'
            }
        }
        stage('Test') {
            steps {
                sh 'npm test'
            }
        }
        stage('Deploy') {
            steps {
                sh './deploy.sh'
            }
        }
    }
}

# GitHub Actions (YAML)
name: CI/CD
on: [push]
jobs:
  build-test-deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Build
        run: |
          npm install
          npm run build
      
      - name: Test
        run: npm test
      
      - name: Deploy
        run: ./deploy.sh
        env:
          DEPLOY_KEY: ${{ secrets.DEPLOY_KEY }}
```

### 4. Monitoring and Observability Migration

**Migration Scenarios:**

```markdown
Traditional → Modern:
- Nagios → Prometheus + Grafana
- Zabbix → Datadog
- ELK → Loki + Grafana

Cloud-Native Transitions:
- CloudWatch → Prometheus
- Stackdriver → Datadog
- Azure Monitor → New Relic

Consolidation:
- Multiple tools → Unified platform (Datadog, New Relic)
```

**Metrics Migration:**

```markdown
1. Metric Inventory
   - Document all monitored metrics
   - Identify critical alerts
   - Map dashboard requirements
   - List notification channels

2. Metric Mapping
   Source: CloudWatch CPU Utilization
   Target: Prometheus node_cpu_seconds_total
   
   Source: CloudWatch Request Count
   Target: Prometheus http_requests_total

3. Exporter Setup
   - Deploy Prometheus exporters
   - Configure scrape configs
   - Test metric collection
   - Validate data accuracy

4. Dashboard Migration
   - Recreate dashboards in Grafana
   - Import compatible dashboards
   - Customize visualizations
   - Test dashboard functionality

5. Alerting Migration
   - Define alert rules
   - Configure notification channels
   - Set up escalation policies
   - Test alert delivery
```

**Logging Migration:**

```markdown
From: ELK (Elasticsearch, Logstash, Kibana)
To: Loki + Grafana

Steps:
1. Deploy Loki
   - Install Loki server
   - Configure storage (S3, GCS)
   - Set retention policies

2. Deploy Promtail
   - Install on all nodes
   - Configure log collection
   - Set up label extraction

3. Migrate Queries
   - Convert Lucene queries to LogQL
   - Example:
     ELK: status:500 AND service:api
     Loki: {service="api"} |= "500"

4. Recreate Dashboards
   - Import log dashboards to Grafana
   - Configure log panels
   - Set up variable templates

5. Migrate Alerts
   - Convert log-based alerts
   - Configure alert rules
   - Test notifications
```

### 5. Identity and Access Management Migration

**IAM Migration Scenarios:**

```markdown
On-Premise AD → Azure AD
On-Premise AD → Okta
AWS IAM → GCP IAM
Custom Auth → Auth0
LDAP → Cloud IAM
```

**Migration Workflow:**

```markdown
Phase 1: User Inventory
- Export user list
- Document group memberships
- Map roles and permissions
- Identify service accounts

Phase 2: Setup Target IAM
- Create organization
- Configure SSO
- Set up MFA
- Define roles and groups

Phase 3: User Migration
- Bulk import users
- Assign groups
- Configure permissions
- Enable MFA

Phase 4: Application Integration
- Update OIDC/SAML configs
- Test SSO integration
- Migrate API keys
- Update service account credentials

Phase 5: Cutover
- Redirect authentication
- Monitor login success
- Provide user support
- Decommission old IAM
```

## Infrastructure as Code Migration

### Terraform State Migration

```bash
# Migrate state between backends
# From local to S3
terraform init \
  -migrate-state \
  -backend-config="bucket=my-terraform-state" \
  -backend-config="key=prod/terraform.tfstate" \
  -backend-config="region=us-east-1"

# From Terraform Cloud to S3
terraform login  # Authenticate to TFC
terraform init -migrate-state
# Follow prompts to migrate

# Verify state
terraform state list
```

### IaC Platform Migration

```markdown
CloudFormation → Terraform

1. Export Resources:
   - Use CloudFormer or AWS CLI
   - Document all resources
   - Note dependencies

2. Import to Terraform:
   # Generate Terraform config
   terraform import aws_instance.web i-1234567890abcdef0
   
   # Verify import
   terraform plan

3. Refactor:
   - Organize into modules
   - Add variables
   - Apply best practices

4. Validate:
   - Run terraform plan (should be no changes)
   - Test in non-prod first
   - Gradually adopt in production
```

## Network Infrastructure Migration

### VPC/Network Migration

```markdown
Scenario: Migrate from AWS VPC to GCP VPC

Phase 1: Design Target Network
- IP address ranges (avoid overlap)
- Subnet allocation
- Routing tables
- Firewall rules
- NAT gateway configuration

Phase 2: Establish Connectivity
- Set up VPN between clouds
- Configure BGP if needed
- Test connectivity
- Document routes

Phase 3: Migrate Workloads
- Deploy to new network
- Update security groups
- Test internal connectivity
- Validate external access

Phase 4: DNS Migration
- Update DNS records
- Implement DNS failover
- Monitor DNS propagation
- Remove old DNS entries

Phase 5: Decommission
- Remove VPN connection
- Delete old VPC
- Clean up routes
```

## Container Registry Migration

```bash
# Migrate from Docker Hub to AWS ECR

# 1. Authenticate to source
docker login

# 2. Authenticate to target
aws ecr get-login-password --region us-east-1 | \
  docker login --username AWS --password-stdin \
  123456789012.dkr.ecr.us-east-1.amazonaws.com

# 3. Pull, tag, and push
SOURCE_REPO="myorg/myapp"
TARGET_REPO="123456789012.dkr.ecr.us-east-1.amazonaws.com/myapp"

for tag in v1.0 v1.1 v1.2 latest; do
  docker pull $SOURCE_REPO:$tag
  docker tag $SOURCE_REPO:$tag $TARGET_REPO:$tag
  docker push $TARGET_REPO:$tag
done

# 4. Update Kubernetes deployments
kubectl set image deployment/myapp \
  myapp=$TARGET_REPO:v1.2

# 5. Verify
kubectl rollout status deployment/myapp
```

## Database Platform Migration

```markdown
RDS PostgreSQL → Cloud SQL PostgreSQL

Phase 1: Preparation
- Assess database size and load
- Plan maintenance window
- Set up replication if possible
- Test restore procedures

Phase 2: Setup Target
- Create Cloud SQL instance
- Configure network access
- Set parameters matching source
- Create users and permissions

Phase 3: Data Migration
Option A: Dump and Restore
  # Export from RDS
  pg_dump -h rds-host -U postgres -d mydb > dump.sql
  
  # Import to Cloud SQL
  psql -h cloudsql-ip -U postgres -d mydb < dump.sql

Option B: Streaming Replication
  # Set up logical replication
  # Continuous sync during migration
  # Switch over when ready

Phase 4: Validation
- Compare row counts
- Validate data integrity
- Test application queries
- Performance benchmarking

Phase 5: Cutover
- Stop writes to source
- Final sync
- Update connection strings
- Monitor application
```

## Cost Optimization During Migration

```markdown
Strategies:

1. Right-Sizing
   - Don't replicate exact instance types
   - Use target cloud's recommendations
   - Start smaller, scale up if needed

2. Reserved Instances / Savings Plans
   - Wait until migration complete
   - Analyze usage patterns first
   - Commit once stable

3. Spot/Preemptible Instances
   - Use for non-critical workloads
   - Test early in migration
   - Significant cost savings (60-90%)

4. Storage Optimization
   - Use appropriate storage tiers
   - Implement lifecycle policies
   - Compress data where possible
   - Delete unused snapshots

5. Data Transfer Costs
   - Minimize cross-region transfers
   - Use compression for large transfers
   - Plan network topology carefully
   - Consider AWS DataSync, GCP Transfer Service
```

## Risk Management

```markdown
Common Risks:

Risk: Data Loss During Migration
Mitigation:
- Multiple backups before migration
- Test restore procedures
- Incremental migration with validation
- Keep source data until verified

Risk: Downtime Exceeds Window
Mitigation:
- Practice migration in staging
- Have rollback plan ready
- Use blue-green approach when possible
- Prepare communication plan

Risk: Cost Overrun
Mitigation:
- Use cost estimation tools
- Set up billing alerts
- Monitor costs daily
- Right-size early

Risk: Performance Degradation
Mitigation:
- Load testing before cutover
- Monitor key metrics closely
- Have scaling plan ready
- Keep old platform available

Risk: Security Gaps
Mitigation:
- Security review before migration
- Maintain security posture
- Implement least privilege
- Enable audit logging
```

## Migration Tools

```markdown
Multi-Cloud Tools:
- Terraform: IaC across all clouds
- Pulumi: Modern IaC with programming languages
- CloudEndure: Automated migration (AWS)
- Azure Migrate: Microsoft migration service
- Velostrata (Google): Live migration to GCP

Container Tools:
- Velero: K8s backup and migration
- kubectl: Export/import resources
- Helm: Package and deploy applications
- kustomize: K8s configuration management

Database Tools:
- AWS DMS: Database Migration Service
- GCP Database Migration Service
- Azure Database Migration Service
- pg_dump/pg_restore: PostgreSQL
- mysqldump: MySQL
- mongodump/mongorestore: MongoDB

Monitoring Tools:
- Prometheus: Metrics collection
- Grafana: Visualization
- Telegraf: Metrics agent
- Fluentd/Fluent Bit: Log forwarding
```

## Post-Migration Validation

```markdown
Technical Validation:
- [ ] All services running
- [ ] Health checks passing
- [ ] Monitoring data flowing
- [ ] Logs being collected
- [ ] Alerts functioning
- [ ] Backups working
- [ ] DNS resolving correctly
- [ ] SSL certificates valid

Performance Validation:
- [ ] Response times acceptable
- [ ] Throughput meeting SLA
- [ ] Resource utilization normal
- [ ] Database performance good
- [ ] No errors in logs

Cost Validation:
- [ ] Costs match estimates
- [ ] No unexpected charges
- [ ] Billing alerts configured
- [ ] Cost allocation tags applied

Security Validation:
- [ ] IAM policies correct
- [ ] Network security groups configured
- [ ] Encryption enabled
- [ ] Secrets rotated
- [ ] Compliance maintained
- [ ] Audit logs enabled
```

## Best Practices

✅ **Test Everything**: Practice in non-prod first
✅ **Automate**: Use IaC and scripts for repeatability
✅ **Document**: Capture every step and decision
✅ **Monitor**: Watch metrics during and after migration
✅ **Backup**: Multiple backups before any migration
✅ **Validate**: Verify functionality at each step
✅ **Rollback Plan**: Always have a way back
✅ **Communication**: Keep stakeholders informed
✅ **Gradual**: Migrate incrementally when possible
✅ **Optimize**: Don't just replicate, improve

## Anti-Patterns

❌ **Big Bang**: Migrate everything at once
❌ **No Testing**: Skip non-prod validation
❌ **Lift and Shift Only**: Don't optimize during migration
❌ **Ignore Costs**: Fail to estimate target platform costs
❌ **Poor Planning**: Rush without proper design
❌ **Single Point of Failure**: No redundancy during migration
❌ **Inadequate Monitoring**: Can't detect issues quickly
❌ **Forget Cleanup**: Leave resources running in both platforms
❌ **Skip Documentation**: No record of changes made
❌ **Neglect Training**: Team unprepared for new platform

## Notes

- Platform migrations are infrastructure-focused; pair with application migration strategy
- Cloud-neutral design reduces future migration effort
- Multi-cloud strategies increase complexity but reduce vendor lock-in
- Migration is an opportunity to modernize and optimize
- Budget 20-30% more time than estimated
- Keep both platforms running during stabilization period
- Invest in team training for new platform
- Document architecture decisions and rationale
