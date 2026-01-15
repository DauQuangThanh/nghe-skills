---
name: devops
description: Provides comprehensive DevOps guidance including CI/CD pipeline design, infrastructure as code (Terraform, CloudFormation, Bicep), container orchestration (Docker, Kubernetes), deployment strategies (blue-green, canary, rolling), monitoring and observability, configuration management (Ansible, Chef, Puppet), and cloud platform automation (AWS, GCP, Azure). Produces deployment scripts, pipeline configurations, infrastructure code, and operational procedures. Use when designing CI/CD pipelines, automating infrastructure, containerizing applications, setting up monitoring, implementing deployment strategies, managing configurations, or when users mention DevOps, CI/CD, infrastructure automation, Kubernetes, Docker, Terraform, deployment, monitoring, or cloud operations.
---

# DevOps

## Overview

Expert guidance on DevOps practices, tools, and automation. Covers the entire software delivery lifecycle from code commit to production deployment, including infrastructure provisioning, container orchestration, monitoring, and incident response.

## Core Capabilities

1. **CI/CD Pipeline Design** - Automated build, test, and deployment workflows
2. **Infrastructure as Code** - Cloud resource provisioning with Terraform, CloudFormation, Bicep
3. **Container Orchestration** - Docker and Kubernetes deployment patterns
4. **Deployment Strategies** - Blue-green, canary, and rolling deployments
5. **Monitoring & Observability** - Metrics, logging, alerting with Prometheus, Grafana, ELK
6. **Configuration Management** - Ansible, Chef, Puppet automation
7. **Security & Compliance** - DevSecOps practices and container security

## Quick Decision Guide

- **CI/CD Pipeline** → Design automated build, test, and deployment workflows
- **Infrastructure as Code** → Provision and manage cloud resources with code
- **Container Orchestration** → Deploy and manage containerized applications
- **Deployment Strategy** → Implement safe deployment patterns
- **Monitoring & Observability** → Set up metrics, logging, and alerting
- **Configuration Management** → Automate server configuration and management
- **Security & Compliance** → Implement DevSecOps practices

## Detailed Topics

Load reference files based on specific needs:

- **CI/CD Pipeline Design**: See [cicd-pipeline-design.md](references/cicd-pipeline-design.md) when:
  - Designing GitHub Actions, GitLab CI, or Jenkins pipelines
  - Setting up automated build, test, deploy workflows
  - Need pipeline configuration examples

- **Infrastructure as Code**: See [infrastructure-as-code.md](references/infrastructure-as-code.md) when:
  - Working with Terraform, CloudFormation, or Bicep
  - Provisioning cloud resources (AWS, GCP, Azure)
  - Need IaC patterns and module examples

- **Container Orchestration**: See [container-orchestration.md](references/container-orchestration.md) when:
  - Deploying applications to Kubernetes
  - Creating Docker containers
  - Need K8s manifests, Helm charts

- **Deployment Strategies**: See [deployment-strategies.md](references/deployment-strategies.md) when:
  - Implementing blue-green deployments
  - Setting up canary releases
  - Need rolling update patterns

- **Monitoring & Observability**: See [monitoring-and-observability.md](references/monitoring-and-observability.md) when:
  - Setting up Prometheus, Grafana
  - Configuring ELK stack for logging
  - Need monitoring dashboards and alerts

- **Security Best Practices**: See [security-best-practices.md](references/security-best-practices.md) when:
  - Implementing DevSecOps
  - Securing containers and pipelines
  - Need security scanning and compliance

- **Configuration Management**: See [configuration-management.md](references/configuration-management.md) when:
  - Using Ansible, Chef, or Puppet
  - Automating server configuration
  - Need infrastructure automation patterns

## Critical Tips

1. **Automate everything** - Infrastructure, testing, deployment, monitoring
2. **Version control all configurations** - IaC, pipelines, configs
3. **Implement proper security** - Secrets management, scanning, least privilege
4. **Monitor continuously** - Metrics, logs, traces, alerts
5. **Plan for failure** - Rollback strategies, disaster recovery, backups
