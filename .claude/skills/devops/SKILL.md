---
name: devops
description: Provides comprehensive DevOps guidance including CI/CD pipeline design, infrastructure as code (Terraform, CloudFormation, Bicep), container orchestration (Docker, Kubernetes), deployment strategies (blue-green, canary, rolling), monitoring and observability, configuration management (Ansible, Chef, Puppet), and cloud platform automation (AWS, GCP, Azure). Produces deployment scripts, pipeline configurations, infrastructure code, and operational procedures. Use when designing CI/CD pipelines, automating infrastructure, containerizing applications, setting up monitoring, implementing deployment strategies, managing configurations, or when users mention DevOps, CI/CD, infrastructure automation, Kubernetes, Docker, Terraform, deployment, monitoring, or cloud operations.
---

# DevOps

## Overview

This skill provides expert guidance on DevOps practices, tools, and automation. It covers the entire software delivery lifecycle from code commit to production deployment, including infrastructure provisioning, container orchestration, monitoring, and incident response.

## Quick Decision Guide

**Choose the appropriate workflow based on the task:**

- **CI/CD Pipeline** → Design automated build, test, and deployment workflows
- **Infrastructure as Code** → Provision and manage cloud resources with code
- **Container Orchestration** → Deploy and manage containerized applications
- **Deployment Strategy** → Implement safe deployment patterns
- **Monitoring & Observability** → Set up metrics, logging, and alerting
- **Configuration Management** → Automate server configuration and management
- **Security & Compliance** → Implement DevSecOps practices

---

## CI/CD Pipeline Design

### Pipeline Structure

Design pipelines with these stages:

**1. Source Stage**
- Trigger on code commit/merge
- Checkout source code
- Validate branch policies

**2. Build Stage**
- Compile code
- Run unit tests
- Generate artifacts
- Create container images
- Version artifacts (semantic versioning)

**3. Test Stage**
- Integration tests
- Security scanning (SAST, dependency scanning)
- Code quality analysis (SonarQube, linting)
- Performance tests

**4. Deploy to Staging**
- Deploy to staging environment
- Run smoke tests
- Run E2E tests
- Load testing (if applicable)

**5. Approval Gate**
- Manual approval for production
- Automated approval criteria
- Change management integration

**6. Deploy to Production**
- Deployment strategy execution
- Health checks
- Rollback capability
- Post-deployment validation

### Pipeline Configuration Examples

**GitHub Actions:**
```yaml
name: CI/CD Pipeline

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Set up environment
        uses: actions/setup-node@v3
        with:
          node-version: '18'
          cache: 'npm'
      
      - name: Install dependencies
        run: npm ci
      
      - name: Run tests
        run: npm test
      
      - name: Build
        run: npm run build
      
      - name: Build Docker image
        run: |
          docker build -t ${{ secrets.REGISTRY }}/app:${{ github.sha }} .
          docker tag ${{ secrets.REGISTRY }}/app:${{ github.sha }} \
                     ${{ secrets.REGISTRY }}/app:latest
      
      - name: Push to registry
        run: |
          echo ${{ secrets.REGISTRY_TOKEN }} | docker login -u ${{ secrets.REGISTRY_USER }} --password-stdin
          docker push ${{ secrets.REGISTRY }}/app:${{ github.sha }}
          docker push ${{ secrets.REGISTRY }}/app:latest

  deploy-staging:
    needs: build
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/develop'
    steps:
      - name: Deploy to staging
        run: |
          kubectl set image deployment/app \
            app=${{ secrets.REGISTRY }}/app:${{ github.sha }} \
            -n staging
      
      - name: Wait for rollout
        run: kubectl rollout status deployment/app -n staging

  deploy-production:
    needs: build
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'
    environment:
      name: production
      url: https://app.example.com
    steps:
      - name: Deploy to production
        run: |
          kubectl set image deployment/app \
            app=${{ secrets.REGISTRY }}/app:${{ github.sha }} \
            -n production
      
      - name: Wait for rollout
        run: kubectl rollout status deployment/app -n production
```

**GitLab CI:**
```yaml
stages:
  - build
  - test
  - deploy-staging
  - deploy-production

variables:
  DOCKER_DRIVER: overlay2
  IMAGE_TAG: $CI_REGISTRY_IMAGE:$CI_COMMIT_SHORT_SHA

build:
  stage: build
  image: docker:latest
  services:
    - docker:dind
  script:
    - docker login -u $CI_REGISTRY_USER -p $CI_REGISTRY_PASSWORD $CI_REGISTRY
    - docker build -t $IMAGE_TAG .
    - docker push $IMAGE_TAG
  only:
    - main
    - develop

test:
  stage: test
  image: node:18
  script:
    - npm ci
    - npm test
    - npm run lint
  coverage: '/Lines\s*:\s*(\d+\.\d+)%/'

security-scan:
  stage: test
  image: aquasec/trivy:latest
  script:
    - trivy image --severity HIGH,CRITICAL $IMAGE_TAG

deploy-staging:
  stage: deploy-staging
  image: bitnami/kubectl:latest
  script:
    - kubectl config use-context staging
    - kubectl set image deployment/app app=$IMAGE_TAG -n staging
    - kubectl rollout status deployment/app -n staging
  only:
    - develop
  environment:
    name: staging
    url: https://staging.example.com

deploy-production:
  stage: deploy-production
  image: bitnami/kubectl:latest
  script:
    - kubectl config use-context production
    - kubectl set image deployment/app app=$IMAGE_TAG -n production
    - kubectl rollout status deployment/app -n production
  only:
    - main
  when: manual
  environment:
    name: production
    url: https://app.example.com
```

### Best Practices

- **Fast feedback**: Keep pipeline execution under 10 minutes
- **Fail fast**: Run quick tests first, expensive tests later
- **Parallel execution**: Run independent jobs concurrently
- **Artifact caching**: Cache dependencies and build artifacts
- **Secrets management**: Use platform secret stores, never commit secrets
- **Pipeline as code**: Version control all pipeline definitions
- **Idempotent scripts**: Scripts should be safely re-runnable

---

## Infrastructure as Code

### Terraform

**Project Structure:**
```
terraform/
├── environments/
│   ├── dev/
│   │   ├── main.tf
│   │   ├── variables.tf
│   │   └── terraform.tfvars
│   ├── staging/
│   └── production/
├── modules/
│   ├── vpc/
│   ├── eks/
│   ├── rds/
│   └── s3/
└── shared/
    └── backend.tf
```

**Example Module (EKS Cluster):**
```hcl
# modules/eks/main.tf
resource "aws_eks_cluster" "main" {
  name     = var.cluster_name
  role_arn = aws_iam_role.cluster.arn
  version  = var.kubernetes_version

  vpc_config {
    subnet_ids              = var.subnet_ids
    endpoint_private_access = true
    endpoint_public_access  = var.public_access
    security_group_ids      = [aws_security_group.cluster.id]
  }

  enabled_cluster_log_types = ["api", "audit", "authenticator", "controllerManager", "scheduler"]

  tags = merge(
    var.tags,
    {
      Name = var.cluster_name
    }
  )
}

resource "aws_eks_node_group" "main" {
  cluster_name    = aws_eks_cluster.main.name
  node_group_name = "${var.cluster_name}-node-group"
  node_role_arn   = aws_iam_role.node.arn
  subnet_ids      = var.subnet_ids

  scaling_config {
    desired_size = var.desired_size
    max_size     = var.max_size
    min_size     = var.min_size
  }

  instance_types = var.instance_types
  capacity_type  = var.capacity_type

  update_config {
    max_unavailable = 1
  }

  tags = var.tags
}

# modules/eks/variables.tf
variable "cluster_name" {
  description = "Name of the EKS cluster"
  type        = string
}

variable "kubernetes_version" {
  description = "Kubernetes version"
  type        = string
  default     = "1.28"
}

variable "subnet_ids" {
  description = "List of subnet IDs"
  type        = list(string)
}

variable "desired_size" {
  description = "Desired number of nodes"
  type        = number
  default     = 3
}

variable "max_size" {
  description = "Maximum number of nodes"
  type        = number
  default     = 5
}

variable "min_size" {
  description = "Minimum number of nodes"
  type        = number
  default     = 1
}

# modules/eks/outputs.tf
output "cluster_id" {
  value = aws_eks_cluster.main.id
}

output "cluster_endpoint" {
  value = aws_eks_cluster.main.endpoint
}

output "cluster_security_group_id" {
  value = aws_security_group.cluster.id
}
```

**Environment Configuration:**
```hcl
# environments/production/main.tf
terraform {
  required_version = ">= 1.5"
  
  backend "s3" {
    bucket         = "company-terraform-state"
    key            = "production/eks/terraform.tfstate"
    region         = "us-east-1"
    encrypt        = true
    dynamodb_table = "terraform-locks"
  }
}

provider "aws" {
  region = var.aws_region
  
  default_tags {
    tags = {
      Environment = "production"
      ManagedBy   = "Terraform"
      Project     = "main-app"
    }
  }
}

module "vpc" {
  source = "../../modules/vpc"
  
  vpc_cidr           = "10.0.0.0/16"
  availability_zones = ["us-east-1a", "us-east-1b", "us-east-1c"]
  environment        = "production"
}

module "eks" {
  source = "../../modules/eks"
  
  cluster_name       = "production-cluster"
  kubernetes_version = "1.28"
  subnet_ids         = module.vpc.private_subnet_ids
  desired_size       = 5
  max_size           = 10
  min_size           = 3
  instance_types     = ["t3.large"]
}
```

### Terraform Best Practices

- **State management**: Use remote state with locking (S3 + DynamoDB)
- **Module reusability**: Create reusable modules for common resources
- **Variable validation**: Add validation rules to variables
- **Output values**: Expose useful outputs for other modules
- **Workspace separation**: Use workspaces or directories for environments
- **Plan before apply**: Always review `terraform plan` before applying
- **Version pinning**: Pin provider and module versions

---

## Container Orchestration

### Kubernetes Deployments

**Application Deployment:**
```yaml
# deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: web-app
  namespace: production
  labels:
    app: web-app
    version: v1.0.0
spec:
  replicas: 3
  strategy:
    type: RollingUpdate
    rollingUpdate:
      maxSurge: 1
      maxUnavailable: 0
  selector:
    matchLabels:
      app: web-app
  template:
    metadata:
      labels:
        app: web-app
        version: v1.0.0
    spec:
      containers:
      - name: web-app
        image: registry.example.com/web-app:v1.0.0
        ports:
        - containerPort: 8080
          name: http
        env:
        - name: DATABASE_URL
          valueFrom:
            secretKeyRef:
              name: app-secrets
              key: database-url
        - name: REDIS_URL
          valueFrom:
            configMapKeyRef:
              name: app-config
              key: redis-url
        resources:
          requests:
            memory: "256Mi"
            cpu: "250m"
          limits:
            memory: "512Mi"
            cpu: "500m"
        livenessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
          timeoutSeconds: 5
          failureThreshold: 3
        readinessProbe:
          httpGet:
            path: /ready
            port: 8080
          initialDelaySeconds: 10
          periodSeconds: 5
          timeoutSeconds: 3
          failureThreshold: 3
      affinity:
        podAntiAffinity:
          preferredDuringSchedulingIgnoredDuringExecution:
          - weight: 100
            podAffinityTerm:
              labelSelector:
                matchExpressions:
                - key: app
                  operator: In
                  values:
                  - web-app
              topologyKey: kubernetes.io/hostname

---
apiVersion: v1
kind: Service
metadata:
  name: web-app
  namespace: production
spec:
  type: ClusterIP
  selector:
    app: web-app
  ports:
  - port: 80
    targetPort: 8080
    name: http

---
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: web-app
  namespace: production
  annotations:
    cert-manager.io/cluster-issuer: letsencrypt-prod
    nginx.ingress.kubernetes.io/ssl-redirect: "true"
spec:
  ingressClassName: nginx
  tls:
  - hosts:
    - app.example.com
    secretName: web-app-tls
  rules:
  - host: app.example.com
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: web-app
            port:
              number: 80

---
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: web-app
  namespace: production
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: web-app
  minReplicas: 3
  maxReplicas: 10
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
```

### Helm Charts

**Chart Structure:**
```
web-app/
├── Chart.yaml
├── values.yaml
├── templates/
│   ├── deployment.yaml
│   ├── service.yaml
│   ├── ingress.yaml
│   ├── configmap.yaml
│   ├── secret.yaml
│   └── hpa.yaml
└── values/
    ├── dev.yaml
    ├── staging.yaml
    └── production.yaml
```

**Chart.yaml:**
```yaml
apiVersion: v2
name: web-app
description: Web application Helm chart
type: application
version: 1.0.0
appVersion: "1.0.0"
```

**values.yaml:**
```yaml
replicaCount: 3

image:
  repository: registry.example.com/web-app
  pullPolicy: IfNotPresent
  tag: ""  # Overrides appVersion

service:
  type: ClusterIP
  port: 80
  targetPort: 8080

ingress:
  enabled: true
  className: nginx
  annotations:
    cert-manager.io/cluster-issuer: letsencrypt-prod
  hosts:
    - host: app.example.com
      paths:
        - path: /
          pathType: Prefix
  tls:
    - secretName: web-app-tls
      hosts:
        - app.example.com

resources:
  requests:
    memory: "256Mi"
    cpu: "250m"
  limits:
    memory: "512Mi"
    cpu: "500m"

autoscaling:
  enabled: true
  minReplicas: 3
  maxReplicas: 10
  targetCPUUtilizationPercentage: 70
  targetMemoryUtilizationPercentage: 80
```

---

## Deployment Strategies

### Blue-Green Deployment

**Kubernetes Implementation:**
```yaml
# Blue deployment (current)
apiVersion: apps/v1
kind: Deployment
metadata:
  name: app-blue
spec:
  replicas: 3
  selector:
    matchLabels:
      app: myapp
      version: blue
  template:
    metadata:
      labels:
        app: myapp
        version: blue
    spec:
      containers:
      - name: app
        image: myapp:v1.0.0

---
# Green deployment (new)
apiVersion: apps/v1
kind: Deployment
metadata:
  name: app-green
spec:
  replicas: 3
  selector:
    matchLabels:
      app: myapp
      version: green
  template:
    metadata:
      labels:
        app: myapp
        version: green
    spec:
      containers:
      - name: app
        image: myapp:v2.0.0

---
# Service (switch between blue/green)
apiVersion: v1
kind: Service
metadata:
  name: app-service
spec:
  selector:
    app: myapp
    version: blue  # Change to 'green' to switch traffic
  ports:
  - port: 80
    targetPort: 8080
```

**Switch Script:**
```bash
#!/bin/bash
# Switch traffic from blue to green

# Deploy green
kubectl apply -f app-green.yaml

# Wait for green to be ready
kubectl rollout status deployment/app-green

# Run smoke tests
./run-smoke-tests.sh http://app-service-green.default.svc.cluster.local

# Switch service to green
kubectl patch service app-service -p '{"spec":{"selector":{"version":"green"}}}'

# Monitor for issues
sleep 300

# If successful, scale down blue
kubectl scale deployment app-blue --replicas=0
```

### Canary Deployment

**Using Istio:**
```yaml
apiVersion: v1
kind: Service
metadata:
  name: app
spec:
  selector:
    app: myapp
  ports:
  - port: 80
    targetPort: 8080

---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: app-v1
spec:
  replicas: 9
  selector:
    matchLabels:
      app: myapp
      version: v1
  template:
    metadata:
      labels:
        app: myapp
        version: v1
    spec:
      containers:
      - name: app
        image: myapp:v1.0.0

---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: app-v2
spec:
  replicas: 1
  selector:
    matchLabels:
      app: myapp
      version: v2
  template:
    metadata:
      labels:
        app: myapp
        version: v2
    spec:
      containers:
      - name: app
        image: myapp:v2.0.0

---
apiVersion: networking.istio.io/v1beta1
kind: VirtualService
metadata:
  name: app
spec:
  hosts:
  - app
  http:
  - match:
    - headers:
        canary:
          exact: "true"
    route:
    - destination:
        host: app
        subset: v2
  - route:
    - destination:
        host: app
        subset: v1
      weight: 90
    - destination:
        host: app
        subset: v2
      weight: 10

---
apiVersion: networking.istio.io/v1beta1
kind: DestinationRule
metadata:
  name: app
spec:
  host: app
  subsets:
  - name: v1
    labels:
      version: v1
  - name: v2
    labels:
      version: v2
```

---

## Monitoring & Observability

### Prometheus Setup

**Prometheus Configuration:**
```yaml
# prometheus-config.yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: prometheus-config
  namespace: monitoring
data:
  prometheus.yml: |
    global:
      scrape_interval: 15s
      evaluation_interval: 15s
    
    alerting:
      alertmanagers:
      - static_configs:
        - targets: ['alertmanager:9093']
    
    rule_files:
      - '/etc/prometheus/rules/*.yml'
    
    scrape_configs:
      - job_name: 'kubernetes-pods'
        kubernetes_sd_configs:
        - role: pod
        relabel_configs:
        - source_labels: [__meta_kubernetes_pod_annotation_prometheus_io_scrape]
          action: keep
          regex: true
        - source_labels: [__meta_kubernetes_pod_annotation_prometheus_io_path]
          action: replace
          target_label: __metrics_path__
          regex: (.+)
        - source_labels: [__address__, __meta_kubernetes_pod_annotation_prometheus_io_port]
          action: replace
          regex: ([^:]+)(?::\d+)?;(\d+)
          replacement: $1:$2
          target_label: __address__
      
      - job_name: 'kubernetes-nodes'
        kubernetes_sd_configs:
        - role: node
        scheme: https
        tls_config:
          ca_file: /var/run/secrets/kubernetes.io/serviceaccount/ca.crt
        bearer_token_file: /var/run/secrets/kubernetes.io/serviceaccount/token
```

**Alert Rules:**
```yaml
# alert-rules.yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: prometheus-rules
  namespace: monitoring
data:
  app-alerts.yml: |
    groups:
    - name: application
      interval: 30s
      rules:
      - alert: HighErrorRate
        expr: |
          rate(http_requests_total{status=~"5.."}[5m]) 
          / rate(http_requests_total[5m]) > 0.05
        for: 5m
        labels:
          severity: critical
        annotations:
          summary: "High error rate detected"
          description: "Error rate is {{ $value }}% on {{ $labels.instance }}"
      
      - alert: HighLatency
        expr: |
          histogram_quantile(0.95, 
            rate(http_request_duration_seconds_bucket[5m])
          ) > 1
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "High latency detected"
          description: "95th percentile latency is {{ $value }}s"
      
      - alert: PodCrashLooping
        expr: |
          rate(kube_pod_container_status_restarts_total[15m]) > 0
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "Pod is crash looping"
          description: "Pod {{ $labels.pod }} is restarting frequently"
```

### Grafana Dashboards

**Application Dashboard JSON (key sections):**
```json
{
  "dashboard": {
    "title": "Application Metrics",
    "panels": [
      {
        "title": "Request Rate",
        "targets": [
          {
            "expr": "rate(http_requests_total[5m])"
          }
        ]
      },
      {
        "title": "Error Rate",
        "targets": [
          {
            "expr": "rate(http_requests_total{status=~\"5..\"}[5m])"
          }
        ]
      },
      {
        "title": "Latency (p95)",
        "targets": [
          {
            "expr": "histogram_quantile(0.95, rate(http_request_duration_seconds_bucket[5m]))"
          }
        ]
      }
    ]
  }
}
```

### ELK Stack for Logging

**Filebeat Configuration:**
```yaml
# filebeat-config.yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: filebeat-config
  namespace: logging
data:
  filebeat.yml: |
    filebeat.inputs:
    - type: container
      paths:
        - /var/log/containers/*.log
      processors:
        - add_kubernetes_metadata:
            host: ${NODE_NAME}
            matchers:
            - logs_path:
                logs_path: "/var/log/containers/"
    
    output.elasticsearch:
      hosts: ['${ELASTICSEARCH_HOST:elasticsearch}:${ELASTICSEARCH_PORT:9200}']
      username: ${ELASTICSEARCH_USERNAME}
      password: ${ELASTICSEARCH_PASSWORD}
    
    setup.kibana:
      host: "${KIBANA_HOST:kibana}:${KIBANA_PORT:5601}"
```

---

## Security Best Practices

### Container Security

**Dockerfile Security:**
```dockerfile
# Use specific version tags, not 'latest'
FROM node:18.17.0-alpine3.18

# Run as non-root user
RUN addgroup -g 1001 -S appuser && \
    adduser -u 1001 -S appuser -G appuser

# Set working directory
WORKDIR /app

# Copy dependency files first (layer caching)
COPY --chown=appuser:appuser package*.json ./

# Install dependencies
RUN npm ci --only=production && \
    npm cache clean --force

# Copy application code
COPY --chown=appuser:appuser . .

# Remove unnecessary files
RUN rm -rf .git tests docs

# Switch to non-root user
USER appuser

# Expose port (documentary, doesn't publish)
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD node healthcheck.js

# Run application
CMD ["node", "server.js"]
```

### Kubernetes Security

**Pod Security Standards:**
```yaml
apiVersion: v1
kind: Pod
metadata:
  name: secure-app
spec:
  securityContext:
    runAsNonRoot: true
    runAsUser: 1001
    fsGroup: 1001
    seccompProfile:
      type: RuntimeDefault
  containers:
  - name: app
    image: myapp:latest
    securityContext:
      allowPrivilegeEscalation: false
      readOnlyRootFilesystem: true
      capabilities:
        drop:
        - ALL
    volumeMounts:
    - name: tmp
      mountPath: /tmp
    - name: cache
      mountPath: /app/cache
  volumes:
  - name: tmp
    emptyDir: {}
  - name: cache
    emptyDir: {}
```

### Secrets Management

**Using External Secrets Operator:**
```yaml
apiVersion: external-secrets.io/v1beta1
kind: SecretStore
metadata:
  name: aws-secrets-manager
  namespace: production
spec:
  provider:
    aws:
      service: SecretsManager
      region: us-east-1
      auth:
        jwt:
          serviceAccountRef:
            name: external-secrets-sa

---
apiVersion: external-secrets.io/v1beta1
kind: ExternalSecret
metadata:
  name: app-secrets
  namespace: production
spec:
  refreshInterval: 1h
  secretStoreRef:
    name: aws-secrets-manager
    kind: SecretStore
  target:
    name: app-secrets
    creationPolicy: Owner
  data:
  - secretKey: database-url
    remoteRef:
      key: prod/app/database
      property: url
  - secretKey: api-key
    remoteRef:
      key: prod/app/api
      property: key
```

---

## Configuration Management

### Ansible Playbook Example

```yaml
# playbook.yml
---
- name: Configure web servers
  hosts: webservers
  become: yes
  vars:
    app_version: "1.0.0"
    app_port: 8080
  
  tasks:
    - name: Update package cache
      apt:
        update_cache: yes
        cache_valid_time: 3600
    
    - name: Install required packages
      apt:
        name:
          - nginx
          - nodejs
          - npm
        state: present
    
    - name: Create app user
      user:
        name: appuser
        shell: /bin/bash
        create_home: yes
        state: present
    
    - name: Create app directory
      file:
        path: /opt/app
        state: directory
        owner: appuser
        group: appuser
        mode: '0755'
    
    - name: Deploy application
      copy:
        src: "dist/app-{{ app_version }}.tar.gz"
        dest: "/opt/app/app.tar.gz"
        owner: appuser
        group: appuser
      notify: Restart application
    
    - name: Extract application
      unarchive:
        src: "/opt/app/app.tar.gz"
        dest: /opt/app
        remote_src: yes
        owner: appuser
        group: appuser
    
    - name: Configure nginx
      template:
        src: templates/nginx.conf.j2
        dest: /etc/nginx/sites-available/app
        mode: '0644'
      notify: Reload nginx
    
    - name: Enable nginx site
      file:
        src: /etc/nginx/sites-available/app
        dest: /etc/nginx/sites-enabled/app
        state: link
      notify: Reload nginx
    
    - name: Configure systemd service
      template:
        src: templates/app.service.j2
        dest: /etc/systemd/system/app.service
        mode: '0644'
      notify: Restart application
  
  handlers:
    - name: Reload nginx
      systemd:
        name: nginx
        state: reloaded
    
    - name: Restart application
      systemd:
        name: app
        state: restarted
        daemon_reload: yes
```

---

## Best Practices Summary

### CI/CD
- Keep pipelines fast (< 10 minutes)
- Fail fast with quick tests first
- Use pipeline as code (version controlled)
- Implement proper secret management
- Enable artifact caching
- Parallelize independent jobs

### Infrastructure as Code
- Use remote state with locking
- Create reusable modules
- Pin versions (providers, modules)
- Always review plan before apply
- Implement proper tagging strategy
- Document resource dependencies

### Container Orchestration
- Set resource requests and limits
- Implement health checks (liveness/readiness)
- Use pod anti-affinity for HA
- Enable horizontal pod autoscaling
- Use init containers for setup tasks
- Implement proper logging and monitoring

### Deployment
- Use rolling updates with zero downtime
- Implement proper health checks
- Enable rollback capabilities
- Use canary/blue-green for critical apps
- Test thoroughly in staging
- Monitor post-deployment metrics

### Security
- Run containers as non-root
- Use read-only root filesystems
- Scan images for vulnerabilities
- Implement network policies
- Use secrets management solutions
- Enable pod security standards
- Implement least privilege access

### Monitoring
- Collect metrics (RED/USE methods)
- Implement structured logging
- Set up meaningful alerts
- Create actionable dashboards
- Monitor SLIs/SLOs
- Implement distributed tracing

---

## Common Commands

### Kubernetes
```bash
# Get resources
kubectl get pods -n namespace
kubectl get deployments -n namespace
kubectl get services -n namespace

# Describe resources
kubectl describe pod pod-name -n namespace
kubectl describe deployment deployment-name -n namespace

# Logs
kubectl logs pod-name -n namespace
kubectl logs -f pod-name -n namespace --tail=100
kubectl logs pod-name -c container-name -n namespace

# Execute commands in pod
kubectl exec -it pod-name -n namespace -- /bin/sh
kubectl exec pod-name -n namespace -- command

# Port forwarding
kubectl port-forward pod-name 8080:8080 -n namespace
kubectl port-forward service/service-name 8080:80 -n namespace

# Apply/Delete resources
kubectl apply -f manifest.yaml
kubectl delete -f manifest.yaml

# Scaling
kubectl scale deployment deployment-name --replicas=5 -n namespace

# Rollout management
kubectl rollout status deployment/deployment-name -n namespace
kubectl rollout history deployment/deployment-name -n namespace
kubectl rollout undo deployment/deployment-name -n namespace
```

### Docker
```bash
# Build
docker build -t image-name:tag .
docker build -t image-name:tag -f Dockerfile.prod .

# Run
docker run -d -p 8080:8080 --name container-name image-name:tag
docker run -it --rm image-name:tag /bin/sh

# Manage containers
docker ps
docker ps -a
docker stop container-name
docker start container-name
docker restart container-name
docker rm container-name

# Manage images
docker images
docker rmi image-name:tag
docker pull image-name:tag
docker push image-name:tag

# Logs and debugging
docker logs container-name
docker logs -f container-name --tail=100
docker exec -it container-name /bin/sh
docker inspect container-name
```

### Terraform
```bash
# Initialize
terraform init
terraform init -upgrade

# Plan and apply
terraform plan
terraform plan -out=plan.tfplan
terraform apply
terraform apply plan.tfplan
terraform apply -auto-approve

# Destroy
terraform destroy
terraform destroy -auto-approve

# State management
terraform state list
terraform state show resource-name
terraform state rm resource-name

# Workspace management
terraform workspace list
terraform workspace new environment
terraform workspace select environment

# Import existing resources
terraform import resource-type.name resource-id
```

---

## Troubleshooting Guide

### Pod Issues

**Pod not starting:**
```bash
# Check pod status and events
kubectl describe pod pod-name -n namespace

# Common causes:
# - ImagePullBackOff: Check image name/tag, registry credentials
# - CrashLoopBackOff: Check logs for application errors
# - Pending: Check resource availability, node selectors
```

**Application errors:**
```bash
# Check application logs
kubectl logs pod-name -n namespace --tail=100

# Check previous container logs (if restarted)
kubectl logs pod-name -n namespace --previous

# Check events
kubectl get events -n namespace --sort-by='.lastTimestamp'
```

### Deployment Issues

**Rollout stuck:**
```bash
# Check rollout status
kubectl rollout status deployment/deployment-name -n namespace

# Check replica set status
kubectl get rs -n namespace

# Rollback if needed
kubectl rollout undo deployment/deployment-name -n namespace
```

### Network Issues

**Service not reachable:**
```bash
# Check service endpoints
kubectl get endpoints service-name -n namespace

# Test from within cluster
kubectl run test-pod --rm -it --image=alpine -- /bin/sh
# Inside pod: wget -O- http://service-name.namespace.svc.cluster.local

# Check network policies
kubectl get networkpolicies -n namespace
```

---

## Additional Resources

For detailed platform-specific guidance, cloud provider documentation, and advanced patterns, consult official documentation:

- **Kubernetes**: https://kubernetes.io/docs/
- **Docker**: https://docs.docker.com/
- **Terraform**: https://www.terraform.io/docs/
- **Helm**: https://helm.sh/docs/
- **Prometheus**: https://prometheus.io/docs/
- **AWS**: https://docs.aws.amazon.com/
- **GCP**: https://cloud.google.com/docs
- **Azure**: https://docs.microsoft.com/azure/
