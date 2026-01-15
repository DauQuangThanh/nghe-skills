---
name: backend-coding
description: Expert backend development guidance covering Node.js, Python, Java, Go, API design (REST/GraphQL/gRPC), database patterns, authentication, caching, message queues, microservices, and testing. Produces production-ready, scalable, and secure backend code with industry best practices. Use when building APIs, implementing business logic, designing data models, integrating services, or when users mention backend development, server-side code, APIs, databases, or microservices.
---

# Backend Coding

## Overview

Expert guidance for modern backend development, covering frameworks, architectural patterns, best practices, and implementation techniques for building production-ready server-side applications.

## Core Capabilities

1. **API Development** - RESTful, GraphQL, gRPC design and implementation
2. **Framework Expertise** - Node.js, Python, Java, Go frameworks
3. **Database Integration** - SQL, NoSQL, ORMs, migrations, optimization
4. **Authentication & Authorization** - JWT, OAuth, RBAC, security
5. **Microservices Architecture** - Service communication, API gateways
6. **Message Queues** - RabbitMQ, Kafka, event-driven patterns
7. **Caching** - Redis, cache strategies, performance optimization
8. **Testing** - Unit, integration, API testing best practices

## Quick Start

For a simple Express.js API:
```javascript
const express = require('express');
const app = express();
app.use(express.json());

app.get('/api/users/:id', async (req, res) => {
  const user = await getUserById(req.params.id);
  res.json(user);
});
```

## Framework Selection

- **Node.js**: See [nodejs-development.md](references/nodejs-development.md) - Express, NestJS, Fastify
- **Python**: See [python-development.md](references/python-development.md) - Django, Flask, FastAPI
- **Authentication**: See [authentication-and-authorization.md](references/authentication-and-authorization.md) - JWT, OAuth, RBAC
- **Database**: See [database-patterns.md](references/database-patterns.md) - SQL, NoSQL, ORMs
- **Caching**: See [caching-strategies.md](references/caching-strategies.md) - Redis, strategies

Load reference files based on specific needs:
- Load language-specific patterns when user mentions framework or language
- Load authentication patterns when implementing auth/security
- Load database patterns when working with data persistence
- Load caching strategies when optimizing performance

## Critical Tips

1. **Security first** - Validate inputs, use parameterized queries, hash passwords
2. **Error handling** - Use proper HTTP status codes, handle errors gracefully
3. **Testing** - Write tests before deploying, aim for >80% coverage
4. **Documentation** - Document APIs using OpenAPI/Swagger
5. **Performance** - Use caching, optimize queries, monitor bottlenecks
