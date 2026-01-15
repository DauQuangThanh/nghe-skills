---
name: frontend-coding
description: Expert frontend development guidance covering React, Vue, Angular, TypeScript, state management, component architecture, performance optimization, accessibility, testing, and modern web APIs. Produces production-ready, maintainable, and performant frontend code with best practices. Use when building web applications, implementing UI components, managing application state, optimizing performance, or when users mention React, Vue, Angular, TypeScript, hooks, state management, components, or frontend development.
---


# Frontend Coding

## Overview

Expert frontend development guidance covering React, Vue, Angular, TypeScript, state management, component architecture, performance optimization, accessibility, testing, and modern web APIs.


## Core Capabilities

1. **Framework Expertise** - React, Vue, Angular, Svelte
2. **TypeScript** - Type-safe development
3. **State Management** - Redux, Vuex, Pinia, Context API
4. **Component Patterns** - Composition, hooks, composables
5. **Performance** - Code splitting, lazy loading, optimization
6. **Accessibility** - WCAG compliance, ARIA
7. **Testing** - Jest, Testing Library, Cypress


## Quick Start

**React Component Example:**

```tsx
import React, { useState, useEffect } from 'react';

interface User {
  id: number;
  name: string;
}

export const UserList: React.FC = () => {
  const [users, setUsers] = useState<User[]>([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    fetch('/api/users')
      .then(res => res.json())
      .then(data => {
        setUsers(data);
        setLoading(false);
      });
  }, []);

  if (loading) return <div>Loading...</div>;

  return (
    <ul>
      {users.map(user => (
        <li key={user.id}>{user.name}</li>
      ))}
    </ul>
  );
};
```


## Critical Tips

1. **Use TypeScript** - Type safety prevents runtime errors
2. **Component composition** - Build reusable, composable components
3. **Performance matters** - Memoization, lazy loading, code splitting
4. **Accessibility first** - WCAG compliance from the start
5. **Test thoroughly** - Unit, integration, E2E testing

## Detailed Topics

Load reference files based on specific needs:

- **Activation Guidelines**: See [activation-guidelines.md](references/activation-guidelines.md) when:
  - Working with related functionality
  - Need specific patterns or examples
  - Require detailed guidance

- **Best Practices**: See [best-practices.md](references/best-practices.md) when:
  - Working with related functionality
  - Need specific patterns or examples
  - Require detailed guidance

- **Component Patterns**: See [component-patterns.md](references/component-patterns.md) when:
  - Working with related functionality
  - Need specific patterns or examples
  - Require detailed guidance

- **Core Capabilities**: See [core-capabilities.md](references/core-capabilities.md) when:
  - Working with related functionality
  - Need specific patterns or examples
  - Require detailed guidance

- **Performance Testing**: See [performance-testing.md](references/performance-testing.md) when:
  - Working with related functionality
  - Need specific patterns or examples
  - Require detailed guidance

- **React Development**: See [react-development.md](references/react-development.md) when:
  - Working with related functionality
  - Need specific patterns or examples
  - Require detailed guidance

- **React Patterns**: See [react-patterns.md](references/react-patterns.md) when:
  - Working with related functionality
  - Need specific patterns or examples
  - Require detailed guidance

- **State Management**: See [state-management.md](references/state-management.md) when:
  - Working with related functionality
  - Need specific patterns or examples
  - Require detailed guidance

- **Typescript Best Practices**: See [typescript-best-practices.md](references/typescript-best-practices.md) when:
  - Working with related functionality
  - Need specific patterns or examples
  - Require detailed guidance

- **Vue Patterns**: See [vue-patterns.md](references/vue-patterns.md) when:
  - Working with related functionality
  - Need specific patterns or examples
  - Require detailed guidance

- **Vuejs Development**: See [vuejs-development.md](references/vuejs-development.md) when:
  - Working with related functionality
  - Need specific patterns or examples
  - Require detailed guidance

