---
name: frontend-coding
description: Expert frontend development guidance covering React, Vue, Angular, TypeScript, state management, component architecture, performance optimization, accessibility, testing, and modern web APIs. Produces production-ready, maintainable, and performant frontend code with best practices. Use when building web applications, implementing UI components, managing application state, optimizing performance, or when users mention React, Vue, Angular, TypeScript, hooks, state management, components, or frontend development.
license: MIT
metadata:
  author: Dau Quang Thanh
  version: "1.0"
  category: development
---

# Frontend Coding Skill

This skill provides expert guidance for modern frontend development, covering frameworks, patterns, best practices, and implementation techniques for building production-ready web applications.

## Core Capabilities

When activated, this skill enables you to:

1. **React Development**
   - Functional components and hooks
   - Custom hooks and composition
   - Context API and state management
   - Performance optimization
   - Server components and Next.js

2. **Vue.js Development**
   - Composition API and Options API
   - Composables and reactivity
   - Vue Router and Pinia
   - Performance optimization
   - Nuxt.js and SSR

3. **Angular Development**
   - Components and directives
   - Services and dependency injection
   - RxJS and reactive programming
   - Modules and lazy loading
   - Angular Universal and SSR

4. **TypeScript Best Practices**
   - Type safety and inference
   - Generics and utility types
   - Advanced type patterns
   - Type guards and assertions
   - Integration with frameworks

5. **State Management**
   - Redux, Zustand, Jotai
   - Pinia, Vuex
   - NgRx, Akita
   - Context API and local state
   - Server state with React Query/SWR

6. **Component Architecture**
   - Design patterns and composition
   - Atomic design principles
   - Component libraries
   - Prop drilling solutions
   - Code organization

7. **Performance Optimization**
   - Code splitting and lazy loading
   - Memoization and optimization
   - Virtual scrolling
   - Bundle size optimization
   - Rendering performance

8. **Modern Web APIs**
   - Fetch and Axios
   - Web Workers
   - Service Workers and PWA
   - WebSockets and real-time
   - Browser storage

## React Development

### Functional Components and Hooks

**Modern React Component**:
```typescript
import React, { useState, useEffect, useMemo, useCallback } from 'react';

interface User {
  id: string;
  name: string;
  email: string;
}

interface UserProfileProps {
  userId: string;
  onUpdate?: (user: User) => void;
}

export const UserProfile: React.FC<UserProfileProps> = ({ userId, onUpdate }) => {
  const [user, setUser] = useState<User | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<Error | null>(null);

  // Fetch user data
  useEffect(() => {
    const fetchUser = async () => {
      try {
        setLoading(true);
        const response = await fetch(`/api/users/${userId}`);
        if (!response.ok) throw new Error('Failed to fetch user');
        const data = await response.json();
        setUser(data);
      } catch (err) {
        setError(err instanceof Error ? err : new Error('Unknown error'));
      } finally {
        setLoading(false);
      }
    };

    fetchUser();
  }, [userId]);

  // Memoized computed value
  const displayName = useMemo(() => {
    if (!user) return '';
    return `${user.name} (${user.email})`;
  }, [user]);

  // Memoized callback
  const handleUpdate = useCallback((updates: Partial<User>) => {
    if (!user) return;
    
    const updatedUser = { ...user, ...updates };
    setUser(updatedUser);
    onUpdate?.(updatedUser);
  }, [user, onUpdate]);

  if (loading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;
  if (!user) return <div>User not found</div>;

  return (
    <div className="user-profile">
      <h2>{displayName}</h2>
      <button onClick={() => handleUpdate({ name: 'Updated Name' })}>
        Update Name
      </button>
    </div>
  );
};
```

**Custom Hooks**:
```typescript
// useAsync - Generic async operation hook
function useAsync<T>(
  asyncFunction: () => Promise<T>,
  dependencies: React.DependencyList = []
) {
  const [data, setData] = useState<T | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<Error | null>(null);

  useEffect(() => {
    let cancelled = false;

    const execute = async () => {
      try {
        setLoading(true);
        setError(null);
        const result = await asyncFunction();
        if (!cancelled) {
          setData(result);
        }
      } catch (err) {
        if (!cancelled) {
          setError(err instanceof Error ? err : new Error('Unknown error'));
        }
      } finally {
        if (!cancelled) {
          setLoading(false);
        }
      }
    };

    execute();

    return () => {
      cancelled = true;
    };
  }, dependencies);

  return { data, loading, error };
}

// useDebounce - Debounce value updates
function useDebounce<T>(value: T, delay: number): T {
  const [debouncedValue, setDebouncedValue] = useState<T>(value);

  useEffect(() => {
    const handler = setTimeout(() => {
      setDebouncedValue(value);
    }, delay);

    return () => {
      clearTimeout(handler);
    };
  }, [value, delay]);

  return debouncedValue;
}

// useLocalStorage - Sync state with localStorage
function useLocalStorage<T>(key: string, initialValue: T) {
  const [storedValue, setStoredValue] = useState<T>(() => {
    try {
      const item = window.localStorage.getItem(key);
      return item ? JSON.parse(item) : initialValue;
    } catch (error) {
      console.error(error);
      return initialValue;
    }
  });

  const setValue = (value: T | ((val: T) => T)) => {
    try {
      const valueToStore = value instanceof Function ? value(storedValue) : value;
      setStoredValue(valueToStore);
      window.localStorage.setItem(key, JSON.stringify(valueToStore));
    } catch (error) {
      console.error(error);
    }
  };

  return [storedValue, setValue] as const;
}

// Usage
function SearchComponent() {
  const [searchTerm, setSearchTerm] = useState('');
  const debouncedSearch = useDebounce(searchTerm, 500);

  const { data: results, loading } = useAsync(
    () => fetch(`/api/search?q=${debouncedSearch}`).then(r => r.json()),
    [debouncedSearch]
  );

  return (
    <div>
      <input
        value={searchTerm}
        onChange={(e) => setSearchTerm(e.target.value)}
        placeholder="Search..."
      />
      {loading && <div>Searching...</div>}
      {results && <ResultsList results={results} />}
    </div>
  );
}
```

**Context API for State Management**:
```typescript
import React, { createContext, useContext, useReducer } from 'react';

// Types
interface Todo {
  id: string;
  text: string;
  completed: boolean;
}

interface TodoState {
  todos: Todo[];
}

type TodoAction =
  | { type: 'ADD_TODO'; payload: { text: string } }
  | { type: 'TOGGLE_TODO'; payload: { id: string } }
  | { type: 'DELETE_TODO'; payload: { id: string } }
  | { type: 'LOAD_TODOS'; payload: { todos: Todo[] } };

// Reducer
function todoReducer(state: TodoState, action: TodoAction): TodoState {
  switch (action.type) {
    case 'ADD_TODO':
      return {
        todos: [
          ...state.todos,
          {
            id: Date.now().toString(),
            text: action.payload.text,
            completed: false
          }
        ]
      };
    
    case 'TOGGLE_TODO':
      return {
        todos: state.todos.map(todo =>
          todo.id === action.payload.id
            ? { ...todo, completed: !todo.completed }
            : todo
        )
      };
    
    case 'DELETE_TODO':
      return {
        todos: state.todos.filter(todo => todo.id !== action.payload.id)
      };
    
    case 'LOAD_TODOS':
      return {
        todos: action.payload.todos
      };
    
    default:
      return state;
  }
}

// Context
const TodoContext = createContext<{
  state: TodoState;
  dispatch: React.Dispatch<TodoAction>;
} | undefined>(undefined);

// Provider
export function TodoProvider({ children }: { children: React.ReactNode }) {
  const [state, dispatch] = useReducer(todoReducer, { todos: [] });

  return (
    <TodoContext.Provider value={{ state, dispatch }}>
      {children}
    </TodoContext.Provider>
  );
}

// Hook
export function useTodos() {
  const context = useContext(TodoContext);
  if (!context) {
    throw new Error('useTodos must be used within TodoProvider');
  }

  const { state, dispatch } = context;

  const addTodo = (text: string) => {
    dispatch({ type: 'ADD_TODO', payload: { text } });
  };

  const toggleTodo = (id: string) => {
    dispatch({ type: 'TOGGLE_TODO', payload: { id } });
  };

  const deleteTodo = (id: string) => {
    dispatch({ type: 'DELETE_TODO', payload: { id } });
  };

  return {
    todos: state.todos,
    addTodo,
    toggleTodo,
    deleteTodo
  };
}

// Usage
function TodoList() {
  const { todos, toggleTodo, deleteTodo } = useTodos();

  return (
    <ul>
      {todos.map(todo => (
        <li key={todo.id}>
          <input
            type="checkbox"
            checked={todo.completed}
            onChange={() => toggleTodo(todo.id)}
          />
          <span style={{ textDecoration: todo.completed ? 'line-through' : 'none' }}>
            {todo.text}
          </span>
          <button onClick={() => deleteTodo(todo.id)}>Delete</button>
        </li>
      ))}
    </ul>
  );
}

function AddTodo() {
  const [text, setText] = useState('');
  const { addTodo } = useTodos();

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    if (text.trim()) {
      addTodo(text);
      setText('');
    }
  };

  return (
    <form onSubmit={handleSubmit}>
      <input
        value={text}
        onChange={(e) => setText(e.target.value)}
        placeholder="Add todo..."
      />
      <button type="submit">Add</button>
    </form>
  );
}
```

### Performance Optimization

**React.memo and useMemo**:
```typescript
import React, { memo, useMemo } from 'react';

// Expensive computation
function calculatePrimeNumbers(max: number): number[] {
  const primes: number[] = [];
  for (let i = 2; i <= max; i++) {
    let isPrime = true;
    for (let j = 2; j <= Math.sqrt(i); j++) {
      if (i % j === 0) {
        isPrime = false;
        break;
      }
    }
    if (isPrime) primes.push(i);
  }
  return primes;
}

// Memoized component
interface PrimeListProps {
  max: number;
  filter?: string;
}

export const PrimeList = memo<PrimeListProps>(({ max, filter }) => {
  // Only recalculate when max changes
  const primes = useMemo(() => calculatePrimeNumbers(max), [max]);

  // Filter results (cheap operation, no need to memoize)
  const filtered = filter
    ? primes.filter(p => p.toString().includes(filter))
    : primes;

  return (
    <div>
      <p>Found {filtered.length} prime numbers</p>
      <ul>
        {filtered.map(prime => (
          <li key={prime}>{prime}</li>
        ))}
      </ul>
    </div>
  );
}, (prevProps, nextProps) => {
  // Custom comparison function
  return prevProps.max === nextProps.max && prevProps.filter === nextProps.filter;
});
```

**Code Splitting with React.lazy**:
```typescript
import React, { Suspense, lazy } from 'react';
import { BrowserRouter, Routes, Route } from 'react-router-dom';

// Lazy load components
const Dashboard = lazy(() => import('./pages/Dashboard'));
const UserProfile = lazy(() => import('./pages/UserProfile'));
const Settings = lazy(() => import('./pages/Settings'));

// Loading fallback
const LoadingSpinner = () => (
  <div className="loading-spinner">Loading...</div>
);

function App() {
  return (
    <BrowserRouter>
      <Suspense fallback={<LoadingSpinner />}>
        <Routes>
          <Route path="/" element={<Dashboard />} />
          <Route path="/user/:id" element={<UserProfile />} />
          <Route path="/settings" element={<Settings />} />
        </Routes>
      </Suspense>
    </BrowserRouter>
  );
}

// Preload on hover
function NavLink({ to, children }: { to: string; children: React.ReactNode }) {
  const handleMouseEnter = () => {
    // Preload the component
    if (to === '/user/:id') {
      import('./pages/UserProfile');
    }
  };

  return (
    <a href={to} onMouseEnter={handleMouseEnter}>
      {children}
    </a>
  );
}
```

## Vue.js Development

### Composition API

**Modern Vue 3 Component**:
```vue
<script setup lang="ts">
import { ref, computed, watch, onMounted } from 'vue';

interface User {
  id: string;
  name: string;
  email: string;
}

interface Props {
  userId: string;
}

const props = defineProps<Props>();
const emit = defineEmits<{
  (e: 'update', user: User): void;
  (e: 'error', error: Error): void;
}>();

// Reactive state
const user = ref<User | null>(null);
const loading = ref(true);
const error = ref<Error | null>(null);

// Computed property
const displayName = computed(() => {
  if (!user.value) return '';
  return `${user.value.name} (${user.value.email})`;
});

// Methods
async function fetchUser() {
  try {
    loading.value = true;
    error.value = null;
    const response = await fetch(`/api/users/${props.userId}`);
    if (!response.ok) throw new Error('Failed to fetch user');
    user.value = await response.json();
  } catch (err) {
    error.value = err instanceof Error ? err : new Error('Unknown error');
    emit('error', error.value);
  } finally {
    loading.value = false;
  }
}

function updateUser(updates: Partial<User>) {
  if (!user.value) return;
  
  user.value = { ...user.value, ...updates };
  emit('update', user.value);
}

// Watch for prop changes
watch(() => props.userId, () => {
  fetchUser();
}, { immediate: true });

// Lifecycle
onMounted(() => {
  console.log('Component mounted');
});
</script>

<template>
  <div class="user-profile">
    <div v-if="loading">Loading...</div>
    <div v-else-if="error">Error: {{ error.message }}</div>
    <div v-else-if="!user">User not found</div>
    <div v-else>
      <h2>{{ displayName }}</h2>
      <button @click="updateUser({ name: 'Updated Name' })">
        Update Name
      </button>
    </div>
  </div>
</template>

<style scoped>
.user-profile {
  padding: 1rem;
}
</style>
```

**Composables (Reusable Logic)**:
```typescript
// composables/useAsync.ts
import { ref, Ref } from 'vue';

export function useAsync<T>(
  asyncFunction: () => Promise<T>
): {
  data: Ref<T | null>;
  loading: Ref<boolean>;
  error: Ref<Error | null>;
  execute: () => Promise<void>;
} {
  const data = ref<T | null>(null);
  const loading = ref(false);
  const error = ref<Error | null>(null);

  const execute = async () => {
    try {
      loading.value = true;
      error.value = null;
      data.value = await asyncFunction();
    } catch (err) {
      error.value = err instanceof Error ? err : new Error('Unknown error');
    } finally {
      loading.value = false;
    }
  };

  return { data, loading, error, execute };
}

// composables/useLocalStorage.ts
import { ref, watch, Ref } from 'vue';

export function useLocalStorage<T>(key: string, initialValue: T): Ref<T> {
  const storedValue = ref<T>(initialValue);

  // Initialize from localStorage
  try {
    const item = window.localStorage.getItem(key);
    if (item) {
      storedValue.value = JSON.parse(item);
    }
  } catch (error) {
    console.error(error);
  }

  // Watch for changes and sync to localStorage
  watch(storedValue, (newValue) => {
    try {
      window.localStorage.setItem(key, JSON.stringify(newValue));
    } catch (error) {
      console.error(error);
    }
  }, { deep: true });

  return storedValue;
}

// Usage in component
<script setup lang="ts">
import { useAsync } from '@/composables/useAsync';
import { useLocalStorage } from '@/composables/useLocalStorage';

const preferences = useLocalStorage('user-preferences', {
  theme: 'light',
  language: 'en'
});

const { data: users, loading, execute } = useAsync(() =>
  fetch('/api/users').then(r => r.json())
);

onMounted(() => {
  execute();
});
</script>
```

### Pinia Store

```typescript
// stores/userStore.ts
import { defineStore } from 'pinia';
import { ref, computed } from 'vue';

interface User {
  id: string;
  name: string;
  email: string;
  role: string;
}

export const useUserStore = defineStore('user', () => {
  // State
  const currentUser = ref<User | null>(null);
  const users = ref<User[]>([]);
  const loading = ref(false);

  // Getters
  const isAdmin = computed(() => currentUser.value?.role === 'admin');
  const userCount = computed(() => users.value.length);

  // Actions
  async function fetchCurrentUser() {
    loading.value = true;
    try {
      const response = await fetch('/api/user/me');
      currentUser.value = await response.json();
    } catch (error) {
      console.error('Failed to fetch user', error);
    } finally {
      loading.value = false;
    }
  }

  async function fetchUsers() {
    loading.value = true;
    try {
      const response = await fetch('/api/users');
      users.value = await response.json();
    } catch (error) {
      console.error('Failed to fetch users', error);
    } finally {
      loading.value = false;
    }
  }

  async function updateUser(id: string, updates: Partial<User>) {
    const response = await fetch(`/api/users/${id}`, {
      method: 'PATCH',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(updates)
    });

    if (response.ok) {
      const updated = await response.json();
      const index = users.value.findIndex(u => u.id === id);
      if (index !== -1) {
        users.value[index] = updated;
      }
      if (currentUser.value?.id === id) {
        currentUser.value = updated;
      }
    }
  }

  function logout() {
    currentUser.value = null;
    users.value = [];
  }

  return {
    // State
    currentUser,
    users,
    loading,
    // Getters
    isAdmin,
    userCount,
    // Actions
    fetchCurrentUser,
    fetchUsers,
    updateUser,
    logout
  };
});

// Usage in component
<script setup lang="ts">
import { useUserStore } from '@/stores/userStore';

const userStore = useUserStore();

onMounted(() => {
  userStore.fetchCurrentUser();
});
</script>

<template>
  <div>
    <div v-if="userStore.loading">Loading...</div>
    <div v-else-if="userStore.currentUser">
      <h2>{{ userStore.currentUser.name }}</h2>
      <p v-if="userStore.isAdmin">Admin User</p>
    </div>
  </div>
</template>
```

## TypeScript Best Practices

### Type Safety and Inference

```typescript
// Utility Types
type Nullable<T> = T | null;
type Optional<T> = T | undefined;
type DeepPartial<T> = {
  [P in keyof T]?: T[P] extends object ? DeepPartial<T[P]> : T[P];
};

// Generic API Response
interface ApiResponse<T> {
  data: T;
  status: number;
  message: string;
}

async function fetchApi<T>(url: string): Promise<ApiResponse<T>> {
  const response = await fetch(url);
  return response.json();
}

// Usage with type inference
const userResponse = await fetchApi<User>('/api/users/1');
// userResponse.data is typed as User

// Discriminated Unions
type Result<T, E = Error> =
  | { success: true; data: T }
  | { success: false; error: E };

function divide(a: number, b: number): Result<number> {
  if (b === 0) {
    return { success: false, error: new Error('Division by zero') };
  }
  return { success: true, data: a / b };
}

const result = divide(10, 2);
if (result.success) {
  console.log(result.data); // TypeScript knows this exists
} else {
  console.error(result.error); // TypeScript knows this exists
}

// Type Guards
function isUser(value: unknown): value is User {
  return (
    typeof value === 'object' &&
    value !== null &&
    'id' in value &&
    'name' in value &&
    'email' in value
  );
}

function processData(data: unknown) {
  if (isUser(data)) {
    // TypeScript knows data is User here
    console.log(data.name);
  }
}

// Const Assertions
const ROUTES = {
  HOME: '/',
  DASHBOARD: '/dashboard',
  PROFILE: '/profile'
} as const;

type Route = typeof ROUTES[keyof typeof ROUTES];
// Route = '/' | '/dashboard' | '/profile'

// Template Literal Types
type HTTPMethod = 'GET' | 'POST' | 'PUT' | 'DELETE';
type Endpoint = `/api/${string}`;
type HTTPRequest = `${HTTPMethod} ${Endpoint}`;

const request: HTTPRequest = 'GET /api/users'; // Valid
// const invalid: HTTPRequest = 'GET /users'; // Error
```

### Advanced Patterns

```typescript
// Builder Pattern with Fluent API
class QueryBuilder<T> {
  private filters: Array<(item: T) => boolean> = [];
  private sortFn?: (a: T, b: T) => number;
  private limitValue?: number;

  where(predicate: (item: T) => boolean): this {
    this.filters.push(predicate);
    return this;
  }

  sortBy(fn: (a: T, b: T) => number): this {
    this.sortFn = fn;
    return this;
  }

  limit(n: number): this {
    this.limitValue = n;
    return this;
  }

  execute(data: T[]): T[] {
    let result = data;

    // Apply filters
    for (const filter of this.filters) {
      result = result.filter(filter);
    }

    // Apply sort
    if (this.sortFn) {
      result = [...result].sort(this.sortFn);
    }

    // Apply limit
    if (this.limitValue) {
      result = result.slice(0, this.limitValue);
    }

    return result;
  }
}

// Usage
const query = new QueryBuilder<User>()
  .where(user => user.role === 'admin')
  .where(user => user.age > 18)
  .sortBy((a, b) => a.name.localeCompare(b.name))
  .limit(10);

const results = query.execute(users);

// Mapped Types
type ReadOnly<T> = {
  readonly [P in keyof T]: T[P];
};

type Mutable<T> = {
  -readonly [P in keyof T]: T[P];
};

// Conditional Types
type IsArray<T> = T extends any[] ? true : false;
type ArrayElement<T> = T extends (infer E)[] ? E : never;

// Example
type X = IsArray<number[]>; // true
type Y = IsArray<string>;   // false
type Z = ArrayElement<User[]>; // User
```

## State Management

### Zustand (React)

```typescript
import create from 'zustand';
import { devtools, persist } from 'zustand/middleware';

interface Todo {
  id: string;
  text: string;
  completed: boolean;
}

interface TodoStore {
  todos: Todo[];
  filter: 'all' | 'active' | 'completed';
  
  addTodo: (text: string) => void;
  toggleTodo: (id: string) => void;
  deleteTodo: (id: string) => void;
  setFilter: (filter: TodoStore['filter']) => void;
  
  // Computed
  filteredTodos: () => Todo[];
  completedCount: () => number;
}

export const useTodoStore = create<TodoStore>()(
  devtools(
    persist(
      (set, get) => ({
        todos: [],
        filter: 'all',

        addTodo: (text) => set((state) => ({
          todos: [
            ...state.todos,
            {
              id: Date.now().toString(),
              text,
              completed: false
            }
          ]
        })),

        toggleTodo: (id) => set((state) => ({
          todos: state.todos.map(todo =>
            todo.id === id
              ? { ...todo, completed: !todo.completed }
              : todo
          )
        })),

        deleteTodo: (id) => set((state) => ({
          todos: state.todos.filter(todo => todo.id !== id)
        })),

        setFilter: (filter) => set({ filter }),

        filteredTodos: () => {
          const { todos, filter } = get();
          if (filter === 'active') return todos.filter(t => !t.completed);
          if (filter === 'completed') return todos.filter(t => t.completed);
          return todos;
        },

        completedCount: () => {
          return get().todos.filter(t => t.completed).length;
        }
      }),
      { name: 'todo-storage' }
    )
  )
);

// Usage
function TodoApp() {
  const { todos, filter, addTodo, toggleTodo, deleteTodo, setFilter, filteredTodos } = useTodoStore();

  return (
    <div>
      <AddTodoForm onAdd={addTodo} />
      <FilterButtons current={filter} onChange={setFilter} />
      <TodoList
        todos={filteredTodos()}
        onToggle={toggleTodo}
        onDelete={deleteTodo}
      />
    </div>
  );
}
```

### React Query (Server State)

```typescript
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';

interface User {
  id: string;
  name: string;
  email: string;
}

// API functions
const userApi = {
  getAll: () => fetch('/api/users').then(r => r.json()),
  getById: (id: string) => fetch(`/api/users/${id}`).then(r => r.json()),
  create: (user: Omit<User, 'id'>) =>
    fetch('/api/users', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(user)
    }).then(r => r.json()),
  update: (id: string, updates: Partial<User>) =>
    fetch(`/api/users/${id}`, {
      method: 'PATCH',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(updates)
    }).then(r => r.json())
};

// Hooks
function useUsers() {
  return useQuery({
    queryKey: ['users'],
    queryFn: userApi.getAll,
    staleTime: 5 * 60 * 1000, // 5 minutes
    cacheTime: 10 * 60 * 1000, // 10 minutes
  });
}

function useUser(id: string) {
  return useQuery({
    queryKey: ['users', id],
    queryFn: () => userApi.getById(id),
    enabled: !!id, // Only run if id is provided
  });
}

function useCreateUser() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: userApi.create,
    onSuccess: () => {
      // Invalidate and refetch users
      queryClient.invalidateQueries({ queryKey: ['users'] });
    },
  });
}

function useUpdateUser() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: ({ id, updates }: { id: string; updates: Partial<User> }) =>
      userApi.update(id, updates),
    onSuccess: (data, variables) => {
      // Update cache immediately
      queryClient.setQueryData(['users', variables.id], data);
      // Invalidate list
      queryClient.invalidateQueries({ queryKey: ['users'] });
    },
  });
}

// Component
function UserList() {
  const { data: users, isLoading, error } = useUsers();
  const createUser = useCreateUser();
  const updateUser = useUpdateUser();

  if (isLoading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;

  return (
    <div>
      <button
        onClick={() => createUser.mutate({ name: 'New User', email: 'new@example.com' })}
        disabled={createUser.isPending}
      >
        {createUser.isPending ? 'Creating...' : 'Add User'}
      </button>

      <ul>
        {users?.map(user => (
          <li key={user.id}>
            {user.name}
            <button
              onClick={() => updateUser.mutate({
                id: user.id,
                updates: { name: 'Updated Name' }
              })}
            >
              Update
            </button>
          </li>
        ))}
      </ul>
    </div>
  );
}
```

## Component Patterns

### Compound Components

```typescript
import React, { createContext, useContext, useState } from 'react';

// Context
interface TabsContextValue {
  activeTab: string;
  setActiveTab: (id: string) => void;
}

const TabsContext = createContext<TabsContextValue | undefined>(undefined);

function useTabs() {
  const context = useContext(TabsContext);
  if (!context) throw new Error('Tabs compound components must be used within Tabs');
  return context;
}

// Main component
interface TabsProps {
  defaultTab?: string;
  children: React.ReactNode;
}

function Tabs({ defaultTab, children }: TabsProps) {
  const [activeTab, setActiveTab] = useState(defaultTab || '');

  return (
    <TabsContext.Provider value={{ activeTab, setActiveTab }}>
      <div className="tabs">{children}</div>
    </TabsContext.Provider>
  );
}

// Sub-components
function TabList({ children }: { children: React.ReactNode }) {
  return <div className="tab-list" role="tablist">{children}</div>;
}

interface TabProps {
  id: string;
  children: React.ReactNode;
}

function Tab({ id, children }: TabProps) {
  const { activeTab, setActiveTab } = useTabs();
  const isActive = activeTab === id;

  return (
    <button
      role="tab"
      aria-selected={isActive}
      onClick={() => setActiveTab(id)}
      className={isActive ? 'tab active' : 'tab'}
    >
      {children}
    </button>
  );
}

function TabPanels({ children }: { children: React.ReactNode }) {
  return <div className="tab-panels">{children}</div>;
}

interface TabPanelProps {
  id: string;
  children: React.ReactNode;
}

function TabPanel({ id, children }: TabPanelProps) {
  const { activeTab } = useTabs();
  if (activeTab !== id) return null;

  return (
    <div role="tabpanel" className="tab-panel">
      {children}
    </div>
  );
}

// Attach sub-components
Tabs.List = TabList;
Tabs.Tab = Tab;
Tabs.Panels = TabPanels;
Tabs.Panel = TabPanel;

// Usage
function App() {
  return (
    <Tabs defaultTab="profile">
      <Tabs.List>
        <Tabs.Tab id="profile">Profile</Tabs.Tab>
        <Tabs.Tab id="settings">Settings</Tabs.Tab>
        <Tabs.Tab id="notifications">Notifications</Tabs.Tab>
      </Tabs.List>

      <Tabs.Panels>
        <Tabs.Panel id="profile">
          <h2>Profile Content</h2>
        </Tabs.Panel>
        <Tabs.Panel id="settings">
          <h2>Settings Content</h2>
        </Tabs.Panel>
        <Tabs.Panel id="notifications">
          <h2>Notifications Content</h2>
        </Tabs.Panel>
      </Tabs.Panels>
    </Tabs>
  );
}
```

### Render Props

```typescript
interface MousePosition {
  x: number;
  y: number;
}

interface MouseTrackerProps {
  children: (position: MousePosition) => React.ReactNode;
}

function MouseTracker({ children }: MouseTrackerProps) {
  const [position, setPosition] = useState<MousePosition>({ x: 0, y: 0 });

  useEffect(() => {
    const handleMouseMove = (event: MouseEvent) => {
      setPosition({ x: event.clientX, y: event.clientY });
    };

    window.addEventListener('mousemove', handleMouseMove);
    return () => window.removeEventListener('mousemove', handleMouseMove);
  }, []);

  return <>{children(position)}</>;
}

// Usage
function App() {
  return (
    <MouseTracker>
      {({ x, y }) => (
        <div>
          <h1>Move your mouse around</h1>
          <p>Position: {x}, {y}</p>
        </div>
      )}
    </MouseTracker>
  );
}
```

## Best Practices

### Code Organization
```
src/
├── components/
│   ├── common/          # Reusable UI components
│   │   ├── Button/
│   │   ├── Input/
│   │   └── Modal/
│   ├── features/        # Feature-specific components
│   │   ├── auth/
│   │   ├── dashboard/
│   │   └── profile/
│   └── layouts/         # Layout components
├── hooks/               # Custom hooks
├── stores/              # State management
├── services/            # API services
├── utils/               # Helper functions
├── types/               # TypeScript types
└── styles/              # Global styles
```

### Performance Tips
1. Use React.memo for expensive components
2. Implement virtual scrolling for long lists
3. Code split with React.lazy
4. Optimize images (WebP, lazy loading)
5. Use CSS-in-JS efficiently
6. Avoid unnecessary re-renders
7. Debounce/throttle expensive operations

### Accessibility
- Use semantic HTML
- Add ARIA labels
- Keyboard navigation support
- Focus management
- Screen reader testing
- Color contrast (WCAG AA/AAA)

## Activation Guidelines

This skill should be activated when:
- Building React, Vue, or Angular applications
- Implementing UI components
- Managing application state
- Working with TypeScript
- Optimizing frontend performance
- Implementing hooks or composables
- Setting up routing
- Creating component libraries
- Integrating APIs
- Building forms and validation
- Implementing authentication
- Creating responsive layouts

The skill provides the most value when given clear requirements, tech stack preferences, and specific coding challenges.
