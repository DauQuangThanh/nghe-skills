# Common Architecture Patterns


### Layered Architecture
```
┌─────────────────────────────────┐
│   Presentation Layer            │  (UI, Controllers)
├─────────────────────────────────┤
│   Application Layer             │  (Use Cases, Orchestration)
├─────────────────────────────────┤
│   Domain Layer                  │  (Business Logic, Entities)
├─────────────────────────────────┤
│   Infrastructure Layer          │  (DB, External APIs)
└─────────────────────────────────┘
```

### Microservices Architecture
```
┌──────────┐     ┌──────────┐     ┌──────────┐
│  API     │────▶│ Service  │────▶│ Service  │
│ Gateway  │     │    A     │     │    B     │
└──────────┘     └─────┬────┘     └─────┬────┘
                       │                 │
                  ┌────▼────┐       ┌───▼────┐
                  │   DB    │       │   DB   │
                  └─────────┘       └────────┘
```

### Event-Driven Architecture
```
┌─────────┐       ┌─────────────┐       ┌─────────┐
│Producer │──────▶│ Event Bus/  │──────▶│Consumer │
│Service  │       │Message Queue│       │Service  │
└─────────┘       └─────────────┘       └─────────┘
```

### Clean Architecture (Hexagonal)
```
         ┌────────────────────────┐
         │    UI / Controllers    │
         └───────────┬────────────┘
                     │
         ┌───────────▼────────────┐
         │   Application Layer    │
         │   (Use Cases/Ports)    │
         └───────────┬────────────┘
                     │
         ┌───────────▼────────────┐
         │     Domain Layer       │
         │  (Business Rules)      │
         └───────────┬────────────┘
                     │
         ┌───────────▼────────────┐
         │   Adapters Layer       │
         │ (DB, APIs, External)   │
         └────────────────────────┘
```
