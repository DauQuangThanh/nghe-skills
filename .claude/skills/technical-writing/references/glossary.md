# Glossary

- **Workspace:** A container for projects and team members
- **Project:** A collection of tasks and resources
- **Task:** A unit of work with assignees and due dates
```

### Code Examples

**Complete and Runnable:**
```javascript
// ✅ Good: Complete, runnable example
const express = require('express');
const app = express();

app.get('/api/users', (req, res) => {
  res.json({ users: [] });
});

app.listen(3000, () => {
  console.log('Server running on port 3000');
});
```

```javascript
// ❌ Bad: Incomplete, won't run
app.get('/api/users', (req, res) => {
  // ... do something
});
```

**Include Context:**
```python
# ✅ Good: Shows file structure and imports
# file: src/models/user.py
from dataclasses import dataclass
from datetime import datetime

@dataclass
class User:
    id: int
    email: str
    created_at: datetime
```

```python
# ❌ Bad: No context about where this goes
@dataclass
class User:
    # ...
```

**Explain Non-Obvious Code:**
```javascript
// ✅ Good: Explains the why
// Use exponential backoff to avoid overwhelming the server
// during transient failures
const delay = Math.min(1000 * Math.pow(2, attempt), 30000);
await sleep(delay);
```

```javascript
// ❌ Bad: No explanation of complex logic
const delay = Math.min(1000 * Math.pow(2, attempt), 30000);
await sleep(delay);
```

### Formatting Standards

**Headings:**
- H1 (#): Document title only
- H2 (##): Major sections
- H3 (###): Subsections
- H4 (####): Sub-subsections (use sparingly)

**Lists:**
- Use bullets for unordered items
- Use numbers for sequential steps
- Keep list items parallel in structure

**Code Blocks:**
- Always specify language: ```javascript, ```python, ```bash
- Include file paths when relevant: `// file: src/app.js`
- Keep examples under 50 lines (split into multiple if longer)

**Tables:**
- Use for structured data comparison
- Keep columns to 5 or fewer
- Use markdown tables for simple data
- Consider alternative formats for complex data

**Callouts:**
```markdown
**Note:** For additional information that's helpful but not critical

**Warning:** For important cautions about potential issues

**Tip:** For best practices and optimization suggestions

**Important:** For critical information that must not be missed
```

### Accessibility

**Alt Text for Images:**
```markdown
![Dashboard showing user statistics with bar chart](dashboard.png)
```

**Descriptive Links:**
✅ "See the [API authentication guide](link) for details"
❌ "Click [here](link) for more information"

**Semantic HTML in Docs:**
Use proper heading hierarchy, lists, and structure
