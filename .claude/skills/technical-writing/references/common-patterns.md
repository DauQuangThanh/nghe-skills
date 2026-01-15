# Common Patterns


### Before/After Examples

When documenting changes or migrations:

```markdown
### Migration Example

**Before (v1.x):**
```javascript
const result = api.oldMethod(param1, param2);
```

**After (v2.x):**
```javascript
const result = api.newMethod({
  param1: value1,
  param2: value2
});
```

**Why this change:** Improved flexibility and clarity with named parameters
```

### Decision Trees

For helping users choose between options:

```markdown