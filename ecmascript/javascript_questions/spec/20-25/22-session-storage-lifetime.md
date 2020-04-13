# Session Storage LifeTime

## Example

```javascript
sessionStorage.setItem("cool_secret", "123");
```

- How long is cool_secret accessible?
- When the user closes the tab.

## Explanation

The data stored in `sessionStorage` is removed after closing the tab.

If you used `localStorage`, the data would've been there forever, unless
for example `localStorage.clear()` is invoked. 
