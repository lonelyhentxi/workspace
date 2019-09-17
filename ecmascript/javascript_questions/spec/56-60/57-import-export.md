# Import Export

```js
// counter.js
let counter = 10;
export default counter;
```

```js
// index.js
import myCounter from "./counter";
myCounter += 1;
console.log(myCounter);
```

An imported module is read-only: you cannot modify the imported module. 
Only the module that exports them can change its value.

When we try to increment the value of `myCounter`, 
it throws an error: `myCounter` is read-only and cannot be modified.
