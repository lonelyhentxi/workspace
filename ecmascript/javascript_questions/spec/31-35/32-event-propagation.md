# Event Propagation

```html
<div onclick="console.log('div')">
    <p onclick="console.log('p')">
        Click here!
    </p>
</div>
```

If we click `p`, we see two logs: `p` and `div`. During event propagation
, there are 3 phases: capturing, target, and bubbling. By default, event
handlers are executed in the bubbling phase (unless you set `useCapture` to
`true`). It goes from the deepest nested element outwards.


