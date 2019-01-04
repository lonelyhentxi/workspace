# Event Target

```html
<div onclick="console.log('first div')">
    <div onclick="console.log('second div')">
        <div onclick="console.log('button')">
            Click!
        </div>
    </div>
</div>
```

'Button' is event.target when click the button. The deepest nested element
that caused the event is the target of the event. You can stop bubbling
by `event.stopPropagation`.
