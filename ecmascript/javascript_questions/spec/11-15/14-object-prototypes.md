# Not All object has prototype

All objects expect the base object has prototypes. The base object is 
the object created by the user, or an object that is created using the new 
keyword. The base object has access to some method and properties, such 
as `.toString`. This is the reason why you can use built-in javascript
methods. All of such methods are available on the prototype. Although
JavaScript can't find it directly on your object, it goes down the prototype
chain and finds it there, which makes it accessible for you.
