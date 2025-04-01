# Anonymous functions

And... yeah. Anonymous functions are just functions without a name, or functions that don't have a name **necessarily**.

Let's say you have a list like this:

```
>>> let data := [5, 24, 9, 1]
[5, 24, 9, 1]
```

and you want a list with the values of that list **multiplied by 2**.

There are several ways of doing this. You could use a recursive [named function](./named_functions.md):

```
let double([]) := []
let double([first|tail]) := [first*2|double(tail)]
assert(double(data) = [10, 48, 18, 2])
```

This works just fine. You can use an anonymous function to do the same like this:

```
from utils import map
let double(data) := data.map(val -> val * 2)
assert(double(data) = [10, 48, 18, 2])
```

Should you prefer one over the other? I think yes. This reason is simple: `map` is a function that applies a change to all the elements of the list, and that's exactly what we want. The `map` function, combined with the anonymous function `val -> val * 2`, gets us a very short implementation that relies on code that was previously written for us in the [standard library](../the_standard_library.md).

This is the main use of anonymous functions: writing them in-place to pass them to other functions. However, you can do something like this:

```
let inspect := value ->
    println(value)
    value
```

You have created a variable called `inspect`, whose value is... a function. This will just behave like a function. However, you can also do this:

```
var inspect := value ->
    println(value)
    value
```

The only difference is that we used the `var` keyword to create the `inspect` variable. This makes `inspect` mutable, so you can do this:

```
inspect := 1
```

and this will work just fine.
