# case expressions

`case` expressions allow you to do the same that we did with functions, but without defining a function. You write an expression that pairs patterns and results, and the action from the first matched pattern is performed:

We can implement the previous example with a `case` expression:

```
let transform("") := []
let transform([first|tail]) := transform((first, 1), tail)

let transform(tuple, tail) :=
    case (tuple, tail) do
        (tuple, []) => [tuple]
        ((currentChar, count), [currentChar|tail]) =>
            transform((currentChar, count + 1), tail)
        (tuple, tail) => [tuple|transform(tail)]
```

Of course, the advantage of this is that you don't need to write functions in order to describe rules with patterns.
