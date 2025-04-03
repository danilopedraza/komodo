# if expressions

Let's say we want to know if an integer is even or odd, and display the answer. We can do something like this with pattern matching:

```
let parityMessage(n: Integer) :=
    case n % 2 do
        0 => String(n) + " is even!"
        1 => String(n) + " is odd!" 
```

It's fine, but I think it looks too complicated for a problem that should be simpler. Let's use an `if` expression:

```
let parityMessage(n: Integer) :=
    if n % 2 = 0 then
        String(n) + " is even!"
    else
        String(n) + " is odd!"
```

I think this is more explicit, and is less surprising when you read it. A nice thing about `if`s in Komodo is that they are expressions themselves, so they return a result. You can do this:

```
let parityMessage(n: Integer) :=
    let text :=
        if n % 2 = 0 then
            " is even!"
        else
            " is odd!"
    
    String(n) + text
```

The thing with `if` expressions in Komodo is that you **must** write the `else` part. Always. Not doing it will result in an error when you try to execute the program.
