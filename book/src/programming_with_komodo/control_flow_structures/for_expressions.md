# for expressions

We love recursion and array functions, but sometimes loops are just the most convenient way to do things. This is the reason Komodo has `for` loops.

Let's say you want to print some numbers. You can do this:

```
let f(n) := 2**n

for k in 0..10 do
    let (k, fk) := (String(k), String(f(k)))
    println("f(" + k + ") = " + f(k))
```

It would be tiresome to do something like this without loops, and a lot of operations are better with loops.

`for` loops can also have a single line inside, without an indented block:

```
for i in 0..10 do println("F" + "U"*i)
```

`for` loops are expressions that always return the empty tuple `()`.

`for` loops do not have `break` or `continue` interruptions... yet.
