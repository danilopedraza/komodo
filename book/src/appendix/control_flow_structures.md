# Control flow structures

Komodo includes some control flow structures from imperative languages.

## `if`

These are expressions, and always are complete.

```
if 5 % 2 = 0 then
    "5 is even"
else
    "5 is odd"
```

## `for`

Although these are expressions, they are not meant to return anything. They always return an empty tuple `()`.

```
for i in 0..5 do
    let x := i*i
    println(x)
```
