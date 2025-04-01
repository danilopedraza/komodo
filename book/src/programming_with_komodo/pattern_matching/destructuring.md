# Destructuring

The first use for pattern matching in Komodo occurs in variable declarations.

Let's imagine you have a list of values like this:

```
>>> let data := [5, 442, 533, 2, 5334]
[5, 442, 533, 2, 5334]
```

Maybe you want some values from there. Komodo allows you to do that using pattern matching. You can do this:

```
>>> let [first, _, _, fourth, ..] := data
[5, 442, 533, 2, 5334]
```

and then, the variables `first` and `fourth` will have the values `5` and `2`, respectively:

```
>>> (first, fourth)
(5, 2)
```

You can do this with every Komodo value.
