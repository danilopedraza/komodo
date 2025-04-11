# Mutability

In Komodo, everything is inmutable by default. If you declare something with the `let` keyword, that's inmutable. Every argument you get in a function body is inmutable. Every imported variable is inmutable. Komodo wants you to write more declarative code, but it does not force you. If you want to declare a mutable variable, you can (with some limitations).

## The `var` keyword

Just declare variables with the `var` keyword instead of the `let` keyword. That's it.

For example, you can do this:

```
var sum := 0

for i in 0..10 do sum := sum + i

println(sum)
```

## The limitations

If you declare a mutable variable outside a function, you can't change its value outside of it. For example, the program

```
var x := 0

let setX(val) :=
    x := val

setX(5)
println(x)
```

will fail when you call `setX`. Functions are a barrier for mutable variables. Even if you pass them as arguments to a functions, its value won't change.

It's clear that mutable state is not our favorite, but we wanted to have some options.
