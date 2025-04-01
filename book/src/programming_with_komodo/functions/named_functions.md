# Named functions

Named functions are... just that: functions that always have a name.

Let's see the Fibonacci example again:

```
let f(0) := 0
let f(1) := 1
let f(n) := fib(n - 1) + fib(n - 2)
```

In this example, each line defines a pattern that the input can match and a result when the input matches. This function can be phrased as:

- When the input of `fib` is 0, its output is 0.
- When the input of `fib` is 1, its output is 1.
- When the input of `fib` is n (something other than 0 or 1), its output is `fib(n - 1) + fib(n - 2)`.

Of course, you can define functions with only one pattern:

```
let half(n) := n * 0.5
```

This will work just fine.

The thing with patterns is that defining piecewise functions can become the preferred way of doing things. You can get very comfortable and write stuff like this:

```
let isZero(0) := true
let isZero(n) := false
```

This can be [cute](https://wiki.c2.com/?CuteProgramming), but maybe is better to use a simpler approach, like this:

```
let isZero(n) := n = zero
```

You can also define a function with more than one line of code:

```
let fibPrint(n) :=
    let val := fib(n)
    println("fib(" + String(n) + "): " + String(val))
    val
```

In this example, `fib` is the function with the same name that we defined above.
