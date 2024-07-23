# Core elements

## Expression-oriented

Symstatic is an expression-oriented language, which means that almost everything that you write returns something.

For example, languages like Python or JavaScript have a clear distinction between statements (pieces of code that declare something) and expressions (pieces of code that describe a value). JavaScript has statements like `for`, `if`, and `let`, and expressions like `5` or `() => null`. You can't do something like `let a = (let b = 5);`, because the value in the right does not describe a value, and the point of the `let` statement is to save a value!

In Symstatic, we think of this differently. Every piece of code that you write returns something. For example, the expression `let a := 1` returns 1, and the expression `let chr := 'x'` returns `'x'`.

## Symbolic

Symstatic is a symbolic language, which means that you can manipulate symbols without necessarily having a value attached to them. While in Python or JavaScript you use symbols to name certain values and refer to them later, in Symstatic you can use symbols alone, with nothing attached to them. This lets us do things like this:

```
let a := b
a = b # true
```

This works like it does because symbols are also values that we can store with names, just like integers or strings.

This part of the language is being actively developed, so is constantly changing. For now, keep in mind that you can store symbols with names like anything else!

## With pattern matching

Pattern matching is the main mechanism to write rules in Symstatic. You can use it when declaring functions. For example, this code computes the 10th Fibonacci number:

```
let fib(0) := 0
let fib(1) := 1
let fib(n) := fib(n - 1) + fib(n - 2)

fib(10) # 55
```

This code gives the last element of a list:

```
let last([val]) := val
let last([first|tail]) := last(tail)

last([1, 2, 3]) # 3
```

The `[first|tail]` expression represents a list whose first element is `first` and `tail` is a list with the rest. This syntax exists in languages like [Prolog](https://en.wikipedia.org/wiki/Prolog), [Erlang](https://en.wikipedia.org/wiki/Erlang_(programming_language)), and [Picat](https://picat-lang.org/).
