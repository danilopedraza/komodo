# Functions

Functions are Komodo values. You can pass other values to them (the input), and they are going to return another value that depends on these values (the output).

Every function receives exactly one value. `f(x)` passes `x`, `f(a, b)` passes the tuple `(a, b)`, and `f()` passes the empty tuple `()`. Extra parentheses do not change the argument: `f((a, b))` and `f(a, b)` are equivalent, as are `f(())` and `f()`.

For example, `let add(a, b) := a + b` matches and destructures one tuple argument. An anonymous function such as `(a, b) -> a + b` does the same. A function declared with `let answer() := 42` matches the empty tuple; a symbol parameter such as `let identity(x) := x` accepts any single value, including tuples and `()`.

When combining clauses, a symbol parameter is a catch-all for tuples too. Use a constraint such as `codes: Set` when a clause should accept only sets and another clause should accept a tuple.

Functions are the main characters of Komodo: They are the main tool to describe routines and reuse them.

There are two ways of describing functions: [named functions](./functions/named_functions.md) and [anonymous functions](./functions/anonymous_functions.md). But first, we'll look at some (named functions) that come pre-implemented and included when you write a Komodo program: the [builtin functions](./functions/builtin_functions.md).
