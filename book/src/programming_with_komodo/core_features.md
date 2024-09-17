# Core elements

## **What is Komodo good at?**

The aim of Komodo is to easily express **experimental** solutions to problems.

What does **experimental** mean here? They are probably not complete nor correct solutions. When people solve a problem, they usually get to the solution iteratively. They start by some intuition-based guess, and tweak until it satisfies all of the problem's constraints. Komodo wants to make this process easier, making assumptions that are reasonable and offering tools that are convenient.

## **What is Komodo bad at?**

Everything else. We also have to confess that, currently, Komodo is painfully slow. This will probably hurt the user experience and limit the use cases of the language.

## Expression-oriented

Komodo is an expression-oriented language, which means that almost everything that you write returns something.

For example, languages like Python or JavaScript have a clear distinction between statements (pieces of code that declare something) and expressions (pieces of code that describe a value). JavaScript has statements like `for`, `if`, and `let`, and expressions like `5` or `() => null`. You can't do something like `let a = (let b = 5);`, because the value in the right does not describe a value, and the point of the `let` statement is to save a value!

In Komodo, we think of this differently. Every piece of code that you write returns something. For example, the expression `let a := 1` returns 1, and the expression `let chr := 'x'` returns `'x'`.

Komodo **does** have some statements, but they are a few.

## With pattern matching

Pattern matching is the main mechanism to write rules in Komodo. You can use it when declaring functions. For example, this code computes the 10th Fibonacci number:

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

## With weak typing

komodo does not enforce type rules, so you can pass any type to a function. This feature is motivated for two reasons:

- Komodo does not have mechanisms like polymorphism. Weak typing (with pattern matching) allows to (kinda) solve the same problem that polymorphism solves.

- It enriches pattern matching! And invites you, dear programmer, to use it.

## Caching/memoization by default

You can save the result of certain function calls, to avoid repetitive calculations. This is very useful when writing recursive functions. Take the fibonacci example again:

```
let fib(0) := 0
let fib(1) := 1
let fib(n) := fib(n - 1) + fib(n - 2)

fib(10) # 55
```

The recursive call repeats a lot of calculations because is calling the same recursive function two times, and they will do the same (leading to exponential growth on the number of calls!). With large enough inputs (something like `fib(50)`), the program will be unacceptably slow. We can fix this just by adding a word:

```
let fib(0) := 0
let fib(1) := 1
let memoize fib(n) := fib(n - 1) + fib(n - 2)

fib(10) # 55
```

Adding `memoize` before defining a function will save the results computed from that call, so they can used later, when the function is called with the same arguments again.
