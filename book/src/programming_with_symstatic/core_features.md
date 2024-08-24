# Core elements

## **What is Symstatic good at?**

The aim of Symstatic is to easily express **experimental** solutions to some specific class of problems. These problems are related to combinatorics, algorithm design, number theory, and stuff like that.

What does **experimental** mean here? They are probably not complete nor correct solutions. When people solve a math problem, they usually get to the solution iteratively. They start by some intuition-based guess, and tweak until it satisfies all of the problem's constraints. Symstatic wants to make this process easier, making assumptions that are reasonable in this context and offering tools that are convenient. This means that Symstatic is (clearly) an opinionated tool.

## **What is Symstatic bad at?**

Everything else. We also have to confess that, currently, Symstatic is painfully slow. This will probably hurt the user experience and limit the use cases of the language.

## Expression-oriented

Symstatic is an expression-oriented language, which means that almost everything that you write returns something.

For example, languages like Python or JavaScript have a clear distinction between statements (pieces of code that declare something) and expressions (pieces of code that describe a value). JavaScript has statements like `for`, `if`, and `let`, and expressions like `5` or `() => null`. You can't do something like `let a = (let b = 5);`, because the value in the right does not describe a value, and the point of the `let` statement is to save a value!

In Symstatic, we think of this differently. Every piece of code that you write returns something. For example, the expression `let a := 1` returns 1, and the expression `let chr := 'x'` returns `'x'`.

Symstatic **does** have some statements, but they are very specific.

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

## With weak typing

symstatic does not enforce type rules, so you can pass any type to a function. This feature is motivated for two reasons:

- Symstatic does not have mechanisms like polymorphism. Weak typing (with pattern matching) allows to (kinda) solve the same problem that polymorphism solves.

- It enriches pattern matching! And invites you, dear programmer, to use it.
