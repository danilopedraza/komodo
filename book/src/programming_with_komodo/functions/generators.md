# Generators

A function containing `yield` produces a `Generator`. Calling the function captures its arguments and its body runs when the generator is iterated. Each `yield` supplies one value and pauses the function execution until another value is requested.

```komodo
let numbers(a, b) :=
    if a >= b then
        ()
    else
        yield a
        numbers(a + 1, b)

assert([i for i in numbers(1, 4)] = [1, 2, 3])
```

When a generator's final expression returns another generator, iteration continues through that generator. This allows recursive sequences, including infinite sequences. Other final values end iteration without yielding them. To yield a list or another generator as one element, use `yield` explicitly.

Generators work in `for` loops, list and set comprehensions, `List` and `Set` conversions, and operations such as `map`, `reduce`, `sum`, `some`, and `every`. Operations that consume an entire sequence (like `map` which transforms all of its input) require a finite generator. `some` and `every` stop as soon as their answer is known:

```komodo
from utils import some

let integers(n) :=
    yield n
    integers(n + 1)

assert(some(integers(0), n -> n = 100))
```

Let's say we have a generator inside another:

```komodo
let primes() :=
    let integers(n) :=
        yield n
        integers(n + 1)
    for n in in integers() do
        if isPrime(n) then
            yield n
        else ()
```

This could be confusing because there are two candidates for who "owns" the first `yield`. However, we have a very easy way of deciding this: A `yield` is assigned to its *most inmediate* parent. This means that the first `yield` is for `integers(n)`, and not for `primes()`.
