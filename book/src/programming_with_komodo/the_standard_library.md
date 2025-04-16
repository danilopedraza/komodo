# The standard library

We already saw the [builtin functions](./builtin_functions.md), functions available everywhere in Komodo programs. These functions are useful, but definitely are not enough. In order to build complex programs, we will probably need a lot of common subroutines. The standard library tries to provide these.

Let's suppose that we want to get the `cos` function from the `math` module. We use this syntax:

```
from math import cos
```

and the `cos` function will be available for you in the program. As you can see, the syntax is almost the same as when you want to import a file with code, but you just write the name of the module instead of a string with a path.

The standard library has four modules right now, that we will see briefly.

## `utils`

Utils has some basic utilities for data manipulation that are extremely common. Most of the functions in `utils` work in lists and sets. `utils` is entirely written in Komodo. All the functions are [here](https://github.com/danilopedraza/komodo/blob/main/std/utils.komodo).

The most common example is the `map` function:

```
from utils import map

println([1, 3, 4].map(num -> num * 5))
# this will print [5, 15, 20]
```

## `math`

As the name suggests, `math` has a lot of math functions. We can split them in the following categories:

- Trigonometric functions: `sin`, `cos`, `tan`, `asin`, `acos`, `atan`.

- Exponential functions: `exp`, `ln`, `log`.

- Roots: `sqrt`, `cbrt`.

- Misc: `abs`, `hypot`, `round`, `floor`, `ceil`.

## `time`

The `time` module has two for dealing with time:

- `time`: Gives you the current [Unix time](https://en.wikipedia.org/wiki/Unix_time).

- `sleep(amount)`: Freezes the program for `amount` seconds.

## `json`

The `json` module allows you to serialize Komodo values as [JSON](https://en.wikipedia.org/wiki/JSON), and parsing JSON into Komodo values.

- `parse(str)`: Parses `str` as JSON and returns a corresponding Komodo value.

- `stringify(val)`: Interprets `val` as a JSON object and returns the corresponding JSON string.
