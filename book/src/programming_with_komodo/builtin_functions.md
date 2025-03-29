# Builtin functions

Komodo has some built-in functions for basic utilities. Komodo offers more helpful functions in the [standard library](./the_standard_library.md), but you can do some basic things with these. Let's take a look at them.

## I/O Functions

These functions allow you interact with the standard input and output of your system.

- `print(value)`: Prints whatever you pass it. Komodo will convert any Komodo value into its string representation in pass it to standard output.
  
  Returns an empty tuple `()`.

  Here is an example:

  ```
  >>> print(1)
  1()
  ```
  
  The `()` at the end is strange, but has a reason: `print` does not put a linebreak after printing, and since we are using the REPL, the return value of `print`, the empty tuple, is printed afterwards with nothing in between.

- `println(value)`: Prints whatever you pass it, with a linebreak at the end.

  Returns the empty tuple `()`.

  Here is an example:

  ```
  >>> println(0)
  0
  ()
  ```

  Here's the difference: `println` puts a linebreak after the 0, so its actual return value goes in a new line. Remember, this is something specific to the REPL. When you execute a file with Komodo code, it will only print what you want to print, by calling `print` or `println`.

- `getln()`: Takes a line from standard input and returns it.

  Here is an example:

  ```
  >>> "Hello, " + getln() + "!"
  Danilo # you should type your name in this part, I just put my name
  Hello, Danilo!
  ```

## Assert

This function allows you to make assertions while your program is executing, and to stop it they do not hold.

- `assert(value)`: Takes a Komodo value. When that value is the boolean `true`, it does nothing. Otherwise the program stops, with an error message saying that the assertion failed. It always returns the empty tuple.

  Here is an example:
  ```
  >>> assert(25)
  Failed assertion
  >>> assert(25 % 2 = 1)
  ()
  ```

- `assert(value, msg)`: Does the same as `assert(value)`, but when if the assertion fails, it adds `msg` to the error message. `msg` can be anything, it will be converted into its string representation. It always returns the empty tuple.

  Here is an example:
  ```
  let x := 25
  >>> assert(x % 2 = 0, String(x) + " is not an even number")
  ```

## Casting functions

These functions allow to convert values of certain type into types of another. In some cases the types can't be equivalent, so Komodo will do some assumptions about how would you like to transform values. For these cases, the [standard library](./the_standard_library.md) has some functions that allow you to do different transformations.

## Misc
