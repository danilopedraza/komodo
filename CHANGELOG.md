# v0.1.0

- Mutable values: You can set mutable values with the `var` keyword. Mutation is restricted to the scope of declaration.

- `cons` notation: You can use `[first|rest]` or `{some|rest}` to pass recursively or build new lists or sets, respectively.

- Code blocks: now indentation can be used to specify code blocks.

# 0.2.0

- Avoid exponential scaling of decimals when doing exponentiation with an integer.

- The standard library is now up, with the `utils` module.

- The standard library path can be customized setting it with the `KOMODO_STD` environment variable.

- Errors occurring when executing imported modules are now shown correctly.

# 0.2.1

- Fix bad default path for the standard library.

# 0.3.0

- Add object-like notation for dictionaries. Add criteria for selecting between object function calls and pseudo-OOP calls.

# 0.4.0

- Enable mutability inside loops.

- Enable value destructuring of loop variables. Now you can use any pattern as a loop variable.

- Add the remainder operator to the Float type.

- Now an argument can be matched against several signatures using the same pattern using the `||` operator.

- Now an argument can be matched against several patterns in the same pattern using the `||` operator.

- Hexadecimal numbers now can be written with uppercase digits.

- The `math` module was added to the standard library. It has:
    - The main trigonometric functions
    - Round functions
    - Square and cube root
    - Natural logarithm and arbitrary base logarithm
    - Exponential function
    - Absolute value

- The `utils` module has some new members: `some`, `every`, and `indexOf`.

- The `json` module was added, with the `parse` and `stringify` functions.

- The `time` module was added, with the `time` and `sleep` functions.

- Fix bug where non-empty sets were being matched to the `{}` expression.

# 0.4.1

- Improve error handling and fix a bug in `json.stringify`

- Fix bug in pattern matching of lists

# Upstream

- Fix bug where a list-cons produced from a new element and a set returned a set. Now it returns a list!
