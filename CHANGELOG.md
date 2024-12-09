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
