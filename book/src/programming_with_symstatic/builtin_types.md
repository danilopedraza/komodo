# Built-in Types

Symstatic has a few built-in types:

- Integers: Signed, arbitrary precision integers. You can write them in decimal form, or the usual prefixed binary (`0x`), octal (`0o`) or hex (`0x`) form.

- Decimals: Signed, arbitrary precision decimal numbers. You can write them in decimal form with a dot.

- Fractions: Signed, arbitrary precision fractions, made with Integers. You can write them like this: `1 // 2`.

- Characters: Old-fashioned ASCII characters. You can write them like this: `'x'`, where `x` is a representation of some ASCII value. (Support for escaped characters is pretty bad right now!)

- Strings: A bunch of characters, ordered. You can write them like this: `"Hello, world!"`.
  
  **Note**: Many programming languages use the built-in list type to define strings. Symstatic does not do this. The string type is completely different and defined independently in the interpreter.

- Lists: An ordered collection of anything. You can write them in two ways:
  - By extension: `[1, 2, 4, 8]`
  - By comprehension: `[ 2**k : k in 0..4 ]`

  Both of these examples describe the same thing! I should also remind you that the elements of a list can be of different type. This is valid: `[1, '2', "3"]`.
  
  **Note**: You probably noticed the `0..4` expression. This is a **range**, and it behaves exactly like you expect: It goes from 0 to 3 (it always excludes the last number).

- Tuples: An ordered collection of anything. Wait... That's the same as lists! Yes. There are differences, though. You can't do operations with tuples just like you can with lists, and you can't compose them like we did above with lists. You can write them like this: `('1', 11, "111")`.

- Sets: An unordered and extendable collection of anything. You can write them like this: `{ 1, 2, 3 }`.

- Functions: Pieces of code that receive values and return a value. There are two ways of writing them:
  - Named, with patterns:
  ```
  let f(0) := 25
  let f(n) := f(n - 1) + 1
  ```
  - Anonymously:
  ```
  n ->
      let x = n + 2,
      x*x
  ```
  With this syntax, the last expression (i.e `x*x`) is returned as the result. Of course, this is an expression and you can save it to a value, or execute it in place like this: `(x -> x * 2)(1)`.
  
  **Note**: You may have noticed that we used parenthesis to denote a series of steps. This is the only case where something that could be a tuple will be interpreted in another way.

- Dictionaries: A collection of key-value pairs. There are no restrictions on the keys or the values. You can use anything at the same time! You can write them like this: `{"a": 5, []: 'b'}`.
