# Types

Komodo has a very simple type system. You cannot create custom types in Komodo. Let's describe all of them and see a few examples.

## Numerical types

Komodo has three kinds of numbers, and each of them has their own type: Integers, Floats, and Fractions.

Let's see how to generate, manipulate, and do operations with numbers.

### Integers

The `Integer` type represents a signed, arbitrary size whole number. You can write integers in the following ways:

- In binary form, starting the number with `0b` or `0B`: `0b10`, `0b001`, `0B1011`,...
  
  Writing `0b` only, with nothing after it, is illegal.

- In octal form, starting the number with `0o` or `0O`: `0o15`, `0O80`, `0o01`,...

  Writing `0o` only is illegal, just like with binary numbers.

- In decimal form, the most common way of describing them: `15`, `1`, `0`,...

  The decimal format does not allow unnecessary zeros: `000` is illegal, as well as `0001`, for example.

  Basically, all leading zeros are illegal when you are writing decimal numbers.

- In hexadecimal form, starting the numbers with `0x` or `0X`: `0xff`, `0x1e`, `0x01`,...

  As you may have guessed, writing `0x` alone is illegal.

Komodo implements all the essential arithmetic operations between integers. It also implements bitwise operations.

Even when you can write integers in these formats, Komodo will always print them in decimal by default. For example, if you type `0b1011` in the REPL, like this:

```
>>> 0b1011
```

You get the following result:

```
11
```

That makes sense: 1011 in binary is just 11 in decimal.

### Floats

The `Float` type represents a double precision floating-point binary number, equivalent at first to the one specified in the [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754) standard. The difference is that `Float`s have arbitrary precision, so they will increase their digits when necessary.

You can write floats as two decimal integers with a dot in between: `0.1`, `0.5`, `115.2555`,...

Komodo implements all the essential arithmetic operations between floats. When you operate an integer and a float, the result will be a float.

## Characters

The `Char` type represents any [Unicode](https://home.unicode.org/) symbol. You can write chars with single quotes, like this: `'a'`, `'\''`, `'\\'`,...

You can "multiply" chars and get a string:

```
>>> 3*'z'
"zzz"
```

You can concatenate chars and get a string:

```
>>> 'a' + 'z'
"az"
```

## Strings

The `String` type represents a sequence of Unicode symbols. You can write strings with double quotes: `"foo"`, `"\"bar\""`, `"hello world"`,...

You can make operations between characters and strings:

- You can concatenate strings:
  ```
  >>> "ab" + "cd"
  "abcd"
  ```

- You can concatenate chars and strings:
  ```
  >>> "ab" + 'c'
  "abc"
  ```

- You can "multiply" strings too:
  ```
  >>> "F" + "U"*10
  "FUUUUUUUUUU"
  ```

## Lists

The `List` type represents a sequence of Komodo values. You can write lists with square brackets: `[]`, `[1, 2]`, `["foo", 'z', 5.5, []]`,...

- You can concatenate lists:
  ```
  >>> [1, 2] + [3]
  [1,2,3]
  ```
- You can prepend elements to a list:
  ```
  >>> [1|[2, 3]]
  [1,2,3]
  ```

  This is known as the `cons` notation. The name comes from a [basic function in Lisp](https://en.wikipedia.org/wiki/Cons).

- You can write lists by comprehension:
  ```
  >>> [x*2 for x in 0..3]
  [0,2,4]
  ```

Lists are the most basic container in Komodo. They will be very useful.
