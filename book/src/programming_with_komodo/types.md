# Types

Komodo has a very simple type system. You cannot create custom types in Komodo. Let's describe all of them and see a few examples.

## Numerical types

Komodo has three kinds of numbers, and each of them has their own type: Integers, Floats, and Fractions.

Let's see how to generate, manipulate, and do operations with numbers.

### Integers

The `Integer` type represents a signed, arbitrary size whole number. You can write them in the following ways:

- In binary form, starting the number with `0b` or `0B`: `0b10`, `0b001`, `0B1011`,...
  
  Writing `0b` only, with nothing after it, is illegal.

- In octal form, starting the number with `0o` or `0O`: `0o15`, `0O80`, `0o01`,...

  Writing `0o` only is illegal, just like with binary numbers.

- In decimal form, the most common way of describing them: `15`, `1`, `0`,...

  The decimal format does not allow unnecessary zeros: `000` is illegal, as well as `0001`, for example.

  Basically, all leading zeros are illegal when you are writing decimal numbers.

- In hexadecimal form, starting the numbers with `0x` or `0X`:

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

### Characters

The `Char` type represents any [Unicode](https://home.unicode.org/) symbol. You can write it with single quotes, like this: `'a'`, `'\''`, `'\\'`,...

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

### Strings

The `String` type represents a sequence of Unicode symbols. You can write it with double quotes: `"foo"`, `"\"bar\""`, `"hello world"`,...

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
