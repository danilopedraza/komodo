# Types

Komodo has a very simple type system. You cannot create custom types in Komodo. Let's describe all of them and see a few examples.

## Numerical types

Komodo has three kinds of numbers, and each of them has their own type: Integers, Floats, and Fractions.

Let's see how to generate, manipulate, and do operations with numbers.

### Integers

The `Integer` type represents a signed, arbitrary size whole number. You can write them in the following ways:

- In binary form, starting the number with `0b`: `0b10`, `0b001`, `0b1011`,...
  
  Writing `0b` only, with nothing after it, is illegal.

- In octal form, starting the number with `0o`: `0o15`, `0o80`, `0o01`,...

  Writing `0o` only is illegal, just like with binary numbers.

- In decimal form, the most common: `15`, `1`, `0`,...

  The decimal format does not allow unnecessary zeros: `000` is illegal, as well as `0001`, for example.

  Basically, all leading zeros are illegal when you are writing decimal numbers.

Komodo implements all the essential arithmetic operations between integers, using infix operators. 
