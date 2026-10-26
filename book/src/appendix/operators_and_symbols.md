# Operators and symbols

## Infix operators

This list contains all the infix operators, an example of how the operator would be used, a short explanation, and its precedence. An operator with a higher precedence will be grouped before an operator with less precedence.

| Operator | Example           | Explanation                   | Precedence |
| -------- | ----------------- | ----------------------------- | ---------- |
| `in`     | `a in []`         | Membership operator           | 1          |
| `..`     | `0..10`           | Range generation              | 2          |
| `\|\|`   | `true \|\| false` | Logic OR                      | 3          |
| `&&`     | `true && false`   | Logic AND                     | 4          |
| `>`      | `1 > 0`           | "Greater" comparison          | 5          |
| `>=`     | `1 >= 0`          | "Greater or equal" comparison | 5          |
| `<`      | `1 < 0`           | "Less" comparison             | 5          |
| `<=`     | `1 <= 0`          | "Less or equal" comparison    | 5          |
| `/=`     | `9 /= 9`          | Negated equality              | 5          |
| `=`      | `'a' = 'a'`       | Equality                      | 5          |
| `^`      | `2 ^ 3`           | Bitwise XOR                   | 6          |
| `&`      | `1 & 0`           | Bitwise AND                   | 7          |
| `<<`     | `1 << 2`          | Left shift                    | 8          |
| `>>`     | `8 >> 2`          | Right shift                   | 8          |
| `-`      | `8 - 3`           | Arithmetic substraction       | 9          |
| `+`      | `1 + 1`           | Arithmetic addition           | 9          |
| `/`      | `1 / 2`           | Arithmetic division           | 10         |
| `%`      | `5 % 7`           | Division remainder            | 10         |
| `*`      | `5 * 7`           | Arithmetic multiplication     | 10         |
| `**`     | `2 ** 5`          | Exponentiation                | 11         |

### Comparison order

All six comparison operators work between any two values and use the same total
order as `sorted`. Different types are ordered as follows:

`Bool < Char < Number < Symbol < String < Tuple < List < Set < Dictionary < Function < Range < Generator < Error`.

Integers, floats, and fractions share numeric ordering. Comparisons use the exact
stored value, without rounding integers or fractions to floats. Thus `1 = 1.0`,
but `1//10 < 0.1` because the float is slightly greater than one tenth. Positive
and negative zero compare equal. Infinities bound finite numbers; negative NaNs
sort before negative infinity and positive NaNs after positive infinity, with
NaN bit patterns providing a deterministic order within each group.

Collections compare lexicographically, recursively using this order. Sets compare
their sorted elements; dictionaries compare their sorted key/value pairs. Equal
numeric values are also interchangeable as set elements and dictionary keys.
Function and generator ordering can depend on identity within an execution;
comparing a generator does not consume it.

For example, `0.5 < "cesar"` and `[0.5] < [1]` are both true.

## Prefix operators

This list contains all the prefix operators, an example of how the operator would be used, and a short explanation.

| Operator | Example  | Explanation      |
| -------- | -------- | ---------------- |
| `~`      | `~1`     | Bitwise negation |
| `!`      | `!false` | Logic negation   |
| `-`      | `-5`     | Additive inverse |
