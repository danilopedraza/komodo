# Data structures

Apart from the primitive types, Komodo has three main data structures: Lists, Sets and Dictionaries.

## List

Lists are ordered, possibly mutable and non-growable. They can be written exhaustively:

```
let evens := [0, 2, 4]
```

Or by comprehension:

```
let evens := [2*i for i in 0..3]
```

You can create a new list by putting a new element at the beggining of an existing list:

```
[5|[1, 2]] = [5, 1, 2]
```

They can be concatenated with each other:

```
[1, 2, 3] + [4, 5, 6] = [1, 2, 3, 4, 5, 6]
```

And multiplied:

```
[1, 3]*2 = [1, 3, 1, 3]
```

## Set

Sets are unoredered, inmutable and non-growable. They can be written exhaustively:

```
let Z3 := {0, 1, 2}
```

Or by comprehension:

```
let Z3 := {i for i in 0..3}
```

They can be joined (with the addition operator):

```
{1, 2} + {1, 3} = {1, 2, 3}
```

## Dictionary

Dictionaries are unordered, inmutable collections of key-value pairs. The pairs can be anything. They can be defined exhaustively:

```
let dict := {
    5 => 7,
    "1" => 1,
    [] => 0
}
```