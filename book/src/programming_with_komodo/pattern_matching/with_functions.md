# With functions

Functions are, in my opinion, the place where using patterns is the most fun and productive. You can use patterns to describe rules easily, doing a lot of work with very little code.

The thing with patterns is that they can do a lot of background work for you, at the cost of describing your problem in a pattern matching compatible way.

Let's see this with an example. We want to reduce the size of a piece of text by applying a transformation. We transform the word

```aabcccdd```

into the list

```[('a', 2), ('b', 1), ('c', 3), ('d', 2)]```

Basically, we want to reduce the size of the word by putting every character with the number of times it is repeated. This will save us some space if the word has a lot of repetitions.

If you have tried to do this using loops, conditionals, and an accumulator list, you know that it's kinda tricky. Patterns and recursion offers us a nice, short, and simple solution:

```
let transform("") := []
let transform([first|tail]) := transform((first, 1), tail)
let transform(tuple, []) := [tuple]
let transform((currentChar, count), [currentChar|tail]) :=
    transform((currentChar, count + 1), tail)
let transform(tuple, tail) := [tuple|transform(tail)]
```

We just solved the problem in a pattern matching compatible way: every possible case is represented as a pattern and an action to perform when that case appears. The solution is simple and easy to understand for humans, so it is a good implementation.
