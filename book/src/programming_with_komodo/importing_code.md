# Importing code

You can import any variables you declare in a Komodo file into another Komodo file. For example, if you have a function called `foo` in a file called `foo.komodo` in the same directory as the file where your program is, you can do the following:

```
from "./foo.komodo" import foo

foo()
```

and it just works! You don't have to define `foo` as public, or something like that. In general, Komodo does not have explicit mechanisms to define privacy of code.

You can import several names from one module:

```
from "./foo.komodo" import (foo, bar)

bar(foo())
```

What you cannot do currently, is to import an entire file:

```
import foo # this will throw an error!

```

Imports are specific to code blocks. You can do this:

```
let f(x) :=
    from "./foo.komodo" import bar
    bar(x)
```

and `bar` will not be available outside the definition of `f`. This is very useful if you want to import something for a very specific purpose.
