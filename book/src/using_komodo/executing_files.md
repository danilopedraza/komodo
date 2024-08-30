# Executing files

The Komodo interpreter can read files provided by you. You just have to type `komodo <path>`. Create a file called `fib.smtc` and put the following in it:

```
let fib(0) := 0
let fib(1) := 1
let fib(n) := fib(n - 1) + fib(n - 2)

println(fib(10)) # 55
```

Now, in the same directory where the file is, type `komodo fib.smtc`.
