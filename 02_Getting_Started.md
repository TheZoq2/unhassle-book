# Getting Started

There are two major modes of using Haskell:
As a compiled language and in the REPL.

We will use the interactive REPL most of the time,
but for now let's write a simple program to get
the hang of it.

The program we will write lies in a file "test.hs"
and reads exactly one line of input and then prints it out.

The code is as follows.

```haskell
main =
  getLine >>= putStrLn
```

We can run this script using `runhaskell`.

```
$ runhaskell test.hs
Hello World!
Hello World!
```

Okay, now that we have seen it in action,
let's actually understand how it works.

The first thing that stands out is that Haskell is a pure language:
functions don't do stuff other than returning.

getLine is therefore not a function returning a string,
it isn't even a function at all, because it does not depend on any
argument.

The type signature of `getLine` is as follows.

```haskell
getLine :: IO String
```

`getLine` is a value of an `IO` type.
They are a bit like functions taking a callback,
except that they aren't functions in the mathematical sense,
since they actually do stuff besides returning a value
(in this case reading input), so they can't be functions in Haskell.

More specifically `getLine` has the type `IO String`,
which means, that the callback function we provide has to take
a `String` and return a value of `IO something`,
where `something` is just a generic placeholder for any type.

We can attach a callback to `getLine` using the `>>=` operator.
This operator is also available for other types,
but we will get to that later.

The function we will provide as a callback is `putStrLn`.

```haskell
putStrLn :: String -> IO ()
```

It is a function taking a string and returning `IO ()`
(IO unit), that is an IO value expecting a callback of type
`() -> IO something`.

`()` is a bit like `null`, `nil`, or `None` in other languages.

As you have seen how to attach callbacks you might ask by now
how to actually do things.

Of course, we can provide a callback for getLine, which returns
`IO something`, but then the only thing we can do is attach another
callback, which takes `something` and returns `IO somethingElse`.

This is enforced by the compiler.

So we can build up large callback chains, but it will always require
another one and it will never execute.

This is due to Haskell being a pure language:
Functions can't do stuff.

The only way we can actually execute an `IO` action is by assigning
it to `main`.

Since this is an introductory chapter of a programming book,
let's complete it with a "Hello World!" example.

```haskell
main = putStrLn "Hello World!"
```

`putStrLn` takes a `String` (`"Hello World!"`) and
returns an `IO ()` value, which we then assign to main.

The restriction of being a pure language might seem fairly
random by now, but in future chapters we will learn to love
this property of Haskell: It might make easy things a bit harder,
but it helps a lot when solving larger problems.
