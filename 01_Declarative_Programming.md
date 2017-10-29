# Declarative Programming

Haskell is a great tool for writing very reliable programs.

The way it achieves that is by being a fully declarative language:
You tell what you want, not how to get it.

Your programs are constructed the same way:
Instead of forcing the computer to do something,
you specify what has to be done.

Haskell then lets your computer do exactly what you want.

Think about those program descriptions as TO-DO lists for the computer:
they are values, which contain all the information the computer needs to do something.

Let us create such a "TO-DO list", which prints a string.

We can do this using the `putStrLn` ("put string line") function.
`putStrLn` has the type of a function transforming a `String` into an `IO ()`.

The `IO ()` type is the type of the "TO-DO lists," which produce nothing.
Similarly, an action producing an `Int` might be denoted `IO Int`.

```haskell
putStrLn :: String -> IO ()
```

This means, that we can give `putStrLn` a string and get an `IO ()`,
a task description which on completion produces no value
(but which does print a string on execution, in our case).

```haskell
putStrLn "hello world" :: IO ()
```


In Haskell, the description of the main task to execute is called `main`.
Thus if we define `main` to be our "Hello World" description,
the Haskell runtime performs it when executing our application.

```haskell
main = putStrLn "hello world"
```

We can now execute this from our console. (TODO: Explain `runhaskell`)
