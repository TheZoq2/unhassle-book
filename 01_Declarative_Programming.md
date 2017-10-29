# Declarative Programming

Haskell is a great tool for writing very reliable programs.

The way it achieves that is by being a fully declarative language:
You tell what you want, not how to get it.

Your programs are constructed the same way:
Instead of forcing the computer to do something,
you specify what has to be done.

The Haskell runtime then lets your computer do exactly what you want.

The type for those descriptions are called `IO`,
which stands for Input/Output.

A description of a program producing an `Int` is therefore of type `IO Int`.
Similarly the types `IO String` and `IO Bool` exist.
The type `IO ()` is used to denote a program, which does not produce any value at all.

Let us create a description of a program, whose result is nothing (`IO ()`),
but which prints a string.

We can do this using the `putStrLn` ("put string line") function.
`putStrLn` has the type of a function transforming a `String` into an `IO ()`.

```haskell
putStrLn :: String -> IO ()
```

This means, that we can give `putStrLn` a string and get an `IO ()`.

```haskell
putStrLn "hello world" :: IO ()
```

Note, that it doesn't print the string, it describes a program that does.

In Haskell, the description of the main task to execute is called `main`.
Thus if we define `main` to be our "Hello World" description,
the Haskell runtime performs it when executing our application.

```haskell
main = putStrLn "hello world"
```

We can now execute this from our console. (TODO: Explain `runhaskell`)
