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

It is really important to understand, that we don't write the program,
we write the job requirements.

Our `IO` programs are values, so we can operate on them.

Let's say we have to jobs, which we wanna concatenate into one large job.

We have our TO-DO lists 1 and 2, and we want to merge them into one large TO-DO list.

```
List 1
  - Do A
  - Do B

List 2
  - Do C
  - Do D

Large List
  - Do everything on List 1
  - Do everything on List 2
```

To do this in Haskell, we use the `>>` operator.

```haskell
main = putStrLn "Hello" >> putStrLn "World"
```

We just build a job, where we do the first one (print "Hello") and then the second
one (print "World").

Now, those printing jobs both have a type `IO ()`, which means they effectively
don't produce a meaningful value.

But what if we had a job that does?

One example would be the predefined `getLine`.

```haskell
getLine :: IO String
```

It is a simple job eventually producing a `String`.

Of course we could do something like this:

```haskell
main =
  getLine >> putStrLn "Hello World"
```

But then we would read a line just to effectively ignore it
and print our generic text.

What we want to do is write a program,
that does something with the `String` `getLine` produces.

We could simply write a function, that takes a `String` and returns
a printing job as the first step.

```haskell
reactToInput string =
  putStrLn "You said: " >> putStrLn string
```

As we can glue two jobs together using `>>`,
the `>>=` operator allows us to concatenate an `IO a`,
that is a job producing something of type `a`,
with a function `a -> IO b`, that is a function taking that `a`
and returning another job to continue with.

We have `getLine :: IO String` and we have `reactToInput :: String -> IO ()`,
so we can glue them together using `>>=` now.

```haskell
main =
  getLine >>= reactToInput
```

Of course, `>>=` and `>>` can be arbitrarily chained,
so we could add another job or "reactor" at the beginning or end.

```haskell
main =
  putStrLn "Alright, here it goes - just enter something!"
  >> getLine
  >>= reactToInput
  >> putStrLn "And we're done!"
```
