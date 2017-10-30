
This chapter will introduce the function declaration syntax of haskell by comparing it to
function definitions in languages like Java, C++ or C#. I.E, statically typed languages
with generics.

# Basic function definition

Lets start with a simple function that adds two numbers and returns the result.

```C++
int add(int x, int y) {
    return x + y
}
```

```haskell
add :: Integer -> Integer -> Interger
add x y =
    x + y
```

As you can see, the function definitions look very different, but if you look closely you will notice that
both languages contain the same components, only moved around a bit.

The first thing you might notice is that the name of the function is specified twice, once on the first
line and once on the second. The first line specifies the type of the function while the rest specifies what
the function does.

## Type annotations

Lets have a closer look at the first line:
```haskell
add :: Integer -> Integer -> Integer
```

A type annotation of a function starts with the name of the function followed by `::`. After that,
the type of each parameter of the function is speicifed followed by the return value. The
types are separated by `->` and the second type specified specifies the type of the second parameter.

This notation might seem strange, but as we dive into functional programming and concepts like
partially applied functions, you will notice that this notation is very convenient.

## Function definitions

Lets shift our attention to the body of the function
```haskell
add x y =
    x + y
```

There are two main difference between the C++-like function and haskell function.
First, the parameters are not surrounded by parentheses and they are separated by
spaces rather than `,`. This will seem strange at first but again, as you dive into
the language more, you will notice that it makes things much neater down the line.

The other difference is that normal functions in haskell do not have a return statement,
you simply write the value you want to return in the function body. 

## Generics

Let's move away from our `add` example and look at a function that is more general. So far, we've only
looked used integers, but lets say we want to write a function to add all values in a list. For now, we will
only look at the defintion of the function and not the implementation:

```C++
int sum(List<int> values) {
    //Implementation details
}
```
```haskell
sum :: List Integer -> Integer
sum values =
    -- implementation details
```

*Note that lists are denoted as [Integer] instead of List Integer in haskell, but for demonstration purposes,
lets assume this is how they look. Plenty of other types do*

As you can tell from this example, 'generic parameters' to types are specified by the typename followed
by a space followed by the type of the generic parameter.


## Let and where

*I feel like people who are experimenting with the language as they read will want to know how to
write an 'imperative' function with a bunch of variable assignments and probably side effects. Should
we introduce let notation?

Having let notation might make it easier to give examples in the following sections*

## Type inference

You might wonder why the name of the function has to be specified twice. The simple reason is that 
type annotations for functions are optional in haskell which means that our add function could
be simplified to this:

```haskell
add x y = x + y
```

<!--- Still type chekced -->


## Variables are functions




