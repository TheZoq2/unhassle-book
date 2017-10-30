# Use you a haskell

If you're like me, you're interested in haskell, you've heard good things about it and you've
looked into some tutorials and books. But most of the existing books and tutorials out there
feel long and drawn out, they rarely show you how to write a program that actually does something.

Spending lots of time learning a programming language without actually writing a program in it is
boring and at least for me, a bad way to learn. This "book" will try to take a different approach.
I will show you enough haskell to let you write a program that accepts strings of user input, or perhaps
strings from a file, and output something else.

It is not a rigorous tutorial and I highly suggest that after you've dipped your feet in writing some haskell,
go to a real tutorial and read about the concepts that underly the things discussed here, like functors, monads
and type classes. 

## Function declarations and type signatures

If you've programmed in C++, Java, C# or some other similar language, you're probably used to function
declarations that look like this:

```c++
string bullshitFunction<T>(int aNumber, T somethingGeneric) {
    ..
    return someString
}
```

In haskell, that would look like this

```haskell
bullshitFunction :: Integer -> t -> String
bullshitFunction aNumber somethingGeneric =
    ..
```

As you can see, the types of the parameters are separate from the parameter names. This is because
haskell can automatically infer the type of a function for you. I wouldn't recommend doing that
for top level functions, but as you will see later it will come in handy.

You will probably also notice that the return type of the function is specified after all the input
parameters rather than separately as it is done in most languages you're probably used to.

<!--- TODO: Generics -->



Finally, notice that the function definition looks somewhat like a variable assignment. It's a name,
followed by parameters, followed by an equals sign. A function that takes no parameters and returns
a constant value would therefore look like this:

```haskell
five :: Integer
five = 5
```

You might say that this looks similar to simple variable assignment which is the case. The notion of variables
does not exist in haskell, it's all functions. As mentioned earlier, haskell can infer types for us, so in this
case we can simplify this to

```haskell
five = 5
```


## Function calls

Now that we know how to define a function, we probably want to use it as well. Again, lets compare a function
call in a C++-like language to a haskell function call. Lets look at a simple function that calls another function
and returns the result

```c++
float callingSomething() {
    return anotherFunction(param1, param2);
}
```

```haskell
callingSomething :: Float
callingSomething =
    anotherFunction param1 param2
```

Like before, we have a type definiton on a separate line with the name repeated. And like before, the compiler
can figure out the return type of `callingSomething` so we don't actually need the type annotation.

Lets instead focus on the actual function call. As you can see, just like in the definition, all the parenthisis are
gone and parmeters to the function are just separated by spaces instead of `,`.


## Partially applied functions

In the example above, the C++-like code is very similar to the haskell code, just with a different syntax. But the haskell
code allow you to easily express a concept that barley exists in non-functional languages: partially applied functions.

It might sound a bit scary, and it takes a while to get used to, but the concept is simple. Let's say you have a function
that adds two numbers together that looks like this

```haskell
add :: Integer -> Integer -> Integer
add x y =
    x + y
```

What if we, for whatever reason want a function that adds 5 to every number (why we might want something like that will
hopefully become clear later). In haskell, we simply 'call' the function with x specified but not y:

```haskell
>>> add 5
function Function (Integer -> Integer)
```

<!--- Clarify the output here -->

Unlike an imperative language where this would fail, in haskell it gives us a new function. This new function
has the first fields of the old function already 'filled' in but allows you to specify the remaining ones. This
is called a partially applied function.

This also explains the somewhat strange syntax for function calls and declarations. The return type of of a function
is as we saw previosuly the last type specified in the type declaration. But we also saw that a function that takes
no parameters is essentially a constant. 
