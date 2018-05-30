# Notes on this Document

Mostly, these are notes from going through [A Gentle Introduction
to Haskell, Version 98](https://www.haskell.org/tutorial/index.html),
while comparing Idris and Haskell. Of course, Idris certainly has features 
that extend beyond Haskell, and the same may be said of Haskell and Idris; for the  former, we will likely not see very much due to this being a 
book on Haskell, but the differences should still be instructive. 
I may also comment on Scala, as it is the FP language I’m most familiar 
with. 

A more useful document for those already familiar with Haskell (which I also use as a reference):
[Idris for Haskellers](https://github.com/idris-lang/Idris-dev/wiki/Idris-for-Haskellers).


## Basic REPL and syntax differences

I'll be using the Haskell REPL `ghci` and the Idris REPL `idris`
(but see the [section](#Scripting) on scripts below. 
In `idris`, it is important to note that to declare 
things **in the REPL**, you have to prefix the declaration with
`:let`, like:

```idris
:let x: Nat; x = 5
:let y = 42
:let data Color         = Red | Green | Blue | Indigo | Violet
```

In Haskell, this would be:

```haskell
let {x :: Int; x = 5} 
let y = 42
data Color              = Red | Green | Blue | Indigo | Violet
```

Notably, Haskell doesn't need `let` for data declarations, whereas the 
Idris Repl does. Idris uses a single `:` to begin type annotation, whereas
Haskell uses `::`; it is almost as if this difference was put here so
you couldn't just copy and paste code between Haskell and Idris
(though I'm sure there are more subtle reasons that I haven't looked into).
Idris also uses `Nat` where possible, which as might
be expected, does not allow negative values:

```idris
:let xneg : Nat; xneg = 5
```

yields the type error `Can't find implementation for Neg Nat`; just use `Int` instead of `Nat`.
In Haskell this would be:

```haskell
let {xneg :: Int; xneg = -5}
```

Conversely, using such refined types 
[in Haskell](https://hackage.haskell.org/package/natural-numbers-0.1.2.0/docs/Data-Natural.html)
(and in [Scala](https://github.com/fthomas/refined)) appears to be possible only through libraries,
and is still a bit unwieldly compared to Idris.

So a point for Idris in terms of flexibility and Haskell/Scala in terms of user friendliness (for now).

As an aside, Scala actually improves over both Idris and Haskell in verbosity
when it comes to combing type declarations with value assignment - maybe the only time Scala will do so, since in Scala, we just write 
`val xneg: Int = -5`.

Another difference in the `ghci` and `idris` is that `ghci` (like Scala REPLs) let you redefine an
existing value; Idris does not. There are pros and cons to both approaches: usability of the former,
[safety](https://github.com/idris-lang/Idris-dev/issues/3733) in the latter.

## Scripting

Both Idris and Haskell are compiled languages, but Haskell has
had the time (or whatever) to make a fairly polished REPL. At
this stage, I'll admit the Idris REPL does not work well for me
beyond the most trivial cases, so generally I'll be using scripts
to test out ideas instead of the REPL.

As we will see, I use the term script loosely, as we are really
compiling and running a single-filie program. To run a Haskell script, make sure you implement `main`, like this:

```haskell
main = do
  putStrLn "hello"
  print (fringe aTree)
```

then use either `runghc my_script.hs` or `runhaskell my_script.hs`.


In Idris, it is similar, but we are required to declare the `Main`
`module` and the type of the `main` function:

```idris
module Main
main: IO ()

--
-- other definitions
--
main = do
  putStrLn "hello"
  print (fringe aTree)
```

Idris scripts can be run as follows:

```
idris my_script.hs -o out && ./out
```

For demos, see `2_goodies.idr` and `2_goodies.hs`
in this repo, that correspond to the first significant chapter,
**Chapter 2: Values, Types, and Other Goodies**.

### NixOS or nixpgs

In Nix, there appears to be a linker issue for Idris executables
unless you use the Idris nix shell; just shart `nix-shell -p idris`,
then you shoudl be ready to run the above "script".
 

# Introduction

Nothing to be said for this section. 

# Values, Types, and Other Goodies

`head []` gives a runtime Exception in Haskell, but a type error in Idris 
(`Can't find a value of type NonEmpty []`). Point for Idris, here, I think.

When considering polymorphism, it would seem that functions in Haskell (and Idris)
will basically behave as methods in Scala. However, functions in Scala are a
[different matter](http://milessabin.com/blog/2012/04/27/shapeless-polymorphic-function-values-1/#methods-vs-function-values),
and are monomorphic. I don't know if any language has the advantage here: 
the entire reason Scala has this dichotomy between methods and functions
is precisely because it is an OO and functional language, functions are represented
as objects, and it doesnt't really make sense to talk about polymorphic objects.
You should always be able to use a method in Scala, e.g, a so called "static method"
in Java would suffice for a function that would need to appear objectless.

While Haskell uses the Hindley-Milner type system, allowing a specific kind of
type inference that allows for the notion of a principal type (a most spcefic type),
I believe Scala and Idris also have a notion of a most principal type that can generally
be inferred, but due to Idris using [dependent types](https://cs.stackexchange.com/questions/12691/what-makes-type-inference-for-dependent-types-undecidable) 
and Scala also using a limited form of them and recently (Scala 3/dotty) being based on 
System F, as well as having subclassing, I imagine they must go beyond HM and play 
[some](https://stackoverflow.com/questions/26691666/why-does-idris-need-mutual) 
[tricks](https://www.scala-lang.org/old/node/4654), but I'm getting a bit far afield 
here. 


## Type Synonyms

In Haskell:

```haskell
type Person               = (Name,Address)
type Name                 = String
data Address              = None | Addr String
```

In Idris, we make use of the fact that `Type` seems
to be a first-class type, and use the standard syntax accordingly,
which, while more natural, is also more verbose.

```idris
Name: Type
Name                = String
data Address {- Nested-} = None | Addr String
Person: Type
Person              = (Name,Address)
```

In Idris, due to eager evaluation, we have to arrange
declarations in the order of use as well, which seems
to be a bit of a dowside to Idris as this introduced a bit
of cognitive overhead, but may have other limitations as well
when it comes to lazy data structures (we will see!).

## Built-in Types Are Not Special

One comment I noticed from this section, pertaining to running
scripts:  Idris is more strict in that it appears to not
allow shadowing. Haskell allows:

```haskell
a:: Int 
a = 3
data MyList a               = Nil | Cons a (MyList a) 
```

Whereas Idris will complain with `a is bound as an implicit`, 
and I need to use a different type argument:

```idris
a: Int 
a = 3
data MyList b               = Nil | Cons b (MyList b)
```

This seems to be less than ideal in Idris at first glance, 
since `a` is clearly a value in the first case and a `type` later on,
but the reason is that `Idris` supports using values in types 
(that is, dependant types). While it actually won't make a mistake in
this case, since the type `MyList 3` makes no sense (apart from the
obvious reasons, try defining `myList: MyList 3` versus 
`myList: MyList Int`).

## List Comprehensions and Arithmetic Sequences

The naive quicksort for Idris is similar to Haskell, but with one important difference.
Let's take a look.

Haskell:

```haskell
quicksort  []           =  []
quicksort (x:xs)        =  quicksort [y | y <- xs, y<x ]
                        ++ [x]
                        ++ quicksort [y | y <- xs, y>=x]
```

Idris:

```idris
quicksort: Ord b => List b -> List b
quicksort  []           =  []
quicksort (x::xs)       =  quicksort [y | y <- xs, y<x ]
                        ++ [x]
                        ++ quicksort [y | y <- xs, y>=x]
```

In Idris, type inference isn't powerful enough to infer the type here.
A side note is that this allows you to ignore, for now, the need to specify
the type class `Ord` for type parameter `b`. To understand how this is
required by Idris and Haskell (as it should be), take a look at
[monad comprehensions](http://docs.idris-lang.org/en/latest/tutorial/interfaces.html#monad-comprehensions).


# Functions

## Lambda Abstractions

Idris is like Haskell, but uses comma syntax for variable separation and a fat arrow (`=>`);

Haskell:

```haskell
myadd :: Num a => a -> a -> a
myadd = \x y -> x + y

```

Idris:

```idris
myadd : Num a => a -> a -> a
myadd = \x, y => x + y
```

## Infix Operators

In Idris, using an infix operator is similar to Haskell, but the fixity and priority
of the operator must be declared before use, e.g.:

```idris
infixr 10 ++~
(++~)                    : List a -> List a -> List a
[]     ++~ ys            =  ys
(x::xs) ++~ ys           =  x :: (xs++~ys)
```

## Functions are Non-strict

This is a good time to read about [laziness in Idris](https://github.com/idris-lang/Idris-dev/wiki/Unofficial-FAQ#why-isnt-idris-lazy).

## "Infinite" Data Structures

There is an advantage for Haskell here, as is fairly well advertised,
and as can be expected after reading the previous section. The following works
in Haskell:

```haskell
ones = 1:ones
-- ...
print(head ones)

```

But the equivalent in Idris will cause a segmentation fault:

```idris
ones: List Int
ones = 1 :: ones
-- ...
printLn(head ones) -- seg fault!
```


```
   |
37 |     printLn(head ones) -- seg fault!
   |             ~~~~~~~~~
When checking right hand side of main with expected type
        IO ()

When checking argument ok to function Prelude.List.head:
        Can't find a value of type
                NonEmpty ones                                                                                                         
```

The error message is not entirely clear, but we basically know what we
are doing wrong. The Idris tutorial 
[explains](http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html)
that codata types, in particular `Stream`, can be used to avoid the issue.
In short, we just write `ones: Stream Int` instead of `ones: List Int`.
But, this is still something of a win for Haskell, or at least a tradeoff
of safety for (possible) performance benefits in Idris, since we can still
construct infinite `List` types (despite the claim in the prior section to
the contrary that "in a total language [e.g. Idris] we don't have undefined 
and  non-terminating terms").

However, there is a way to avoid this in Idris: you can 
[require it](https://stackoverflow.com/questions/50558591/does-idris-have-non-terminating-terms) 
to be a total language by setting `%default total`.

## The Error Function

In Idris, in order to use the `error function`, we have to use 
[ElabReflection](http://docs.idris-lang.org/en/latest/reference/elaborator-reflection.html#elaborator-reflection), e.g.:

```idris
module Main
import Debug.Error

%language ElabReflection
main: IO ()

-- ...

myerr: Int
myerr = error "foo"

-- ...

main = do 
    printLn(myerr)

```

### TODO: Exception effects

I suspect [exceptions](http://docs.idris-lang.org/en/latest/effects/simpleeff.html#exceptions)
may be a better way to deal with errors in Idris, but haven't looked into this at all.


# Case Expressions and Pattern Matching

One thing to note in this section is that Idris 
[doesn't support](http://docs.idris-lang.org/en/v1.3.0/faq/faq.html#why-does-idris-use-double-instead-of-float64) 
32 bit floats at present, so use `Double` instead of `Float`.