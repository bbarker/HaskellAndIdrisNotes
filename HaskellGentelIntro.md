# Notes on this Document

Mostly, these are notes from going through [A Gentle Introduction
to Haskell, Version 98](https://www.haskell.org/tutorial/index.html),
while comparing Idris and Haskell. Of course, Idris certainly has features 
that extend beyond Haskell, and the same may be said of Haskell and Idris; for the 
former, we will likely not see very much due to this being a book on Haskell,
but the differences should still be instructive. I may also comment on Scala, 
as it is the FP language I’m most familiar with.


## Basic REPL and syntax differences

I'll be using the Haskell REPL `ghci` and the Idris REPL `idris`. In `idris`,
it is important to note that to declare things **in the REPL**, you have to
prefix the declaration with `:let`, like:

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

Notably, Haskell doesn't need `let` for data declarations, whereas the Idris Repl does. Idris
also uses `Nat` where possible, which as might be expected, does not allow negative values:

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

Another difference in the `ghci` and `idris` is that `ghci` (like Scala REPLs) let you redefine an
existing value; Idris does not. There are pros and cons to both approaches: usability of the former,
[safety](https://github.com/idris-lang/Idris-dev/issues/3733) in the latter.

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
