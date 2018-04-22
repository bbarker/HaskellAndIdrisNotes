# Notes on this Document

Mostly, these are notes from going through [A Gentle Introduction
to Haskell, Version 98](https://www.haskell.org/tutorial/index.html),
while comparing Idris and Haskell. Of course, Idris certainly has features 
that extend beyond Haskell, and the same may be said of Haskell and Idris; for the 
former, we will likely not see very much due to this being a book on Haskell,
but the differences should still be instructive. I may also comment on Scala, 
as it is the FP language I’m most familiar with.

I'll be using the Haskell REPL `ghci` and the Idris REPL `idris`. In `idris`,
it is important to note that to declare things **in the REPL**, you have to
prefix the declaration with `:let`, like:

```idris
:let x: Nat; x = 5
:let data Color              = Red | Green | Blue | Indigo | Violet
```

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
You should always be able to use a method in Scala, e.g., a so called "static method"
in Java would suffice for a function that would need to appear objectless.

While Haskell uses the Hindley-Milner type system, allowing a specific kind of
type inference that allows for the notion of a principal type (a most spcefic type),
I believe Scala and Idris also have a notion of a most principal type that can generally
be inferred, but due to Idris using [dependent types](https://cs.stackexchange.com/questions/12691/what-makes-type-inference-for-dependent-types-undecidable) 
and Scala also using a limited form of them and recently (Scala 3/dotty) being based on 
System F, as well as having subclassing, I imagine they must go beyond HM and play 
[some tricks](https://www.scala-lang.org/old/node/4654), but I'm getting a bit far afield 
here. 
