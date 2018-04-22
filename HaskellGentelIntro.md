# Notes on this Document

Mostly, these are notes from going through [A Gentle Introduction
to Haskell, Version 98](https://www.haskell.org/tutorial/index.html),
while comparing Idris and Haskell. Of course, Idris certainly has features 
that extend beyond Haskell, and the same may be said of Haskell and Idris; for the 
former, we will likely not see very much due to this being a book on Haskell,
but the differences should still be instructive. I may also comment on Scala, 
as it is the FP language I’m most familiar with.

# Introduction

Nothing to be said for this section

# Values, Types, and Other Goodies

`head []` gives a runtime Exception in Haskell, but a type error in Idris 
(`Can't find a value of type NonEmpty []`). Point for Idris, here, I think.

When considering polymorphism, it would seem that functions in Haskell (and Idris)
will basically behave as methods in Scala. However, functions in Scala are a
[different matter](http://milessabin.com/blog/2012/04/27/shapeless-polymorphic-function-values-1/#methods-vs-function-values),
and are monomorphic. So, this is a slight advantage for Idris and Haskell over
Scala, but the entire reason Scala has this dichotomy between methods and functions
is precisely because it is an OO and functional language, functions are represented
as objects, and it doesnt't really make sense to talk about polymorphic objects.
