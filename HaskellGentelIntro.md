# Notes on this Document

Mostly, these are notes from going through [A Gentle Introduction
to Haskell, Version 98](https://www.haskell.org/tutorial/index.html),
while comparing Idris and Haskell. Of course, Idris certainly has features 
that extend beyond Haskell, and the same may be said of Haskell and Idris; for the  former, we will likely not see very much due to this being a 
book on Haskell, but the differences should still be instructive. 
I may also comment on Scala, as it is the FP language I'm most familiar 
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
[explains](http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html#codata-types)
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

As usual, Idris doesn't infer types as well as Haskell or Scala, 
so in the `contrived` example, whereas the call for Haskell looks like this,

```haskell
contrived([],  'b',  (1,   2.0),   "hi",   True)
```

Idris requires [some method](https://stackoverflow.com/questions/50604648/how-to-do-basic-pattern-matching-with-formal-parameters-value-binding-in-idris) 
for ascribing a type, e.g.:


```idris
contrived {a = Nat} ([],  'b',  (1,   2.0),   "hi",   True)
```

Sometimes ascribing a type to `[]` appears to be necessary in Haskell as well; this can be done
like `([]::[Int])` (the equivalent syntax doesn't appear to be supported in Idris). We will
need this in the next section.

## Pattern-Matching Semantics

I didn't notice the `sign` example didn't work in Idris on the first pass, so 
see additional discussion below in 
[Lexical Scoping and Nested Forms](#lexical-scoping-and-nested-forms).

But briefly we need to use `with` in Idris (or a combination of `let` and `case`). 
Here is the Haskell example:

```haskell
mysign x |  x >  0        =   1
         |  x == 0        =   0
         |  x <  0        =  -1
```

And here is a working Idris equivalent:

```idris
mysign: Integer -> Integer
mysign x with (x > 0, x < 0)
 |  (True, _)       =   1
 |  (_, True)       =  -1
 |  (False, False)  =   0
```

## Pattern-Matching Semantics: An Example

In Haskell, to actually get the `take` (renamed `mytake`) and `take1` examples to call,
we have to use a value of type `_|_`, which we could easily define by throwing an error
from a function value, but Haskell [provides](https://wiki.haskell.org/Bottom) 
`undefined` for this:

```haskell
mytake 0 (undefined::[Int])
take1  0 (undefined::[Int]) -- runtime error
mytake (undefined::Integer) ([]::[Int]) -- runtime error
take1  (undefined::Integer) ([]::[Int])
```

In Idris, there is no `undefined` defined, but Idris has the more robust notion of
[holes](http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html#holes), and
we can simply use `?undefined`, for example:

```idris
mytake {a = Nat} ?undefined [] 
```

It becomes apparent here that the `{a = Type}` syntax in Idris is quite handy when
having multiple function arguments that have the same type parameter `a`.

Since Idris [isn't lazy](https://github.com/idris-lang/Idris-dev/wiki/Unofficial-FAQ#why-isnt-idris-lazy),
however, all of the above function calls will give a runtime error in Idris. There is a potential
way to get around this using `Lazy (List a)` as input to `mytake`, but this currently
generates a [compiler error](https://github.com/idris-lang/Idris-dev/issues/4131).


## Case Expressions

Idris uses `=>` in case expressions (like Scala), whereas Haskell uses `->`.

## Lazy Patterns

Idris doesn't have lazy pattern matching; function arguments get evaluated upon
call. We could make the second argument to `client` lazy by changing the type from
`Stream Int` to `Lazy (Stream Int)`, but, the stream being split (`::`) causes
evaluation of the `respOut`, which unfortunately, hasn't been defined upon
the call to `client`. To avoid this we need to get rid of the split. So instead of writing

```idris
client initreq (resp :: resps) = initreq :: client resp resps
```

we write:

```idris
client initreq resps = initreq :: client (head resps) (tail resps)
```

Now this seems very familiar; it was identical to the initial solution proposed by
**AGItH**:

```haskell
client init resps         = init : client (next (head resps)) (tail resps)
```

So what happens if we remove `Lazy` from `Lazy (Stream Int)`? After all, if like me,
you might be thinking _but isn't `Stream` already lazy?_ Well, the bad news is that
we still get a segmentation fault. We need to recall that `Stream` is only lazy in
its tail - the head of Stream will still be evaluated when passed into the client
function. Stream's definition is just:

```idris
data Stream : Type -> Type where
(::) : (value : elem) -> Inf (Stream elem) -> Stream elem
```

We can see that cons (`::`) has two parameters, and the first (the head) does not
have `Inf` applied. Going into more detail for [codata typs](http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html#codata-types) (which was mentioned earlier
when introducing `Stream`), `Inf` is what actually makes the value lazy in Idris.

So we might now wonder: do we really need Stream at all? If we try we get the error:

```
   |
10 | client initreq resps = initreq :: client (head resps) (tail resps)
   |                                           ~~~~~~~~~~
When checking right hand side of client with expected type
	List Int

When checking argument ok to function Prelude.List.head:
	Can't find a value of type
		NonEmpty (Force resps)
```



Ah, `resps` is evaluated, and even though it is lazy upon function call thanks to 
`Lazy`, the data type itself is _not_ lazy. In the Idris tutorial it is stated:

>A value of type `Lazy a` is unevaluated until it is forced by `Force`.

And, clearly, `head resps` is empty when `client` is first called on (on `respsOut`).
So we can see that we still need a `Stream`. `Lazy` seems to be interchangeable with
`Inf` in this context. Apparently, the difference stems from how `Lazy` 
[can be erased](https://stackoverflow.com/questions/48236700/why-not-always-use-inf-instead-of-lazy-in-idris)
during totality checking. However, Idris still can not prove the program is total as written
if we add `%default total` with `Lazy`. Likely, more advanced methods are needed from
Idris to show this.

So we can see that we can still achieve laziness in `Idris`, but it is not as simple,
and it is more verbose. Point for Haskell, perhaps, though again, the entire reason
Idris is eagerly evaluated is to be able to have more 
[stable performance characteristics](http://docs.idris-lang.org/en/latest/faq/faq.html#why-does-idris-use-eager-evaluation-rather-than-lazy) allowing lower level
code to be written. We might think of Haskell as being somewhere closer to Java, 
whereas Idris would be closer to C, though not as close as [ATS](http://www.ats-lang.org/),
which shares identical semantics to C.

Enough digression; the complete solution [credit](https://stackoverflow.com/questions/50785672/how-does-one-use-mutually-defined-streams-in-idris)
looks like:

```idris
process: Int -> Int
process req = req+1
server: Stream Int -> Stream Int
server (req :: reqs)            = process req :: server reqs
client: Int -> Lazy (Stream Int) -> Stream Int
client initreq resps = initreq :: client (head resps) (tail resps)
mutual
  reqsOut: Stream Int
  respsOut: Stream Int
  reqsOut  = client 0 respsOut
  respsOut = server reqsOut
```

An important point that we've realized a bit ahead of time thanks to needing to use
 `mutual` in Idris is [indentation blocks](https://en.wikibooks.org/wiki/Haskell/Indentation):
in certain contexts, indentation does matter, such as in `do` blocks, which we did already
see as part of our `main` `do` block, but this may have been overlooked as a stylistic effect only.
It is not just for `style`.


It appears that pattern binding at the value definition level does not work in Idris, e.g. the
following Haskell program, when converted to Idris, will fail to parse:

```haskell
fib@(1:tfib)    = 1 : 1 : [ a+b | (a,b) <- zip fib tfib ]
```

## Lexical Scoping and Nested Forms

### Let Expressions


One issue that came up was converting the following Haskell code to Idris:

```haskell
a = 2
b = 5
c = 3
d = 1

f1 = let y   = a*b
         f x = (x+y)/y
     in f c + f d

```

In Idris, this ends up being a fair bit more verbose because we have to: 

1. Specify types of the input values, as usual
2  Specify types of values declared in the `let` expression in an unusual way
3. For function values, we must seemingly use lambda functions (related to **2** above)
4. We need to use `cast` to convert from Int to Double, which is implicitly used in Haskell.

```idris

a: Int
b: Int
c: Int
d: Int
f1: Double

a = 2
b = 5
c = 3
d = 1

f1 = let y: Int = a*b
         f: (Int -> Double) = (\ x => (cast(x+y))/(cast y))
     in f c + f d

```

This slightly unusual method is actually rather familiar in Scala
(and many other languages): we put the type and the value assignment
on the same line; we can'd do this in Haskell: `foo:: Int = 3`, but in
Idris, it is possible to do `foo: Int = 3`.

One final (minor) note that is worth mentioning is that there seems to be difference
in precision (or more likely, in display) when printing the `Double` 
value `f1`. In Haskell we get `2.4000000000000004`, in Idris 
it is just displayed as `2.4`.


### Where Clauses

The `where` keyword appears to be similar to Haskell at first glance, but has an
important difference in Idris: the values defined in a `where` clause only apply to 
one of the function expressions in a function defined using pattern matching. For instance,
in Haskell, we can write:

```haskell
dubOrTripSq y | x > 2  = 2 * x
              | x <= 2 = 3 * x
            where x = y * y
```

But in Idris this is:

```idris
dubOrTripSq: Int -> Int
dubOrTripSq (y) with (y*y > 2)
  | True  = 2 * x where
    x: Int
    x = y * y
  | False = 3 * x where
    x: Int
    x = y * y

```

Obviously the code duplication is not ideal, can can be gotten around in Idris by doing
(for instance) `let` combined with `case`:

```idris
dubOrTripSq: Int -> Int
dubOrTripSq (y) =
  let x = y * y
  in case (x > 2) of
    True  => 2 * x
    False => 3 * x
```



The `where` example covered exposed some difference in syntax and features between the two
languages. While Haskell has `|` guards, Idris has `with`, which appears to be more
flexible in that it supports Idris's dependant types (not covered here), but also
less flexible in that you can only pattern match on a single intermediate value
([ref](https://stackoverflow.com/questions/24981234/is-it-possible-to-use-guards-in-function-definition-in-idris)), 
whereas with guards you can check some arbitrary number of values,
and the first to match will be applied. While rigorous and convenient, I suppose
the Haskell approach could also more easily lead to logic errors when the programmer
doesn't consider possible overlap between the different guard values being
matched against. And of course, one could always uses tuples or other aggregate
data types in Idris.


Note that earlier we did discuss another form of guard (`|`) that is supported by Idris,
but this works in the context of `Applicative` functors.

## Layout

The topic of this section at least appears to be much the same between Haskell and Idris,
though it is possible differences between the two may exist (I haven't checked in detail
as yet). 
