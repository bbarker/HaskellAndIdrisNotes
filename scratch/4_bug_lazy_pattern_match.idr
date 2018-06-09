-- https://stackoverflow.com/questions/50668169/is-it-possible-to-make-pattern-matching-lazy-in-idris
-- Currently produces an error
module Main

mytake : Integer -> Lazy (List a) -> List a
mytake  0     _          = []
mytake  _     []         = []
mytake  n     (x::xs)    = x :: mytake (n-1) xs


main : IO ()
main = printLn (mytake {a = Nat} 0 ?undefined)