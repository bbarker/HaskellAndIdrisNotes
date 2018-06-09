module Main
import Debug.Error

-- %language ElabReflection
-- %default total -- casetake is not proved total as implemented
main: IO ()


contrived : (List a, Char, (Int, Double), String, Bool) -> Bool
contrived   ([]    ,  'b',  (1,   2.0),   "hi" , True) = False
contrived    (a,  b,  c,   d,  e) = True

growHead : List a -> List a
growHead nnl@(x::_) = x::nnl
growHead ([]) = []

mytake : Integer -> List a -> List a
mytake  0     _           =  []
mytake  _     []          =  []
mytake  n     (x::xs)      =  x :: mytake (n-1) xs

take1 : Integer -> List a -> List a
take1  _     []         =  []
take1  0     _          =  []
take1  n    (x::xs)      =  x :: take1 (n-1) xs

casetake : Integer -> List a -> List a
casetake m ys = case (m, ys) of
                  (0, _)       => []
                  (_, [])      => []
                  (n, x::xs)   => x :: casetake (n-1) xs

iffer : Bool -> a -> a -> a
iffer cond v1 v2 = case cond of
                     True => v1
                     False => v2

main = do
  printLn("hi")
  printLn( contrived {a = Nat} ([],  'b',  (1,   2.0),   "hi",   True) )
  printLn( contrived {a = Nat} ([],  'b',  (1,   2.0),   "bye",  True) )
  printLn( growHead([1,2,3]) )

  printLn( mytake {a = Nat} 0 [] )
  --

  -- printLn( mytake {a = Nat} 0 ?undefined ) -- runtime error: eager eval
  -- printLn( take1  {a = Nat} 0 ?undefined) -- runtime error
  -- printLn( mytake {a = Nat} ?undefined [] ) -- runtime error
  -- printLn( take1  {a = Nat} ?undefined [] ) -- runtime error: eager eval
    -- see: https://stackoverflow.com/questions/50668169/is-it-possible-to-make-pattern-matching-lazy-in-idris

  printLn( casetake 3 [1,2,3,4])

  printLn(iffer (1 < 2) 1 2)
  printLn(iffer (2 < 1) 1 2)
  printLn(if 1 < 2 then 1 else 2)
  printLn(if 2 < 1 then 1 else 2)
