module Main
import Debug.Error

-- %language ElabReflection
%default total
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

main = do
  printLn("hi")
  printLn( contrived {a = Nat} ([],  'b',  (1,   2.0),   "hi",   True) )
  printLn( contrived {a = Nat} ([],  'b',  (1,   2.0),   "bye",  True) )
  printLn( growHead([1,2,3]) )

  printLn( mytake {a = Nat} 0 [] )
  --

  printLn( mytake {a = Nat} 0 ?undefined )
  -- printLn( take1  {a = Nat} 0 ?undefined) -- runtime error
  -- printLn( mytake {a = Nat} ?undefined [] ) -- runtime error
  printLn( take1  {a = Nat} ?undefined [] )