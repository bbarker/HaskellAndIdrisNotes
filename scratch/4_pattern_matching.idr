module Main
import Debug.Error

-- %language ElabReflection
%default total
main: IO ()


contrived : (List a, Char, (Int, Double), String, Bool) -> Bool
contrived   ([]    ,  'b',  (1,   2.0),   "hi" , True) = False
contrived    (a,  b,  c,   d,  e) = True

growHead : List a -> List a
growHead nnl@(x::xs) = x::nnl
growHead ([]) = []

main = do
  printLn("hi")
  printLn( contrived {a = Nat} ([],  'b',  (1,   2.0),   "hi",   True) )
  printLn( contrived {a = Nat} ([],  'b',  (1,   2.0),   "bye",  True) )
  printLn( growHead([1,2,3]) )