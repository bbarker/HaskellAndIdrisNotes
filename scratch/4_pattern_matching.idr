module Main
import Debug.Error

-- %language ElabReflection
%default total
main: IO ()


contrived : (List a, Char, (Int, Double), String, Bool) -> Bool
contrived   ([]    ,  'b',  (1,   2.0),   "hi" , True) = False
contrived    (a,  b,  c,   d,  e) = True

main = do
  printLn("hi")
  printLn( contrived([],  'b',  (1,   2.0),   "hi",   True) )
  -- printLn( contrived([],  'b',  (1,   2.0),   "bye",  True) )