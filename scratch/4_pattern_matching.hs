

contrived :: ([a], Char, (Int, Float), String, Bool) -> Bool
contrived    ([],  'b',  (1,   2.0),   "hi",   True) = False
contrived    (a,  b,  c,   d,  e) = True

growHead :: [a] -> [a]
growHead nnl@(x:_) = x:nnl
growHead ([]) = []

-- mytake :: Integer -> [a] -> [a]
mytake  0     _           =  []
mytake  _     []          =  []
mytake  n     (x:xs)      =  x : mytake (n-1) xs

-- take1 :: Integer -> [a] -> [a]
take1  _     []         =  []
take1  0     _          =  []
take1  n    (x:xs)      =  x : take1 (n-1) xs

main = do
  print( contrived([],  'b',  (1,   2.0),   "hi",   True) )
  print( contrived([],  'b',  (1,   2.0),   "bye",  True) )
  print( growHead([1,2,3]) )

  print( mytake 0 ([]::[Int]) )
  --
  print( mytake 0 (undefined::[Int]) )
  -- print( take1  0 (undefined::[Int]) ) -- runtime error
  -- print( mytake (undefined::Integer) ([]::[Int]) ) -- runtime error
  print( take1  (undefined::Integer) ([]::[Int]) )

