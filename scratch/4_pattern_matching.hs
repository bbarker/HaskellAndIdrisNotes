

contrived :: ([a], Char, (Int, Float), String, Bool) -> Bool
contrived    ([],  'b',  (1,   2.0),   "hi",   True) = False
contrived    (a,  b,  c,   d,  e) = True

growHead :: [a] -> [a]
growHead nnl@(x:xs) = x:nnl
growHead ([]) = []

main = do
  print( contrived([],  'b',  (1,   2.0),   "hi",   True) )
  print( contrived([],  'b',  (1,   2.0),   "bye",  True) )
  print(growHead([1,2,3]))