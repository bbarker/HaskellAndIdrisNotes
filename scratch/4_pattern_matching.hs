

contrived :: ([a], Char, (Int, Float), String, Bool) -> Bool
contrived    ([],  'b',  (1,   2.0),   "hi",   True) = False
contrived    (a,  b,  c,   d,  e) = True

main = do
  print( contrived([],  'b',  (1,   2.0),   "hi",   True) )
  print( contrived([],  'b',  (1,   2.0),   "bye",  True) )