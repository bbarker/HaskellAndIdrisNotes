

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

--casetake :: Integer -> [a] -> [a]
casetake m ys = case (m, ys) of
                  (0, _)        -> []
                  (_, [])       -> []
                  (n, x:xs)     -> x : casetake (n-1) xs


iffer :: Bool -> a -> a -> a
iffer cond v1 v2 = case cond of
                     True -> v1
                     False -> v2

server (req:reqs)            = process req : server reqs
client initreq ~(resp:resps) = initreq : client resp resps
process req = req+1
reqs  = client 0 resps
resps = server reqs

fib@(1:tfib)    = 1 : 1 : [ a+b | (a,b) <- zip fib tfib ]

main = do
  print( contrived([],  'b',  (1,   2.0),   "hi",   True) )
  print( contrived([],  'b',  (1,   2.0),   "bye",  True) )
  print( growHead([1,2,3]) )

  print( mytake 0 ([]::[Int]) )
  --
  print( mytake 0 (undefined::[Int]) )
  -- print( take1  0 (undefined::[Int]) ) -- runtime error
  -- print( mytake (undefined::Integer) ([]::[Int]) ) -- runtime error
  print( take1  (undefined::Integer) ([]::[Int]) ) -- runtime error: eager eval

  print( casetake 3 [1,2,3,4])

  print(iffer (1 < 2) 1 2)
  print(iffer (2 < 1) 1 2)
  print(if 1 < 2 then 1 else 2)
  print(if 2 < 1 then 1 else 2)
  print(mytake 5 reqs)
  print(mytake 5 fib)




