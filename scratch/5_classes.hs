x `myelem` []     = False
x `myelem` (y:ys) = x == y || (x `myelem` ys)

main = do
  print(myelem 1 [2, 1, 3])
  print(4 `myelem` [2, 1, 3])