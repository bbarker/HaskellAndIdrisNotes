rev1 :: [a] -> [a]
rev1 xs = revAcc [] xs where
  revAcc :: [a] -> [a] -> [a]
  revAcc acc [] = acc
  revAcc acc (x : xs) = revAcc (x : acc) xs

myList:: [Int]
myList = [-1,2,3]

myListRev:: [Int]
myListRev = rev1 myList
