rev1 : List a -> List a
rev1 xs = revAcc [] xs where
  revAcc : List a -> List a -> List a
  revAcc acc [] = acc
  revAcc acc (x :: xs) = revAcc (x :: acc) xs

myList: List Int
myList = [-1,2,3]

myListRev: List Int
myListRev = rev1 myList
