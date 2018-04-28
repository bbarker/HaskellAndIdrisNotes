-- The following is to make runghc or runhaskell happy:
main :: IO ()    -- This says that main is an IO action.
--main = return () -- This tells main to do nothing.

data Point a = Point a a

data Tree a             = Leaf a | Branch (Tree a) (Tree a)

fringe                     :: Tree a -> [a]
fringe (Leaf x)            =  [x]
fringe (Branch left right) =  fringe left ++ fringe right

aTree :: Tree Int
aTree = Branch (Branch (Leaf 10)(Leaf (-5))) (Leaf 3)

main = do
  putStrLn "hello"
  print (fringe aTree)
