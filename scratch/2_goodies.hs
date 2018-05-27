--module Main
-- The following is to make runghc or runhaskell happy:
-- main :: IO ()    -- This says that main is an IO action.
--main = return () -- This tells main to do nothing.

data Point a = Point a a

data Tree a             = Leaf a | Branch (Tree a) (Tree a)

fringe                     :: Tree a -> [a]
fringe (Leaf x)            =  [x]
fringe (Branch left right) =  fringe left ++ fringe right

aTree :: Tree Int
aTree = Branch (Branch (Leaf 10)(Leaf (-5))) (Leaf 3)

-- type String            = [Char]
type Person               = (Name,Address)
type Name                 = String
data Address {- Nested-}  = None | Addr String

a:: Int 
a = 3
aa:: Int
aa = 3
-- Note that we can't compare Int and Integer:
-- aa:: Integer would be a failure
a_aa:: Bool
a_aa = a == aa

-- This is the closest I could get to 
--
data MyList a               = Nil | Cons a (MyList a) 

--myquicksort:: Ord b => [b] -> [b]
myquicksort  []           =  []
myquicksort (x:xs)        =  myquicksort [y | y <- xs, y<x ]
                          ++ [x]
                          ++ myquicksort [y | y <- xs, y>=x]

main = do
  putStrLn "hello"
  print [1..5]
  putStrLn $ ['h', 'e', 'l', 'l', 'o']
  print (fringe aTree)
