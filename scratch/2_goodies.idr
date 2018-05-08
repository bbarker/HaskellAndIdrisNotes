module Main
main: IO ()
-- main = pure ()

-- Need to figure out hide
-- %hide Point
-- data Point a = Point a a

data Tree a             = Leaf a | Branch (Tree a) (Tree a)

fringe                     :  Tree a -> List a
fringe (Leaf x)            =  [x]
fringe (Branch left right) =  fringe left ++ fringe right

aTree: Tree Int
aTree = Branch (Branch (Leaf 10)(Leaf (-5))) (Leaf 3)

String: Type
String              = List Char
Name: Type
Name                = String
data Address {- Nested-} = None | Addr String
Person: Type
Person              = (Name,Address)


a: Int
a = 3
aa: Int
aa = 3
-- Note that we can't compare Int and Integer:
-- aa: Integer would be a failure
a_aa: Bool
a_aa = a == aa

data MyList b               = Nil | Cons b (MyList b)

-- myList: MyList 3

{- -- TODO
quicksort: List (Ord b) -> List (Ord b)
quicksort  []           =  []
quicksort (x::xs)       =  quicksort [y | y <- xs, y<x ]
                        ++ [x]
                        ++ quicksort [y | y <- xs, y>=x]   
-}

main = do
  putStrLn "hello"
  printLn ([1..5])
  print (fringe aTree)