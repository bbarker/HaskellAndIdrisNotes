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
aa: Integer 
aa = 3

main = do
  putStrLn "hello"
  print (fringe aTree)