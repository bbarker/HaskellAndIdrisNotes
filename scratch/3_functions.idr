module Main
main: IO ()


mymap           : (a->b) -> List a -> List b
mymap f []      = []
mymap f (x::xs) = f x :: mymap f xs

myadd : Num a => a -> a -> a
myadd = \x, y => x + y

infixr 10 ++~
(++~)                    : List a -> List a -> List a
[]     ++~ ys            =  ys
(x::xs) ++~ ys           =  x :: (xs++~ys)

main = do
    printLn (mymap (+1) [1, 2, 3])
    printLn (myadd 3 5)
    printLn( [1, 2] ++~ [3, 4])
