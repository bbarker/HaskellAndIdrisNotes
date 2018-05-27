module Main
main: IO ()


mymap           : (a->b) -> List a -> List b
mymap f []      = []
mymap f (x::xs) = f x :: mymap f xs

myadd : Num a => a -> a -> a
myadd = \x, y => x + y

main = do
    printLn (mymap (+1) [1, 2, 3])
    printLn (myadd 3 5)