module Main
main: IO ()


mymap           : (a->b) -> List a -> List b
mymap f []      = []
mymap f (x::xs) = f x :: mymap f xs

main = do
    print (mymap (+1) [1, 2, 3])