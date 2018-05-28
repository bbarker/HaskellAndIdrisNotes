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

add5: Int -> Int
add5 y = (5+) y

const5: a -> Int
const5 x = 5

-- ones: List Int
ones: Stream Int
ones = 1::ones

main = do
    printLn (mymap (+1) [1, 2, 3])
    printLn (myadd 3 5)
    printLn( [1, 2] ++~ [3, 4])
    printLn(add5 3)
    printLn(1 `elem` [1, 2, 3])
    printLn(const5 "foo")
    --printLn(const5 (1/0))
    printLn(const5 (1.0/0.0))
    printLn((1.0/0.0))
    printLn(const5 (head ones))
    printLn(head ones) -- seg fault if List instead of Stream





