
mymap          :: (a->b) -> [a] -> [b]
mymap f []     =  []
mymap f (x:xs) = f x : mymap f xs


myadd :: Num a => a -> a -> a
myadd = \x y -> x + y

(++~)                    :: [a] -> [a] -> [a]
[]     ++~ ys            =  ys
(x:xs) ++~ ys            =  x : (xs++~ys)

add5 :: Int -> Int
add5 y = (5+) y

const5:: a -> Int
const5 x = 5

ones = 1:ones
twos = map (2*) ones

myerr:: Int
myerr = error "foo"

main = do
    print (mymap (+1) [1, 2, 3])
    print (myadd 3 5)
    print( [1, 2] ++~ [3, 4])
    print(add5 3)
    print(1 `elem` [1, 2, 3])
    print(const5 "foo")
    print(const5 (1.0/0.0))
    print((1/0))
    print(head ones)
    print(take 2 twos)
    print(myerr)
