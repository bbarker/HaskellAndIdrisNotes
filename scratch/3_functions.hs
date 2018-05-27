
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

main = do
    print (mymap (+1) [1, 2, 3])
    print (myadd 3 5)
    print( [1, 2] ++~ [3, 4])
    print(add5 3)
    print(1 `elem` [1, 2, 3])
