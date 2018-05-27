
mymap          :: (a->b) -> [a] -> [b]
mymap f []     =  []
mymap f (x:xs) = f x : mymap f xs


myadd :: Num a => a -> a -> a
myadd = \x y -> x + y

(++~)                    :: [a] -> [a] -> [a]
[]     ++~ ys            =  ys
(x:xs) ++~ ys            =  x : (xs++~ys)

main = do
    print (mymap (+1) [1, 2, 3])
    print (myadd 3 5)
    print( [1, 2] ++~ [3, 4])
