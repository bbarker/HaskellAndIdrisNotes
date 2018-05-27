
mymap          :: (a->b) -> [a] -> [b]
mymap f []     =  []
mymap f (x:xs) = f x : mymap f xs


myadd :: Num a => a -> a -> a
myadd = \x y -> x + y

main = do
    print (mymap (+1) [1, 2, 3])
    print (myadd 3 5)
