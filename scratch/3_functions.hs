
mymap          :: (a->b) -> [a] -> [b]
mymap f []     =  []
mymap f (x:xs) = f x : mymap f xs

main = do
    print (mymap (+1) [1, 2, 3])