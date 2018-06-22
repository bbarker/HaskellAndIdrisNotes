module Main
import Debug.Error

%language ElabReflection
%default total

myelem: (Eq a) => a -> List a -> Bool
myelem x []     = False
myelem x (y::ys) = x == y || (myelem x ys)

main: IO ()


main = do
  printLn(1 `myelem` [2, 1, 3])
  printLn(myelem 4 [2, 1, 3])
