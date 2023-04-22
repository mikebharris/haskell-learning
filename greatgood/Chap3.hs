lucky :: Int -> String
lucky 7 =  "LUCKY NUMBER SEVEN!"
lucky x = "foo"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors2 :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

tell :: (Show a) => [a] -> String
tell [] = "Empty"
tell (x:[]) = "One thing: " ++ show x
tell (x:y:[]) = "Two things: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "Long list"

firstLetter :: String -> String
firstLetter [] = "empty"
firstLetter all@(x:xs) = "first letter of " ++ all ++ " is " ++ [x]