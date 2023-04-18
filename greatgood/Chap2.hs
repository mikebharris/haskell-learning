add3together :: Int -> Int -> Int -> Int
add3together x y z = x + y + z

add2to x y = add3together 2 x y

factorial :: Integer -> Integer
factorial x = product [1..x]

circumference :: Float -> Float
circumference r = 2 * pi * r

addtogether:: (Int, Int) -> Int
addtogether (a, b) = a + b

addtogether2 :: Num a => (a, a) -> a
addtogether2 (a, b) = a + b

range1 :: Enum a => a -> a -> [a]
range1 x y = [x..y]