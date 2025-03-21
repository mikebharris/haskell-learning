x::Integer
x=2

half n = fromIntegral n/2

divhalf :: Int -> Int
divhalf n = n `div` 2

printDouble :: Int -> String
printDouble n = show (2*n)

z::Integer
z = read "6"

y = read "6" :: Integer

-- As each argument is passed to makeAddress, write out the type signature of the returned function
makeAddress number street town = (number, street, town)

makeAddress6 :: String -> String -> (Int, String, String)
makeAddress6 = makeAddress 6

makeAddress6HightSt :: String -> (Int, String, String)
makeAddress6HightSt = makeAddress6 "High St"

makeAddress6HightStOxford :: (Int, String, String)
makeAddress6HightStOxford = makeAddress6HightSt "Oxford"

ifEven :: (Int -> Int) -> Int -> Int
ifEven f n = if even n
             then f n
             else n

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs) = if f x then x:myFilter f xs else myFilter f xs

-- myHead :: [a] -> a
-- myTail :: [a] -> [a]

-- no I can't, stupid!

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
    where newInit = f init x
