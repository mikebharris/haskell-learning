-- every function in haskell takes only one parameter
-- those with multiple parameters are curried

-- will this help Naomi with bracket placement?

-- partially applied functions

-- is a function that takes an Int and returns a function that takes an Int
-- and returns a function that takes an Int and returns an Int
-- Here the brackets are not necessary because type definitions are right-associative
-- and expressions are left-associative
multThree :: Int -> (Int -> (Int -> Int))
multThree x y z = x * y * z
r1 = ((multThree 3) 5) 9

multTwoWithNine :: Int -> (Int -> Int)
multTwoWithNine = multThree 9
r2 = multTwoWithNine 2 3

compWithHundred :: Int -> Ordering
--compWithHundred x = compare 100 x
-- is equivalent to, the x's are on both sides and can be cancelled out
compWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

divideTenBy :: (Floating a) => a -> a
divideTenBy = (10/)

isUpperAlpha :: Char -> Bool
isUpperAlpha = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f a = f (f a)


