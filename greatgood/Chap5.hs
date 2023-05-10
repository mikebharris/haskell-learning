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

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y):zipWith' f xs ys

zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' _ [] _ = []
zipWith'' _ _ [] = []
zipWith'' f xs ys = [f x y|x <- xs, y <- ys]
-- the above was a good exploration but it wasn't equivalent as it ends up with a list of 9 items

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x

curriedFlip' :: (a -> b -> c) -> (b -> a -> c)
curriedFlip' f y x = f x y

-- therefore: flip' (-) 5 1 => -4






