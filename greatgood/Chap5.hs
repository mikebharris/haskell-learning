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
compWithHundred x = compare x 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

divideTenBy :: (Floating a) => a -> a
divideTenBy = (10/)

-- the brackets turn the infix into a prefix function that takes the parameters in a different order
-- (`elem` ['A'..'Z']) is equivalent to \x -> elem x ['A'..'Z']
isUpperAlpha :: Char -> Bool
isUpperAlpha = (`elem` ['A'..'Z'])

-- applyTwice (*3) 2 => 18
-- applyTwice (3-) 2 => 2
-- applyTwice (subtract 3) 2 => -4
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

-- flipminus = curriedFlip' (-) 
curriedFlip' :: (a -> b -> c) -> (b -> a -> c)
curriedFlip' f y x = f x y

-- therefore: flip' (-) 5 1 => -4

powersOfTwo :: Int -> [Int]
powersOfTwo n = zipWith' (^) (replicate n 2) [0..]


evenLessThan15 = filter (<15) (filter even [1..20])
evenLessThan15Lc =  [x | x <- [1..20], x < 15, even x]

qs :: (Ord a) => [a] -> [a]
qs [] = []
qs (p:xs) =
  let lhs = filter (<= p) xs
      rhs = filter (> p) xs
  in qs lhs ++ [p] ++ qs rhs

someNums = head (let isDivisible x = (mod x 3289 == 0) in filter isDivisible [99999,99998..1])
-- or could be done with a where clause:
someOtherNums = head(filter isDivisible [99999,99998..1])
  where isDivisible x = (mod x 3289 == 0)

-- any natural
-- if 1 stop
-- if even / 2
-- if odd * 3  + 1

-- for all numbers twixt 1 and 100
-- how many have a chain of length > 15

chain :: Int -> [Int]
chain 1 = [1]
chain n = n:chain (result)
  where result = if even n
                 then n `div` 2
                 else (n * 3 ) + 1

doThing = length [ n | n <- [1..100], length (chain n) > 15 ]

theirThing =  length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

theirThing' = length (filter (\xs -> length xs > 15) (map chain [1..100]))
theirThingWithFunctionComposition =  length . filter (\xs -> length xs > 15) $ map chain [1..100]

-- zipWith' ($) listOfFuns  [1,2,3] => [0,2,6]
listOfFuns :: (Num a, Enum a) => [a -> a]
listOfFuns = map (*) [0..]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

addThree' :: Int -> Int -> Int -> Int
addThree' = \x -> \y -> \z -> x + y + z

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

conc :: [[Char]] -> [Char]
conc = foldl (++) "rabbit"
--
rconc :: [[Char]] -> [Char]
rconc = foldr (++) "wabbit"

surround x y =  x ++ y ++ x
concL = foldl surround "rabbit"
-- concL ["bob","joe","maisie"]
-- "rabbitbobrabbitjoerabbitbobrabbitmaisierabbitbobrabbitjoerabbitbobrabbit"
concR = foldr surround "rabbit"
-- concR ["bob","joe","maisie"]
-- "bobjoemaisierabbitmaisiejoebob"

-- for foldr we take the list right to left
-- and at each iteration subtract the accumulator from x
-- ghci> foldr (-) 1 [0,2,3,4]
-- 4-1 = 3 
-- 3-3 = 0
-- 2-0 = 2 
-- 0-2 = -2

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f z []     = z
-- foldr f z (x:xs) = f x (foldr f z xs)
-- foldr (-) 1 [0,2,3,4] = (-) 0 (foldr (-) 1 [2,3,4])
-- (-) 0 ((-) 2 (foldr (-) 1 [3,4]))
-- (-) 0 ((-) 2 ((-) 3 (foldr (-) 1 [4])))
-- (-) 0 ((-) 2 ((-) 3 ((-) 4 (foldr (-) 1 []))))
-- (-) 0 ((-) 2 ((-) 3 ((-) 4 (1))))


-- ghci> foldr1 (-) [0,2,3,4,1]
-- -2

-- ghci> scanr1 (-) [0,2,3,4,1]
-- [-2,2,0,3,1]
-- ghci> scanl1 (-) [0,2,3,4,1]
-- [0,-2,-5,-9,-10]

fib :: Int -> Int
fib n
  | n < 2 = n
  | otherwise = fib (n - 1) + fib (n - 2)


--  foldr k z = go
--  	  where
--  	    go []     = z
--  	    go (y:ys) = y `k` (go ys)

-- using foldr (\x _ -> x+3) 0 [7..]
-- first time round: y = 7, 7 `(\x _ -> x)` (go [2..]) => 7 + 3 => 10
-- ends

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

--foldl2 :: (a -> a -> a) -> a -> [a] -> a
--foldl2 f acc xs = f acc (go xs)
--    where
--      go f acc []     = acc
--      go f acc (y:ys) = foldl2 `f` (go ys)

--foldl3 fn acc [] = acc
--foldl3 fn acc (x:xs) = foldl3 fn (fn acc x)

reverse' :: [a] -> [a]
reverse' = foldl (\accs y -> y : accs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if (p x) then x : acc else acc) []

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

-- how many elements before the (sum of sqrts of natural numbers) > 1000?
thingy :: [Float]
thingy = takeWhile (<=1000) (scanl1 (+) [(sqrt a) | a <- [1..]])

-- pg 81:
eighty :: Int
eighty = sum $ filter (>10) $ map (*2) [2..10] --  sum (filter (>10) (map (*2) [2..10]))

wibble :: Floating a => a -> [a]
wibble a = map ($ a) [(4+), sqrt]

--pg 82: function composition
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- therefore (negate . abs) = (\x -> negate(abs x))

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
oddSquareSum' = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]