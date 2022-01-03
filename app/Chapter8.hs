
-- Recursive functions

-- from chapter 7

firstHalf x l = x `elem` theFirstHalf
    where theFirstHalf = take half l 
          half = div (length l) 2

myGCD a b = if remainder == 0 then b else myGCD b remainder
    where remainder = a `mod` b

-- Q7.1
myTail [] = []
myTail (_:xs) = xs

-- Q7.2
myGCD2 a 0 = a
myGCD2 a b = myGCD b (a `mod` b)

-- **** Chapter 8 ****

-- Identify the end goal(s).
-- Determine what happens when a goal is reached.
-- List all alternate possibilities.
-- Determine your “rinse and repeat” process.
-- Ensure that each alternative moves you toward the goal.

myTake 0 _ = []
myTake _ [] = []
myTake n (x:xs) = x : myTake (n-1) xs

myDrop 0 xs = xs
myDrop _ []  = []
myDrop n (x:xs) = myDrop (n-1) xs

myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myLength2 xs = foldr (\x -> (+) 1) 0 xs

-- a -> a -> a IS EQUIVALENT TO a -> (a -> a)

-- (/) :: Fractional a => a -> a -> a
-- (\x -> (+) 1) :: Num a => p -> a -> a
-- takes a Fractional and a Fractional and returns a Fractional
-- or takes a Fractional and it returns a function that takes a Fract and returns a Fract

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))

fastFib n1 n2 0 = n1
fastFib n1 n2 1 = n1
fastFib n1 n2 counter = fastFib n2 (n1 + n2) (counter - 1)
fib = fastFib 1 1

