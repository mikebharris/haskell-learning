import Data.Char

doToAll _ [] = []
doToAll f (x:xs) = f x: doToAll f xs

add3ToAll = doToAll (+3)
mul3ByAll = doToAll (*3)


remove test [] = []
remove test (x:xs) = 
    if test x
    then remove test xs
    else x:remove test xs

-- write a function myProduct that calcs the product of a list of numbers Naomi
myProduct :: Num a => [a] -> a
myProduct [] = 0
myProduct xs = foldl (*) 1 xs

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) [] 

-- foldr 
--myFoldr f init [] = init
--myFoldr f init xs = myFoldr f newInit init
--   where newInit = f (head (reverse xs))

myFoldr f init [] = init
myFoldr f init xs = myFoldr f newInit (reverse (tail (reverse xs)))
    where newInit = f (last xs) init

myFoldrX f init [] = init
myFoldrX f init (x:xs) = f x rightResult
  where rightResult = myFoldr f init xs


-- fn  :: takes any type that implements equality 
myElem :: Eq a => a -> [a] -> Bool
myElem x xs = length filteredList > 0
    where filteredList = filter (== x) xs

-- Your isPalindrome function from lesson 6 doesn’t handle sentences with spaces or capitals. 
-- Use map and filter to make sure the phrase “A man a plan a canal Panama” is recognized 
-- as a palindrome.
-- 

isPalindrome word = wordNoSpaces == reverse wordNoSpaces
    where wordNoSpaces = map toLower (filter (/=' ') word)

-- isPalindrome "A man a plan a canal Panama"

-- In mathematics, the harmonic series is the sum of 1/1 + 1/2 + 1/3 + 1/4 .... 
-- Write a function harmonic that takes an argument n and calculates the sum of the series to n. 
-- Make sure to use lazy evaluation.
har 1 = 1
har n = 1/n + har (n - 1)

harmonic n = sum (take n seriesValues)
  where seriesPairs = zip (cycle [1.0])  [1.0,2.0 .. ]
        seriesValues = map
                       (\pair -> (fst pair)/(snd pair))
                       seriesPairs