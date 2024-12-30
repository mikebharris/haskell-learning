-- Modules

import Data.List
import Data.Char
import qualified Data.Map as Map

numuniques :: (Eq a) => [a] -> Int
numuniques = length . nub

tuplify [] = []
tuplify (x:xs) = (head x, length x) : tuplify xs

countWords :: String -> [(String, Int)]
countWords = tuplify . group . sort . words . (map toLower)

--wordNums :: String -> [(String, Int)
--wordNums = map (\xs -> (head xs, length xs)) . group . sort . words
-- so group . sort . words gives us a list
-- map takes a list and applies it's argument to each element
-- the argument creats a tuple from the element

findIn :: (Eq a) => [a] -> [a] -> Bool
findIn xs ys = length (filter (isPrefixOf xs) $ tails ys) > 0
findIn' needle haystack = any (needle `isPrefixOf`) (tails haystack) 

caesar :: Int -> String -> String
caesar n = map $ chr . (+ n) . ord   
--uncaesar n = caesar (-n)
uncaesar = caesar . negate

multunlessten :: Int -> Int -> Int
multunlessten 10 _ = 10
multunlessten x y = x * y

--- find the first natural number whose digits add up to 40
-- addDigits x = sum (map digitToInt $ show x
addDigits :: Int -> Int
addDigits = sum . map digitToInt . show
-- head [x | x <- [1..], addDigits x == 40]

firstToN :: Int -> Maybe Int 
firstToN n = find (\x -> addDigits x == n) [1..]

-- [["am","am"],["hello"],["here"],["i","i"]]
-- [("am", 2), ("hello, ") ... ]

-- [("mike", "skidmore"), ("naomi", "rosenberg")]
lookup' :: (Eq a) => a -> [(a, b)] -> Maybe b
lookup' key []  = Nothing
lookup' key ((k, v):xs) 
  | k == key = Just v
  | otherwise = lookup' key xs  


findKey key xs = foldr (\(k,v) acc -> if k == key then Just v else acc) Nothing xs

coolDevsList = [("mike", "skidmore"), ("naomi", "rosenberg"), ("mike", "harris")]

coolDevs :: Map.Map String String
coolDevs = Map.fromList [("mike", "skidmore"), ("naomi", "rosenberg"), ("mike", "harris")]

-- We can implement our own fromList by using the empty map, insert and a fold
fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldl (\acc (k, v) -> Map.insert k v acc) Map.empty

fromListR :: (Ord k) => [(k,v)] -> Map.Map k v
fromListR = foldr (\(k, v) acc -> Map.insert k v acc) Map.empty

--- Finished  Data.Map section 
--- Go to https://learnyouahaskell.com/modules#data-set


