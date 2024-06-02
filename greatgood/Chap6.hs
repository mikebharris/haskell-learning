-- Modules

import Data.List
import Data.Char

numuniques :: (Eq a) => [a] -> Int
numuniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\xs -> (head xs, length xs)) . group . sort . words

findIn :: (Eq a) => [a] -> [a] -> Bool
findIn xs ys = length (filter (isPrefixOf xs) $ tails ys) > 0
findIn' needle haystack = any (needle `isPrefixOf`) (tails haystack) 

caesar :: Int -> String -> String
caesar n = map $ chr . (+ n) . ord   
--uncaesar n = caesar (-n)
uncaesar = caesar . negate