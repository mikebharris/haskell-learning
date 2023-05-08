maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

--[3,4,2,6,5]
--max 3 (maximum' [4,2,6,5])
--max 3 (max 4 (maximum' [2,6,5]))
--max 3 (max 4 (max 2 (maximum' [6,5])))
--max 3 (max 4 (max 2 (max 6 5)))

replicate' :: Int -> a -> [a]
replicate' 0 x = []
--replicate' 1 x = [x]
replicate' n x = x : replicate' (n - 1) x


replicatec :: Int -> a -> [a]
replicatec n x = case n of 0 -> []
                           n -> x : replicatec (n - 1) x

replicateg :: Int -> a -> [a]
replicateg n x
  | n <= 0 = []
  | otherwise = x : replicateg (n - 1) x

take' :: Int -> [a] -> [a]
take' n (x:xs)
  | n <= 0 = []
  | otherwise = x : take' (n-1) xs
take' _ [] = []

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = a `elem'` xs



