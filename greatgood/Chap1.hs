doubleSmallNumber :: Int -> Int
doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

lostNumbers = [4, 8, 15, 16, 23, 42]

moreNumbers = lostNumbers ++ [56, 101]

perl = 'P' : "early the cat"

notethat = [1, 2, 3] == 1 : 2 : 3 : []

y = perl !! 5

p = head perl

t = last perl

notail = init perl

nohead = tail perl

lerp = reverse perl

name = take 6 perl

lotsofcats = take 1000 (cycle (perl ++ " "))

morecats = replicate 1000 (perl ++ " ")

boomBangs :: [Int] -> [String]
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

fizzBuzz :: [String]
fizzBuzz = [ if ((x `mod` 15) == 0)
              then "FizzBuzz"
              else if x `mod` 5 == 0
                then "Buzz"
                else if x `mod` 3 == 0
                  then "Fizz"
                  else show x
             | x <- [1 .. 100]]

nouns = ["n00b", "lolcat", "grentling"]
adjectives = ["dozy", "blue silver", "argumentative"]
entourage = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

removeNonCaps :: String -> String
removeNonCaps st = [c | c <- st, c `elem` ['A'..'Z']]
removeCaps :: String -> String
removeCaps st = [c | c <- st, not (c `elem` (removeNonCaps st))]

rightTriangles :: [Int] -> [(Int, Int, Int)]
rightTriangles xs = [ (a,b,c) | c <- xs, a <- [1..c], b <- [1..a], a^2 + b^2 == c^2]