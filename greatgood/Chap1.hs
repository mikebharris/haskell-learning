import Data.Complex (Complex)
import Data.ByteString (intercalate)
import qualified Data.ByteString as Data.List
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
fizzBuzz = [ if x `mod` 15 == 0
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

biggerOblongs :: [(Integer, Integer)]
biggerOblongs = [ (a,b) | a <- [2..10], b <- [(a-1)..10], b /= a, a * b >= 64 ]

redundantOblongAreas :: [Integer]
redundantOblongAreas = [ x * y | x <- map snd biggerOblongs, y <- map fst biggerOblongs, x /= y]

biggerOblongAreas :: [Integer]
biggerOblongAreas = [uncurry (*) o | o <- biggerOblongs]

buzzFizz :: [String]
buzzFizz = [ fb x |  x <- [1 .. 100]]
  where fb x | 0 == mod x 15 = "BuzzFizz" | 0 == mod x 5 = "Fizz" | 0 == mod x 3 = "Buzz" | otherwise = show x

-- a Mandelbrot Set
--
-- for every value of the complex number C on the complexplane 
-- starting with Z = (0 + 0i)
-- compute the iteration Z -> Z^2 + C
-- and if the result tends to infinity it is unstable and outside the set (Z > some threshold, say 10,000)
-- and if the result tends to zero it is stable and inside the set (maxiters exceeded after n iterations)

plottedMset :: Double -> Double -> Data.List.ByteString
plottedMset xRes yRes = intercalate '' [ toChar p | p <- mandelbrot xRes yRes] where
  toChar (6.0, _, True) = "\n*"
  toChar (6.0, _, False) = "\n."
  toChar (_, _, True) = "*"
  toChar _ = "."

mandelbrot :: Double -> Double -> [(Double, Double, Bool)]
mandelbrot xRes yRes = [(x, y, mSetCompute (xMin + (xStep * x)) (yMin + (yStep * y))) | y <- [1..yRes], x <- [1..xRes]]
  where
    xStep = abs (xMax - xMin) / xRes
    yStep = abs (yMax - yMin) / yRes
    xMax = 0.8
    xMin = -2.5
    yMax = 1.25
    yMin = -1.25

mSetCompute :: Double -> Double -> Bool
mSetCompute = mSetIterate iters zReal zImaginary
  where
    zReal = 0
    zImaginary = 0
    iters = 0

mSetIterate :: Int -> Double -> Double -> Double -> Double -> Bool
mSetIterate iters zR zI cR cI 
  | zR^2 + zI^2 > threshold = False 
  | iters > maxIters = True
  | otherwise = mSetIterate (iters+1) (zR^2 - zI^2 + cR) (2 * zR * zI + cI) cR cI
  where
    maxIters = 100
    threshold = 10000


-- is there a complex type in Haskell, do we write our own here, or just break into the a + ib parts?
-- iterate :: Complex -> Complex -> Complex
-- iterate z c = z ^ 2 + c

