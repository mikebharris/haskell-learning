import Data.List
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




plottedMset :: Double -> Double -> String
plottedMset xRes yRes = intercalate "" [ toString p | p <- mandelbrot xRes yRes] where
  toString (1.0, _, True) = "\n*"
  toString (1.0, _, False) = "\n."
  toString (_, _, True) = "*"
  toString _ = "."


-- Takes x resolution and y resolution, and returns a tuple for each point
-- i.e. (xCoord, yCoord, InOrOutOfSet)
-- The two dimensional graph represents the complex plane (x for real, y for imnaginary)
mandelbrot :: Double -> Double -> [(Double, Double, Bool)]
mandelbrot xRes yRes = [(x, y, mSetCompute (realPart x) (imaginaryPart y)) | y <- [1..yRes], x <- [1..xRes]]
  where
    xMax = 0.8 -- rightmost end of the area covered by the set (with a bit of padding)
    xMin = -2.5 -- leftmost
    yMax = 1.25 -- top
    yMin = -1.25 -- bottom
    xStep = abs (xMax - xMin) / xRes -- the distance along the x axis covered by each pixel (i.e. how wide a pixel is)
    yStep = abs (yMax - yMin) / yRes -- ditto y axis
    realPart x = xMin + (xStep * x) -- the real component of the point on the complex plane 
    imaginaryPart y = yMin + (yStep * y) -- the imaginary compoenent

mSetCompute :: Double -> Double -> Bool
mSetCompute = mSetIterate iters zReal zImaginary
  where
    zReal = 0
    zImaginary = 0
    iters = 0

-- For each point c on the complex plane, we do a certain number of iterations of z = z^2 + c
-- to see if z will tend towards infinity or not.
-- if z reaches the threshold then we assume it is on its way to infinity (and therefore not in the mandelbrot set)
-- Otherwise we assume it is not going towards infinity & is therefore in the set.
mSetIterate :: Int -> Double -> Double -> Double -> Double -> Bool
mSetIterate iters zReal zImaginary cReal cImaginary 
  | zReal^2 + zImaginary^2 > threshold = False -- the threshold has been reached indicating that z is tending towards infinity so c is not in the set
  | iters > maxIters = True -- it hasn't reached the threshold before max iterations
  | otherwise = mSetIterate (iters+1) newZReal newZImaginary cReal cImaginary -- move on to the next iteration
  where
    maxIters = 100
    threshold = 10000
    newZReal = zReal^2 - zImaginary^2 + cReal -- calculation of z = z^2 + c for the real component of z
    newZImaginary = 2 * zReal * zImaginary + cImaginary -- calculation of z = z^2 + c for the imaginary component of z


-- is there a complex type in Haskell, do we write our own here, or just break into the a + ib parts?
-- iterate :: Complex -> Complex -> Complex
-- iterate z c = z ^ 2 + c

