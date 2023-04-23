lucky :: Int -> String
lucky 7 =  "LUCKY NUMBER SEVEN!"
lucky x = "foo"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors2 :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

tell :: (Show a) => [a] -> String
tell [] = "Empty"
tell (x:[]) = "One thing: " ++ show x
tell (x:y:[]) = "Two things: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "Long list"

firstLetter :: String -> String
firstLetter [] = "empty"
firstLetter all@(x:xs) = "first letter of " ++ all ++ " is " ++ [x]

anorexiaWarning :: String
anorexiaWarning = "Anorexic!"

fattyDetector :: Double -> Double -> String
fattyDetector weightKg heightM
  | bmi <= skinny = anorexiaWarning
  | bmi <= normal = "Hench!"
  | bmi <= fat = "Sort it out fatso!"
  | otherwise = "Go see a doctor fatarse!"
  where bmi = weightKg / heightM ^ 2
        skinny = 18.5
        normal = 25.0
        fat = 30.0

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

initials2 :: String -> String -> String
initials2 (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <-xs, bmi w h > 25.0]
  where bmi weight height = weight / height ^ 2

cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

--[ x * x | x <- [5, 3, 2]] == let square x = x * x in [square 5, square 3, square 2] -- True

calcBmisLet :: [(Double, Double)] -> [Double]
calcBmisLet xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

head' :: [a] -> a
head' xs = case xs of []    -> error "You're a putz!"
                      (x:_) -> x

describeList :: [a] -> String
describeList ls = "blah " ++ case ls of []  -> "empty"
                                        [x] -> "singleton"
                                        xs  -> "longer"

--fattyDetector2 :: Double -> Double -> String
--fattyDetector2 weightKg heightM = case bmi of skinny -> anorexiaWarning
--                                              normal -> "Hench!"
--                                              fat    -> "Sort it out fatso!"
--                                              _      -> "Go see a doctor fatarse!"
--  where bmi = weightKg / heightM ^ 2
--        skinny = 18.5
--        normal = 25.0
--        fat = 30.0