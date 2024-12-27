import Data.Char (isSpace)
import Data.List (dropWhileEnd)

collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz a 
    | even a = a:collatz (a `div` 2)
    | otherwise = a:collatz (a * 3 + 1) -- note the cons; this is key and you forgot it!

collatzProblem :: Integer
collatzProblem = fromIntegral $ length $ [n | n <- [1..100], (length $ collatz n) > 15] -- note use of fromIntegral to convert from Int to Integer

infiniteDoubles = map (*2) [0..]
partialApplication = map (*) [0..]
foo = partialApplication !! 100
bar = foo 50 -- 5,000

-- expanding the initials function from Chapter 3 to handle names with any number of names in them
-- the result of input "Mike Brinsley Harris" becomes "M. B. H."
initials :: String -> String
initials n = map (\(x:_) -> x) (words n) -- but how does one insert the dots and spaces between the letters?
                                         -- getting stuck here because the Lambda is producing an array of Char (a String) and the concat would produce [String]

-- smartarse Naomi suggested
-- "map head" is a partial application that applies head to every item in a list of lists
-- if we don't have the . then we're trying to apply it to words, but words isn't a list, but rather it's a function; therefore it fails
-- using function composition therefore says "take the output of words, which is a list, and apply head to every item in it"
--
-- the error message is 
-- <interactive>:11:20: error:
--     • Couldn't match expected type: [[b]]  (-- a list of lists)
--                   with actual type: String -> [String] (-- a function that takes a string and retuns a list of strings)
--     • Probable cause: ‘words’ is applied to too few arguments
--       In the second argument of ‘map’, namely ‘words’
--       In the expression: (map head) words
--       In an equation for ‘inits’: inits = (map head) words

-- Mike and Naomi came up with ....
initialsPlus = withDotsAndSpaces . map head . words where 
    withDotsAndSpaces "" = ""
    withDotsAndSpaces (x:"") = x : "."
    withDotsAndSpaces (x:xs) = x : (". " ++ withDotsAndSpaces xs)

-- I guess the answer is that map is not the right thing to use; this is with foldl:
initialsWithDots :: String -> String
initialsWithDots n = foldl (\acc (x:xs) -> acc ++ [x] ++ ". ") "" (words n)

-- at one point I was trying to use zipWith with some infinite dots
infiniteDots = ["." | _ <- [0..]]

-- now the question is how would you not have the trailing space at the end?
-- with the suffixing of the dot within the Lambda we then can't tell if there's anything else in the list result of words
-- this solution sends the result into a function that strips off any whitespace at the end of a string
initialsWithDotsAndNoTrailingSpace :: String -> String
initialsWithDotsAndNoTrailingSpace name = dropWhileEnd isSpace $ foldl (\acc (x:xs) -> acc ++ [x] ++ ". ") "" (words name)

-- but is there a more elegant "Haskelly" way of doing it?