module FizzBuzz where

    -- I had to look into this sadly, but the solution is a hybrid between
    -- me realising I needed to use that gated if thingy and the range

    isFizz :: Int -> Bool
    isFizz x = mod x 3 == 0

    isBuzz :: Int -> Bool
    isBuzz x = mod x 5 == 0

    isFizzBuzz :: Int -> Bool
    isFizzBuzz x = isFizz x && isBuzz x

    fizzBuzz :: Int -> String
    fizzBuzz n | isFizzBuzz n = "FizzBuzz"
               | isBuzz n = "Buzz"
               | isFizz n = "Fizz"
               | otherwise = show n
                   
    doFizzBuzz :: [String]
    doFizzBuzz = [fizzBuzz x  | x <- [1..100]]