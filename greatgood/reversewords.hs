main :: IO ()
main = do
    putStrLn "Please enter a sentence:"
    sentence <- getLine
    if null sentence
        then do
            bye <- do -- performs the do and assigns the value to bye
                putStr "ok, "
                return "Goodbye!" -- boxes a string in an IO Monad
            putStrLn bye
        else do
            putStrLn ("The string is now: " ++ reverseWords sentence)
            main

reverseWords :: String -> String
reverseWords = unwords . reverse . words

foo :: Maybe String
foo = do
    three <- Just "three"
    Just three

bar :: IO String
bar = do
    return "hello"

baz :: a -> Maybe a
baz = return :: a -> Maybe a

wibble :: (Eq a, Num a) => a -> IO String
wibble x = do
    if x == 1 then 
        return "wobble"
    else do
        putStrLn "hjhj"
        return "lobster"