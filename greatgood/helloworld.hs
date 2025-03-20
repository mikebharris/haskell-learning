import qualified Data.Char
main = do
    welcome
    putStrLn "Who are you?"
    name <- getLine
    let bigName = map Data.Char.toUpper name
    if name == "Mike" then
        putStrLn ("You are cool " ++ name)
    else
        putStrLn (hello bigName)


welcome = do
    putStrLn "Welcome to the program"
    putStrLn "======================"

foo = putStrLn "Hello, World"

hello :: String -> String
hello n = "Hello " ++ n
