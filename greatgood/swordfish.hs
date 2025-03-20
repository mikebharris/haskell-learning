import Control.Monad

main = forever $ do
    putStrLn "Enter a fish:"
    fish <- getLine
    when (fish == "SWORDFISH") $ do 
        putStrLn fish 
    