import Control.Monad

main :: IO [()]
main = do 
    colours <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which colour do you associate with the number " ++ show a ++ "?"
        getLine)
    putStrLn "The colours that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colours