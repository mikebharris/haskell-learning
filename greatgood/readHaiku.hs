import Data.Char

main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly = unlines .filter (\line -> length line < 25) . lines
