main = interact isPalindrome

isPalindrome :: String -> String
isPalindrome = unlines . map (\x -> if reverse x == x then "palindrome" else "not a palindrome") . lines