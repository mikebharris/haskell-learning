module Chapter15 where

    data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

    largestCharNumber :: Int
    largestCharNumber = fromEnum (maxBound :: Char)
        
    message :: [FourLetterAlphabet]
    message = [L1,L2, L4, L1, L3, L1]

    fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
    fourLetterAlphabetEncoder vals = map rot4l vals
        where 
            alphabetSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
            rot4l = rotN alphabetSize


    -- entire rot13 program

    rotN :: (Enum a) => Int -> a -> a
    rotN alphabetSize c = toEnum rotation where
        halfAlphabet = div alphabetSize 2
        rotation = (fromEnum c + halfAlphabet) `mod` alphabetSize

    rotChar :: Char -> Char
    rotChar c = rotN alphabetSize c
        where alphabetSize =  1 + fromEnum (maxBound :: Char)

    rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
    rotNdecoder alphabetSize c = toEnum rotation
        where 
            halfAlphabet = alphabetSize `div` 2
            offset = if even alphabetSize 
            then fromEnum c + halfAlphabet
            else 1 + fromEnum c + halfAlphabet
            rotation = offset `mod` alphabetSize

    rotEncoder :: String -> String
    rotEncoder text = map rotChar text

    rotDecoder :: String -> String
    rotDecoder text = map rotCharDecoder text
        where
            alphabetSize =  1 + fromEnum (maxBound :: Char)
            rotCharDecoder = rotNdecoder alphabetSize


    xorBool :: Bool -> Bool -> Bool
    xorBool v1 v2 = (v1 || v2) && (not (v1 && v2))

    xorPair :: (Bool, Bool) -> Bool
    xorPair (v1, v2) = xorBool v1 v2

    xor :: [Bool] -> [Bool] -> [Bool]
    xor l1 l2 = map xorPair (zip l1 l2)