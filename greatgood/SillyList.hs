data MyList a = Empty | Cons a (MyList a) deriving (Show, Read, Eq, Ord)

myElem :: (Eq a) => MyList a -> a -> Bool
myElem Empty _ = False
myElem (Cons x xs) w  
    | w == x = True
    | xs `myElem` w = True
    | otherwise = False

infixr 5 :-:
data SillyList a = Blank | a :-: (SillyList a) deriving (Show, Read, Eq, Ord)

infixr 5 ^++
(^++) :: SillyList a -> SillyList a -> SillyList a
(^++) Blank ys = ys
(^++) (x:-:xs) ys = x :-: (xs ^++ ys)

-- data RealList a = [] | a : RealList a deriving ()
-- a :-: b :-: c => a :-: (b :-: c)

someFunc :: (Eq a, Num a) => a -> a
someFunc 8 = 9
someFunc a = a

