-- {-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE DeriveAnyClass #-}

module Shapes
( Shape, area, nudge, zeroCircle, zeroRect, Baz(..)) where

    import Data.Map
    import GHC.Builtin.Types (falseDataCon)
    
    data Baz b = Foo | Bar b

    data Point = Point Float Float deriving (Show)
    data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

    area :: Shape -> Float
    area (Circle _ r) = pi * r * r
    area (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

    concentricCircles = Prelude.map (Circle (Point 0 0)) [1, 2, 3, 4]

    -- shape, amount to move on x, on y, returns new shape

    nudge :: Shape -> Float -> Float -> Shape
    nudge (Circle (Point x y) r) dx dy = Circle (Point new_x new_y) r where
        new_x = x + dx
        new_y = y + dy

    nudge (Rectangle (Point x1 y1) (Point x2 y2)) dx dy = Rectangle (Point new_x1 new_y1) (Point new_x2 new_y2) where
        new_x1 = x1 + dx
        new_x2 = x2 + dx
        new_y1 = y1 + dy
        new_y2 = y2 + dy

    zeroCircle :: Float -> Shape
    zeroCircle = Circle (Point 0 0)

    zeroRect :: Float -> Float -> Shape
    zeroRect a b = Rectangle (Point 0 0) (Point a b)
    ton = area $ nudge (zeroRect 10 10) 10 10

    -- import Shapes as S
    -- r=S.Rectangle (Point 3 2) (Point 34 6)
    -- Not in scope: data constructor ‘S.Rectangle’
    -- area $ S.nudge (S.zeroRect 10 10) 10 10
    -- 100.0

    data Person = Person {
        firstName :: String,
        lastName :: String,
        age :: Int,
        height :: Float,
        phoneNumber :: String,
        flavor:: String
    } deriving Show

    type Make = String
    type Model = String
    type Year = Int
    type Wibble = Thing Int

    type Jalloppy = (Make, Model)

    data Car = Car { make :: Make, model :: Model, year :: Year, wibble :: Wibble} deriving (Show, Eq, Read)
    tellCar :: Car -> String
    tellCar (Car {make = a, model = b, year = c}) = "This car is a " ++ a ++ " " ++ b ++ " made in " ++ show c

    -- In "data (Ord a) => Thing a = Hand a | Foot" - (Ord a) is a "type class constraint"
    -- page ??? what the fuck is he talking about?

    -- data (Num a) => Thing a = Hand a | Foot
    data Thing a = Hand a | Foot deriving (Show, Eq, Read, Ord)

    somefunc :: (Thing Int) -> (Thing Int) -> Int
    somefunc (Hand t) (Hand r) = t * r

    -- someotherfunc (Hand t) (Hand r) = t : ' ' : r : ""

    data Vector a = Vector a a a deriving (Show)
    vplus :: (Num a) => Vector a -> Vector a -> Vector a
    vplus (Vector i j k) (Vector l m n) = Vector (i+l) (j+m) (k+n)

    vproduct :: (Num a) => Vector a -> Vector a -> Vector a
    vproduct (Vector i j k) (Vector l m n) = Vector (i*l) (j*m) (k*n)

    vsproduct :: (Num a) => Vector a -> a -> Vector a
    vsproduct (Vector i j k) x = Vector (i*x) (j*x) (k*x)

    mysteryJalloppy = "Car { make=\"Fiat\", model=\"Uno\", year=1991, wibble=(Hand 23) }"

    data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Enum, Bounded, Eq, Ord, Show, Read)

    type AL k v = [(k, v)]

    type IntMap = Map Int

    silly :: a -> Bool
    sillier :: a -> Bool
    silly a = not $ sillier a
    sillier a = not $ silly a

    data TrafficLight = Red | Yellow | Green
    
    instance Eq TrafficLight where
        Red == Red = True
        Green == Green = True
        Yellow == Yellow = True
        _ == _ = False

    class Foo a where
        bar :: a -> a
        bar a = a 

    data TrifficLight = Blue | Amber | Purple deriving (Foo, Show)

    instance Foo TrafficLight where
        bar a = Red

    instance Show TrafficLight where
        show :: TrafficLight -> String
        show Red = "Stop!"
        show Green = "Go!"
        show Yellow = "Quick!"
    
    class (Num a) => Bar a where
        baz :: a -> a
        baz a = a + 2 

    instance Bar Int

    instance Foo (Maybe TrafficLight) where
        bar Nothing = Nothing
        bar (Just Red) = Just Green
        bar (Just x) = Just x

    -- Perhaps is an 'algebraic type'
    data Perhaps a = Nowt | Owt a 

    -- given any Equalable type one can make a Perhaps of it
    -- and here is the implementation of Eq, which it will need
    -- in order to be equalable: this is how one tests for the equality 
    -- of the Perhaps of the type
    instance (Eq m) => Eq (Perhaps m) where
        (==) :: Eq m => Perhaps m -> Perhaps m -> Bool
        (==) Nowt Nowt = True
        (==) (Owt x) (Owt y) = x == y
        (==) _ _ = False

    -- class constraints in instance declarations
    -- are used to express requirements about the contents of some type
    -- i.e: the constraint (Show m) means that m must meat the requirements of the type class Show
    -- effectively, it needs to implement certain functions (mappings)
    instance (Show m) => Show (Perhaps m) where
        show Nowt = "I don't know nowt about it"
        show (Owt x) =  "It's just owt to do with " ++ show x

    data Wubble = Wobble | Wabble | Wybl
    -- the following doesn't work, because show isn't implemented
    -- and thus you end up with a recursive loop when doing "show Wybl"
    -- this is because in the definition of show there's two functions
    -- show and showPrec that are defined in terms of each other
    -- 
    -- use of deriving Show implements the function
    -- you can start ghc or ghci with -ddump-deriv to see them
    -- instance Show Wubble where
    --     show = show

    -- class constraints in class declarations
    -- used for making a type class a subclass of another type class, i.e:
    -- the set of Addables is a sub-set of Nums
    -- Addable is a subclass of Num
    class (Num a) => Addable a where
        square :: a -> a
        square x  = x * x

    -- JS considers any non-empty string to be a true value


    class YesNo a where
        yesno :: a -> Bool

    instance YesNo Bool where
        yesno :: Bool -> Bool
        yesno x = x

    instance YesNo Int where
        yesno :: Int -> Bool
        yesno x = x /= 0

    instance YesNo Float where
        yesno :: Float -> Bool
        yesno x = x /= 0.0

    instance YesNo [a] where
        yesno :: [a] -> Bool
        yesno [] = False
        yesno _ = True

    instance YesNo (Maybe a) where
        yesno :: Maybe a -> Bool
        yesno Nothing = False
        yesno _ = True

    -- The constraint ‘Num a’
    -- is no smaller than the instance head ‘YesNo a’
    --   (Use UndecidableInstances to permit this)
    -- instance (Num a) => YesNo a where
    --     yesno :: Num -> Bool
    --     yesno x = x /= (0 :: a)

    instance YesNo TrafficLight where
        yesno :: TrafficLight -> Bool
        yesno Red = False
        yesno _ = True

    -- if condition then smthng else smthngels

    yesnoIf :: (YesNo y) => y -> a -> a -> a
    yesnoIf yn t f = if yesno yn then t else f