-- {-# LANGUAGE DatatypeContexts #-}

module Shapes
( Shape, area, nudge, zeroCircle, zeroRect, Baz(..)) where

    import Data.Map
    
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

    instance Foo TrafficLight where
        bar a = Red

    instance Show TrafficLight where
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

    data Perhaps a = Nowt | Owt a 

    instance (Eq m) => Eq (Perhaps m) where
        (==) :: Eq m => Perhaps m -> Perhaps m -> Bool
        (==) Nowt Nowt = True
        (==) (Owt x) (Owt y) = x == y
        (==) _ _ = False

    instance (Show m) => Show (Perhaps m) where
        show Nowt = "I don't know nowt about it"
        show (Owt x) =  "It's just owt to do with " ++ show x