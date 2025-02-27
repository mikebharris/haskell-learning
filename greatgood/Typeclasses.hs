module Shapes
( Shape, area, nudge, zeroCircle, zeroRect, Baz(..)) where

    data Baz b = Foo | Bar b

    data Point = Point Float Float deriving (Show)
    data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

    area :: Shape -> Float
    area (Circle _ r) = pi * r * r
    area (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

    concentricCircles = map (Circle (Point 0 0)) [1, 2, 3, 4]

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

    data Car = Car { make :: String, model :: String, year :: Int} deriving (Show)
    tellCar :: Car -> String
    tellCar (Car {make = a, model = b, year = c}) = "This car is a " ++ a ++ " " ++ b ++ " made in " ++ show c

    -- data (Ord a) => Thing a = Hand a | Foot
    -- page ??? what the fuck is he talking about?