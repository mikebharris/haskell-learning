module Chapter13 where
-----
-- types and type classes
-----

-- a new type is defined using the 'data' keyword:
data Icecream = Chocolate | Pistaccio
-- in the above this type is like an enumerated type or alias in other languages

-- types can use record syntax, like named struct members:
type FirstName = String
type LastName = String
type MiddleName = String
data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName
data Sex = Male | Female | Woke | Undecided | Hermaphrodite
data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType
data Patient = Patient
  { name :: Name,
    sex :: Sex,
    age :: Int,
    height :: Int,
    weight :: Int,
    bloodType :: BloodType
  }
-- so I think in the above, 'type' is defining kind of like a type alias, whereas
-- in Haskell this is called a 'type synonym'
-- 'data' is defining a new type
w :: String
x :: String
y :: FirstName
z :: Name
z = Name "Mike" "Harris"
y = "Hello"
x = y
w = "Hola"
-- this is not allowed as types don't match: w = z

-- a type class is like an interface. it is used to define the properties and functions that
-- a type implementing the class must implement in order to be a member of that type class
-- so in the following
cycleSucc :: (Bounded a, Enum a, Eq a) =>  a -> a
cycleSucc n = if n == maxBound then minBound else succ n

-- this says "cycleSucc is a function that takes any type that implements the Bounded, Enum and Eq type classes
-- and returns a value of the same type"
-- in the implementation, the three classes were needed to implement the following
-- (==) from Eq
-- maxBound and minBound from Bounded
-- succ from Enum

-- type classes are defined thus
class Describable a where
  describe :: a -> String

-- not sure then how to implement it
--describe :: Describable x => x -> [Char]
--describe 3 = "This is an x"

-- deriving is used to 'inherit' from another type class
-- so that in the following the type Snacks gets a show method
data Snacks = Mars | KitKat | Marathon deriving (Show)
-- if it didn't, it wouldn't print, which is the source Mike's perennial confusion!
-- compare typing "Pistaccio" and "Mars" into ghci