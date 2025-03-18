import GHC.CmmToAsm.AArch64.Instr (ExtMode)
import GHC.Cmm (b16)
import qualified Data.Map
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

-- Show is the type class
-- Tree is the type (Tree a is also a Set with a bottom)

singletonne :: a -> Tree a
singletonne x = Node x EmptyTree EmptyTree

insertion :: (Ord a) => a -> Tree a -> Tree a
insertion x EmptyTree = singletonne x
insertion x (Node y left right)
    | x == y = Node y left right
    | x < y = Node y (insertion x left) right
    | x > y = Node y left (insertion x right)

left :: Tree a -> Tree a
left (Node _ left _) = left
left EmptyTree = EmptyTree

right :: Tree a -> Tree a
right (Node _ _ right) = right
right EmptyTree = EmptyTree

node :: Tree a -> Maybe a
node (Node y _ _) = Just y
node EmptyTree = Nothing

isInTree :: (Ord a) => a -> Tree a -> Bool
isInTree _ EmptyTree = False
isInTree x (Node y left right)
    | x == y = True
    | x < y = isInTree x left
    | x > y = isInTree x right


treeFromList :: (Ord a) => [a] -> Tree a
treeFromList = foldr insertion EmptyTree
-- isInTree 10 $ treeFromList [5,10,3,2,6] = True

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap g EmptyTree = EmptyTree
    fmap g (Node v l r) = Node (g v) (fmap  g l) (fmap g r)

fmap' :: (a -> b) -> [(k, a)] -> [(k, b)]
fmap' f [] = []
fmap' f ((k, v):xs) = (k, f v) : fmap' f xs
-- fmap' (:"e") [(1,'a'),(2, 'b'),(3,'c')]

-- data constructor constructs a value
-- type constructor constructs a type

-- is Either a monadic constructor?