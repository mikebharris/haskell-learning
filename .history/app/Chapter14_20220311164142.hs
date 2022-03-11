module Chapter14 where

    data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6
    
    data NewEngland = ME | VT | NH | MA | RI | CT deriving(Eq, Ord, Enum)
    instance Show NewEngland where
        show ME = "Maine"
        show VT = "Vermont"
        show NH = "New Hampshire"
        show MA = "Massachusetts"
        show RI = "Rhode Island"
        show CT = "Connecticut"
    instance Show NewEngland where
        fromEnum ME 

    -- instance Eq NewEngland where
    --     (==) ME ME = True
    --     (==) VT VT = True
    --     (==) NH NH = True
    --     (==) MA MA = True
    --     (==) RI RI = True
    --     (==) CT CT = True
    --     (==) _ _ = False
        
    -- class Eq a => Ord a where
    --     compare :: a -> a -> Ordering
    --     (<) :: a -> a -> Bool
    --     (<=) :: a -> a -> Bool
    --     (>) :: a -> a -> Bool
    --     (>=) :: a -> a -> Bool
    --     max :: a -> a -> a
    --     min :: a -> a -> a

    -- instance Ord NewEngland where
    --     (<=) ME ME = True
    --     (<=) NH ME = True 
    --     (<=) MA NH = True
    --     (<=) _ _ = False