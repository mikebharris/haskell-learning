import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

-- search for code in locker map
-- either String Code 

search :: Int -> LockerMap -> Either String Code
search n lockerMap = case Map.lookup n lockerMap of
    Nothing -> Left "No such locker"
    Just (lockerState, code) -> case lockerState of
        Free -> Right code
        Taken -> Left "Locker is taken"

lockerMap = Map.fromList [(1, (Taken, "1234")), (2, (Free, "4567"))]