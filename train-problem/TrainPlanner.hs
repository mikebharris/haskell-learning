module TrainPlanner where

  import qualified Data.Map
  import Data.Char ( isDigit )
  
  type Station = String
  type Time = String
  type Timetable = [[String]]
  type Train = Data.Map.Map Station Time
  data JourneyPlannerError = InvalidTimeError deriving (Show, Eq)

  extractTrainsFrom :: Timetable -> [Train]
  extractTrainsFrom (stations:timesOfTrains) = map (makeTrain stations) timesOfTrains

  makeTrain :: [Station] -> [Time] -> Train
  makeTrain stations times = Data.Map.fromList $ zip stations times

  duration :: Timetable -> Time -> Station -> Station -> Either JourneyPlannerError Int
  duration timetable timeAtDepartureStation departureStation destinationStation 
    | toMinutes timeAtDepartureStation == Nothing = Left InvalidTimeError
    | otherwise = timeBetween arrivalTime (toMinutes timeAtDepartureStation) 
    where
      arrivalTime = lookupTimeAt destinationStation nextTrain
      nextTrain = head $ filter (\t -> Just timeAtDepartureStation <= Data.Map.lookup departureStation t) trains 
      trains = extractTrainsFrom timetable

  timeBetween :: Maybe Int -> Maybe Int -> Either JourneyPlannerError Int
  timeBetween Nothing _ = Left InvalidTimeError
  timeBetween _ Nothing = Left InvalidTimeError
  timeBetween (Just x) (Just y) = Right (abs $ x - y)

  lookupTimeAt :: Station -> Train -> Maybe Int
  lookupTimeAt station = toMinutes . unbox . Data.Map.lookup station

  unbox :: Maybe Time -> Time
  unbox Nothing = ""
  unbox (Just x) = x

  toMinutes :: Time -> Maybe Int
  toMinutes t 
    | length t /= 4 = Nothing
    | foldl (\b c -> b && not (isDigit c)) True t = Nothing
    | m > 59 = Nothing
    | h > 23 = Nothing
    | otherwise = Just $ h * 60 + m
    where
        h = read (take 2 t) :: Int
        m = read (drop 2 t) :: Int