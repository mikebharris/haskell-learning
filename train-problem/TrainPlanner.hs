module TrainPlanner where

  import qualified Data.Map
  import Data.Char ( isDigit )
  import Data.Maybe (isNothing)

  type Station = String
  type Time = String
  type Timetable = [[String]]
  type Train = Data.Map.Map Station Time
  data JourneyPlannerError = InvalidTimeError | NoSuchStationError | NoSuchJourneyError deriving (Show, Eq)

  extractTrainsFrom :: Timetable -> [Train]
  extractTrainsFrom (stations:timesOfTrains) = map (makeTrain stations) timesOfTrains

  makeTrain :: [Station] -> [Time] -> Train
  makeTrain stations times = Data.Map.fromList $ zip stations times

  duration :: Timetable -> Time -> Station -> Station -> Either JourneyPlannerError Int
  duration timetable timeWeArriveAtDepartureStation departureStation destinationStation
    | isNothing (toMinutes (Just timeWeArriveAtDepartureStation)) = Left InvalidTimeError
    | isNothing (timeTrainLeavesDepartureStation (head trains)) = Left NoSuchStationError
    | isNothing (timeTrainArrivesAtDestinationStation nextTrain) = Left NoSuchStationError
    | earlier (timeTrainArrivesAtDestinationStation nextTrain) (Data.Map.lookup departureStation nextTrain) = Left NoSuchJourneyError
    | otherwise = timeBetween (toMinutes (Just timeWeArriveAtDepartureStation)) (toMinutes (timeTrainArrivesAtDestinationStation nextTrain))
    where
      timeTrainArrivesAtDestinationStation = Data.Map.lookup destinationStation
      nextTrain = head $ filter (\t -> Just timeWeArriveAtDepartureStation <= timeTrainLeavesDepartureStation t) trains
      trains = extractTrainsFrom timetable
      timeTrainLeavesDepartureStation = Data.Map.lookup departureStation

  timeBetween :: Maybe Int -> Maybe Int -> Either JourneyPlannerError Int
  timeBetween Nothing _ = Left InvalidTimeError
  timeBetween _ Nothing = Left InvalidTimeError
  timeBetween (Just x) (Just y) = Right (abs $ x - y)

  earlier :: Maybe Time -> Maybe Time -> Bool
  earlier Nothing _ = False
  earlier _ Nothing = False
  earlier x y = toMinutes x <= toMinutes y

  toMinutes :: Maybe Time -> Maybe Int
  toMinutes Nothing = Nothing
  toMinutes (Just t)
    | length t /= 4 = Nothing
    | not (all isDigit t) = Nothing
    | m > 59 = Nothing
    | h > 23 = Nothing
    | otherwise = Just $ h * 60 + m
    where
        h = read (take 2 t) :: Int
        m = read (drop 2 t) :: Int