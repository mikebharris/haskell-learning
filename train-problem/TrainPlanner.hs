module TrainPlanner where

  import qualified Data.Map

  type Station = String
  type Time = String
  type Timetable = [[String]]
  type Train = Data.Map.Map Station Time

  extractTrainsFrom :: Timetable -> [Train]
  extractTrainsFrom (stations:timesOfTrains) = map (makeTrain stations) timesOfTrains

  makeTrain :: [Station] -> [Time] -> Train
  makeTrain stations times = Data.Map.fromList $ zip stations times

  duration :: Timetable -> Time -> Station -> Station -> Int
  duration timetable timeAtDepartureStation departureStation destinationStation = arrivalTime - toMinutes timeAtDepartureStation where
    arrivalTime = lookupTimeAt destinationStation nextTrain
    nextTrain = head $ filter (\t -> Just timeAtDepartureStation <= Data.Map.lookup departureStation t) trains 
    trains = extractTrainsFrom timetable

  lookupTimeAt :: Station -> Train -> Int
  lookupTimeAt station = toMinutes . unbox . Data.Map.lookup station

  unbox :: Maybe Time -> Time
  unbox Nothing = ""
  unbox (Just x) = x

  toMinutes :: Time -> Int
  toMinutes t = h * 60 + m where
      h = read (take 2 t) :: Int
      m = read (drop 2 t) :: Int