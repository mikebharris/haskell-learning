module TrainPlanner where

  import qualified Data.Map

  type Station = String
  type Time = String
  type Timetable = [[String]]
  type Train = Data.Map.Map Station Time

  parse :: Timetable -> [Train]
  parse (stations:timesOfTrains) = map (makeTrain stations) timesOfTrains

  makeTrain :: [Station] -> [Time] -> Train
  makeTrain stations times = Data.Map.fromList $ zip stations times

  duration :: Timetable -> Time -> Station -> Station -> Int
  duration timetable timeAtDepartureStation departureStation destinationStation = arrivalTime - departureTime where
    departureTime = toMinutes timeAtDepartureStation
    arrivalTime = lookupTimeAt destinationStation nextTrain
    trains = parse timetable
    nextTrain = head $ filter (\t -> Data.Map.lookup departureStation t == Just timeAtDepartureStation) trains 

  lookupTimeAt :: Station -> Train -> Int
  lookupTimeAt station = toMinutes . unbox . Data.Map.lookup station

  unbox :: Maybe Time -> Time
  unbox Nothing = ""
  unbox (Just x) = x

  toMinutes :: Time -> Int
  toMinutes t = h * 60 + m where
      h = read (take 2 t) :: Int
      m = read (drop 2 t) :: Int