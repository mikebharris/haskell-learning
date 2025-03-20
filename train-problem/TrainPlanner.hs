module TrainPlanner where

  import qualified Data.Map

  type Station = String
  type Time = String
  type Timetable = [[String]]
  type Train = Data.Map.Map Station Time

  parse :: Timetable -> [Train]
  parse (x:xs) = [Data.Map.fromList $ zip x (head xs)]

  duration :: Timetable -> Time -> Station -> Station -> Int
  duration timetable timeAtDepartureStation departureStation destinationStation = arrivalTime - departureTime where
    departureTime = toMinutes timeAtDepartureStation
    arrivalTime = lookupTimeAt destinationStation (head trains)
    trains = parse timetable

  lookupTimeAt :: Station -> Train -> Int
  lookupTimeAt station = toMinutes . unbox . Data.Map.lookup station

  unbox :: Maybe Time -> Time
  unbox Nothing = ""
  unbox (Just x) = x

  toMinutes :: Time -> Int
  toMinutes t = h * 60 + m where
      h = read (take 2 t) :: Int
      m = read (drop 2 t) :: Int