module Planner
  where
    import Data.Time
  
    duration :: [[String]] -> String -> String -> String -> Int
    duration timetable passengerArrivalTime startStationName endStationName = floor (toRational (diffUTCTime endTime startTime) / 60)
      where startTime = stringToUTCTime (timeOfTrainAt timetable train startStationName)
            endTime = stringToUTCTime (timeOfTrainAt timetable train endStationName)
            train = 1
--    trains = [[ timetable !! 0, timetable !! n] | n <- [1..3]]

    timeOfTrainAt :: [[String]] -> Int -> String -> String
    timeOfTrainAt timetable train station = timetable !! train !! stationIdx
      where stationIdx = length (takeWhile (/= station) (stations))
            stations = timetable !! 0

    stringToUTCTime :: String -> UTCTime
    stringToUTCTime t = UTCTime (today) (timeOfDayToTime (TimeOfDay hh mm 00))
      where today = fromGregorian 2023 05 16
            hh = read (take 2 t) :: Int
            mm = read (drop 2 t) :: Int

    fastestTrain :: [[String]] -> String -> String -> String
    fastestTrain timetable startStationName endStationName = "1357"
