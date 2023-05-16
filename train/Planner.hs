module Planner
  where
    import Data.Time
  
    duration :: [[String]] -> String -> String -> String -> Int
    duration timetable passengerArrivalTime startStationName endStationName = floor (toRational (diffUTCTime endTime startTime) / 60)
      where startTime = stringToUTCTime "0907"
            endTime = stringToUTCTime "1137"

--        endStationTime = timetable !! train !! endStationIdx
--            endStationIdx = length (takeWhile (/= endStationName) (stations))
--            startStationTime = timetable !! train !! startStationIdx
--            startStationIdx = length (takeWhile (/= startStationName) (stations))
--            stations = timetable !! 0
--            train = 1
--            train = [a | a <- [1..3]]

    fastestTrain :: [[String]] -> String -> String -> String
    fastestTrain timetable startStationName endStationName = "1357"

--    trains = [[ timetable !! 0, timetable !! n] | n <- [1..3]]


--hh = read (take 2 "0907") :: Int
--mm = read (drop 2 "0907") :: Int
--
--st = UTCTime (fromGregorian 2023 05 16) (timeOfDayToTime (TimeOfDay hh mm 00))
--et = UTCTime (fromGregorian 2023 05 16) (timeOfDayToTime (TimeOfDay 11 57 00))
--journeyInMinutes = (/) (diffUTCTime et st) 60

    stringToUTCTime t = UTCTime (today) (timeOfDayToTime (TimeOfDay hh mm 00))
      where today = fromGregorian 2023 05 16
            hh = read (take 2 t) :: Int
            mm = read (drop 2 t) :: Int
