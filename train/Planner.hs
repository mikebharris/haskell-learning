module Planner
  where
    import Data.Time
  
    duration :: [[String]] -> String -> String -> String -> Int
    duration timetable passengerArrivalTime startStationName endStationName = endStationTime - startStationTime
      where endStationTime = timetable !! train !! endStationIdx
            endStationIdx = length (takeWhile (/= endStationName) (stations))
            startStationTime = timetable !! train !! startStationIdx
            startStationIdx = length (takeWhile (/= startStationName) (stations))
            stations = timetable !! 0
            train = [a | a <- [1..3]]

    fastestTrain :: [[String]] -> String -> String -> String
    fastestTrain timetable startStationName endStationName = "1357"
