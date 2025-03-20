module TrainPlanner
  where
    import Data.Time
    -- import Data.List (elemIndex) -- considered using this but it returns a Maybe and I can't use that as an index.

    -- duration timetable "0907" "Camborne" "Exeter St Davids" == 150 : PASS
    -- duration timetable "1023" "Camborne" "Exeter St Davids" == 159 : PASS
    -- duration timetable "1101" "St Austell" "Par" == 56 : Error!

    -- timetable :: [[String]]
    -- timetable = [["Penzance", "St Erth", "Camborne", "Redruth", "Truro", "St Austell", "Par", "Bodmin Parkway", "Liskeard", "Plymouth", "Exeter St Davids", "Reading", "London Paddington"],
    --             ["0844", "0854", "0907", "0914", "0927", "0944", "0951", "1003", "1016", "1040", "1137", "1316", "1344"],
    --             ["1000", "1010", "1023", "1030", "1043", "1100", "1108", "1119", "1133", "1157", "1302", "1450", "1521"],
    --             ["1047", "1057", "1112", "1119", "1132", "1150", "1157", "1208", "1221", "1252", "1357", "1539", "1602"]]


    duration :: [[String]] -> String -> String -> String -> Int
    duration tt arrivalTime startStation endStation = duration where
      duration = floor (toRational (diffUTCTime endTime startTime) / 60) 
      endTime = stringToUTCTime finishTime 
      startTime = stringToUTCTime arrivalTime
      finishTime = tt !! train !! (stationIndex endStation (tt !! 0))
      train = whichTrain tt (stationIndex startStation (tt !! 0)) arrivalTime

    whichTrain :: [[String]] -> Int -> String -> Int
    whichTrain tt i runningTime = [t | t <- [1..(length tt - 1)], (tt !! t !! i) == runningTime ] !! 0

    stationIndex :: (Eq a) => a -> [a] -> Int
    stationIndex a [] = 0
    stationIndex a b = searchIndex a b 0

    searchIndex :: (Eq a) => a -> [a] -> Int -> Int
    searchIndex a (x:xs) i
      | a == x = i
      | otherwise = searchIndex a xs (i + 1)

    stringToUTCTime :: String -> UTCTime
    stringToUTCTime t = UTCTime (today) (timeOfDayToTime (TimeOfDay hh mm 00)) where 
      today = fromGregorian 2023 05 16
      hh = read (take 2 t) :: Int
      mm = read (drop 2 t) :: Int

    fastestTrain :: [[String]] -> String -> String -> String
    fastestTrain a b c = "1357"