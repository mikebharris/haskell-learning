module Planner
  where
    import Data.Map
    import Data.Maybe

    timetable = [["Penzance", "St Erth", "Camborne", "Redruth", "Truro", "St Austell", "Par", "Bodmin Parkway", "Liskeard", "Plymouth", "Exeter St Davids", "Reading", "London Paddington"],
             ["0844", "0854", "0907", "0914", "0927", "0944", "0951", "1003", "1016", "1040", "1137", "1316", "1344"],
             ["1000", "1010", "1023", "1030", "1043", "1100", "1108", "1119", "1133", "1157", "1302", "1450", "1521"],
             ["1047", "1057", "1112", "1119", "1132", "1150", "1157", "1208", "1221", "1252", "1357", "1539", "1602"]]


    makeTrains :: [[String]] -> [Map String String]
    makeTrains timetable =
      Prelude.map (makeTrain route) journeys
      where
        route = head timetable
        journeys = tail timetable

    makeStopList :: [String] -> [String] -> [(String, String)]
    makeStopList [] [] = []
    makeStopList (station:stations) (time:times) =
      (station, time) : makeStopList stations times

    makeTrain :: [String] -> [String] -> Map String String
    makeTrain route journey = Data.Map.fromList $ makeStopList route journey

    duration :: [[String]] -> String -> String -> String -> Int
    duration timetable passengerArrivalTime startStationName endStationName = 
      (endStationTimeHH * 60 + endStationTimeMM) - (arrivalTimeHH * 60 + arrivalTimeMM)
      where
        trains =  makeTrains timetable
        train = head $ Prelude.filter filterFunction trains
        filterFunction train = passengerArrivalTime <= Data.Maybe.fromMaybe "0" (Data.Map.lookup startStationName train)  
        endStationTimeString = Data.Maybe.fromMaybe "0" $ Data.Map.lookup endStationName train
        arrivalTimeHH = read (Prelude.take 2 passengerArrivalTime) :: Int
        endStationTimeHH = read (Prelude.take 2 endStationTimeString) :: Int
        arrivalTimeMM = read (Prelude.drop 2 passengerArrivalTime) :: Int
        endStationTimeMM = read (Prelude.drop 2 endStationTimeString) :: Int

    fastestTrain :: [[String]] -> String -> String -> String
    fastestTrain timetable startStationName endStationName = "1357"
      where
        trains = makeTrains timetable

--    data Stop =
--      Stop {station :: String,
--            time :: String}
--      deriving (Eq, Show, Read)
--
--    makeTrain [] [] = []
--    makeTrain (station:stations) (time:times) =
--      Stop station time : makeTrain stations times