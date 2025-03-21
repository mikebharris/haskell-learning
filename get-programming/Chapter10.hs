cup floz = \message -> message floz
getSize aCup = aCup id
getDoubleSize aCup = aCup (*2)

cup6 = cup 6

drink aCup ozDrank = if ozLeft < 0 then cup 0 else cup ozLeft where ozLeft = getSize aCup - ozDrank
ifEmpty aCup = getSize aCup == 0
lotsOfSips = foldl drink cup6 [1,1,1,1,1]

-- Fighting Robots
robot (name,attack,hp) = \message -> message (name, attack, hp)
killerRobot = robot ("Kill3r", 25, 200)

name(n,_,_) = n 
attack(_,a,_) = a 
hp(_,_,h) = h

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp 

setName aRobot n = robot (n, getAttack aRobot, getHP aRobot)
setAttack aRobot attack = aRobot (\(n,a,h) -> robot(n, attack, h))

megaRobot = setAttack killerRobot 1000

printRobot aRobot = aRobot (\(n,a,h) -> n ++ " " ++ show a ++ " " ++ show h)

damage aRobot attackDamage = aRobot (\(n,a,h) -> robot (n,a,h-attackDamage))

fight aggressor defender = damage defender attack
    where attack = if getHP aggressor > 10 then getAttack aggressor else 0

gentleGiant = robot ("Mr Friendly", 10, 300)

-- Use map on a list of robot objects to get the life of each robot in the list.

-- Write a threeRoundFight function that takes two robots and has them fight for three rounds,
-- returning the winner. To avoid having so many different variables for robot state, 
-- use a series of nested lambda functions so you can just overwrite robotA and robotB.

-- Create a list of three robots. Then create a fourth robot. 
-- Use partial application to create a closure for the fight method so the fourth robot can fight 
-- all three robots at once, using map. Finally, use map to get the remaining life from the rest 
-- of the robots.

robot1 = robot("Robot1", 100, 200)
robot2 = robot("Robot2", 500, 1000)
robot3 = robot("Robot3", 400, 800)
robot4 = robot("Robot4", 346, 750)

robots = [robot1, robot2, robot3]

robot4fights = fight robot4

r4battle = map robot4fights robots

hps = map getHP robots

-- threeRoundFight :: Num t => ((([Char], Integer, Integer) -> t) -> t) -> ((([Char], Integer, Integer) -> t) -> t) -> ((([Char], Integer, Integer) -> t) -> t)
-- threeRoundFight r1 r2 = let r2 = fight r1 r2
--                     in
--                         let r1 = fight r2 r1
--                     in 
--                         let r2 = fight r1 r2 
--                     in 

--                            where winner = if r1hp > r2hp then robot1 else robot2
--                                  r1hp = getHP r1
--                                  r2hp = getHP r2

-- End of chapter sucked!


stupidCunt r1 r2 = if 1 > 2 then r1 else r2

-- “overwrite x = let x = 2
--               in
--                let x = 3
--                in
--                 let x = 4
--                 in
--                  x

-- “overwrite x = (\x ->
--               (\x ->
--                 (\x -> x) 4
--                )3
--               )2=