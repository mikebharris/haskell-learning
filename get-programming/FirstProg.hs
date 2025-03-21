messyMain :: IO()
messyMain = do
    print "Who is the email for?"
    recipient <- getLine
    print "What is the title?"
    title <- getLine
    print "Who is the Author?"
    author <- getLine
    print (createEmail recipient title author)

createEmail recipient title author = 
    toPart recipient ++ bodyPart title ++ fromPart author

toPart recipient = "Dear " ++ recipient ++ ",\n"
bodyPart title = "Thanks for buying " ++ title ++ ".\n"
fromPart author = "Thanks,\n" ++ author


inc x = x + 1
double x = x * 2
square x = x * x

q23 n = 
    if even n then
        less2 n
    else 
        threeTimes (inc n)
    where
        less2 x = x -2
        threeTimes x = 3 * x

q23lambda n = (\less2 threeTimes -> if even n 
    then less2 
    else threeTimes) (n - 2) (3 * inc n)


sumSquareOrSquareSum x y = if sumSquare > squareSum 
                            then sumSquare
                            else squareSum
    where
        sumSquare = x^2 + y^2
        squareSum = (x + y)^2


sumSquareOrSquareSum2 x y = (\sumSquare squareSum -> if sumSquare > squareSum
                            then sumSquare
                            else squareSum) (x^2 + y^2) ((x+y)^2)
                            
sumSquareOrSquareSum3 x y = 
    let sumSquare = x^2 + y^2
        squareSum = (x+y)^2
    in
        if sumSquare > squareSum 
                            then sumSquare
                            else squareSum 


f y = y * 2
doubleDouble x = (\dubs -> dubs x * 2) (\y -> y * 2)

overwrite x = (\x -> (\y -> (\z -> z) 4) 3) 2

names = [("Naomi", "Rosenberg"),("Mike","Harris"),("Marshall","Rosenberg")]

compareLastNames name1 name2 = if lastName1 > lastName2
    then GT 
    else if lastName1 < lastName2
        then LT 
        else if firstName1 > firstName2
            then GT
            else if firstName1 < firstName2
                then LT 
                else EQ
    where lastName1 = snd name1
          lastName2 = snd name2
          firstName1 = fst name1
          firstName2 = fst name2

compareLastNamesNaomi name1 name2 = if lastName1 == lastName2
    then compare firstName1 firstName2
    else compare lastName1 lastName2
    where lastName1 = snd name1
          lastName2 = snd name2
          firstName1 = fst name1
          firstName2 = fst name2

compareLastNamesBook name1 name2 = if result == EQ
                               then compare (fst name1) (fst name2)
                               else result
  where result = compare (snd name1) (snd name2)

sfOffice name = if lastName < "L"
                then nameText
                     ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText
                     ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where lastName = snd name
        nameText = fst name ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where nameText = fst name ++ " " ++ snd name

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where nameText = snd name

dcOffice name = nameText ++ " ESQ - PO Box 789 - New York, NY, 10013"
  where nameText = fst name ++ " " ++ snd name

getLocationFunction location = case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "dc" -> dcOffice
    _ -> (\name -> fst name ++ " " ++ snd name)

addressLetter name location = locationFunction name
    where locationFunction = getLocationFunction location

getRequestUrl host apiKey resource id = host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

genHostRequestBuilder host = (\apiKey resource id ->
    getRequestUrl host apiKey resource id)

genApiRequestBuilder hostBuilder apiKey = (\resource id ->
    hostBuilder apiKey resource id)

-- stopped at quick check 5.2 Naomi's brain melting.
ifEven f x = if even x then f x else x

ifEvenInc = ifEven inc


binaryPartialApplication binaryFunction x = \y -> binaryFunction x y

firstHalf x l = x `elem` theFirstHalf
    where theFirstHalf = take half l 
          half = div (length l) 2

myGCD a b = if remainder == 0 then b else myGCD b remainder
    where remainder = a `mod` b

-- Q7.1
myTail [] = []
myTail (_:xs) = xs

-- Q7.2
myGCD2 a 0 = a
myGCD2 a b = myGCD b (a `mod` b)

-- Identify the end goal(s).
-- Determine what happens when a goal is reached.
-- List all alternate possibilities.
-- Determine your “rinse and repeat” process.
-- Ensure that each alternative moves you toward the goal.

