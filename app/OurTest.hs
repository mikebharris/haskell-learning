module OurTest
  where
    x = 5
    y = (6, "Hello")
    z = x * fst y
    square x = x * x
    signum x =
        if x < 0
          then -1
          else if x > 0
            then 1
            else 0
    f x =
        case x of
          0 -> 1
          1 -> 5
          2 -> 2
          _ -> 1000

    {- Fibonacci series -}
    g 0 = 1
    g 1 = 1
    g x = g(x-1) + g(x-2)

    h x =
      case x of
        0 -> 1
        1 -> 1
        _ -> h(x-1) + h(x-2)

    roots a b c =
        let discr = sqrt (b*b - 4*a*c)
            twoa = 2*a
        in  ((-b + discr) / twoa,
             (-b - discr) / twoa)

    factorial 1 = 1
    factorial n = n * factorial (n-1)

    fib n = go n (0,1)
      where
          go n (a, b) | n==0      = a
                      | otherwise = go (n-1) (b, a+b)

    my_filter p [] = []
    my_filter p (x:xs) =
      if p x
        then x : my_filter p xs -- recursion
        else my_filter p xs

    mult a 0 = 0
    mult a b = a + (mult a (b - 1))

    my_map f [] = []
    my_map f (x:xs) =
      f x : my_map f xs

    askForWords = do
      putStrLn "Please enter a word:"
      word <- getLine
      if word == ""
        then return []
        else do
          rest <- askForWords
          return (word : rest)

    numberGame = do
      result <- askForNumbers
      putStrLn ("The sum is " ++ show(foldl (+) 0 result))
      putStrLn ("The product is " ++ show(foldl (*) 0 result))
      printFactorial result

    printFactorial [] = return ()

    printFactorial (x:xs) = do
      putStrLn (stringFactorial x)
      printFactorial xs

    stringFactorial n = show(n) ++ " factorial is " ++ show(factorial n)

    askForNumbers = do
      putStrLn "Give me a number: "
      number <- getLine
      let intNumber = read number
      if intNumber == 0
        then return []
        else do
          morenumbers <- askForNumbers
          return (intNumber : morenumbers)

    firstElement :: [a] -> Maybe a
    firstElement []     = Nothing
    firstElement (x:xs) = Just x

    findElement :: (a -> Bool) -> [a] -> Maybe a
    findElement p [] = Nothing
    findElement p (x:xs) =
        if p x then Just x
        else findElement p xs

    eitherStringNum :: Int -> Either String Int
    eitherStringNum 0 = Left "zero"
    eitherStringNum x = Right x

    data Tuple a b c d = Tuple4 a b c d
                       | Tuple3 a b c
                       | Tuple2 a b
                       | Tuple a

    -- tuple1 :: Tuple a b c d -> Maybe a
    -- tuple1 (Tuple x) = Just x
    -- tuple2 (Tuple x) =  Nothing
    -- tuple2 (Tuple2 x y) = Just y
-- 
    -- convert (Tuple x) = Left x
    -- convert (Tuple2 x y) = (x,y)
    -- 
    fuck :: Either x y -> String
    fuck (Left x) = "hello"
    -- fuck (Left x) = "result " ++ x 

    wank :: Either String String -> Int
    wank x = 2
    er y = 0
