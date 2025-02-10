type Opt a = Num a => (a, Bool)

safeRoot :: (Ord a, Floating a) => a -> Opt a
safeRoot x
    | x >= 0.0 = (sqrt x, True)
    | otherwise = (0, False)

safeRecip :: (Ord a, Floating a) => a -> Opt a
safeRecip x
    | x == 0.0 = (0, False)
    | otherwise = (1/x, True)

id :: a -> Opt a
id x = (x, True)

(>=>) ::  (Num a, Num b, Num c) => (a -> Opt b) -> (b -> Opt c) -> (a -> Opt c)
f1 >=> f2 = \x ->
    let (val1, bool1) = f1 x
        (val2, bool2) = if bool1 then f2 val1 else (0, False)
    in (val2, bool2)

safeRR = safeRoot >=> safeRecip

