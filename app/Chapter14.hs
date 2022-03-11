module Chapter14 where

    data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6
    
    data NewEngland = ME | VT | NH | MA | RI | CT 
    instance Show NewEngland where
        show ME = "Maine"
        show VT = "Vermont"
        show NH = "New Hampshire"
        show MA = "Massachusetts"
        show RI = "Rhode Island"
        show CT = "Connecticut"

    instance Eq NewEngland where
        (==) ME ME = True
        (==) VT VT = True
        (==) NH NH = True
        (==) MA MA = True
        (==) RI RI = True
        (==) CT CT = True
        (==) _ _ = False
        