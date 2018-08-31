data MyNat =  Zero
            | Succ MyNat
              deriving (Show)

natToInt :: MyNat -> Int
natToInt Zero = 0

natToInt (Succ x) = 1 + (natToInt x)

myIterate :: (a -> a) -> a -> [a]

myIterate f i = [i] ++ myIterate f (f i)
