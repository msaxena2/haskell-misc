{-# LANGUAGE RankNTypes #-}

type Nat f g a =   f a -> g a

maybeToList :: (Num a) => Nat Maybe [] a
maybeToList (Just a) = [a + 1]
maybeToList Nothing = []


