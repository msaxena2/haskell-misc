{- LANGUAGE GeneralizedNewtypeDeriving -}

import qualified Data.Map as Map

type Taps = Int

type NumKey = Int

data PhoneKey = KeypadNum NumKey Taps
  deriving (Ord, Eq, Show)

type DaPhone = Map.Map Char PhoneKey

foldLambda :: (Char, Int) -> DaPhone -> DaPhone

foldLambda (char, charIndex) map =
  let keypadNum = (2 :: Int) + (charIndex `div` 3) in
      Map.insert char
      (KeypadNum keypadNum (let taps = charIndex `mod` 3 in if taps /= 0 then taps else 3 )) map


phoneMap = foldr foldLambda (Map.empty :: DaPhone) (zipWith (\x y -> (x, y)) ['a' .. 'o'] [1 .. ])


