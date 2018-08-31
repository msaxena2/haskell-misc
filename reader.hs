{- LANGUAGE GeneralizedNewtypeDeriving -}

import Control.Applicative
import Control.Monad.Reader

boop = (*2)
doop = (+10)

bip = boop . doop

bloop = fmap boop doop

bbop = (+) <$> boop <*> bloop

data MyReader i o =
  MyReader {function :: (i -> o)}

instance Functor (MyReader i) where
  fmap f (MyReader f') = MyReader (\i -> f . f' $ i)

newtype HumanName = HumanName String
  deriving (Eq, Show)

newtype DogName = DogName String
  deriving (Eq, Show)

newtype Address = Address String
  deriving (Eq, Show)

data Person = Person {
    humanName :: HumanName
  , dogName   :: DogName
  , address   :: Address
} deriving (Eq, Show)

data Dog = Dog {
    dogsName    :: DogName
  , dogsAddress :: Address
} deriving (Eq, Show)


pers :: Person

pers = Person (HumanName "Lalu") (DogName "Lala") (Address "Bihar")

getDog :: Person -> Dog

getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

