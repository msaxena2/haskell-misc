{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn y) = x == y


data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a1 a2) (Two b1 b2) = (a1 == b1) && (a2 == b2)


data StringOrInt = TisAString String | TisAnInt Int

instance Eq StringOrInt where
  (==) (TisAString s1) (TisAString s2) = s1 == s2
  (==) (TisAnInt i1) (TisAnInt i2) = i1 == i2
  (==) _ _ = False

data Pair a = Pair a a

instance (Eq a) => Eq (Pair a) where
  (==) (Pair a1 a2) (Pair b1 b2) = (a1 == b1) && (a2 == b2)

data EitherOr a b = Hello a | Bye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello y) = x == y
  (==) (Bye x) (Bye y) = x == y
  (==) _ _ = False


myPrint :: (Show a) => a -> IO ()
myPrint = putStrLn . show

data Doggies a =
      Husky a
    | Mastiff a


data DogueDeBordeaux doge = DogueDeBordeaux doge

data Manufacturer = Mini | Mazda | Tata deriving (Show, Eq)

data Airline = PapuAir | United | Catapult deriving (Show, Eq)

data Price = Price Integer deriving (Show, Eq)

data Vehicle =
    Car Manufacturer Price
  | Plane Airline
  deriving (Show, Eq)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

areCars :: [Vehicle] -> Bool
areCars l = foldr (\a b -> b && (isCar a)) True l


newtype Goats = Goats Int deriving (TooMany)

type MyInt = Int

instance TooMany MyInt where
  tooMany x = x > 2

tooManyGoats :: Goats -> Bool

tooManyGoats (Goats n)  = n > 42

class TooMany n where
  tooMany :: n -> Bool


instance TooMany (Int, String) where
  tooMany (x, _) = x > 32

instance TooMany (Int, Int) where
  tooMany (x, y) = x + y > 32

data Person = Person {
  name :: String,
  age :: Int
              }
              deriving (Eq, Show)



data Fiction = Fiction deriving Show
data NonFiction = NonFiction deriving Show

data BookType = FictionBook Fiction
                | NonFictionBook NonFiction
                  deriving Show


type AuthorName = String

data Author = Author (AuthorName, BookType)



