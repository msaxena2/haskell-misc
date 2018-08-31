added :: Maybe Integer
added = pure (+3) <*> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])




validateLength :: Int
               -> String
               -> Maybe String
validateLength maxLen s
  = if (length s) > maxLen then Nothing else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)
mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s
mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = let mp = pure Person  :: Maybe (Name -> Address -> Person)
                in mp <*> mkName n <*> mkAddress a

data Cow = Cow {
  name :: String
  , age :: Int
  , weight :: Int
} deriving (Eq, Show)
noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str
noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing


cowFromString :: String -> Int -> Int -> Maybe Cow

cowFromString n a w = Cow <$> noEmpty n <*> noNegative a <*> noNegative w
