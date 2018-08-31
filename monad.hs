import Control.Monad

a = liftM2 (\x y  -> x * y * 2) [1,2,3] [2, 3, 4]

bindAndSequencing :: IO ()
bindAndSequencing  = do
  putStrLn "Enter Name: "
  name <- getLine
  putStrLn ("Your name is " ++ name)
  age <- getLine
  putStrLn ("Your age is " ++ age)


trueAndFalse :: (Integral a) => [a] -> [Bool]
trueAndFalse l = do
  x <- l
  if even x then [True] else [False]

data Cow = Cow {
    name   :: String
  , age    :: Int
  , weight :: Int
  } deriving (Show, Eq)



noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str


noNegative :: Int -> Maybe Int

noNegative n | n >= 0    = Just n
             | otherwise = Nothing


weightCheck :: Cow -> Maybe Cow
weightCheck c = let w = weight c
                    n = name c
                      in if n == "Bess" && w > 499 then Nothing else Just c

makeSphericalCow :: String -> Int -> Int -> Maybe Cow
makeSphericalCow n a w = do
  maybeN <- noEmpty    n
  maybeA <- noNegative a
  maybeW <- noNegative w
  weightCheck $ Cow maybeN maybeA maybeW


data MyMaybe a = MyJust a | MyNothing
                    deriving (Eq, Show)

instance Functor MyMaybe where
  fmap f (MyJust a) = MyJust $ f a
  fmap f _ = MyNothing

instance Applicative MyMaybe where
  pure a                      = MyJust a
  (<*>) (MyJust f) (MyJust a) = MyJust $ f a
  (<*>) (MyJust _) MyNothing  = MyNothing
  (<*>) MyNothing (MyJust _)  = MyNothing
  (<*>) _ _                   = MyNothing

instance Monad MyMaybe where
  (>>= ) (MyJust a) f  =  f a
  (>>= ) (MyNothing) _ =  MyNothing

-- Monadic Composotion -

mcomp :: (Monad m) =>
          (b -> m c) ->
          (a -> m b) ->
          (a -> m c)

mcomp f g x = g x >>= f

-- Kliesli Composotion

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn "Hi"
  x <- getLine
  putStrLn x
  return x

readM :: (Read a) => String -> IO a

readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM



