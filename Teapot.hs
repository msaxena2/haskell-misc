module TestModule where

class  (Show a) => A a where
  print :: a -> String
  

data B = B  deriving Show

instance A B where 
  print B = show B

myPrint :: (A a) => a -> String 


myPrint x = TestModule.print x
