{-
    fact (x) = { 1                if x = 0
                 x * fact (x - 1) otherwise
               }

   def fact(x) = return 1 if x == 0 else return fact(x - 1) * x
-}

fact :: Int -> Int

fact x | x == 0    = 1
fact x | otherwise = fact (x - 1) * x

printMyFactorial :: Int -> IO ()
printMyFactorial n = putStrLn $ show $ fact n

-- (>>=) :: M a -> ( a -> M b) -> M b

-- return a :: a -> M a

monadEx :: IO ()
monadEx = putStrLn "Enter A Number" >> readLn >>= (\a -> putStrLn a)

listMonadEx = do
  putStrLn "Enter Name"
  x <- getLine
  if (x == "Balaji") then putStrLn "Noob" else putStrLn "Not Noob"


