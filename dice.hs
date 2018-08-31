import Control.Applicative
import System.Random
import Control.Monad.State.Strict


--
-- rollDiceIO :: IO (Int, Int)
--
-- rollDiceIO =  (randomRIO (1, 6)) >>=
--   (\r1 -> (randomRIO (1, 6)) >>= \r2 -> return (r1, r2))
--
-- rollDiceIOList' :: Int -> IO [Int] -> IO [Int]
--
-- rollDiceIOList' n l =
--   if n == 0 then l
--   else (>>=) l (\l' -> (
--     (>>=) (randomRIO (1, 6)) (
--       \r -> rollDiceIOList' (n - 1) (return (r:l')
--                                     )
--                              )
--                        )
--                )
--
--
--
-- rollDiceIOList :: Int -> IO [Int]
--
-- rollDiceIOList n = rollDiceIOList' n $ return []
--
-- charCount :: State (String -> (Int, String))
--
-- charCount input = input >>= (\str -> if (length str == 0)
--                                         then return (0, "")
--                                         else
--                                           let (c, s) = charcount
--
--
--
--

plus :: Int -> Int -> Int

plus a b = if (a == 0) then b else (plus 1 (a - 1)) `plus` b

