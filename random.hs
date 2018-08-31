import System.Random
import Control.Applicative (liftA3, liftA2)
import Control.Monad (replicateM)
import Control.Monad.Trans.State


main :: IO ()
main = putStrLn "ok"


data Die =  DieOne
          | DieTwo
          | DieThree
          | DieFour
          | DieFive
          | DieSix
          deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
               1 -> DieOne
               2 -> DieTwo
               3 -> DieThree
               4 -> DieFour
               5 -> DieFive
               6 -> DieSix
               _ -> error  "Incorrect Int Value for die"

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes =
  let stdGen = mkStdGen 5
   in let
    (roll1, s1) = randomR (1, 6) stdGen
    (roll2, s2) = randomR (1, 6) s1
    (roll3, _)  = randomR (1, 6) s2
 in (intToDie roll1, intToDie roll2, intToDie roll3)

rollDie :: State StdGen Die

rollDie = state $ \state' ->
  let (randomNum, newState) = randomR (1, 6) state'
   in (intToDie randomNum, newState)

rollNDie :: Int -> State StdGen [Die]
rollNDie n = replicateM n rollDie

rollsToGet20 :: StdGen -> Int
rollsToGet20 g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen =
      if (sum >= 20) then count
                     else let (acc, nextGen) = randomR (1, 6) gen
                           in go (sum + acc) (count + 1) nextGen

rollsToGetn :: Int -> StdGen -> Int
rollsToGetn n g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen =
      if (sum >= n) then count
                     else let (acc, nextGen) = randomR (1, 6) gen
                           in go (sum + acc) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 0 [] g
  where
    go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
    go sum count log gen =
      if (sum >= n) then (count, log)
                    else let (r, newGen) = randomR (1, 6) gen in
                             go (sum + r) (count + 1) (log ++ [intToDie r]) newGen


newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

-- (a -> b) -> f a -> f b
-- (a -> b) -> Moi s a -> Moi s b
instance Functor (Moi s) where
  fmap f (Moi f') =
    Moi $ \x -> case f' x of
      (out, s') -> (f out, s')

instance Applicative (Moi s) where
-- pure :: a -> f a
-- pure :: a -> Moi s a
  pure x = Moi $ \s -> (x, s)

-- f (a -> b) -> f a -> f b
-- Moi (s -> ((a -> b), s) -> Moi (s -> (a, s)) -> Moi (s -> (b, s))
  (<*>) (Moi f1) (Moi f2) = Moi $ \s -> let f = fst (f1 s)
                                         in case (f2 s) of
                                              (a', s') -> (f a', s')

instance Monad (Moi s) where
   return = pure
   (>>=) (Moi f1) f2 = Moi $
     \s -> let (a, s') = f1 s in runMoi (f2 a) s'


-- The state monad is a wrapper for (s -> (a, s)), where
-- s is the state, a is the accumulator.
-- Let's look at the bind operation for the state monad -

(>>=) (Moi f) (f') =
  Moi $ \s -> let (a, s') = f s in (runState (f' a)) s'


fromStoAandS :: Int -> (String, Int)

fromStoAandS c | c `mod` 5 == 0 = ("foo", c + 1)
fromStoAandS c | otherwise      = ("bar", c + 1)

stateIntString :: State Int String
stateIntString = state fromStoAandS

sumToN' :: Int -> Int -> Int

sumToN' n a | n == 0    = a
sumToN' n a | otherwise = sumToN' (n - 1) (a + n)

-- State monad example for a stack

type Stack = [Int]

pushToStack :: Int -> Stack -> Stack
pushToStack a s = a : s

popFromStack :: Stack -> (Maybe Int, Stack)

popFromStack [] = (Nothing, [])
popFromStack (x:xs) = (Just x, xs)


pop :: State Stack Int
pop = state $ \(a:s) -> (a, s)

push :: Int -> State Stack ()
push x = state $ \s -> ((), x:s)

pushAndPop :: IO ()
pushAndPop = mapM_ (\x -> if x `mod` 15 == 0 then putStrLn "fizzbuzz"
                                       else if x `mod` 5 == 0 then putStrLn "buzz"
                                         else if x `mod` 3 == 0 then putStrLn "fizz"
                                           else putStrLn $ show x) [1 .. 100]

-- Functor Composition

functor1 :: (Functor f) => f Int -> f Bool
functor1 = (<$>) (\x -> if x > 2 then True else False)

functor2 :: (Functor f) => f Bool -> f String
functor2 = (<$>) (\x -> show x)

fcomp :: (Functor f) => f Int -> f String
fcomp = functor2 . functor1


