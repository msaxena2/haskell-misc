import Control.Monad.State.Strict

type Stack = [Integer]

pop :: State Stack Integer

pop = state $ \(x:xs) -> (x, xs)

push :: Integer -> State Stack ()

push a = state $ \s -> ((), a:s)
