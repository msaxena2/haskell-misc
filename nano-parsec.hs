
module NanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative

-- Parser takes a stream and converts to AST

newtype Parser a = Parser { parse :: String -> [(a, String)] }


-- Running consumes string to produce a
runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)]   -> error "Parser did not consume entire stream."
    _           -> error "Parser error."


item :: Parser Char

-- Note, the Parser Constructor is being used
-- to construct data to type Parser char.
-- The Constructor's record type is String -> [(char, String)]

item = Parser $ \s ->
  case s of
   []     -> []
   (c:cs) -> [(c,cs)]


