replaceThe :: String -> String

replaceThe s =
  let s' = foldr (\w a -> if w == "the" then a ++ "a " else a ++ w ++ " ") "" (words s)
    in take ((length s') - 1) s'


