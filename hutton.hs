data Expr = Lit Int
          | Add Expr Expr


instance Show Expr where
  show (Lit i) = show i
  show (Add e1 e2) = (show e1) ++ " + " ++ (show e2)

eval :: Expr -> Int
eval (Lit i) = i
eval (Add e1 e2) = (eval e1) + (eval e2)

printExpr :: Expr -> String
printExpr = show

