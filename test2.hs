data Expr = Lit Test | Add Expr Expr | Sub Expr Expr | Mult Expr Expr
        deriving Show
type Test = Int

eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mult e1 e2) = (eval e1) * (eval e2)