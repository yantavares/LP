data Expr = Lit Int | Add Expr Expr | Sub Expr Expr | Mult Expr Expr
        deriving Show
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mult e1 e2) = eval e1 * eval e2

last1 [x] = x
last1 (x:xs) = last1 xs

elem1 n l = l!!(n-1)