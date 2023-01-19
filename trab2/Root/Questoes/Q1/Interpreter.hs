module Root.Questoes.Q1.Interpreter where

import Root.Questoes.Q1.AbsLI
import Prelude hiding (lookup)

type RContext = [(String, Integer)]

executeP :: RContext -> Program -> RContext
executeP context (Prog stm) = execute context stm

execute :: RContext -> Stm -> RContext
execute context x = case x of
  SAss id exp -> update context (getStr id) (eval context exp)
  SBlock [] -> context
  SBlock (s : stms) -> execute (execute context s) (SBlock stms)
  SWhile exp stm ->
    if ((eval context exp) /= 0)
      then execute (execute context stm) (SWhile exp stm)
      else context
  {- trate aqui o caso de o comando "x" ser um comando "SdoWhile"
   dica: uma solucao mais curta tem 1 linha, e uma solucao mais "longa" tem menos de 5 linhas
  -}

eval :: RContext -> Exp -> Integer
eval context x = case x of
  EAdd exp0 exp -> eval context exp0 + eval context exp
  ESub exp0 exp -> eval context exp0 - eval context exp
  EMul exp0 exp -> eval context exp0 * eval context exp
  EDiv exp0 exp -> eval context exp0 `div` eval context exp
  EInt n -> n
  EVar id -> lookup context (getStr id)


getStr :: Ident -> String
getStr (Ident s) = s

lookup :: RContext -> String -> Integer
lookup ((i, v) : cs) s
  | i == s = v
  | otherwise = lookup cs s

update :: RContext -> String -> Integer -> RContext
update [] s v = [(s, v)]
update ((i, v) : cs) s nv
  | i == s = (i, nv) : cs
  | otherwise = (i, v) : update cs s nv
