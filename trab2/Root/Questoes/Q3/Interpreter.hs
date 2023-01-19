module Root.Questoes.Q3.Interpreter where

import Root.Questoes.Q3.AbsLI
import Prelude hiding (lookup)

type RContext = [(String, Integer)]
type ErrorMessage = String

{- Dica: somente o tipo de executeP precisa mudar (conforme abaixo),
   mas a sua definicao (corpo) pode continuar a mesma dos exercícios anteriores
-}
executeP :: RContext -> Program -> Either ErrorMessage RContext
-- executeP context (Prog stm) = execute context stm
executeP = undefined 

{- Dica: o tipo de execute deve mudar para
 execute :: RContext -> Stm -> Either ErrorMessage RContext
 Alem disso, o corpo dessa funcao deve ser modificado, pois eval
 retorna tipo diferente agora, ou seja, a avaliacao pode falhar
 e, consequentemente o execute tambem. Assim, todos tipos de comandos
 serao afetados
-}
execute :: RContext -> Stm -> RContext
execute context x = case x of
  SAss id exp -> update context (getStr id) (eval context exp)
  SBlock [] -> context
  SBlock (s : stms) -> execute (execute context s) (SBlock stms)
  SWhile exp stm ->
    if ((eval context exp) /= 0)
      then execute (execute context stm) (SWhile exp stm)
      else context

{- Dica: o tipo de eval deve mudar para
 eval :: RContext -> Exp -> Either ErrorMessage Integer
-}
eval :: RContext -> Exp -> Integer
eval context x = case x of
  EAdd exp0 exp -> eval context exp0 + eval context exp
  ESub exp0 exp -> eval context exp0 - eval context exp
  EMul exp0 exp -> eval context exp0 * eval context exp
  EDiv exp0 exp -> eval context exp0 `div` eval context exp
  EInt n -> n
  EVar id -> lookup context (getStr id)
{-  algumas dicas abaixo...para voce adaptar o codigo acima
    EDiv e1 e2 -> case eval context e1 of
                    Right ve1 -> case eval context e2 of
                                   Right ve2 -> if (ve2 == 0)
                                                 then Left ("divisao por 0 na expressao: "
                                                            ++ show (EDiv e1 e2))
                                                 else Right (ve1 `div` ve2)
                                  Left msg -> Left msg
                    Left msg -> Left msg
    EInt n  ->  Right n
-}


-- Dica: voce nao precisa mudar o codigo a partir daqui

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
