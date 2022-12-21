import Data.List

-- ! 1)

--  a)
diffList :: Eq a => [a] -> [a] -> [a]
diffList [] _ = []
diffList (x:xs) l2
    | elem x l2  = diffList xs l2
    | otherwise  = x : diffList xs l2

--  b)
interList :: Eq a => [a] -> [a] -> [a]
interList [] _ = []
interList (x:xs) l2
    | elem x l2  = x : interList xs l2
    | otherwise  = interList xs l2

--  c)
uList :: [a] -> [a] -> [a]
uList l1 l2 = l1 ++ l2

--  d)
noRpt :: Eq a => [a] -> [a]
noRpt [] = []
noRpt (x:xs)
    | elem x xs = noRpt xs
    | otherwise = x : noRpt xs

uListNoRpt :: Eq a => [a] -> [a] -> [a]
uListNoRpt l1 l2 = noRpt (uList l1 l2)

--  e)
lastElem :: [a] -> a
lastElem [a]    = a
lastElem (x:xs) = lastElem xs

--  f)
nElem :: (Eq t, Num t) => [a] -> t -> a
nElem (x:xs) n
    | 0 == n = x
    | otherwise = nElem xs (n-1)

--  g)
removeLast :: [a] -> [a]
removeLast [a]    = []
removeLast (x:xs) = x : removeLast xs

inv :: [a] -> [a]
inv [] = []
inv l1 = lastElem l1 : inv (removeLast l1)

betterInv :: [a] -> [a]
betterInv []     = []
betterInv (x:xs) = betterInv xs ++ [x]

--  g)
decList :: Ord a => [a] -> [a]
decList l1 = betterInv(sort l1)

--  h)
decListNoRpt :: Ord a => [a] -> [a]
decListNoRpt l1 = noRpt (decList l1)

--  i)
isDecSorted :: Ord a => [a] -> Bool
isDecSorted l1 = decList l1 == l1

--  ii)
isDecSorted2 :: Ord a => [a] -> Bool
isDecSorted2 [] = True
isDecSorted2 [a] = True
isDecSorted2 (xa:xb:xs) = xa >= xb && isDecSorted2 xs

--  iii)
fold :: (t -> t -> t) -> [t] -> t
fold f [a]    = a
fold f (a:as) = f a (fold f as)

isOrdered :: [Int] -> Bool
isOrdered (x:xs) = fold (&&) (map isOrderedPair (zip (x:xs) (xs)))

isOrderedPair :: (Int, Int) -> Bool
isOrderedPair (x, y) = x <= y

-- ! 2)
type Test = Int
data Expr = Lit Test | Add Expr Expr | Sub Expr Expr | Mult Expr Expr
        deriving Show
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mult e1 e2) = (eval e1) * (eval e2)

-- ! 5)

addNum :: Num p => p -> p -> p
addNum n = h
  where h m = n + m

addNum2 :: Num a => a -> a -> a
addNum2 n = (n+)

-- ! 6)
-- Resolva em Haskell o seguinte problema: a partir de duas notas das provas de cada aluno,
-- determinar a lista dos alunos aprovados, com suas respectivas médias. O resultado deve estar
-- ordenado crescentemente pela média aritmética das notas. A aprovação ocorre se, e somente se, tal
-- média é maior ou igual a cinco.

aprovados :: (Ord a1, Fractional a1) => [(a2, a1, a1)] -> [(a2, a1)]
aprovados db = sortGrade [(p, media a b) | (p, a ,b) <- db, media a b >= 5.0 ] 
    where media a b = (a + b)/2
          sortGrade [] = []
          sortGrade ((p1,m1) : as) = sortGrade [(p2, m2) | (p2, m2) <- as, m2 <= m1] ++ [(p1,m1)] ++ sortGrade [(p2, m2) | (p2, m2) <- as, m2 > m1]