import Data.Char

removeOdd :: Integral a => [a] -> [a]
removeOdd [] = []
removeOdd (x : xs)
    | mod x 2 == 0 = x : (removeOdd xs)
    | otherwise    = removeOdd xs

anyEven :: [Int] -> Bool
anyEven nums = case (removeOdd nums) of
    []       -> False
    (x : xs) -> True

numEven :: Integral a => [a] -> Int
numEven nums =
    let evenNums = removeOdd nums
    in length evenNums

compose :: (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
compose f g x = f (g x)

add1 :: Int -> Int
add1 x = x + 1

add2 :: Int -> Int
add2 x = x + 2

mult2 :: Num a => a -> a
mult2 x = x * 2

-- squares :: Int -> Int -> [Int]
squares :: (Enum a, Num a) => a -> a -> [a]
squares x y = [ i * i | i <- [x..y] ]

makeSpaces :: Int -> String
makeSpaces x
    | x <= 0 = ""
    | otherwise = " " ++ makeSpaces (x-1)

pushRight :: Int -> [Char] -> [Char]
pushRight x s = makeSpaces x ++ s

f :: Float -> Float -> Float
f x y = x + y


sales :: Int -> Int
sales n = n

totalSales :: Int -> Int
totalSales n 
    | n == 0    = 0
    | otherwise = totalSales (n-1) + sales n

averageSales :: Int -> Float
averageSales n = fromIntegral (totalSales n) / fromIntegral n

test :: Int -> Int -> Int
test x y
    | x==0 = 100
    | x==1 = 200
    | otherwise = 0

double :: Num a => [a] -> [a]
double [] = []
double (x:xs) = x * 2 : double xs

member :: Eq t => [t] -> t -> Bool
member [] _ = False
member (x:xs) i
    | x == i = True
    | otherwise = member xs i

sumPairs :: Num a => [(a, a)] -> [a]
sumPairs [] = []
sumPairs ((x,y) : xys) = (x + y) : sumPairs xys

digits :: [Char] -> [Char]
digits [] = []
digits (x:xs)
    | member [1..9] (digitToInt x) = x : digits xs
    | otherwise = digits xs

digits2 :: [Char] -> [Char]
digits2 [] = []
digits2 (x:xs)
    | member ['1'..'9'] x = x : digits xs
    | otherwise = digits xs

books :: Eq t => [(t, a)] -> t -> [a]
books [] _ = []
books ((p, l) : pls) per
    | per == p  = l : books pls per
    | otherwise = books pls per

books2 :: Eq a1 => [(a1, a2)] -> a1 -> [a2]
books2 db p = [l | (per, l) <- db, per == p]

borrowers :: Eq t => [(a, t)] -> t -> [a]
borrowers [] _ = []
borrowers ((p, l) : pls) book
    | book == l = p : borrowers pls book
    | otherwise = borrowers pls book

borrowed :: (Eq a, Eq t) => [(a, t)] -> t -> Bool
borrowed db b = borrowers db b /= []

numBorrowed :: Eq t => [(t, a)] -> t -> Int
numBorrowed db p = length (books db p)

makeLoan :: [(a, b)] -> a -> b -> [(a, b)]
makeLoan db p l = (p, l) : db

doubleIfEven :: Integral a => [a] -> [a]
doubleIfEven xs = [2 * a | a <- xs, isEven a]
    where isEven n = mod n 2 == 0

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (a:as) = quickSort [e | e <- as, e <= a] ++ [a] ++ quickSort [ e | e <- as, e > a]

bookFunc :: t1 -> t2 -> (t2 -> t1 -> b) -> (t1, b)
bookFunc a db f = (a, f db a)

allBooksBorrowed :: (Eq a, Eq b) => [(a, b)] -> [(a, [b])]
allBooksBorrowed [] = []
allBooksBorrowed ((a,b):as) = ((bookFunc a ((a,b):as) books2 ) : allBooksBorrowed as)

foldr2 :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
foldr2 f s [] = s
foldr2 f s (a:as) = f a (foldr2 f s as)

foldl2 :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
foldl2 _ acc [] = acc
foldl2 f acc (a:as) = foldl2 f (f acc a) as

foldFac :: (Num b, Enum b) => b -> b
foldFac num = foldr (*) 1 [2..num]

fold :: (t -> t -> t) -> [t] -> t
fold f [a]    = a
fold f (a:as) = f a (fold f as)

twice :: (a -> a) -> a -> a
twice f = f . f

twoFuncComp :: (b -> c) -> (a -> b) -> a -> c
twoFuncComp f g = f . g


addNum :: Int -> (Int -> Int)
addNum n = h
    where
        h m = n + m

addNum2 :: Num a => a -> a -> a
addNum2 n = (\m -> n+m)

multiply :: Num a => a -> a -> a
multiply x y = x * y

doubleList :: [Int] -> [Int]
doubleList = map (multiply 2)

doubleList2 :: Num b => [b] -> [b]
doubleList2 l = map mult2 l

doubleList3 :: [Integer] -> [Integer]
doubleList3 = map (*2)

getEvens :: [Integer] -> [Integer]
getEvens = filter ((==0) . (`mod` 2))

data Aluno = Yan Int | Renato String | GuiGo Float
        deriving Show

data Tree = Leaf | Branch Int (Tree) (Tree)
        deriving Show

depth :: Num a => Tree -> a
depth Leaf = 0
depth (Branch _ ae ad) = 1 + depth ae + depth ad

sumTree Leaf = 0
sumTree (Branch n ae ad) = n + sumTree ae + sumTree ad

-- PEGA 2 FUNÇÕES E APLICA EM X DEPENDENDO DO VALOR DE A
-- Ex man add1 add2 [1,0,0] 12 ----> 16
man :: (Eq a, Num a) => (b -> b) -> (b -> b) -> [a] -> b -> b
man _ _ [] x = x
man f1 f2 (a:as) x
    | a == 0 = (f1 . (man f1 f2 as)) x 
    | otherwise = (f2 . (man f1 f2 as)) x 

