-- Daka Bence Neptun: I5RQFI
module MagicSquare where
    
    import Data.List

    type Matrix = [[Int]]

    --2.szint 
    isRowEq :: Matrix -> Bool --a sorok osszegei megegyeznek-e
    isRowEq [] = True
    isRowEq (x:xs)
        | xs == [] = True
        | sum x == sum (head xs) = True && isRowEq xs
        | otherwise = False

    isUniqueList :: [Int] -> Int -> Bool --Egy adott ertek elofordul e a listaban 
    isUniqueList [] _ = True
    isUniqueList (x:xs) y
            |  x == y = False
            | otherwise = True && isUniqueList xs y 

    isUnique :: Matrix -> Bool   --egyedi az osszes ertek
    isUnique [] = True
    isUnique (x:xs)
        | tail x == [] = isUnique xs
        | isUniqueList (concat ((tail x):xs)) (head x) = isUnique ( (tail x): xs)
        | otherwise = False

    isSquare :: Matrix -> Bool --nxn matrix-e
    isSquare [] = True
    isSquare (x:xs) = length x == length (x:xs)

    zipWith' :: (x -> y -> z) -> [x] -> [y] -> [z]
    zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys
    zipWith' f _ _ = []

    sumCol :: Matrix -> [Int] --oszlopok osszege
    sumCol [] = []
    sumCol (x:xs)
        | xs /= [] =  zipWith' (+) x (sumCol xs)
        | otherwise = x

    sumColFirst :: Matrix -> Int --az 1. oszlopnak az osszege
    sumColFirst [] = 0
    sumColFirst (x:xs) = (head x) + sumColFirst xs
  

    isColEq :: [Int] -> Bool  --oszlopok osszege egyezik-e
    isColEq [] = True
    isColEq (x:xs)
        | xs == [] = True
        | head (x:xs) == head (tail (x:xs)) = True && isColEq xs
        | otherwise = False

    helper :: [Int] -> Int -> Int
    helper (x:xs) y = (x:xs) !! y

    diagonalSum :: Matrix -> Int -> Int    --foatlo osszege (y = 0 kezdessel az index es azzal vegig halad)
    diagonalSum [] y = 0
    diagonalSum (x:xs) y = (helper x y) + (diagonalSum xs (y+1))

    reverseMatrix :: Matrix -> Matrix  --megforditja a matrixot ==> a fo atlok megcserelodnek igy csak ujra a diagonalSum eleg
    reverseMatrix [] = []
    reverseMatrix (x:xs) = reverseMatrix (xs) ++ [x]

    isDiagEq :: Matrix -> Bool --megegyezik e a foatlok osszege
    isDiagEq [] = True
    isDiagEq (x:xs)
        | (diagonalSum (x:xs) 0) == (diagonalSum (reverseMatrix (x:xs)) 0) = True
        | otherwise = False

    isOnein :: [Int] -> Bool  --van-e 1-es a matrixban
    isOnein [] = False
    isOnein (x:xs) 
        | x == 1 = True
        | otherwise = isOnein xs

    isSeq :: [Int] -> Bool --1-esevel novekednek-e a matrix ertekei
    isSeq [] = True
    isSeq (x:xs)
        | xs == [] = True
        | x + 1 == head (xs) = True && isSeq xs
        | otherwise = False

    isReq :: Matrix -> Bool --megfelel e az 1-tol valo 1-el valo novekedes feltetelenek
    isReq [] = True
    isReq x
        | isOnein (concat x) && isSeq (sort (concat x)) = True
        | otherwise = False

    magicSquare :: Matrix -> Maybe Int
    magicSquare [] = Just 0
    magicSquare (x:xs)
        | sum(x) == sumColFirst (x:xs) && sum(x) == (diagonalSum (x:xs) 0) && isDiagEq(x:xs) && isReq (x:xs) && isSquare (x:xs) && isUnique (x:xs) && isRowEq (x:xs) && isColEq (sumCol(x:xs))  = Just (sum x)
        |otherwise = Nothing
    