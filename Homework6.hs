-- Daka Bence Neptun: I5RQFI
module Homework6 where

    --1 feladat
    sort :: Eq a => [a] -> a -> [a]
    sort [] _ = []
    sort (x:xs) y
        | x == y =  sort xs y
        | otherwise = x : sort xs y


    listDiff :: Eq a => [a] -> [a] -> [a]
    listDiff [] _ = []
    listDiff a [] = a
    listDiff (x:xs) (y:ys) = listDiff (sort (x:xs) y) ys
     
        

    --2 feladat
    validGame :: String -> Bool
    validGame [] = True
    validGame (x:xs)
         | last head words (x:xs) == head head words (xs) = True && validGame xs
         | otherwise = False


    --3 feladat
    middle :: [a] -> [a]
    middle [] = []
    middle [x] = []
    middle xs = tail (init xs)
    --4 feladat



    --5 feladat
    average :: [Double] -> Double
    average [] = 0
    average a =  sum a / len a
        where  len :: [a] -> Double
               len [] = 0 
               len (x:xs) = 1 + len (xs)


    --6 feladat
    removeDuplicateSpaces :: String -> String
    removeDuplicateSpaces [] = []
    removeDuplicateSpaces (x:xs)
        | x == ' ' && head xs == ' ' = removeDuplicateSpaces (xs) 
        | otherwise = x : removeDuplicateSpaces (xs) 

    --7 feladat
    doubleElements :: [a] -> [a]
    doubleElements [] = []
    doubleElements (x:xs) = x : x : doubleElements (xs)

    --8 feladat
    diffCube :: Num a => a -> a -> a
    diffCube x y = (x - y) ^ 3
