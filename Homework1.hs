module Homework1 where

    --1 feladat
    intExpr1 :: Int 
    intExpr1 = 1

    intExpr2 :: Int
    intExpr2 = 2

    intExpr3 ::Int
    intExpr3 = 3

    charExpr1 :: Char
    charExpr1 = 'A'

    charExpr2 :: Char
    charExpr2 = 'B'

    charExpr3 :: Char
    charExpr3 = 'x'

    boolExpr1 :: Bool 
    boolExpr1 = True

    boolExpr2 :: Bool 
    boolExpr2 = False

    boolExpr3 :: Bool 
    boolExpr3 = 4 < 2

    --2 feladat
    plantSeed :: Int
    plantSeed = 183

    canPlantAll :: Bool
    canPlantAll = (plantSeed `mod` 13) == 0

    remainingSeeds :: Int
    remainingSeeds = plantSeed `mod` 13

    --3 feladat
    zero :: Int
    zero = 0

    inc :: Int -> Int
    inc x = x + 1

    double :: Int -> Int
    double x = x * 2

    seven1 :: Int
    seven1 = inc (inc (inc (double (double (inc zero)))))

    seven2 :: Int
    seven2 = inc (inc (inc (inc (inc (double (inc zero))))))

    seven3 :: Int
    seven3 = inc (inc (inc (inc (inc (inc (inc zero))))))


    --4 feladat
    cmpRem5Rem7 :: Int -> Bool
    cmpRem5Rem7 x = x `rem` 5 > x `rem` 7

    --5 feladat
    foo :: Int -> Bool -> Bool
    foo x y = x > 2 || y == False

    bar :: Bool -> Int -> Bool
    bar x y = foo y x
