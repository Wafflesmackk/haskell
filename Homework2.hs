--Daka Bence Neptun: I5RQFI
module Homework2 where

    -- 1 feladat
    isSmallPrime :: Int -> Bool
    isSmallPrime 2 = True
    isSmallPrime 3 = True
    isSmallPrime 5 = True
    isSmallPrime 7 = True
    isSmallPrime _ = False

    -- 2 feladat
    equivalent :: Bool -> Bool -> Bool
    equivalent True True = True
    equivalent False False = True
    equivalent _ _ = False

    --3 feladat
    implies :: Bool -> Bool -> Bool
    implies True False = False
    implies _ _ = True

    --4 feladat
    invertO :: (Int, Int) -> (Int, Int)
    invertO (x, y) = (-x, -y)

    --5 feladat
    isOnNegId :: (Int, Int) -> Bool
    isOnNegId (x, y) = x == -y

    --6 feladat
    distance :: (Int, Int) -> (Int, Int) -> Double
    distance (x, y) (a, b) = sqrt(fromIntegral((a - x)^2 + (b - y)^2))

    --7 feladat
    add :: (Int, Int) -> (Int, Int) -> (Int, Int)
    add (x, y) (a, b) = ((x * b) + (a * y), y * b)

    --8 feladat
    multiply :: (Int, Int) -> (Int, Int) -> (Int, Int)
    multiply (x, y) (a, b) = (x * a, y * b)

    --9 feladat
    divide :: (Int, Int) -> (Int, Int) -> (Int, Int)
    divide (x, y) (a, b) = (x * b, y * a)

    g :: (Int, (Double, Bool)) -> Int
    g (a, b) = a