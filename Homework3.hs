--Daka Bence 	Neptun: I5RQFI
module Homework3 where

-- 1 feladat
isSmallPrime :: Int -> Bool
isSmallPrime 2 = True
isSmallPrime 3 = True
isSmallPrime 5 = True
isSmallPrime 7 = True
isSmallPrime _ = False

--2 feladat
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
invertO (x,y) = ((-x),(-y))

--5 feladat 
add :: (Int, Int) -> (Int, Int) -> (Int, Int) 
add (a, b) (c, d) = (a * d + c * b , b * d)

--6 feladat
multiply :: (Int, Int) -> (Int, Int) -> (Int, Int)
multiply (a, b) (c, d) = (a * c , b * d)

--7.1 feladat
divide :: (Int, Int) -> (Int, Int) -> (Int, Int)
divide (a, b) (c, d) = (a * d , b * c)

--7.2 feladat
putIntoList :: a -> [a]
putIntoList a = [a]

-- 8 feladat
interval :: Int -> Int -> [Int]
interval a b = [a .. b]


