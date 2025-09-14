module Homework2 where

-- 1 feladat

cmpRem5Rem7 :: Int -> Bool
cmpRem5Rem7 x = x `rem` 5 > x `rem` 7

--2 feladat

foo :: Int -> Bool -> Bool
foo x y = x == 3 || y == True

bar :: Bool -> Int -> Bool 
bar x y = foo y x

-- 4 feladat
isOnNegId :: Int -> Int -> Bool
isOnNegId x y = -x - y == 0 

--5 feladat
modulo3 :: Int -> Int
modulo3 x = x `mod` 3

--6 feladat
modulo3div :: Int -> Int -> Int
modulo3div x y = modulo3(x `div` y)
