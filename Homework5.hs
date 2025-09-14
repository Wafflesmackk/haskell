-- Daka Bence Neptun: I5RQFI
module Homework5 where

    --1 feladat
    sumFirst :: Int -> [Int] -> Int
    sumFirst _ [] = 0
    sumFirst 0 _ = 0
    sumFirst a (x:xs) =  x + sumFirst(a-1) xs

    --2 feladat
    everySecond :: [a] -> [a]
    everySecond [] = []
    everySecond (x:xs) = x : masodik(xs)
        where  masodik :: [a] -> [a]
               masodik [] = []
               masodik (x:xs) = everySecond(xs)
   
    
    -- 3 feladat
    howMany :: Char -> String -> Int
    howMany _ "" = 0
    howMany a (x:xs)
        | a /= x = howMany a xs
        | otherwise = howMany a xs +1


    --4 feladat 
    letterize :: String -> [String]
    letterize "" = []
    letterize (x:xs) = [x] : letterize xs

    --5 feladat
    swapElems :: [[a]] -> [[a]]
    swapElems [[]] = [[]] 
    swapElems [] = []
    swapElems (x:xs) = csere x : swapElems xs
        where csere :: [a] -> [a]
              csere [] = []
              csere [x] = [x]
              csere (x:y:xs) = (y:x:xs)

    --6 feladat
    insertAt :: Int -> a -> [a] -> [a]
    insertAt a x [] = [x]
    insertAt a b (x:xs) = (vege b (take (a) (x:xs))) ++ (drop (a) (x:xs))
        where  vege a [] = [a]
               vege a (x:xs) = x : vege a xs