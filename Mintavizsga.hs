module Mintavizsga where

    import Data.Char

    --1 feladat
    splitQuadraple :: (a,b,c,d) -> ((a,b),(c,d))
    splitQuadraple (a,b,c,d) = ((a,b), (c,d))



    --2 feladat
    dist1 :: Num a => a -> a -> a
    dist1 a b = abs $ (a - b)

    --3 feladt
    kroeneckerDelta :: Eq a => a -> a -> Int
    kroeneckerDelta a b
        | a == b = 1
        | otherwise = 0

    -- 4 feladat
    freq :: Eq a => a -> [a] -> Int
    freq a [] = 0
    freq a (x:xs)
        | a == x = 1 + freq a (xs)
        | otherwise = 0 + freq  a (xs) 

    --5 feladat
    hasUpperCase :: String -> Bool
    hasUpperCase [] = False
    hasUpperCase (x:xs)
        | not (isUpper x ) = False || hasUpperCase xs
        | otherwise = True

   

    --6 feladat
    identifier :: String -> Bool
    identifier [x] = isAlpha x
    identifier (x:xs) = isAlpha x && seged xs where
        seged :: String -> Bool
        seged [] = True
        seged (x:xs) = (isAlphaNum x || x == '_') && seged xs
    identifier _ = False
    --7 feladat
    replace :: Int -> a -> [a] -> [a]
    replace a b [] = [b]
    replace 0  a (x:xs) = (a:xs)
    replace y a (x:xs)
        | y < 0 = (a:x:xs)
        | length(x:xs) <= y = (x:xs) ++ [a]
        | otherwise = x : replace (y-1) a (xs)
   --9 feladat
    safeDiv :: Int -> Int -> Maybe Int
    safeDiv x 0 = Nothing
    safeDiv x y = Just (x `div` y) 

    
    selectUnless :: (t -> Bool) -> (t -> Bool) -> [t] -> [t]
    selectUnless f g [] = []
    selectUnless f g (x:xs)
        | (f x) && not (g x) = [x] ++ selectUnless f g xs
        | otherwise = selectUnless f g xs

    w :: (a -> a -> a) -> a -> a
    w f a = (f a a)