module Homework12 where

    import Data.Char
    import Data.List

    --1 feladat
    whichOne :: [Char] -> Char -> Bool
    whichOne [] y = False
    whichOne (x:xs) y
        | x /= y = False || whichOne xs y
        | otherwise = True


    which :: ([Char], [Char], [Char]) -> Char -> Int
    which (x,y,z) a
        | whichOne x a = 1
        | whichOne y a = 2
        | whichOne z a = 3
        | otherwise = 0 

    --2 feladat
    matches :: (Int, Int) -> (Int, Int) -> Bool
    matches (a, b) (c, d)
        | b == c = True
        | otherwise = False

    --3 feladat
    toUpperCase :: String -> String
    toUpperCase [] = []
    toUpperCase (x:xs) = toUpper (x) : xs

    --4 feladat
    swap :: Maybe a -> b -> Maybe b
    swap Nothing _ = Nothing
    swap (Just a) b = Just b

    --5 feladat

    numeric :: String -> Int
    numeric [] = 0
    numeric (x:xs)
        | x == 'r' = 4 + numeric(xs)
        | x == 'w' = 2 + numeric(xs)
        | x == 'x' = 1 + numeric(xs)
        | otherwise = 0


    --6 feladat
    pythagoreans :: [(Int, Int, Int)]
    pythagoreans = [(a, b, c) | a <- [1..100], b <- [a..100], c <-[b..100], (a^2) + (b^2) == (c^2)]


    --7 feladat

    hasLongWord :: Int -> String -> Bool
    hasLongWord _ [] = False
    hasLongWord x (y:ys)
        | length  (head (words (y:ys))) >= x = True
        | otherwise = False || (hasLongWord x (drop(length(head(words (y:ys)))) (y:ys)))

    --8 feladat
    align :: Int -> String -> String
    align y (x:xs)
        | length (x:xs) >= y = (x:xs)
        | otherwise = " " ++ align (y-1) (x:xs)

    --9 feladat

    fromJust :: Maybe a -> a
    fromJust Nothing = error "hibas"
    fromJust (Just a) =  a

    isJust :: Maybe a -> Bool
    isJust Nothing = False
    isJust a = True  


    modify :: (a -> Maybe a) -> [a] -> [a]
    modify f [] = []
    modify f (x:xs) 
        | isJust(f x)  = (fromJust(f x):xs)
        | otherwise = xs


    --10 feladat
    isLonger :: [a] -> Int -> Bool
    isLonger [] x = False
    isLonger x 0 = True
    isLonger (x:xs) y = isLonger xs (y-1) 
 
    --11 feladat
    removeOne :: Char -> Char
    removeOne x
        | '\233' == x = 'e'
        | '\337' == x = 'o'
        | '\225' == x = 'a'
        | '\237' == x = 'i'
        | '\252' == x = 'u'
        | '\243' == x = 'o'
        | '\369' == x = 'u'
        | '\250' == x = 'u'
        | '\246' == x = 'o'
        | otherwise = x


    removeAccents :: String -> String
    removeAccents [] = []
    removeAccents (x:xs) = removeOne(x) : removeAccents (xs)

    --12 feladat
    strip :: String -> String
    strip [] = []
    strip (x:xs)
        | x == '_' = strip xs
        | last (x:xs) =='_' = strip (init (x:xs))
        | otherwise = (x:xs)

    --13 feladat

    data RPS = Rock | Paper | Scissors deriving (Eq)

    beats :: RPS -> RPS
    beats x
        | Paper == x  = Rock
        | Rock == x = Scissors
        | Scissors == x = Paper 

    --14 feladat
    firstBeats :: [RPS] -> [RPS] -> Int
    firstBeats [] [] = 0
    firstBeats (x:xs) (y:ys)
        | beats x == y = 1 + firstBeats xs ys
        | otherwise = 0 + firstBeats xs ys


    --15 feladat
    
    data Temperature = Daytime Int | Night Int 

    isDaytime :: Temperature -> Bool
    isDaytime (Daytime x) = True 
    isDaytime (Night x) = False
      
    --16 feladat

    fromTemperature :: Temperature -> Int
    fromTemperature (Night x) = x
    fromTemperature (Daytime x) = x

    daytimeList :: [Temperature] -> [Int]
    daytimeList [] = []
    daytimeList (x:xs) 
        | isDaytime (x) = fromTemperature (x) : daytimeList (xs)
        | otherwise =  daytimeList (xs)
    
    nightList :: [Temperature] -> [Int]
    nightList [] = []
    nightList (x:xs) 
        | not (isDaytime (x)) = fromTemperature (x) : nightList (xs)
        | otherwise =  nightList (xs)
    


    extremes :: [Temperature] -> (Int, Int)
    extremes (x:xs) = (maximum (daytimeList (x:xs) ) , minimum (nightList (x:xs)))