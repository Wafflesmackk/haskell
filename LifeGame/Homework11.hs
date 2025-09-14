--Daka Bence I5RQFI
module Homework11 where
    import Data.Char
    import Data.List

    --1 feladat
    contains :: [Char] -> Char -> Bool
    contains [] _ = False
    contains (x:xs) a 
        | x == a = True
        | otherwise = False || contains xs a

    which :: ([Char], [Char], [Char]) -> Char -> Int
    which (a, b, c) x
        | contains a x = 1
        | contains b x = 2
        | contains c x = 3
        | otherwise = 0

    --2 feladat
    matches :: (Int, Int) -> (Int, Int) -> Bool
    matches (a,b) (x,y) = x == b

    --3 feladat
    toUpperCase :: String -> String
    toUpperCase [] = []
    toUpperCase (x:xs)
        | isLetter(x) && isLower(x) = (toUpper(x) : xs)
        | otherwise = (x:xs)

    --4 feladat
    swap :: Maybe a -> b -> Maybe b
    swap Nothing _ = Nothing
    swap (Just a) b = (Just b)

    --5 feladat
    value :: Char -> Int
    value x
        | x == 'x' = 1
        | x == 'w' = 2
        | x == 'r' = 4
        | otherwise = 0

    numeric :: String -> Int
    numeric [] = 0
    numeric (x:xs) = (value x) + numeric xs 

    --6 feladat
    pythagoreans :: [(Int, Int, Int)]
    pythagoreans = [(a, b, c) | a <- [1..100], b <- [a..100], c <-[1..100], (a * a) + (b * b) == (c * c)]

    -- 7 feladat
    maxLength :: [String] -> Int
    maxLength [] = 0
    maxLength (x:xs)
        | (length x) > maxLength (xs) = length x
        | otherwise = maxLength xs


    hasLongWord :: Int -> String -> Bool
    hasLongWord _ [] = False
    hasLongWord a x
        | a <= 0 = False
        | otherwise = (maxLength (words x) >= a)


    -- 8 feladat

    spaceGenerator :: Int -> String
    spaceGenerator 0 = []
    spaceGenerator a = " " ++ spaceGenerator (a-1) 


    align :: Int -> String -> String
    align a x
        | a > (length x) = spaceGenerator (a-(length x)) ++ x
        | otherwise = x

    --9 feladat
    fromJust :: Maybe a -> a
    fromJust Nothing = error "its Nothing"
    fromJust (Just a) =  a

    isNothing :: Maybe a -> Bool
    isNothing Nothing = True
    isNothing (Just a) = False


    modify :: (a -> Maybe a) -> [a] -> [a]
    modify f [] = []
    modify f (x:xs)
        | isNothing (f(x)) = xs
        | otherwise = (fromJust(f x):xs)

    --10 feladat
    isLonger :: [a] -> Int -> Bool
    isLonger [] _ = False
    isLonger (x:xs) a
        | a > 0 = False || isLonger xs (a-1)
        | otherwise = True

    --11 feladat
    isAccent :: Char -> Bool
    isAccent x =  'á' == x  ||  'é' == x || 'í' == x  ||'ó' == x  ||  'ö' == x || 'ő' == x || 'ú' == x || 'ü' == x ||'ű' == x


    changeAccent :: Char -> Char
    changeAccent x
        | 'á' == x = 'a'
        | 'é' == x = 'e'
        | 'í' == x = 'i'
        | 'ó' == x = 'o'
        | 'ö' == x = 'o'
        | 'ő' == x = 'o'
        | 'ú' == x = 'u'
        | 'ü' == x = 'u'
        | 'ű' == x = 'u'
        | otherwise = x

    removeAccents :: String -> String
    removeAccents [] = []
    removeAccents (x:xs)
        | isAccent (x) = (changeAccent x) : removeAccents xs
        | otherwise = x : removeAccents xs

    --12 feladat
    removeFromEnd :: String -> String
    removeFromEnd [] = []
    removeFromEnd x
        | last x == '_' = removeFromEnd (init x)
        | otherwise = x

    strip :: String -> String
    strip [] = []
    strip (x:xs)
        | x == '_' = strip xs
        | otherwise = removeFromEnd (x:xs)

    --13 feladat
    data RPS = Rock | Paper | Scissors deriving (Eq)

    beats :: RPS -> RPS
    beats x
        | x == Paper  = Rock
        | x == Rock = Scissors
        | x == Scissors = Paper 

    --14 feladat
    firstBeats :: [RPS] -> [RPS] -> Int
    firstBeats [] [] = 0
    firstBeats (x:xs) (y:ys)
        | beats x  == y = 1 + firstBeats xs ys
        | otherwise = 0 + firstBeats xs ys 

    --15 feladat
    data Temperature = Daytime Int | Night Int

    isDaytime :: Temperature -> Bool
    isDaytime (Daytime a) = True
    isDaytime (Night a) = False

    --16 feladat
    getValue :: Temperature -> Int
    getValue (Daytime a) = a
    getValue (Night a) = a

    nightList :: [Temperature] ->  [Int]
    nightList [] = []
    nightList (x:xs)
        | not (isDaytime x) = getValue x : nightList xs
        | otherwise = nightList xs

    minNight :: [Temperature] -> Int
    minNight [] = 0
    minNight x = minimum(nightList x)

    dayList :: [Temperature] ->  [Int]
    dayList [] = []
    dayList (x:xs)
        | isDaytime x = getValue x : dayList xs
        | otherwise = dayList xs


    maxDay :: [Temperature] -> Int
    maxDay [] = 0
    maxDay x = maximum(dayList x)




    extremes :: [Temperature] -> (Int, Int)
    extremes x = (maxDay x, minNight x)