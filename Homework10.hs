-- Daka Bence Neptun: I5RQFI
module Homework10 where
    --1 feladat
    safeTail :: [a] -> Maybe [a]
    safeTail [] = Nothing
    safeTail (x:xs) = Just (xs)

    --2 feladat
    prod' :: [Int] -> Int
    prod' [] = 1
    prod' (x:xs) = x * prod' (xs)

    --3 feladat
    isJust :: Maybe a -> Bool
    isJust Nothing = False
    isJust a = True  

    --4 feladat
    fromJust :: Maybe a -> a
    fromJust Nothing = error "hibas"
    fromJust (Just a) =  a

    --5 feladat
    catMaybes :: [Maybe a] -> [a]
    catMaybes [] = []
    catMaybes ((Nothing): xs) = catMaybes(xs)
    catMaybes ((Just x): xs) = x : catMaybes(xs)
  
    --6 feladat
    mapMaybe :: (a -> Maybe b) -> [a] -> [b]
    mapMaybe f [] = [] 
    mapMaybe f (x:xs) = catMaybes [f x] ++ mapMaybe f xs

    --7 feladat
    type Username = String
    type Password = String

    data Privilege = Simple | Admin deriving (Eq, Show)

    data Cookie = LoggedOut | LoggedIn Username Privilege deriving (Eq, Show)

    data Entry = Entry Password Privilege [Username] deriving (Eq, Show)

    type Database = [(Username, Entry)]

    richard, charlie, carol, david, kate :: (Username, Entry)
    richard = ("Richard", Entry "password1" Admin  ["Kate"])
    charlie = ("Charlie", Entry "password2" Simple ["Carol"])
    carol   = ("Carol",   Entry "password3" Simple ["David", "Charlie"])
    david   = ("David",   Entry "password4" Simple ["Carol"])
    kate    = ("Kate",    Entry "password5" Simple ["Richard"])

    testDB :: Database
    testDB = [ richard, charlie, carol, david, kate ]

    testDBWithoutCarol :: Database
    testDBWithoutCarol = [ ("Richard", Entry "password1" Admin  ["Kate"]), ("Charlie", Entry "password2" Simple []), ("David",   Entry "password4" Simple []), ("Kate", Entry "password5" Simple ["Richard"])]

    --8 feladat
    password :: Entry -> Password
    password (Entry pas pr us) = pas

    --9 feladat
    privilege :: Entry -> Privilege
    privilege (Entry pas pr us) = pr

    --10 feladat
    friends :: Entry -> [Username]
    friends (Entry pas pr us) = us

    --11 feladat
    mkCookie :: Username -> Password -> Entry -> Cookie
    mkCookie us curr ent
        | curr == (password ent) = (LoggedIn us (privilege ent))
        | otherwise = LoggedOut

    --12 feladat
    login :: Username -> Password -> Database -> Cookie
    login us pas dat
        |lookup us dat /= Nothing = mkCookie us pas (fromJust(lookup (us) dat))
        |otherwise = LoggedOut
   
    --13 feladat
    helper :: Username -> [Username] -> [Username]
    helper us [] = []
    helper us (x:xs)
        | us == x = [] ++ helper us xs
        | otherwise = x : helper us xs

    updateEntry :: Username -> (Username, Entry) -> Maybe (Username, Entry)
    updateEntry us (use, (Entry pas pri fr))
        | us == use = Nothing
        | otherwise = Just (use, (Entry pas pri (helper us fr) ))


    --14 feladat
    deleteUser :: Cookie -> Username -> Database -> Database
    deleteUser LoggedOut delus dat = dat
    deleteUser (LoggedIn us pri) delus dat
        | pri == Admin = mapMaybe (updateEntry delus)  dat 
        | otherwise = dat


    --11.1 feladat
    getFriends :: Username -> Database -> [Username]
    getFriends us dat 
        | lookup (us) dat == Nothing = []
        |otherwise = friends (fromJust(lookup (us) dat))

    --11.2 feladat
    getFriendsRefl :: Username -> Database -> [Username]
    getFriendsRefl us dat
            |lookup (us) dat == Nothing = []
            |otherwise= [us] ++ (getFriends us dat) 

    --11.3 feladat
    fixPoint :: Eq a => (a -> a) -> a -> a
    fixPoint f a
        | a == f(a) && f (f(a)) == f(a) = f(a)
        | otherwise = fixPoint f (f(a))

    --11.4 feladat
    sort :: (Ord a) => [a] -> [a]
    sort [] = []
    sort (x:xs) = sort [y | y <- xs, y <= x] ++ [x] ++ sort [y | y <- xs, y > x]

    sortUnique :: (Eq a, Ord a) => [a] -> [a]
    sortUnique [] = []
    sortUnique (x:xs)
        | xs == [] = x : []
        | x > head (sort (xs)) = sortUnique (sort(x:xs))
        | x == head (xs) = sortUnique (sort (xs))
        | otherwise = x : sortUnique (sort(xs))


