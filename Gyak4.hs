module Gyak4 where

--["abc"] = [['a','b','c']]
--"abc" = ['a','b','c']

-- (True || (True && False)) /= ((True || True) && False)
--(a,b) -> (b,a)
--swao (1,0) --> (0,1)

-- []
-- [a]
-- (x:xs)
-- (_:_)

head' :: [a] -> a
head' (x:xs) = x

-- [5,6,7,8,4,56]
-- x 	xs

tail' :: [a] -> [a]
tail' (a:b) = b