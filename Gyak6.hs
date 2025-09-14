module Gyak6 where
pita :: Int -> [(Int,Int,Int)]
pita n = [ (a,b,c) | a <-[1..n], b <- [a..n], c <- [b..n], c^2 == b^2 + a^2 ]