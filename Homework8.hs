-- Daka Bence Neptun: I5RQFI
module Homework8 where

   --1 feladat
    type PlantName = String
    basil :: PlantName
    basil = "Brownie"
    
    --2 feladat
    newtype PlantWater = PW (PlantName, Int) deriving (Eq, Show)

    waterBasil :: PlantWater
    waterBasil = PW (basil, 3)

    --3 feladat
   
    steveeGarden :: [PlantWater]
    steveeGarden = [PW ("Brownie",1), PW ("Olga",3), PW ("Peter",2), PW ("Claus",4), PW ("Sunny",6)]

    --4 feladat
    newtype Garden = G ([PlantWater]) deriving (Eq, Show)

    --5 feladat
    gardener :: [PlantWater] -> Garden
    gardener a = G (a)

    --6 feladat
    summ :: Garden -> Float
    summ  (G [])  = 0
    summ (G (PW (x,y):xs)) = fromIntegral (y) + summ (G xs)

    len :: Garden -> Float
    len (G []) = 0
    len (G (PW (x,y):xs)) = 1 + len (G xs)

    avgWater :: Garden -> Float
    avgWater (G (PW (x,y):xs)) = summ (G (PW (x,y):xs)) / len (G (PW (x,y):xs))
   
