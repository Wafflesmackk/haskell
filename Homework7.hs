-- Daka Bence Neptun: I5RQFI
module Homework7 where

   --1 feladat
    type PlantName = String
    basil :: PlantName
    basil = "Brownie"
    
    --2 felada
    newtype PlantWater = PW (PlantName, Int)

    waterBasil :: PlantWater
    waterBasil = PW (basil, 3)