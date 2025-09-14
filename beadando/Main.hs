module Main where

import TicTacToe
import Data.List
import Text.Read
import Control.Monad
import System.IO

instance Show Player where
    show X = "X"
    show O = "O"

instance Show Cell where
    show E = " "
    show (P p) = show p

instance Show State where
    show Running = "Next player is: "
    show (GameOver p)
        | Just r <- p = "The winner is: " ++ show r 
        | otherwise   = "It's a tie!"

showBoard :: Board -> String
showBoard = intercalate "\n" . map ((\x -> '|':intercalate "|" x ++ "|") . map show)

showGame :: Game -> String
showGame (_,b,p,s@Running) = intercalate "\n" [showBoard b, show s ++ show p]
showGame (_,b,_,s) = intercalate "\n" [showBoard b, show s]

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn e [] = []
splitOn e l = splitOn1 e l
    where
        splitOn1 e [] = [[]]
        splitOn1 e (x : xs)
            | e == x = [] : splitOn1 e xs
            | otherwise = (x : y) : ys
            where
                (y:ys) = splitOn1 e xs

iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> m a -> m a
iterateUntilM p f mv = do
    v <- mv
    if p v then return v else iterateUntilM p f (f v)

putStr' :: String -> IO ()
putStr' s = putStr s >> hFlush stdout

prompt :: String -> IO String
prompt s = putStr' s >> getLine

type PredicateWrongMessage = String
type ErrorMessage          = String
type PromptMessage         = String

promptUntilCorrect :: Read a 
                   => PromptMessage 
                   -> (a -> Bool) 
                   -> PredicateWrongMessage 
                   -> ErrorMessage 
                   -> IO a
promptUntilCorrect pm p pwm em =
    prompt pm >>= \s ->
        case readMaybe s of
            Just x | p x -> return x
            Just _       -> reportError pwm
            _            -> reportError em
    where
        reportError s = putStrLn s >> promptUntilCorrect pm p pwm em

main :: IO ()
main = do
    size <- promptUntilCorrect "Size of board: " (> 0) "Size must be positive!" "Input is not a number! Try again!" :: IO Int
    iterateUntilM isGameOver (\game -> do
        move <- getLine
        let step = (map readMaybe $ splitOn ' ' move) :: [Maybe Int]
        case step of
            [Just a, Just b] -> do 
                    let next = playerTurn game (a,b)
                    putStrLn $ showGame next
                    return next
            _ -> do
                putStrLn "Invalid input format!"
                putStrLn $ showGame game
                return game
        ) (liftM2 (>>) (putStrLn . showGame) return (initGame size))
    return ()