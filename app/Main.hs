module Main where

import qualified Data.Set as Set
import Data.List.Unique
import Data.List (intercalate)

import PokerCards
import PokerEvaluator
import PokerHandsList
 
simulateHand :: Card -> Card -> Card -> Card -> Int -> Ordering
simulateHand a1 a2 b1 b2 n = evaluate (a1 : a2 : board) (b1 : b2 : board)
    where
        board = take 5 $ newDeckWithOut n [a1, a2, b1, b2]

sumResults :: [Ordering] -> (Int, Int, Int)
sumResults results = aux results (0, 0, 0)
    where
        aux :: [Ordering] -> (Int, Int, Int) -> (Int, Int, Int)
        aux [] (w, t, l) = (w, t, l)
        aux (r:rs) (w, t, l)
            | r == GT = aux rs (w + 1, t, l)
            | r == LT = aux rs (w, t, l + 1)
            | otherwise = aux rs (w, t + 1, l)
    
means :: (Int, Int, Int) -> (Float, Float, Float)
means (w, t, l) = (w' / s, t' / s, l' / s)
    where 
        w' = fromIntegral w :: Float
        t' = fromIntegral t :: Float
        l' = fromIntegral l :: Float
        s = w' + t' + l'

meanResults :: [Ordering] -> (Float, Float, Float)
meanResults = means . sumResults

main :: IO ()
main = do
    -- should be 47008
    putStrLn $ show $ Set.size allEquivClasses