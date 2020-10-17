module Main where

import qualified Data.Set as Set
import Data.List.Unique
import Data.List
import Control.Parallel
import Control.Parallel.Strategies

import PokerCards
import PokerEvaluator
import PokerHandsList

kNumSimulations :: Int
kNumSimulations = 1000000

kCard1 = Card Ace Spades
kCard2 = Card King Spades
kCard3 = Card Eight Spades
kCard4 = Card Eight Hearts

simulateHand :: EquivalenceClass -> Int -> Ordering
simulateHand ec n = evaluate (a1 : a2 : board) (b1 : b2 : board)
    where
        board = take 5 $ newDeckWithOut n [a1, a2, b1, b2]
        a1 = (fst . fst) ec; a2 = (snd . fst) ec; b1 = (fst . snd) ec; b2 = (snd . snd) ec

pSimulateHands :: EquivalenceClass -> [Int] -> [Ordering]
pSimulateHands ec ns = parMap rpar (simulateHand ec) ns

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
    let ec = equivClass (kCard1, kCard2) (kCard3, kCard4)
    putStrLn $ show $ meanResults $ pSimulateHands ec [1..kNumSimulations]