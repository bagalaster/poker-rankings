module PokerWinProbabilities where

import Data.List.Unique
import Data.List
import qualified Data.Set as Set
import Control.Parallel
import Control.Parallel.Strategies

import PokerCards
import PokerEvaluator
import PokerHandsList

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

computeOutcomeDistribution :: EquivalenceClass -> Int -> Int -> (Float, Float, Float)
computeOutcomeDistribution ec seed n = meanResults $ pSimulateHands ec [(n * seed) .. ((n + 1) * seed - 1)]

outcomeDistributions :: [Int] -> Int -> [(EquivalenceClass, (Float, Float, Float))]
outcomeDistributions seeds n = map (\(seed, ec) -> (ec, computeOutcomeDistribution ec seed n)) (zip seeds (Set.toList allEquivClasses))

