module Main where

import Data.List
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BSL
import Data.Csv

import PokerCards
import PokerEvaluator
import PokerHandsList
import PokerWinProbabilities

data OutcomeRecord = OutcomeRecord (EquivalenceClass, (Float, Float, Float))

instance ToRecord OutcomeRecord where
    toRecord (OutcomeRecord (ec, (w, t, l))) = record [toField (show ec), toField w, toField t, toField l]

kNumSimulations :: Int
kNumSimulations = 10000

kSeed :: Int
kSeed = 42

kOutFile :: [Char]
kOutFile = "data/outcome-distributions.csv"

main :: IO ()
main = do
    BSL.writeFile kOutFile $ encode $ map OutcomeRecord $ outcomeDistributions [kSeed .. ] kNumSimulations
--    putStrLn $ show $ take 40 $ outcomeDistributions [kSeed ..] kNumSimulations