module Main where

import Control.Monad
import Data.List
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BSL
import Data.Csv

import PokerCards
import PokerEvaluator
import PokerHandsList
import PokerWinProbabilities
import PokerEquivClassSizes

data OutcomeRecord = OutcomeRecord (EquivalenceClass, (Float, Float, Float))
instance ToRecord OutcomeRecord where
    toRecord (OutcomeRecord (ec, (w, t, l))) = record [toField (show ec), toField w, toField t, toField l]

data SizeRecord = SizeRecord (EquivalenceClass, Int)
instance ToRecord SizeRecord where
    toRecord (SizeRecord (ec, n))= record [toField (show ec), toField n]

kNumSimulations :: Int
kNumSimulations = 100000

kSeed :: Int
kSeed = 42

kWinProbabilitiesOutFile :: [Char]
kWinProbabilitiesOutFile = "data/outcome-distributions.csv"

kWriteWinProbabilitiesFlag :: Bool
kWriteWinProbabilitiesFlag = True

writeWinProbabilities :: IO()
writeWinProbabilities = BSL.writeFile kWinProbabilitiesOutFile $ encode $ map OutcomeRecord $ outcomeDistributions [kSeed .. ] kNumSimulations

kSizesOutFile :: [Char]
kSizesOutFile = "data/equiv-class-sizes.csv"

kWriteSizesFlag :: Bool
kWriteSizesFlag = True

writeSizes :: IO()
writeSizes = BSL.writeFile  kSizesOutFile $ encode $ map SizeRecord $ equivClassSizes

main :: IO ()
main = do
    putStrLn "Writing sizes"
    when kWriteSizesFlag writeSizes
    putStrLn "Finished writing sizes"
    putStrLn "Writing win probabilities"
    when kWriteWinProbabilitiesFlag writeWinProbabilities
    putStrLn "Finished writing win probabilities"

    