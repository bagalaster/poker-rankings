module PokerEquivClassSizes where

import Data.List.Unique
import Data.List
import qualified Data.Set as Set

import PokerCards
import PokerEvaluator
import PokerHandsList

computeEquivClassSize :: EquivalenceClass -> Int
computeEquivClassSize h =
    case h1 signature of
        (ValueA, ValueA, False) ->
            case h2 signature of
                (ValueA, ValueA, False) -> 6
                (ValueB, ValueB, False) ->
                    case nSuits of
                        0 -> 6
                        1 -> 24
                        2 -> 6
                (ValueA, ValueB, True) -> 12
                (ValueA, ValueB, False) -> 
                    case nSuits of
                        0 -> 12
                        1 -> 24
                (ValueB, ValueC, True) -> 12
                (ValueB, ValueC, False) ->
                    case nSuits of
                        0 -> 12
                        1 -> 24
                        2 -> 12
        (ValueA, ValueB, True) ->
            case h2 signature of
                (ValueB, ValueB, False) -> 12
                (ValueC, ValueC, False) -> 12
                (ValueA, ValueB, True) -> 12
                (ValueA, ValueB, False) -> 24
                (ValueA, ValueC, True) -> 12
                (ValueA, ValueC, False) -> 
                    case nSuits of
                        0 -> 24
                        1 -> 12
                (ValueB, ValueC, True) -> 12
                (ValueB, ValueC, False) ->
                    case nSuits of
                        0 -> 24
                        1 -> 12
                (ValueC, ValueD, True) ->
                    case nSuits of
                        0 -> 12
                        1 -> 4
                (ValueC, ValueD, False) ->
                    case nSuits of
                        0 -> 24
                        1 -> 12
        (ValueA, ValueB, False) -> 
            case h2 signature of
                (ValueB, ValueB, False) -> 
                    case nSuits of
                        0 -> 12
                        1 -> 24
                (ValueC, ValueC, False) ->
                    case nSuits of
                        0 -> 12
                        1 -> 24
                        2 -> 12
                (ValueA, ValueB, False) ->
                    case nSuits of
                        0 -> 24
                        1 -> 24
                        2 -> 12
                (ValueA, ValueC, True) ->
                    case nSuits of
                        0 -> 24
                        1 -> 12
                (ValueA, ValueC, False) ->
                    case nSuits of
                        0 -> 24
                        1 -> 24
                        2 -> 12
                (ValueB, ValueC, True) ->
                    case nSuits of
                        0 -> 24
                        1 -> 12
                (ValueB, ValueC, False) ->
                    case nSuits of
                        0 -> 24
                        1 -> 24
                        2 -> 12
                (ValueC, ValueD, True) -> 
                    case nSuits of
                        0 -> 24
                        1 -> 12
                (ValueC, ValueD, False) ->
                    case nSuits of
                        0 -> 24
                        1 -> 24
                        2 -> 12
    where
        c1 = (fst . fst) h; c2 = (snd . fst) h; c3 = (fst . snd) h; c4 = (snd . snd) h
        v1 = value c1; v2 = value c2; v3 = value c3; v4 = value c4
        s1 = suit c1; s2 = suit c2; s3 = suit c3; s4 = suit c4
        signature = handSignature h
        nSuits = numSuitsInCommon h
    
equivClassSizes :: [(EquivalenceClass, Int)]
equivClassSizes = map (\ec -> (ec, computeEquivClassSize ec)) $ Set.toList allEquivClasses