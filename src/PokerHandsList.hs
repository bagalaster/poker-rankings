module PokerHandsList where

-- import Data.List
import Prelude hiding (insert, lookup)
import Data.Maybe
import Data.Map as Map
import PokerCards

type HoleCards = (Card, Card)
type EquivalenceClass = (HoleCards, HoleCards)

handPairEquiv :: EquivalenceClass -> EquivalenceClass -> Bool
handPairEquiv p1 p2 = aux [fst h1, snd h1, fst h2, snd h2] [fst h3, snd h3, fst h4, snd h4] empty where
    h1 = fst p1; h2 = snd p1; h3 = fst p2; h4 = snd p2
    aux :: [Card] -> [Card] -> Map Suit Suit -> Bool
    aux [] [] _ = True
    aux (c:cs) (d:ds) m
        | member (suit c) m = if suit d == (fromJust . lookup (suit c)) m then aux cs ds m else False
        | otherwise = aux cs ds $ insert (suit c) (suit d) m 

equivClass :: HoleCards -> HoleCards -> EquivalenceClass
equivClass h1 h2 = equivClassFrom $ aux [fst h1', snd h1', fst h2', snd h2'] Clubs empty
    where
        equivClassFrom :: [Card] -> EquivalenceClass
        equivClassFrom (a:b:c:d:[]) = ((a, b), (c, d))
        reorder :: HoleCards -> HoleCards
        reorder h = if value (fst h) < value (snd h) then (snd h, fst h) else h
        h1' = reorder h1; h2' = reorder h2
        aux :: [Card] -> Suit -> Map Suit Suit -> [Card]
        aux [] _ _ = []
        aux (x:xs) s m
            | member (suit x) m = (Card (value x) $ fromJust $ lookup (suit x) m) : (aux xs s m)
            | otherwise = (Card (value x) s) : (aux xs (succ s) (insert (suit x) s m))