module PokerHandsList where

import Prelude hiding (insert, lookup)
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import PokerCards

type HoleCards = (Card, Card)
type EquivalenceClass = (HoleCards, HoleCards)

data ValueSignature = ValueA | ValueB | ValueC | ValueD deriving (Eq, Ord, Enum, Show)
data HandSignature = HandSignature {
    h1 :: (ValueSignature, ValueSignature, Bool),
    h2 :: (ValueSignature, ValueSignature, Bool)
} deriving (Eq, Show)
instance Ord HandSignature where
    compare (HandSignature (u1, u2, s1) (u3, u4, s2)) (HandSignature (v1, v2, t1) (v3, v4, t2))
        | u1 /= v1 = compare u1 v1
        | u2 /= v2 = compare u2 v2
        | s1 /= t1 = compare s1 t1
        | u3 /= v3 = compare u3 v3
        | u4 /= v4 = compare u4 v4
        | s2 /= t2 = compare s2 t2
        | otherwise = EQ

orderWithin :: (HoleCards, HoleCards) -> (HoleCards, HoleCards)
orderWithin ((c1, c2), (c3, c4)) = (a, b) where
    a = if c1 < c2 then (c2, c1) else (c1, c2)
    b = 
        if (value c3) `elem` map value [c1, c2] then 
            (c3, c4) 
        else if (value c4) `elem` map value [c1, c2] then
            (c4, c3)
        else if c3 < c4 then
            (c4, c3)
        else
            (c3, c4)

orderPairs :: (HoleCards, HoleCards) -> (HoleCards, HoleCards)
orderPairs ((c1, c2), (c3, c4))
    | max1 /= max2 = 
        if max1 > max2 then ((c1, c2), (c3, c4)) else ((c3, c4), (c1, c2))
    | min1 /= min2 =
        if min1 > min2 then ((c1, c2), (c3, c4)) else ((c3, c4), (c1, c2))
    | suit c1 /= suit c2 && suit c3 == suit c4 = ((c3, c4), (c1, c2))
    | otherwise = ((c1, c2), (c3, c4))
    where
        max1 = maximum [value c1, value c2]; max2 = maximum [value c3, value c4]
        min1 = minimum [value c1, value c2]; min2 = minimum [value c3, value c4]

numSuitsInCommon :: (HoleCards, HoleCards) -> Int
numSuitsInCommon ((c1, c2), (c3, c4))
    | sc1 == sc2 && sc3 == sc4 = if sc1 == sc3 then 1 else 0
    | sc1 == sc2 = (if sc1 == sc3 then 1 else 0) + (if sc1 == sc4 then 1 else 0)
    | otherwise = (if sc1 `elem` [sc3, sc4] then 1 else 0) + (if sc2 `elem` [sc3, sc4] then 1 else 0)
    where
        sc1 = suit c1
        sc2 = suit c2
        sc3 = suit c3
        sc4 = suit c4

handSignature :: (HoleCards, HoleCards) -> HandSignature
handSignature ((c1, c2), (c3, c4)) = HandSignature (v1, v2, s1) (v3, v4, s2) where
    s1 = suit c1 == suit c2; s2 = suit c3 == suit c4
    vc1 = value c1; vc2 = value c2; vc3 = value c3; vc4 = value c4
    v1 = ValueA
    v2 = if vc2 == vc1 then ValueA else ValueB
    v3 = 
        if vc3 == vc1 then v1
        else if vc3 == vc2 then v2
        else succ $ maximum $ [v1, v2]
    v4 =
        if vc4 == vc2 then v2
        else if vc4 == vc3 then v3
        else succ $ maximum $ [v2, v3]

modSuit :: (HoleCards, HoleCards) -> EquivalenceClass
modSuit h =
    case h1 signature of
        (ValueA, ValueA, False) ->
            case h2 signature of
                (ValueA, ValueA, False) -> ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Clubs))
                (ValueB, ValueB, False) ->
                    case nSuits of
                        0 -> ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Clubs))
                        1 -> ((Card v1 Spades, Card v2 Hearts), (Card v3 Spades, Card v4 Diamonds))
                        2 -> ((Card v1 Spades, Card v2 Hearts), (Card v3 Spades, Card v4 Hearts))
                (ValueA, ValueB, True) -> ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Diamonds))
                (ValueA, ValueB, False) -> 
                    if s2 == s4 then 
                        ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Hearts)) 
                    else 
                        ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Clubs))
                (ValueB, ValueC, True) ->
                    if s3 `elem` [s1, s2] then 
                        ((Card v1 Spades, Card v2 Hearts), (Card v3 Spades, Card v4 Spades)) 
                    else 
                        ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Diamonds))
                (ValueB, ValueC, False) ->
                    case nSuits of
                        0 -> ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Clubs))
                        1 -> 
                            if s1 == s3 then 
                                ((Card v1 Spades, Card v2 Hearts), (Card v3 Spades, Card v4 Diamonds)) 
                            else 
                                ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Hearts))
                        2 -> ((Card v1 Spades, Card v2 Hearts), (Card v3 Spades, Card v4 Hearts))
        (ValueA, ValueB, True) ->
            case h2 signature of
                (ValueB, ValueB, False) -> ((Card v1 Spades, Card v2 Spades), (Card v3 Hearts, Card v4 Diamonds))
                (ValueC, ValueC, False) -> 
                    if s1 `elem` [s3, s4] then
                        ((Card v1 Spades, Card v2 Spades), (Card v3 Spades, Card v4 Hearts))
                    else
                        ((Card v1 Spades, Card v2 Spades), (Card v3 Hearts, Card v4 Diamonds))
                (ValueA, ValueB, True) -> ((Card v1 Spades, Card v2 Spades), (Card v3 Hearts, Card v4 Hearts))
                (ValueA, ValueB, False) -> ((Card v1 Spades, Card v2 Spades), (Card v3 Hearts, Card v4 Diamonds))
                (ValueA, ValueC, True) -> ((Card v1 Spades, Card v2 Spades), (Card v3 Hearts, Card v4 Hearts))
                (ValueA, ValueC, False) -> 
                    if s2 == s4 then
                        ((Card v1 Spades, Card v2 Spades), (Card v3 Hearts, Card v4 Spades))
                    else
                        ((Card v1 Spades, Card v2 Spades), (Card v3 Hearts, Card v4 Diamonds))
                (ValueB, ValueC, True) -> ((Card v1 Spades, Card v2 Spades), (Card v3 Hearts, Card v4 Hearts))
                (ValueB, ValueC, False) ->
                    if s2 == s4 then
                        ((Card v1 Spades, Card v2 Spades), (Card v3 Hearts, Card v4 Spades))
                    else
                        ((Card v1 Spades, Card v2 Spades), (Card v3 Hearts, Card v4 Diamonds))
                (ValueC, ValueD, True) ->
                    if s1 == s3 then
                        ((Card v1 Spades, Card v2 Spades), (Card v3 Spades, Card v4 Spades))
                    else
                        ((Card v1 Spades, Card v2 Spades), (Card v3 Hearts, Card v4 Hearts))
                (ValueC, ValueD, False) ->
                    case nSuits of
                        0 -> ((Card v1 Spades, Card v2 Spades), (Card v3 Hearts, Card v4 Diamonds))
                        1 -> 
                            if s1 == s3 then 
                                ((Card v1 Spades, Card v2 Spades), (Card v3 Spades, Card v4 Hearts)) 
                            else 
                                ((Card v1 Spades, Card v2 Spades), (Card v3 Hearts, Card v4 Spades))
        (ValueA, ValueB, False) -> 
            case h2 signature of
                (ValueB, ValueB, False) -> 
                    if s1 `elem` [s3, s4] then
                        ((Card v1 Spades, Card v2 Hearts), (Card v3 Spades, Card v4 Diamonds))
                    else
                        ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Clubs))
                (ValueC, ValueC, False) ->
                    case nSuits of
                        0 -> ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Clubs))
                        1 -> 
                            if s1 == s3 then 
                                ((Card v1 Spades, Card v2 Hearts), (Card v3 Spades, Card v4 Diamonds))
                            else
                                ((Card v1 Spades, Card v2 Hearts), (Card v3 Hearts, Card v4 Diamonds))
                        2 -> ((Card v1 Spades, Card v2 Hearts), (Card v3 Spades, Card v4 Hearts))
                (ValueA, ValueB, False) ->
                    case nSuits of
                        0 -> ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Clubs))
                        1 -> ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Spades))
                        2 -> ((Card v1 Spades, Card v2 Hearts), (Card v3 Hearts, Card v4 Spades))
                (ValueA, ValueC, True) ->
                    if s2 == s4 then
                        ((Card v1 Spades, Card v2 Hearts), (Card v3 Hearts, Card v4 Hearts))
                    else
                        ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Diamonds))
                (ValueA, ValueC, False) ->
                    case nSuits of
                        0 -> ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Clubs))
                        1 -> 
                            if s1 == s4 then
                                ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Spades))
                            else if s2 == s3 then
                                ((Card v1 Spades, Card v2 Hearts), (Card v3 Hearts, Card v4 Diamonds))
                            else
                                ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Hearts))
                        2 -> ((Card v1 Spades, Card v2 Hearts), (Card v3 Hearts, Card v4 Spades))
                (ValueB, ValueC, True) ->
                    if s1 == s4 then
                        ((Card v1 Spades, Card v2 Hearts), (Card v3 Spades, Card v4 Spades))
                    else
                        ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Diamonds))
                (ValueB, ValueC, False) ->
                    case nSuits of
                        0 -> ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Clubs))
                        1 -> 
                            if s1 == s3 then
                                ((Card v1 Spades, Card v2 Hearts), (Card v3 Spades, Card v4 Diamonds))
                            else if s1 == s4 then
                                ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Spades))
                            else
                                ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Hearts))
                        2 -> ((Card v1 Spades, Card v2 Hearts), (Card v3 Spades, Card v4 Hearts))
                (ValueC, ValueD, True) -> 
                    if s1 == s3 then
                        ((Card v1 Spades, Card v2 Hearts), (Card v3 Spades, Card v4 Spades))
                    else if s2 == s3 then
                        ((Card v1 Spades, Card v2 Hearts), (Card v3 Hearts, Card v4 Hearts))
                    else
                        ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Diamonds))
                (ValueC, ValueD, False) ->
                    case nSuits of
                        0 -> ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Clubs))
                        1 ->
                            if s1 == s3 then
                                ((Card v1 Spades, Card v2 Hearts), (Card v3 Spades, Card v4 Diamonds))
                            else if s1 == s4 then
                                ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Spades))
                            else if s2 == s3 then
                                ((Card v1 Spades, Card v2 Hearts), (Card v3 Hearts, Card v4 Diamonds))
                            else
                                ((Card v1 Spades, Card v2 Hearts), (Card v3 Diamonds, Card v4 Hearts))
                        2 ->
                            if s1 == s3 then
                                ((Card v1 Spades, Card v2 Hearts), (Card v3 Spades, Card v4 Hearts))
                            else
                                ((Card v1 Spades, Card v2 Hearts), (Card v3 Hearts, Card v4 Spades))
    where
        c1 = (fst . fst) h; c2 = (snd . fst) h; c3 = (fst . snd) h; c4 = (snd . snd) h
        v1 = value c1; v2 = value c2; v3 = value c3; v4 = value c4
        s1 = suit c1; s2 = suit c2; s3 = suit c3; s4 = suit c4
        signature = handSignature h
        nSuits = numSuitsInCommon h

equivClass :: HoleCards -> HoleCards -> EquivalenceClass
equivClass x y = (modSuit . orderPairs . orderWithin) (x, y)

allEquivClasses :: Set.Set EquivalenceClass
allEquivClasses = acc pairs Set.empty 
    where
        pairs = [(x, y) | x <- smallDeck, y <- largeDeck, isValid x y]
        smallDeck = [(c, d) | c <- allCards, d <- allCards, suit c == Spades, suit d >= Hearts, c > d]
        largeDeck = [(c, d) | c <- allCards, d <- allCards, suit c >= Diamonds, c > d]
        -- Card pairs should be ordered by largest card, then smallest card, then suitedness, 
        -- then largest card suit, then smallest card suit. Reduces number of pairs we need to check
        isValid :: HoleCards -> HoleCards -> Bool
        isValid x y
            | fst x == fst y || fst x == snd y || snd x == fst y || snd x == snd y = False
            | vfx /= vfy = vfx > vfy
            | vsx /= vsy = vsx > vsy
            | sfx /= ssx && sfy == ssy = False
            | sfx /= sfy = sfx > sfy
            | otherwise = ssx > ssy
            where
                vfx = (value . fst) x; vfy = (value . fst) y; vsx = (value . snd) x; vsy = (value . snd) y
                sfx = (suit . fst) x; sfy = (suit . fst) y; ssx = (suit . snd) x; ssy = (suit . snd) y
        acc :: [(HoleCards, HoleCards)] -> Set.Set EquivalenceClass -> Set.Set EquivalenceClass
        acc [] out = out
        acc (p:ps) out = acc ps $ Set.insert pe out 
            where
                pe = equivClass (fst p) (snd p)
