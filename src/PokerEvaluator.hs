module PokerEvaluator where

import Data.List
import Data.Maybe
import PokerCards

type PokerHand = [Card]
type PokerValues = [Value]
data HandRank = HighCard | Pair | TwoPair | Trips | Straight | Flush | FullHouse | Quads | StraightFlush deriving (Eq, Ord, Enum, Show)

compareValues :: Card -> Card -> Ordering
compareValues x y = compare (value x) (value y)

sortByValues :: PokerHand -> PokerHand
sortByValues h = reverse $ sortBy compareValues h

compareSuits :: Card -> Card -> Ordering
compareSuits x y = compare (suit x) (suit y)

sortBySuits :: PokerHand -> PokerHand
sortBySuits h = sortBy compareSuits h

filterOutValue :: Value -> PokerHand -> PokerHand
filterOutValue v h = filter (\x -> value x /= v) h

maybeFilterOutValue :: Maybe Value -> PokerHand -> PokerHand
maybeFilterOutValue Nothing h = h
maybeFilterOutValue (Just v) h = filterOutValue v h

filterSuit :: Suit -> PokerHand -> PokerHand
filterSuit s h = filter (\x -> suit x == s) h

data Patterns = Patterns {
    bestQuads :: Maybe Value,
    bestTrips :: Maybe Value,
    bestPair :: Maybe Value,
    bestPair2 :: Maybe Value,
    bestFlush :: Maybe [Value],
    bestStraight :: Maybe Value,
    remainder :: [Value]
} deriving (Show)

getBestNOfAKind :: Int -> PokerHand -> Maybe Value
getBestNOfAKind n h
    | length h < n = Nothing
    | value (head h) == value (h !! (n - 1)) = Just $ value $ head h
    | otherwise =  getBestNOfAKind n $ tail h

getBestQuads :: PokerHand -> Maybe Value
getBestQuads = getBestNOfAKind 4

getBestTrips :: PokerHand -> Maybe Value
getBestTrips = getBestNOfAKind 3

getBestPair :: PokerHand -> Maybe Value
getBestPair = getBestNOfAKind 2

getBestFlush :: PokerHand -> Maybe [Value]
getBestFlush h
    | suit (head h) == suit (h !! 4) = Just $ takeFlush $ suit $ head h
    | suit (h !! 1) == suit (h !! 5) = Just $ takeFlush $ suit $ h !! 1
    | suit (h !! 2) == suit (h !! 6) = Just $ takeFlush $ suit $ h !! 2
    | otherwise = Nothing
    where
        takeFlush :: Suit -> [Value]
        takeFlush s = ((take 5) . (map value) . sortByValues . (filterSuit s)) h

getBestStraight :: PokerHand -> Maybe Value
getBestStraight h
    | out /= Nothing = out
    | all (\x -> x `elem` v) [Ace, Two, Three, Four, Five] = Just Five
    | otherwise = Nothing
    where
        v = map value h
        out = hasStraight 5 (value $ head h) (head h) (tail h)
        hasStraight :: Int -> Value -> Card -> PokerHand -> Maybe Value
        hasStraight n s h []
            | n == 1 = Just s
            | otherwise = Nothing
        hasStraight n s h t
            | n == 1 = Just s
            | value h == (succ . value . head) t = hasStraight (n - 1) s (head t) (tail t)
            | value h == value (head t) = hasStraight n s h (tail t)
            | otherwise = hasStraight 5 (value $ head t) (head t) (tail t)

takeAllButLast2 :: [a] -> [a]
takeAllButLast2 x = take (length x - 2) x

parsePatterns :: PokerHand -> Patterns
parsePatterns hand = Patterns q t p p2 f s r where
    shv = sortByValues hand
    shs = sortBySuits hand
    q = getBestQuads shv
    q_hand = maybeFilterOutValue q shv
    t = if q /= Nothing then Nothing else getBestTrips q_hand
    t_hand = maybeFilterOutValue t q_hand 
    p = if q /= Nothing then Nothing else getBestPair t_hand
    p_hand = maybeFilterOutValue p t_hand
    p2 = if p == Nothing || (t /= Nothing && p /= Nothing) then Nothing else getBestPair p_hand
    p2_hand = maybeFilterOutValue p2 p_hand
    f = getBestFlush shs
    s = getBestStraight shv
    r = if (f /= Nothing || s /= Nothing) then [] else map value $ takeAllButLast2 p2_hand

getRank :: Patterns -> HandRank
getRank (Patterns q t p p2 f s r)
    | fx && sx = StraightFlush
    | qx = Quads
    | tx && px = FullHouse
    | fx = Flush
    | sx = Straight
    | tx = Trips
    | px && p2x = TwoPair
    | px = Pair
    | otherwise = HighCard
    where
        fx = f /= Nothing
        sx = s /= Nothing
        qx = q /= Nothing
        tx = t /= Nothing
        px = p /= Nothing
        p2x = p2 /= Nothing

compareValuesLists :: [Value] -> [Value] -> Ordering
compareValuesLists [] [] = EQ
compareValuesLists (x:xs) (y:ys) 
    | x /= y = compare x y
    | otherwise = compareValuesLists xs ys

quadsList :: Patterns -> [Value]
quadsList p = (fromJust $ bestQuads p) : (remainder p)

fullHouseList :: Patterns -> [Value]
fullHouseList p = [fromJust $ bestTrips p, fromJust $ bestPair p]

flushList :: Patterns -> [Value]
flushList p = fromJust $ bestFlush p

straightList :: Patterns -> [Value]
straightList p = [fromJust $ bestStraight p]

tripsList :: Patterns -> [Value]
tripsList p = (fromJust $ bestTrips p) : (remainder p)

twoPairList :: Patterns -> [Value]
twoPairList p = (fromJust $ bestPair p) : (fromJust $ bestPair2 p) : (remainder p)

pairList :: Patterns -> [Value]
pairList p = (fromJust $ bestPair p) : (remainder p)

evaluate :: PokerHand -> PokerHand -> Ordering
evaluate x y 
    | rx /= ry = compare rx ry
    | otherwise = case rx of
        StraightFlush -> compareValuesLists (straightList px) (straightList py)
        Quads -> compareValuesLists (quadsList px) (quadsList py)
        FullHouse -> compareValuesLists (fullHouseList px) (fullHouseList py)
        Flush -> compareValuesLists (flushList px) (flushList py)
        Straight -> compareValuesLists (straightList px) (straightList py)
        Trips -> compareValuesLists (tripsList px) (tripsList py)
        TwoPair -> compareValuesLists (twoPairList px) (twoPairList py)
        Pair -> compareValuesLists (pairList px) (pairList py)
        _ -> compareValuesLists (remainder px) (remainder py)
    where
        px = parsePatterns x
        py = parsePatterns y
        rx = getRank px
        ry = getRank py

rank :: PokerHand -> HandRank
rank h = (getRank . parsePatterns) h