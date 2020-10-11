module PokerCards where

import Data.List
import System.Random
import System.Random.Shuffle

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Ord, Enum, Show)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace | Inf deriving (Eq, Ord, Enum, Show)

data Card = Card {
    value :: Value,
    suit :: Suit
} deriving (Eq)
instance Show Card where
    show card = "(" ++ (show $ value card) ++ ":" ++ (show $ suit card) ++ ")" 
instance Ord Card where
    compare x y
        | value x /= value y = compare (value x) (value y)
        | otherwise = compare (suit x) (suit y)

type Deck = [Card]

allCards :: Deck
allCards = [Card v s | v <- [Two .. Ace], s <- [Clubs .. Spades]]

newDeck :: Int -> Deck
newDeck n  = shuffle' allCards 52 $ mkStdGen n

newDeckWithOut :: Int -> [Card] -> Deck
newDeckWithOut n cards = shuffle' [c | c <- allCards, not (c `elem` cards)] (52 - length cards) $ mkStdGen n

peekCard :: Deck -> Card
peekCard deck = head deck

peekCardN :: Int -> Deck -> Deck
peekCardN n deck = take n deck

removeCard :: Deck -> Deck
removeCard deck = drop 1 deck

removeCardN :: Int -> Deck -> Deck
removeCardN n deck = drop n deck