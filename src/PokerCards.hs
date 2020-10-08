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

type Deck = [Card]

newDeck :: Int -> Deck
newDeck n  = shuffle' [Card v s | v <- [Two .. Ace], s <- [Clubs .. Spades]] 52 $ mkStdGen n

newDeckWithOut :: Int -> [Card] -> Deck
newDeckWithOut n cards = shuffle' [Card v s | v <- [Two .. Ace], s <- [Clubs .. Spades], not (Card v s `elem` cards)] (52 - length cards) $ mkStdGen n

peekCard :: Deck -> Card
peekCard deck = head deck

peekCardN :: Int -> Deck -> Deck
peekCardN n deck = take n deck

removeCard :: Deck -> Deck
removeCard deck = drop 1 deck

removeCardN :: Int -> Deck -> Deck
removeCardN n deck = drop n deck