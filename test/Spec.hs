import Test.Hspec
import Test.QuickCheck

import PokerCards
import PokerEvaluator

straightFlushHand = [Card Ace Spades, Card Ten Spades, Card Six Diamonds, Card King Spades, Card Queen Spades, Card Three Clubs, Card Jack Spades]
straightFlushHandExtra = [Card Ace Spades, Card Ten Spades, Card Six Diamonds, Card King Spades, Card Queen Spades, Card Nine Spades, Card Jack Spades]

quadsHand = [Card Two Clubs, Card Two Spades, Card Nine Hearts, Card Two Hearts, Card Two Diamonds, Card King Clubs, Card Five Spades]
quadsHandTrips = [Card Ace Hearts, Card Ace Diamonds, Card Ace Clubs, Card Two Spades, Card Two Hearts, Card Ace Spades, Card Two Clubs]

fullHouseHand = [Card Ten Spades, Card Ace Diamonds, Card Three Clubs, Card Ten Hearts, Card Ten Clubs, Card Ace Spades, Card Six Diamonds]
fullHouseHandExtra = [Card Six Hearts, Card Nine Spades, Card Six Clubs, Card Six Diamonds, Card Nine Hearts, Card Nine Clubs, Card Ace Spades]

flushHand = [Card Three Clubs, Card Ace Clubs, Card Two Hearts, Card King Clubs, Card Ten Clubs, Card Six Clubs, Card Nine Diamonds]
flushHandExtra = [Card Two Hearts, Card King Hearts, Card Seven Hearts, Card Jack Hearts, Card Nine Hearts, Card Four Hearts, Card Queen Hearts]

straightHand = [Card Three Clubs, Card Six Diamonds, Card Seven Hearts, Card Ten Spades, Card Jack Clubs, Card Five Diamonds, Card Four Diamonds]
straightHandExtra = [Card Nine Spades, Card Four Clubs, Card Six Diamonds, Card Ten Diamonds, Card Seven Spades, Card Eight Hearts, Card Five Hearts]
straightHandWheel = [Card Five Diamonds, Card Four Hearts, Card King Spades, Card Ace Clubs, Card Two Clubs, Card Three Hearts, Card Ace Spades]
straightHandDups = [Card Six Diamonds, Card Eight Spades, Card Five Spades, Card Eight Hearts, Card Seven Diamonds, Card Ace Clubs, Card Nine Spades]
straightHandPair = [Card Six Diamonds, Card Eight Spades, Card Five Spades, Card Ace Hearts, Card Seven Diamonds, Card Ace Clubs, Card Nine Spades]

tripsHand = [Card Three Clubs, Card Six Spades, Card Nine Hearts, Card Three Hearts, Card Three Diamonds, Card King Clubs, Card Five Spades]

twoPairHand = [Card Eight Clubs, Card Six Spades, Card Eight Hearts, Card Ace Diamonds, Card Jack Clubs, Card Six Diamonds, Card Three Spades]
twoPairHand2 = [Card Eight Clubs, Card Six Spades, Card Eight Hearts, Card Ace Diamonds, Card Ten Clubs, Card Six Diamonds, Card Two Spades]

pairHand = [Card Eight Clubs, Card Six Spades, Card Eight Hearts, Card King Diamonds, Card Jack Clubs, Card Ten Diamonds, Card Two Spades]
pairHand2 = [Card Eight Clubs, Card Six Spades, Card Eight Hearts, Card Ace Diamonds, Card Five Clubs, Card Ten Diamonds, Card Two Spades]
pairHand3 = [Card Eight Clubs, Card Six Spades, Card Eight Hearts, Card King Spades, Card Jack Clubs, Card Nine Diamonds, Card Two Spades]

highCardHand = [Card Eight Clubs, Card Six Spades, Card Three Hearts, Card King Diamonds, Card Jack Clubs, Card Ten Diamonds, Card Two Spades]

main :: IO ()
main = hspec $ do
    describe "Straight-Flushes" $ do
        it "works via the happy path" $ do
            rank straightFlushHand `shouldBe` StraightFlush
            bestStraight (parsePatterns straightFlushHand) `shouldBe` Just Ace
            bestFlush (parsePatterns straightFlushHandExtra) `shouldBe` Just [Ace, King, Queen, Jack, Ten]
        it "picks the best one" $ do
            bestStraight (parsePatterns straightFlushHandExtra) `shouldBe` Just Ace
            bestFlush (parsePatterns straightFlushHandExtra) `shouldBe` Just [Ace, King, Queen, Jack, Ten]
    describe "Quads" $ do
        it "works via the happy path" $ do
            rank quadsHand `shouldBe` Quads
            bestQuads (parsePatterns quadsHand) `shouldBe` Just Two
            remainder (parsePatterns quadsHand) `shouldBe` [King]
        it "can be nothing" $ do
            bestQuads (parsePatterns tripsHand) `shouldBe` Nothing
        it "does not detect trips at the same time" $ do
            let p = parsePatterns quadsHandTrips
            rank quadsHand `shouldBe` Quads
            bestQuads p `shouldBe` Just Ace
            remainder p `shouldBe` [Two]
    describe "FullHouse" $ do
        it "works via the happy path" $ do
            rank fullHouseHand `shouldBe` FullHouse
            bestTrips (parsePatterns fullHouseHand) `shouldBe` Just Ten
            bestPair (parsePatterns fullHouseHand) `shouldBe` Just Ace
            remainder (parsePatterns fullHouseHand) `shouldBe` []
        it "picks the best one" $ do
            rank fullHouseHandExtra `shouldBe` FullHouse
            bestTrips (parsePatterns fullHouseHandExtra) `shouldBe` Just Nine
            bestPair (parsePatterns fullHouseHandExtra) `shouldBe` Just Six
            remainder (parsePatterns fullHouseHandExtra) `shouldBe` []
    describe "Flushes" $ do
        it "works via the happy path" $ do
            rank flushHand `shouldBe` Flush
            bestFlush (parsePatterns flushHand) `shouldBe` Just [Ace, King, Ten, Six, Three]
            remainder (parsePatterns flushHand) `shouldBe` []
        it "picks the best one" $ do
            rank flushHandExtra `shouldBe` Flush
            bestFlush (parsePatterns flushHandExtra) `shouldBe` Just [King, Queen, Jack, Nine, Seven]
            remainder (parsePatterns flushHandExtra) `shouldBe` []
        it "can be nothing" $ do
            bestFlush (parsePatterns straightHand) `shouldBe` Nothing 
    describe "Straights" $ do
        it "works via the happy path" $ do
            rank straightHand `shouldBe` Straight
            bestStraight (parsePatterns straightHand) `shouldBe` Just Seven
            remainder (parsePatterns flushHand) `shouldBe` []
        it "picks the best one" $ do
            rank straightHandExtra `shouldBe` Straight
            bestStraight (parsePatterns straightHandExtra) `shouldBe` Just Ten
            remainder (parsePatterns straightHandExtra) `shouldBe` []
        it "detects wheels" $ do
            rank straightHandWheel `shouldBe` Straight
            bestStraight (parsePatterns straightHandWheel) `shouldBe` Just Five
            remainder (parsePatterns straightHandWheel) `shouldBe` []
        it "works with duplicates" $ do
            rank straightHandDups `shouldBe` Straight
            bestStraight (parsePatterns straightHandDups) `shouldBe` Just Nine
            remainder (parsePatterns straightHandDups) `shouldBe` []
        it "does not detect pairs at the same time" $ do
            let p = parsePatterns straightHandPair
            bestStraight p `shouldBe` Just Nine
            remainder p `shouldBe` []
    describe "Trips" $ do
        it "works via the happy path" $ do
            rank tripsHand `shouldBe` Trips
            bestTrips (parsePatterns tripsHand) `shouldBe` Just Three
            remainder (parsePatterns tripsHand) `shouldBe` [King, Nine]
    describe "TwoPair" $ do
        it "works via the happy path" $ do
            rank twoPairHand `shouldBe` TwoPair
            bestPair (parsePatterns twoPairHand) `shouldBe` Just Eight
            bestPair2 (parsePatterns twoPairHand) `shouldBe` Just Six
            remainder (parsePatterns twoPairHand) `shouldBe` [Ace]
    describe "Pair" $ do
        it "works via the happy path" $ do
            rank pairHand `shouldBe` Pair
            bestPair (parsePatterns pairHand) `shouldBe` Just Eight
            remainder (parsePatterns pairHand) `shouldBe` [King, Jack, Ten]
    describe "HighCard" $ do
        it "works via the happy path" $ do
            rank highCardHand `shouldBe` HighCard
            bestPair (parsePatterns highCardHand) `shouldBe` Nothing
            remainder (parsePatterns highCardHand) `shouldBe` [King, Jack, Ten, Eight, Six]
    describe "Evaluation" $ do
        it "ranks hands correctly" $ do
            evaluate pairHand highCardHand `shouldBe` GT
            evaluate tripsHand flushHand `shouldBe` LT
            evaluate straightFlushHand flushHand `shouldBe` GT
        it "breaks rank ties correctly" $ do
            evaluate pairHand2 pairHand `shouldBe` GT
            evaluate pairHand3 pairHand `shouldBe` LT
            evaluate twoPairHand twoPairHand2 `shouldBe` EQ
    describe "Shuffling" $ do
        it "shuffles cards" $ do
            newDeck 1 == newDeck 2 `shouldBe` False
    describe "Dealing" $ do
        it "can tell what the top card is" $ do
            peekCard (newDeck 1) == Card Seven Spades `shouldBe` True
        it "can tell what the top n cards are" $ do
            peekCardN 5 (newDeck 1) == [Card Seven Spades, Card Nine Diamonds, Card Five Spades, Card Four Hearts, Card Four Spades] `shouldBe` True
        it "can remove a card" $ do
            (length . removeCard . newDeck) 1 == 51 `shouldBe` True
        it "can remove n cards" $ do
            (length . (removeCardN 5) . newDeck) 1 == 47 `shouldBe` True