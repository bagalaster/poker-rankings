import Test.Hspec
import Test.QuickCheck

import PokerCards
import PokerEvaluator
import PokerHandsList

import qualified Data.Set as Set

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

filterFn :: Value -> Value -> Bool -> Value -> Value -> Bool -> ((Card, Card), (Card, Card)) -> Bool
filterFn v1 v2 s1 v3 v4 s2 e
    | value x1 /= v1 = False
    | value x2 /= v2 = False
    | (suit x1 == suit x2) /= s1 = False
    | value x3 /= v3 = False
    | value x4 /= v4 = False
    | (suit x3 == suit x4) /= s2 = False
    | otherwise = True
    where
        x1 = (fst . fst) e; x2 = (snd . fst) e
        x3 = (fst . snd) e; x4 = (snd . snd) e

vff = (value . fst . fst); sff = (suit . fst . fst)
vsf = (value . snd . fst); ssf = (suit . snd . fst)
vfs = (value . fst . snd); sfs = (suit . fst . snd)
vss = (value . snd . snd); sss = (suit . snd . snd)

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
    describe "Equivalence Classes" $ do
        it "works via the happy path" $ do
            let e = equivClass (Card Ace Spades, Card Ten Clubs) (Card Two Hearts, Card Two Clubs)
            fst e `shouldBe` (Card Ace Spades, Card Ten Hearts)
            snd e `shouldBe` (Card Two Hearts, Card Two Diamonds)
        it "orders cards correctly" $ do
            let e = equivClass (Card Six Diamonds, Card Nine Spades) (Card King Spades, Card Ace Spades)
            fst e `shouldBe` (Card Ace Spades, Card King Spades)
            snd e `shouldBe` (Card Nine Spades, Card Six Hearts)
        it "can tell equivalence classes apart" $ do
            let e1 = equivClass (Card Ace Spades, Card Ten Clubs) (Card Two Hearts, Card Two Clubs)
            let e2 = equivClass (Card Ace Hearts, Card Ten Hearts) (Card Two Spades, Card Two Clubs)
            e1 == e2 `shouldBe` False
        context "has the correct number for" $ do
            let lst = Set.toAscList allEquivClasses
            it "(a,a) vs (a,a)" $ do
                let f1a = filterFn Ace Ace False Ace Ace False
                length (filter f1a lst) `shouldBe` 1
                let f1a' = filterFn Two Two False Two Two False
                length (filter f1a' lst) `shouldBe` 1
            it "(a,a) vs (b,b)" $ do
                let f1b = filterFn Ace Ace False King King False
                length (filter f1b lst) `shouldBe` 3
                let f1b' = filterFn Two Two False Three Three False
                length (filter f1b' lst) `shouldBe` 0
            it "(a,a) vs (a,b)s" $ do                
                let f1c = filterFn Ace Ace False Ace King True
                length (filter f1c lst) `shouldBe` 1
                let f1c' = filterFn Two Two False Three Two True
                length (filter f1c' lst) `shouldBe` 0
            it "(a,a) vs (a,b)u" $ do
                let f1d = filterFn Ace Ace False Ace King False
                length (filter f1d lst) `shouldBe` 2
                let f1d' = filterFn Two Two False Three Two False
                length (filter f1d' lst) `shouldBe` 0
            it "(a,a) vs (b,c)s" $ do
                let f1e = filterFn Ace Ace False King Queen True
                length (filter f1e lst) `shouldBe` 2
                let f1e' = filterFn Two Two False Four Three True
                length (filter f1e' lst) `shouldBe` 0
            it "(a,a) vs (b,c)u" $ do
                let f1f = filterFn Ace Ace False King Queen False
                length (filter f1f lst) `shouldBe` 4
                let f1f' = filterFn Two Two False Four Three False
                length (filter f1f' lst) `shouldBe` 0

            it "(a,b)s vs (a,a)" $ do
                let f2a = filterFn Ace King True Ace Ace False
                length (filter f2a lst) `shouldBe` 0
                let f2a' = filterFn Three Two True Three Three False
                length (filter f2a' lst) `shouldBe` 0
            it "(a,b)s vs (b,b)" $ do
                let f2b = filterFn Ace King True King King False
                length (filter f2b lst) `shouldBe` 1
                let f2b' = filterFn Three Two True Two Two False
                length (filter f2b' lst) `shouldBe` 1
            it "(a,b)s vs (c,c)" $ do
                let f2c = filterFn Ace King True Queen Queen False
                length (filter f2c lst) `shouldBe` 2
                let f2c' = filterFn Three Two True Four Four False
                length (filter f2c' lst) `shouldBe` 0
            it "(a,b)s vs (a,b)s" $ do
                let f2d = filterFn Ace King True Ace King True
                length (filter f2d lst) `shouldBe` 1
                let f2d' = filterFn Three Two True Three Two True
                length (filter f2d' lst) `shouldBe` 1
            it "(a,b)s vs (a,b)u" $ do
                let f2e = filterFn Ace King True Ace King False
                length (filter f2e lst) `shouldBe` 1
            it "(a,b)s vs (a,c)s" $ do
                let f2f = filterFn Ace King True Ace Queen True
                length (filter f2f lst) `shouldBe` 1
            it "(a,b)s vs (a,c)u" $ do
                let f2g = filterFn Ace King True Ace Queen False
                length (filter f2g lst) `shouldBe` 2
            it "(a,b)s vs (b,c)s" $ do
                let f2h = filterFn Ace King True King Queen True
                length (filter f2h lst) `shouldBe` 1
            it "(a,b)s vs (b,c)u" $ do
                let f2i = filterFn Ace King True King Queen False
                length (filter f2i lst) `shouldBe` 2
            it "(a,b)s vs (c,d)s" $ do
                let f2j = filterFn Ace King True Queen Jack True
                length (filter f2j lst) `shouldBe` 2
            it "(a,b)s vs (c,d)u" $ do
                let f2k = filterFn Ace King True Queen Jack False
                length (filter f2k lst) `shouldBe` 3
            it "(a,b)u vs (a,a)" $ do
                let f3a = filterFn Ace King False Ace Ace False
                length (filter f3a lst) `shouldBe` 0
            it "(a,b)u vs (b,b)" $ do
                let f3b = filterFn Ace King False King King False
                length (filter f3b lst) `shouldBe` 2
            it "(a,b)u vs (c,c)" $ do
                let f3c = filterFn Ace King False Queen Queen False
                length (filter f3c lst) `shouldBe` 4
            it "(a,b)u vs (a,b)s" $ do
                let f3d = filterFn Ace King False Ace King True
                length (filter f3d lst) `shouldBe` 0
            it "(a,b)u vs (a,b)u" $ do
                let f3e = filterFn Ace King False Ace King False
                length (filter f3e lst) `shouldBe` 3
            it "(a,b)u vs (a,c)s" $ do
                let f3f = filterFn Ace King False Ace Queen True
                length (filter f3f lst) `shouldBe` 2
            it "(a,b)u vs (a,c)u" $ do
                let f3g = filterFn Ace King False Ace Queen False
                length (filter f3g lst) `shouldBe` 5
            it "(a,b)u vs (b,c)s" $ do
                let f3h = filterFn Ace King False King Queen True
                length (filter f3h lst) `shouldBe` 2
            it "(a,b)u vs (b,c)u" $ do
                let f3i = filterFn Ace King False King Queen False
                length (filter f3i lst) `shouldBe` 5
            it "(a,b)u vs (c,d)s" $ do
                let f3j = filterFn Ace King False Queen Jack True
                length (filter f3j lst) `shouldBe` 3
            it "(a,b)u vs (c,d)u" $ do
                let f3k = filterFn Ace King False Queen Jack False
                length (filter f3k lst) `shouldBe` 7
            it "all" $ do
                length lst `shouldBe` 47008
                

