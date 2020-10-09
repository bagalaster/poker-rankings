module Main where

import PokerCards
import PokerEvaluator
import PokerHandsList
 
simulateHand :: Card -> Card -> Card -> Card -> Int -> Ordering
simulateHand a1 a2 b1 b2 n = evaluate (a1 : a2 : board) (b1 : b2 : board)
    where
        board = take 5 $ newDeckWithOut n [a1, a2, b1, b2]

sumResults :: [Ordering] -> (Int, Int, Int)
sumResults results = aux results (0, 0, 0)
    where
        aux :: [Ordering] -> (Int, Int, Int) -> (Int, Int, Int)
        aux [] (w, t, l) = (w, t, l)
        aux (r:rs) (w, t, l)
            | r == GT = aux rs (w + 1, t, l)
            | r == LT = aux rs (w, t, l + 1)
            | otherwise = aux rs (w, t + 1, l)
    
means :: (Int, Int, Int) -> (Float, Float, Float)
means (w, t, l) = (w' / s, t' / s, l' / s)
    where 
        w' = fromIntegral w :: Float
        t' = fromIntegral t :: Float
        l' = fromIntegral l :: Float
        s = w' + t' + l'

meanResults :: [Ordering] -> (Float, Float, Float)
meanResults = means . sumResults
        

card1 = Card Ace Hearts
card2 = Card King Spades
card3 = Card Ace Spades 
card4 = Card Queen Hearts

main :: IO ()
main = do
    -- putStrLn $ show $ parsePatterns (card1 : card2 : (take 5 $ newDeckWithOut seed [card1, card2, card3, card4]))
    -- putStrLn $ show $ parsePatterns (card3 : card4 : (take 5 $ newDeckWithOut seed [card1, card2, card3, card4]))
    -- 43.86 CPU sec for 1e6 simulations ==> 0.4368 CPU sec for 1e4 simulations ==> 5.73 CPU hr for 1e4 simulations for all possible hands
    let results = show $ meanResults $ map (simulateHand card1 card2 card3 card4) [1..1000]
    putStrLn $ results

    
