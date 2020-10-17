# Poker Simulation

This repository contains my simulation code for my course project
for Math 231 regarding the ranking of hole cards in Texas Hold'em poker. 
`src/PokerCards.hs` contains a representation of poker cards,
`src/PokerEvaluator.hs` contains an evaluation engine that determines
the winner of a poker hand. `src/PokerHandsList.hs` contains an
enumeration of all equivalence classes of unordered pairs of hole cards
as well as the logic used to reduce two pairs of hole cards modulo
suit permutation. `test/Spec.hs` contains unit tests that verify the
logic of the source code.
