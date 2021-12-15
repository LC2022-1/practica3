{- |
  Tseitin encoding for propositional formulas
-}
module Logic.Tseitin where

import Logic.Propositions

-- | If a formula can't be replaced by an equivalence
--
-- Examples:
-- >>> isBase T
-- True
--
-- >>> isBase $ Not (Not (Atom "p"))
-- True
--
-- >>> isBase . fromInfix $ Z :|: O :->: Z
-- False
--
-- >>> isBase . fromInfix $ Var "p" :&: Var "q" :->: Z :|: Var "r"
-- False
isBase :: Formula a -> Bool
isBase = undefined

-- | Find a binary subformula that can be replaced by an equivalence in Tseitin
-- encoding. When there are several options available, choose the one on the
-- left most side of the AST
--
-- Examples:
-- >>> findBin (Atom "p")
-- Nothing
--
-- >>> findBin T
-- Nothing
--
-- >>> findBin (Not (Not (Not (Atom "p"))))
-- Nothing
--
-- >>> findBin . fromInfix $ O :->: Neg (Z :|: Neg (Var "p"))
-- Just (F | ¬"p")
--
-- >>> findBin . fromInfix $ Neg (Neg Z) :->: Var "r" :&: Var "t" :|: Var "h"
-- Just ("r" & "t")
findBin :: Formula a -> Maybe (Formula a)
findBin = undefined

-- | Transforms the formula into its Tseitin encoding, using the extra argument
-- to generate new variables.
-- No doctests for this one, as it should only be used with `toTseitin`.
toTseitinMax :: (Eq a, Enum a) => Formula a -> a -> Formula a
toTseitinMax = undefined

-- | Transforms the formula into its Tseitin encoding.
--
-- Examples:
-- >>> toTseitin T
-- T
--
-- >>> toTseitin (Not (Not (Atom 0)))
-- ¬¬0
--
-- >>> toTseitin . fromInfix $ Var 0 :|: Var 1
-- ((2 <-> (0 | 1)) & 2)
--
-- >>> toTseitin . fromInfix $ Z :->: (Neg (Neg (Var 4))) :<->: Var 1 :|: O
-- ((5 <-> (F -> ¬¬4)) & ((6 <-> (1 | T)) & ((7 <-> (5 <-> 6)) & 7)))
toTseitin :: (Eq a, Ord a, Enum a) => Formula a -> Formula a
toTseitin p = undefined
