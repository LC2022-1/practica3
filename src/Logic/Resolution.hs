module Logic.Resolution where

import Logic.Propositions
import Data.Maybe (isJust)

-- | Tries to get a complemantary literal between two clauses. If there are more
-- than one, returns the first one.
--
-- Example:
-- >>> clashes [] []
-- Nothing
--
-- >>> clashes [ P "p", P "t"] [N "s", N "u"]
-- Nothing
--
-- >>> clashes [ P "p"] [N "p"]
-- Just "p"
--
-- >>> clashes [ P "p", P "t", N "s"] [P "q", N "t", P "s"]
-- Just "t"
clashes :: Eq a => Clause a -> Clause a -> Maybe a
clashes = undefined

removeLit :: Eq a => a -> Clause a -> Clause a
removeLit = undefined

-- | Computes the resolution between two clauses. Remember that you can't
-- computer the resolution of two clauses that are not in coflict.
--
-- Examples:
-- >>> resolve [ P "p", P "t", P "s"] [N "p", P "t", P "u"]
-- ["t","s","u"]
--
-- >>> resolve [ P "p", P "t", P "u"] [P "q", N "t", N "u"]
-- ["p","u","q",~"u"]
--
-- >>> resolve [ P "p"] [P "t"]
-- *** Exception: ...
-- ...
resolve :: Eq a => Clause a -> Clause a -> Clause a
resolve = undefined

-- | A clause is trivial if it has a complementary pair of literals
--
-- Examples:
-- >>> isTrivial [ P "p", N "p"]
-- True
--
-- >>> isTrivial []
-- False
--
-- >>> isTrivial [ P "p", P "t", N "u", N "v", P "u"]
-- True
isTrivial :: Eq a => Clause a -> Bool
isTrivial = undefined

-- | If a clause is empty
--
-- Examples:
-- >>> isEmpty []
-- True
--
-- >>> isEmpty [P "q"]
-- False
isEmpty :: Clause a -> Bool
isEmpty = null

-- | If a list of clauses has an empty clause
--
-- Examples:
-- >>> hasEmpty [[ P "p"], [], [P "t", N "u", P "r"]]
-- True
--
-- >>> hasEmpty []
-- False
--
-- >>> hasEmpty [[ P "p"], [N "q"]]
-- False
hasEmpty :: [Clause a] -> Bool
hasEmpty = undefined

-- A pair of clashing clauses
data Clash a = Clash (Clause a) (Clause a)

instance Eq a => Eq (Clash a) where
  (Clash x1 y1) == (Clash x2 y2) =
    (x1 == x2 && y1 == y2) || (x1 == y2 && y1 == x2)

instance Show a => Show (Clash a) where
  show (Clash a b) = "(" ++ show a ++ "," ++ show b ++ ")"

-- | Calculates clauses with clashing literals in a formula
--
-- Examples:
-- >>> clashList [[ P "p"], [N "p"]]
-- [(["p"],[~"p"])]
--
-- >>> clashList [[ P "p", N "t"], [P "s", P "u"], [N "p", N "u"], [P "q"]]
-- [(["p",~"t"],[~"p",~"u"]),(["s","u"],[~"p",~"u"])]
--
-- >>> clashList [[ P "p"], [N "t"]]
-- []
clashList :: Eq a => [Clause a] -> [Clash a]
clashList = undefined

-- | Keeps track of the selected pairs of clauses during resolution
data ResolReg a
  = ResolReg { getClauses :: [Clause a]
             , getRegister :: [Clash a]}
    deriving Show

-- | Tries to obtain a pair of clashing clauses not used before
--
-- Examples:
-- >>> someClash $ ResolReg [[ P "p"], [N "p"]] []
-- Just (["p"],[~"p"])
--
-- >>> :{
-- let
--   list = [[ P "p", N "t"], [P "s", P "u"], [N "p", N "u"], [P "q"]]
--   reg = [Clash [ P "p", N "t"] [N "p", N "u"]]
-- in someClash $ ResolReg list reg
-- :}
-- Just (["s","u"],[~"p",~"u"])
--
-- >>> someClash $ ResolReg [[ P "p"], [N "t"]] [Clash [P "r"] [N "p"]]
-- Nothing
someClash :: Eq a => ResolReg a -> Maybe (Clash a)
someClash = undefined

-- | Calculates the resolution of the clausulal form, keeping track of the
-- selected clauses. No doctests for this one because it should only be used by
-- `resolution`
applyRes :: Eq a => ResolReg a -> Bool
applyRes = undefined

-- | Applies resolution to a list of clauses. Just call `applyRes` with the
-- appropriate arguments. Remember that trivial clauses should be removed.
--
-- Examples:
-- >>> resolution [[P "q", N "r", P "s", N "t"]]
-- True
--
-- >>> resolution [[P "p"], [], [P "r", N "p"]]
-- False
--
-- >>> resolution [[P "p"], [P "q", N "q"], [N "p"]]
-- False
--
-- >>> resolution [[ P "p"], [P "t", N "s"], [N "t", P "s"]]
-- True
--
-- >>> resolution [[P "p"], [N "p", P "q"], [N "r"], [N "p", N "q", P "r"]]
-- False
resolution :: Eq a => [Clause a] -> Bool
resolution = undefined
