module Logic.ThreeCNF where

import Logic.Propositions

-- | Obtain the maximum variable from a clause
--
-- Examples:
-- >>> clauseMax [[P 0]]
-- 0
-- >>> clauseMax [[], [N 1, N 6], [P 4, P 1], [P 3, P 4, P 5, P 6]]
-- 6
clauseMax :: Ord a => [Clause a] -> a
clauseMax = undefined

-- | Transforms a single clause into its 3CNF form, given a list of new
-- variables names to use.
--
-- Examples:
-- >>> canonRule [P 0] 0
-- [[2,1,0],[~2,1,0],[2,~1,0],[~2,~1,0]]
--
-- >>> canonRule [P 0, P 1] 1
-- [[2,0,1],[~2,0,1]]
--
-- >>> canonRule [P 1, P 2, P 0] 2
-- [[1,2,0]]
--
-- >>> canonRule [P 1, P 2, P 3, P 4, P 5, P 6] 6
-- [[1,2,7],[~7,3,8],[~8,4,9],[~9,5,6]]
canonRule :: Enum a => Clause a -> a -> [Clause a]
canonRule = undefined

-- | Returns the maximum between a value and a list of clauses
-- Usefull because `maximum` errors out on empty lists
safeMax :: Ord a => a -> [Clause a] -> a
safeMax x l = if (null . concat $ l) then x else max x (clauseMax l)

-- | Transforms each clause into a list of clauses of size three.
-- No doctests for this one, as it should only be called from `to3CNF`.
-- PD: use `safeMax` to update the max value
to3CNFm :: (Ord a, Enum a) => [Clause a] -> a  -> [Clause a]
to3CNFm = undefined

-- | Transforms a formula in CNF to its equivalent formula en 3CNF
--
-- Examples:
-- >>> to3CNF [[P 0]]
-- [[2,1,0],[~2,1,0],[2,~1,0],[~2,~1,0]]
--
-- >>> to3CNF [[P 0, P 1], [P 1, P 2, P 0]]
-- [[3,0,1],[~3,0,1],[1,2,0]]
--
-- >>> to3CNF [[P 1, P 2, P 3, P 4], [P 5, P 6]]
-- [[1,2,7],[~7,3,4],[8,5,6],[~8,5,6]]
to3CNF :: (Ord a, Enum a) => [Clause a] -> [Clause a]
to3CNF = undefined
