{- |
   Propositional formulas
-}
module Logic.Propositions where

-- | Infix formula.
-- More readable, more ambiguous.
data IFormula a
  = Z
  | O
  | Var a
  | Neg (IFormula a)
  | (IFormula a) :&: (IFormula a)
  | (IFormula a) :|: (IFormula a)
  | (IFormula a) :->: (IFormula a)
  | (IFormula a) :<->: (IFormula a)
  deriving (Eq, Show)

infixr 9 :&:

infixr 8 :|:

infixr 7 :->:

infixr 6 :<->:

-- | Transforms an infix formula to the corresponding prefix formula
--
-- Examples:
-- >>> fromInfix Z
-- F
--
-- >>> fromInfix $ Var "a" :<->: Neg (Var "b")
-- ("a" <-> ¬"b")
--
-- >>> fromInfix $ Var "p" :&: Var "q"
-- ("p" & "q")
fromInfix :: IFormula a -> Formula a
fromInfix Z = F
fromInfix O = T
fromInfix (Var v) = Atom v
fromInfix (Neg p) = Not $ fromInfix p
fromInfix (p :&: q) = Bin Conj (fromInfix p) (fromInfix q)
fromInfix (p :|: q) = Bin Dis (fromInfix p) (fromInfix q)
fromInfix (p :->: q) = Bin Impl (fromInfix p) (fromInfix q)
fromInfix (p :<->: q) = Bin Equiv (fromInfix p) (fromInfix q)

-- | Allowed binary operators for formulas
data Op
  = Dis -- ^ Disjunction (Or)
  | Conj -- ^ Conjunction (And)
  | Impl -- ^ Implication
  | Equiv -- ^ Logical equivalence
  deriving (Eq, Enum)

-- | Propositional formulas.
-- Using polish notation
data Formula a
  = T
  | F
  | Atom a
  | Not (Formula a)
  | Bin Op (Formula a) (Formula a)
  deriving (Eq)

instance Show Op where
  show Dis = "|"
  show Conj = "&"
  show Impl = "->"
  show Equiv = "<->"

instance Show a => Show (Formula a) where
  show T = "T"
  show F = "F"
  show (Atom a) = show a
  show (Not p) = "¬" ++ show p
  show (Bin op l r) = "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"

-- | Gets the atoms of a formula, with repetitions
--
-- Examples:
-- >>> getAtoms . fromInfix $ Z :<->: O
-- []
--
-- >>> getAtoms . fromInfix $ Var "p" :<->: Var "q" :->: Var "p"
-- ["p","q","p"]
getAtoms :: Eq a => Formula a -> [a]
getAtoms (Atom x) = [x]
getAtoms (Not p) = getAtoms p
getAtoms (Bin _ p q) = getAtoms p ++ getAtoms q
getAtoms _ = []

type Subst a = Formula a -> Maybe (Formula a)

-- | Convert a list of formula tuples into a proper substitution function
toSubst :: Eq a => [(Formula a, Formula a)] -> Subst a
toSubst xs a = a `lookup` xs


-- | Applies a substitution to a formula
-- Examples:
-- >>> applySubst (toSubst []) (Atom "p")
-- "p"
--
-- >>> applySubst (toSubst [(Atom "p", Atom "q")]) T
-- T
--
-- >>> applySubst (toSubst [(Atom "p", Atom "q")]) (Atom "p")
-- "q"
--
-- >>> :{
-- let
--   s (Atom "p") = Just $ Atom "q"
--   s (Bin Conj (Atom "p") (Atom "p")) = Just $ Atom "p"
-- in applySubst s (fromInfix $ Var "p" :&: Var "p")
-- :}
-- "p"
applySubst :: Subst a -> Formula a -> Formula a
applySubst s p =
  case s p of
    Nothing -> applySubforms s p
    Just q -> q

applySubforms :: Subst a -> Formula a -> Formula a
applySubforms _ T = T
applySubforms _ F = F
applySubforms _ (Atom v) = Atom v
applySubforms s (Not p) = Not . applySubst s $ p
applySubforms s (Bin op p q) = Bin op (sv p) (sv q) where sv = applySubst s

-- | Mark for literals
data Literal a
  = P {getAtom :: a}
  | N {getAtom :: a} deriving Eq

instance Show a => Show (Literal a) where
  show (P a) = show a
  show (N a) = "~" ++ show a

-- | If two literals are complement
--
-- Examples:
-- >>> areComplement (P "p") (N "p")
-- True
--
-- >>> areComplement (P "p") (P "p")
-- False
--
-- >>> areComplement (P "p") (N "q")
-- False
areComplement :: Eq a => Literal a -> Literal a -> Bool
areComplement (P x) (N y) = x == y
areComplement (N x) (P y) = x == y
areComplement _ _ = False

-- | A clause is an implicit disjuntions of literals
type Clause a = [Literal a]
