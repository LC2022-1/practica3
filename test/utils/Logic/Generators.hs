module Logic.Generators where

import Logic.Propositions

import Test.QuickCheck

instance Arbitrary Op where
  arbitrary = elements [Dis .. Equiv]

genAtom :: Arbitrary a => Gen (Formula a)
genAtom = Atom <$> arbitrary

sizedNot :: Arbitrary a => Int -> Gen (Formula a)
sizedNot n = Not <$> sizedFormula (n - 1)

sizedBinFormula :: Arbitrary a => Int -> Gen (Formula a)
sizedBinFormula n = do
  op <- arbitrary
  leftSize <- choose (0, n-1)
  leftOp <- sizedFormula leftSize
  rightOp <- sizedFormula (n - 1 - leftSize)
  return (Bin op leftOp rightOp)

sizedFormula :: Arbitrary a => Int -> Gen (Formula a)
sizedFormula 0 = frequency [(1, elements [T, F]), (4, genAtom)]
sizedFormula n = frequency [(1, sizedNot n), (4, sizedBinFormula n)]

instance Arbitrary a => Arbitrary (Formula a) where
  arbitrary = sized sizedFormula

newtype StrVar = StrVar { getStrVar :: String } deriving Eq

instance Show StrVar where
  show (StrVar s) = s

instance Arbitrary StrVar where
  arbitrary = StrVar <$> elements ["p", "q", "s", "t", "v"]

newtype FormStr = FormStr {getStrForm :: Formula StrVar} deriving Eq

instance Show FormStr where
  show (FormStr p) = show p

instance Arbitrary FormStr where
  arbitrary = FormStr <$> arbitrary

newtype IntVar = IntVar {getIntVar :: Int} deriving Eq

instance Show IntVar where
  show (IntVar n) = show n

instance Enum IntVar where
  toEnum n = IntVar n
  fromEnum (IntVar n) = n

instance Arbitrary IntVar where
  arbitrary = IntVar <$> elements [1..10]

newtype FormInt = FormInt {getIntForm :: Formula IntVar} deriving Eq

instance Show FormInt where
  show (FormInt p) = show p

instance Arbitrary FormInt where
  arbitrary = FormInt <$> arbitrary
