module Exercise_9_Gen where
import Control.DeepSeq
import Test.QuickCheck.Gen
import Exercise_9 as Ex9

import Data.List (sort)
import Data.Maybe (isJust)

instance NFData (Ex9.Atom) where
  rnf a = ()

instance NFData (Ex9.Literal) where
  rnf l = ()

instance NFData (Ex9.Form) where
  rnf f = ()

instance Ord (Ex9.Atom) where
  compare Ex9.T _ = GT
  compare (Ex9.Var _) Ex9.T = LT
  compare (Ex9.Var n) (Ex9.Var m) = compare m n

instance Ord (Ex9.Literal) where
  compare (Ex9.Pos a) (Ex9.Neg _) = LT
  compare (Ex9.Neg a) (Ex9.Pos _) = GT
  compare (Ex9.Pos a) (Ex9.Pos b) = compare b a
  compare (Ex9.Neg a) (Ex9.Neg b) = compare b a

-- mV: maximum variable index
genName :: Int -> Gen Ex9.Name
genName mV = do
  n <- choose (1, mV)
  return $ show n

-- fV: frequency of variables
-- fT: frequency of True
genAtom :: Int -> Int -> Int -> Gen Ex9.Atom
genAtom mV fV fT = do
    v <- genName mV 
    frequency [(fV, return $ Ex9.Var v), (fT, return $ Ex9.T)]

-- fP: frequency of positive literals
-- fN: frequency of negative literals
genLiteral :: Int -> Int -> Int -> Int -> Int -> Gen Ex9.Literal
genLiteral mV fV fT fP fN = do
    a <- genAtom mV fV fT
    frequency [(fP, return $ Ex9.Pos a), (fN, return $ Ex9.Neg a)]

-- lC: maximum size of clauses
genClause :: Int -> Int -> Int -> Int -> Int -> Int -> Gen Ex9.Clause
genClause mV fV fT fP fN lC = resize lC $ listOf (genLiteral mV fV fT fP fN)

-- lC: maximum number of clauses
genConj :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Gen Ex9.ConjForm
genConj mV fV fT fP fN lC l = resize l $ listOf (genClause mV fV fT fP fN lC)

-- same as genClause but generates only non-empty clauses
genClauseNe :: Int -> Int -> Int -> Int -> Int -> Int -> Gen Ex9.Clause
genClauseNe mV fV fT fP fN lC = resize lC $ listOf1 (genLiteral mV fV fT fP fN)

-- same as genConj but generates at least one non-empty clauses 
genConjNe :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Gen Ex9.ConjForm
genConjNe mV fV fT fP fN lC l = resize l $ listOf1 (genClauseNe mV fV fT fP fN lC)

-- n: exponential depth of formula
genForm :: Int -> Int -> Int -> Int -> Int -> Int -> Gen Ex9.Form
genForm mV fV fT fP fN 0 = do 
  l <- genLiteral mV fV fT fP fN
  return $ Ex9.L l
genForm mV fV fT fP fN n | n > 0 = do 
    f1 <- genForm mV fV fT fP fN (n-1)
    f2 <- genForm mV fV fT fP fN (n-1)
    oneof [return $ (Ex9.:&:) f1 f2, return $ (Ex9.:|:) f1 f2]
