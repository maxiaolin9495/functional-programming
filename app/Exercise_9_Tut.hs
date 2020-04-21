module Exercise_9_Sol where 
import Data.List
import Data.Maybe

{-T9.1.1-}
data NonEmptyList a = Single a | Cons a (NonEmptyList a)
  deriving (Show, Eq)

{-T9.1.2-}
toList :: NonEmptyList a -> [a]
toList (Single a) = [a]
toList (Cons h t) = h : toList t

fromList :: [a] -> Maybe (NonEmptyList a)
fromList [] = Nothing
fromList (x : xs) = Just (go x xs)
  where go x [] = Single x
        go x (y : ys) = Cons x (go y ys)

{-T9.1.3-}
nHead :: NonEmptyList a -> a
nHead (Single a) = a
nHead (Cons a _) = a

nTail :: NonEmptyList a -> Maybe (NonEmptyList a)
nTail (Single _) = Nothing
nTail (Cons _ t) = Just t

nAppend :: NonEmptyList a -> NonEmptyList a -> NonEmptyList a
nAppend (Single a) xs = Cons a xs
nAppend (Cons h t) xs = h `Cons` nAppend t xs

nTake :: Integer -> NonEmptyList a -> Maybe (NonEmptyList a)
nTake n _ | n <= 0 = Nothing
nTake n (Single a) 
  | n == 1 = Just $ Single a 
  | otherwise = Nothing
nTake n (Cons a tl)
  | n == 1 = Just (Single a) 
  | otherwise = case nTake (n-1) tl of
                Nothing -> Nothing 
                Just res -> Just $ Cons a res


{-T9.3.1-}
type Name = String
data Atom = T | Var Name
  deriving (Eq, Show)
data Literal = Pos Atom | Neg Atom
  deriving (Eq, Show)
data Form = L Literal | Form :&: Form | Form :|: Form
  deriving (Eq, Show)


{-T9.3.2-}
top :: Literal
top = Pos T

bottom :: Literal
bottom = Neg T

{-T9.3.3-}
type Clause = [Literal]
type ConjForm = [Clause]
clauseToForm :: Clause -> Form
clauseToForm [] = L bottom
clauseToForm ls = foldr ((:|:) . L) (L $ last ls) (init ls)

conjToForm :: ConjForm -> Form
conjToForm [] = L top
conjToForm ds = foldr ((:&:) . clauseToForm) (clauseToForm $ last ds) (init ds)

{-T9.3.4-}
type Valuation = [(Name,Bool)]

substLiteral :: Valuation -> Literal -> Literal
substLiteral v l@(Pos (Var n)) = case lookup n v of
  Just b -> if b then top else bottom
  Nothing -> l
substLiteral v l@(Neg (Var n)) = case lookup n v of
  Just b -> if b then bottom else top
  Nothing -> l
substLiteral v l = l

substClause :: Valuation -> Clause -> Clause
substClause = map . substLiteral

substConj :: Valuation -> ConjForm -> ConjForm
substConj = map . substClause
