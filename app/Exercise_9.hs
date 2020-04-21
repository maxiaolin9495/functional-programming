module Exercise_9 where

type Name = String
type Valuation = [(Name,Bool)]
data Atom = T | Var Name
  deriving (Eq, Show)
data Literal = Pos Atom | Neg Atom
  deriving (Eq, Show)
data Form = L Literal | Form :&: Form | Form :|: Form
  deriving (Eq, Show)
type Clause = [Literal]
type ConjForm = [Clause]

{-T9.3.2-}
top :: Literal
top = Pos T

bottom :: Literal
bottom = Neg T

{-T9.3.3-}
clauseToForm :: Clause -> Form
clauseToForm [] = L bottom
clauseToForm ls = foldr ((:|:) . L) (L $ last ls) (init ls)

conjToForm :: ConjForm -> Form
conjToForm [] = L top
conjToForm ds = foldr ((:&:) . clauseToForm) (clauseToForm $ last ds) (init ds)

{-T9.3.4-}
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

{-H9.2.1-}
simpConj :: ConjForm -> ConjForm
simpConj [] = []
simpConj cls = let handleTop cs = [c | c<-cs,  notElem top c ]
                   handleBottom cs = [ [ l | l <- c, l /= bottom] | c <- cs]
                   result = handleBottom (handleTop cls) in
               if elem [] result then [[]] else result

{-H9.2.2-}
cnf :: Form -> ConjForm
cnf (L l) = [[l]]
cnf (f1 :&: f2) = cnf f1 ++ cnf f2
cnf (f1 :|: f2) = zipWith (++) (cnf f1) (cnf f2)

{-H9.2.3-}
selectV :: ConjForm -> Maybe (Name, Bool)
selectV f = let getOne [] = Nothing
                getOne (Pos T:xs) = getOne xs
                getOne (Neg T:xs) = getOne xs
                getOne (Pos (Var a):xs) = Just (a, True)
                getOne (Neg (Var a):xs) = Just (a, False) in
            let final = [getOne cs | cs <- f, getOne cs /= Nothing] in
            if final == [] then Nothing else head final
{-H9.2.5-}
satConj :: ConjForm -> Maybe Valuation
satConj = undefined

{-H9.2.6-}
sat :: Form -> Maybe Valuation
sat = undefined
