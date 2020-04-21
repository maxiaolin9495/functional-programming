module Exercise_8 where
import Data.List

{-H.8.1-}

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

showGraphviz :: Show a => Tree a -> String
showGraphviz t =
  let
    show1 x Leaf = []
    show1 x (Node _ y _) = [show x ++ " -- " ++ show y ++ ";"]

    show' Leaf = []
    show' (Node l x r) =
      show1 x l ++ show1 x r ++ show' l ++ show' r
  in
    unlines $ ["graph T {"] ++ show' t ++ ["}"]

{-H.8.1.1-}
symmetric :: Eq a => Tree a -> Bool
symmetric Leaf = True
symmetric (Node l n r) = let mirror Leaf Leaf = True
                             mirror (Node l1 n1 r1) (Node l2 n2 r2) = n1 == n2 && mirror l1 r2 && mirror r1 l2
                             mirror _ _ = False
                         in mirror l r

{-H.8.1.2-}
isBST :: Ord a => Tree a -> Bool
isBST Leaf = True
isBST (Node l n r) = let larger Leaf root = True
                         larger (Node l n r) root = root > n && larger l root && larger r root in
                     let smaller Leaf root = True
                         smaller (Node l n r) root = root < n && smaller l root && smaller r root in
                     larger l n && smaller r n && isBST l && isBST r


getLeafDepth:: Tree a -> Integer -> [Integer]
getLeafDepth Leaf d = [d]
getLeafDepth (Node l n r) d = getLeafDepth l (d + 1) ++ getLeafDepth r (d + 1)

isAlmostComplete :: Tree a -> Bool
isAlmostComplete Leaf = True
isAlmostComplete (Node l n r) =  let leafs = getLeafDepth l 0 ++ getLeafDepth r 0
                                     deepstLeaf = maximum leafs in
                                    and [ abs (leaf - deepstLeaf) <= 1 | leaf <- leafs]  && isAlmostComplete l && isAlmostComplete r

buildBST :: Ord a => [a] -> Tree a
buildBST [] = Leaf
buildBST l = let middleIndex = div (length l) 2
                 leftl = take middleIndex l
                 rightl = drop middleIndex l
                 n = head rightl in
             Node (buildBST leftl) n (buildBST (drop 1 rightl))




rangeSubtree :: Ord a => Tree a -> (a, a) -> Tree a
rangeSubtree Leaf _ = Leaf
rangeSubtree t@(Node l n r) (a, b) | a > n = rangeSubtree r (a, b)
                                   | b < n = rangeSubtree l (a, b)
                                   | otherwise = Node (rangeSubtree l (a, n)) n (rangeSubtree r (n, b))
{-H8.2-}

{-WETT-}
shoefa :: (Num a, Ord a) => [a] -> Int
shoefa l = let l' = filter (/=0) l in
           length [ 1 | n <- zipWith (*) l' (drop 1 l'), n<0]
{-TTEW-}

{-H8.3-}
data Poly a = Poly [(a, Integer)]
  deriving (Eq)

-- takes the derivative of a polynomial
polyDeriv :: Num a => Poly a -> Poly a
polyDeriv (Poly ps) = Poly [(fromInteger e * c, e - 1) | (c, e) <- ps, e /= 0]

-- addition of two polynomials
polyAdd :: (Eq a, Num a) => Poly a -> Poly a -> Poly a
polyAdd (Poly []) q = q
polyAdd p (Poly []) = p
polyAdd p@(Poly ((c1, e1) : ps)) q@(Poly ((c2, e2) : qs))
  | e1 < e2      = let Poly rs = polyAdd (Poly ps) q in
                   Poly ((c1, e1) : rs)
  | e1 > e2      = let Poly rs = polyAdd p (Poly qs) in
                   Poly ((c2, e2) : rs)
  | c1 + c2 == 0 = polyAdd (Poly ps) (Poly qs)
  | otherwise    = let Poly rs = polyAdd (Poly ps) (Poly qs) in
                   Poly ((c1 + c2, e1) : rs)

{-H8.3.1-}
instance Show a => Show (Poly a) where
  show p@(Poly []) = "0"
  show p@(Poly l) = intercalate " + "  [show a ++ "x^{" ++ show i ++ "}" | (a,i) <- l]

{-H8.3.2-}
polyShift :: (Eq a, Num a) => (a, Integer) -> Poly a -> Poly a
polyShift (a, i) (Poly l)= Poly (map (\ (a',i') -> (a*a', i+i')) l)

{-H8.3.3-}
instance (Eq a, Num a) => Num (Poly a) where
  x + y = polyAdd x y
  negate x@(Poly l) = Poly [ (negate a, i) | (a,i) <- l]
  fromInteger 0 = Poly []
  fromInteger i = Poly [(fromInteger i, 0)]
  x - y = polyAdd x (negate y)
  x * y@(Poly l) = foldl polyAdd (Poly []) [polyShift i x | i<- l]
  abs = undefined
  signum = undefined
{-H8.3.4-}

leadCoeff :: Num a => Poly a -> a
leadCoeff (Poly []) = 0
leadCoeff (Poly l) =  fst (maximumBy (\ a b -> compare (snd a) (snd b) ) l)

degree :: Poly a -> Integer
degree (Poly []) = -1
degree (Poly l) =  snd (maximumBy (\ a b -> compare (snd a) (snd b) ) l)

{-H8.3.5-}
polyDivMod :: (Fractional a, Eq a) => Poly a -> Poly a -> (Poly a, Poly a)
polyDivMod (Poly la) (Poly []) = (Poly [], Poly la)
polyDivMod (Poly la) (Poly lb) =
  let help_f (Poly la) (Poly lb) (Poly ls) | degree (Poly la) < degree (Poly lb) = (Poly ls, Poly la)
                                           | otherwise = let t = (leadCoeff (Poly la) / leadCoeff (Poly lb), degree (Poly la) - degree (Poly lb)) in
                                                     help_f (Poly la - polyShift t (Poly lb) ) (Poly lb) (Poly ls + Poly [t])
                                 in help_f (Poly la) (Poly lb) (Poly [])

{-H8.3.6-}
polyDiv :: (Fractional a, Eq a) => Poly a -> Poly a -> Poly a
polyDiv pa pb = fst $ polyDivMod pa pb

polyMod :: (Fractional a, Eq a) => Poly a -> Poly a -> Poly a
polyMod pa pb = snd $ polyDivMod pa pb

{-H8.3.7-}
polyEval :: Num a => Poly a -> a -> a
polyEval (Poly la) x = sum [ a * (x ^i) | (a,i)<- la]

{-H8.3.8-}
genSturm :: (Fractional a, Eq a) => Poly a -> Poly a -> [Poly a]
genSturm = undefined

sturm :: (Fractional a, Eq a) => Poly a -> [Poly a]
sturm = undefined

{-H8.3.9-}
countRootsBetween :: Poly Rational -> Rational -> Rational -> Int
countRootsBetween = undefined
