module Exercise_2 where

import Data.Ratio
import Test.QuickCheck hiding (choose)
import Data.List

{-H2.1.1-}
removeInvalidGuesses :: [(String,Int)] -> [(String,Int)]
removeInvalidGuesses [] = []
removeInvalidGuesses ((s, i): ls) | null ls =  if null s then [] else [(s, i) | i >= 0 && i <= 100]
    | null s = removeInvalidGuesses ls
    | i >= 0 && i <= 100 = (s, i) : removeInvalidGuesses ls
    | otherwise = removeInvalidGuesses ls

{-H2.1.2-}
average :: [(String,Int)] -> Int
average [] = 0
average l = div ( sum  [i | (s,i) <- l] ) $ length l

{-H2.1.3-}

absVa :: Int -> Int
absVa a = if a >= 0  then a else -a

winners :: [(String,Int)] -> [String]
winners l = let l1 = removeInvalidGuesses l in
            let winners_help ((s,i) : ls) avg name value |  null ls = if absVa (avg - i) < value then [s] else if absVa (avg - i) == value then s:name else name
                                                         |  absVa (avg - i) < value = winners_help ls avg [s] (absVa (avg - i))
                                                         |  absVa (avg - i) == value = winners_help ls avg (s:name) value
                                                         |  otherwise = winners_help ls avg name value
            in  if null l1 then [] else winners_help l1 (average l1) [] 100

{-H2.2-}
-- Computes the binomial coefficient "n choose k"
choose :: Integer -> Integer -> Integer
n `choose` k = product [n-k+1..n] `div` product [1..k]

bernoulli :: Integer -> Rational
{-WETT-}
bernoulli 0 = 1
bernoulli n = let bernoulli_help n k l1 l2 = case l1 of [] -> sum l2
                                                        (x:xs) -> bernoulli_help n (k+1) xs ( l2 ++ [choose n k % (k - n - 1) * x])
              in
              let bernoulli_help_help n k l1 | k == n = last l1
                                             | otherwise = bernoulli_help_help n (k+1) (l1 ++ [bernoulli_help (k+1) 0 l1 [] ])
              in bernoulli_help_help n 0 [1]
{-TTEW-}

{-H2.3-}
-- toSet l removes the duplicates of a list l
toSet :: Eq a => [a] -> [a]
toSet = nub

-- union s t builds the union of s and t
union :: Eq a => [a] -> [a] -> [a]
union xs ys = toSet $ xs ++ ys

{-H2.3.1-}
power :: [Integer] -> [[Integer]]
power [] = [[]]
power (x:xs) = power xs ++ [ x:i | i<-power xs]


subsetEq :: [Integer] -> [Integer] -> Bool
subsetEq [] l2 = True
subsetEq (x:xs) l2 = toSet l2 == toSet ( Exercise_2.union l2 [x]) &&
    subsetEq xs l2

{-H2.3.2-}
comparable :: [Integer] -> [Integer] -> Bool
comparable l1 l2 = subsetEq l1 l2 || subsetEq l2 l1

{-H2.3.3-}
isAntichain :: [[Integer]] -> Bool
isAntichain [] = True
isAntichain [_] = True
isAntichain (x:xs) = null [i | i <- xs, comparable x i] && isAntichain xs



{-H2.3.4-}
antichains :: Integer -> [[[Integer]]]
antichains n = let l = power [1..n] in
               let antichains_help [] = [[]]
                   antichains_help (x:xs) = antichains_help xs ++  [x:i | i<- antichains_help xs, isAntichain(x:i)] in
             toSet ([]: antichains_help l)

{-H2.3.5-}
maxAntichainSize :: [[[Integer]]] -> Int
maxAntichainSize [] = 0
maxAntichainSize l = let l1 = [length x | x <- l, isAntichain x] in if null l1 then 0 else maximum l1

prop_spernersTheorem :: Integer -> Property
prop_spernersTheorem n = n > 0 ==> fromIntegral (maxAntichainSize (antichains n)) == choose n (div n 2)
