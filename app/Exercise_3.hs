module Exercise_3 where

import Test.QuickCheck
import Data.List
import Data.Ratio

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

{-H3.1.1-}
decomposition :: Int -> [Int]
decomposition n = let decomposition_help 1 _ = []
                      decomposition_help n i | (i == 2 || i == 3 || not (mod i 6 /= 1 && mod i 6 /= 5) && null [x | x <- [2..(isqrt i)], mod i x == 0 || mod i (x + 2) == 0]) && ((div n i * i) == n) = i : decomposition_help (div n i) i
                                             | otherwise = decomposition_help n (i + 1)
                      in decomposition_help n 2

decomposition2 :: Int -> [(Int, Int)]
decomposition2 n = let decomposition2_help [] i 0 = []
                       decomposition2_help [] i n = [(i, n)]
                       decomposition2_help (p:ps) i n | p == i = decomposition2_help ps i (n + 1)
                                                      | n /= 0 = (i, n) : decomposition2_help ps p 1
                                                      | otherwise = decomposition2_help ps p 1
                       in decomposition2_help (decomposition n) 2 0

{-H3.1.2-}
isPrime :: Int -> Bool
isPrime n = length (decomposition n) == 1



primes :: Int -> [Int]
primes n = [i | i <- [2..n], length (decomposition i) == 1]

{-H3.1.3-}
takes :: [Int] -> [a] -> [[a]]
takes [] _ = []
takes (x:xs) [] = []
takes (x:xs) y = let  takesFirstNInList n [] = []
                      takesFirstNInList n (y:ys) | n == 0 = []
                                                 | otherwise = y : takesFirstNInList (n-1) ys
                  in let removeFristNInList n [] = []
                         removeFristNInList n (y:ys) | n == 0 = y:ys
                                                     | otherwise = removeFristNInList (n-1) ys
                  in if x > length y then [[i] | i <- y] else takesFirstNInList x y : takes xs (removeFristNInList x y)


{-H3.1.4-}
takePrimes :: [a] -> [[a]]
takePrimes l = if length l == 1 then [l] else takes (primes (length l)) l

{-H3.2.1-}
add :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
add [] y = y
add x [] = x
add ((a, e):xs) ((a', e'):ys) | e == e' = if a + a' /= 0 then (a+a', e) : add xs ys else add xs ys
                              | e < e' = (a,e) : add xs ((a', e'):ys)
                              | otherwise = (a', e') : add ((a, e):xs) ys

{-H3.2.2-}
derivative :: [(Integer,Integer)] -> [(Integer,Integer)]
derivative [] = []
derivative ((a, e) :xs) = if e /= 0 then (e*a, e - 1 ) : derivative xs else derivative xs

{-H3.2.3-}
flipNegExp :: [(Integer,Integer)] -> [(Integer,Integer)]
flipNegExp [] = []
flipNegExp l = let  flipNegExp_help neg [] = neg
                    flipNegExp_help neg ((a,e) :xs) = if e < 0 then flipNegExp_help ((a, negate e) : neg ) xs else add neg ((a,e) :xs)
                         in flipNegExp_help [] l

{-H3.3.1-}
unspell :: String -> [Int]
unspell [] = []
unspell (x:xs)   | x == '0' = [0,0,0,0] ++ unspell xs
                 | x == '1' = [0,0,0,1] ++ unspell xs
                 | x == '2' = [0,0,1,0] ++ unspell xs
                 | x == '3' = [0,0,1,1] ++ unspell xs
                 | x == '4' = [0,1,0,0] ++ unspell xs
                 | x == '5' = [0,1,0,1] ++ unspell xs
                 | x == '6' = [0,1,1,0] ++ unspell xs
                 | x == '7' = [0,1,1,1] ++ unspell xs
                 | x == '8' = [1,0,0,0] ++ unspell xs
                 | x == '9' = [1,0,0,1] ++ unspell xs
                 | x == 'a' = [1,0,1,0] ++ unspell xs
                 | x == 'b' = [1,0,1,1] ++ unspell xs
                 | x == 'c' = [1,1,0,0] ++ unspell xs
                 | x == 'd' = [1,1,0,1] ++ unspell xs
                 | x == 'e' = [1,1,1,0] ++ unspell xs
                 | x == 'f' = [1,1,1,1] ++ unspell xs
                 | otherwise = []




{-H3.3.2-}
index :: Int -> Int -> Int -> Int
index l m r = 4 * l + 2 * m + r

{-H3.3.3-}
ritual :: [Int] -> [Int] -> [Int]
ritual rules [] = []
ritual [] line = []
ritual rules line = let findN rules n | n - 1 >length rules = 0
                                      | otherwise = rules!!n
                    in let getIndex [y] first = [index first y 0]
                           getIndex (y:y':ys) first = index first y y' : getIndex (y':ys) y
                    in [findN rules i | i <- getIndex line 0]

{-H3.3.4-}
simulate :: [Int] -> [Int] -> Int -> [[Int]]
simulate _ line 0 = [line]
simulate rules line n = line : simulate rules (ritual rules line) (n-1)

-- Tux's open source visualisation software. GNU3 LICENSE
showPenguins [] _ = return ()
showPenguins state pc = do
  putStrLn $ [c | d <- head state, let c = if d == 0 then ' ' else pc]
  showPenguins (tail state) pc

{-Some example penguin colonies-}
-- showPenguins (simulate (unspell "5a") (take 60 (repeat 0) ++ [1] ++ take 60 (repeat 0)) 31) '*'

-- showPenguins (simulate (unspell "16") (concat $ take 60 (repeat [0,1])) 60) '*'

-- showPenguins (simulate (unspell "2f") ([1,1] ++ take 30 (repeat 0)) 15) '*'

choose :: Integer -> Integer -> Integer
n `choose` k = product [n-k+1..n] `div` product [1..k]

{-H3.4-}
{-WETT-}
bernoulli :: Integer -> Rational
bernoulli 0 = 1
bernoulli n = if n >= 3 && div n 2 * 2 /= n then 0
              else
              let bernoulli_help n k l1 l2 m = case l1 of [] -> sum l2
                                                          (x:xs) -> if m >= 3 && div m 2  * 2 /= m then bernoulli_help n (k+1) xs ( l2 ++ [0]) (m+1)
                                                                    else bernoulli_help n (k+1) xs ( l2 ++ [Exercise_3.choose n k % (k - n - 1) * x]) (m+1)
              in
              let bernoulli_help_help n k l1 | k == n = last l1
                                             | k == 0 =  bernoulli_help_help n (k+1) (l1 ++ [-1%2 ])
                                             | div k 2  * 2 == k = bernoulli_help_help n (k+1) (l1 ++ [0])
                                             | otherwise = bernoulli_help_help n (k+1) (l1 ++ [bernoulli_help (k+1) 0 l1 [] 0])
              in bernoulli_help_help n 0 [1]
{-TTEW-}
