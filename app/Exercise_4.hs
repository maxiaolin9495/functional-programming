module Exercise_4 where

{-H4.1.1-}
isMultiSet :: Eq a => [(a,Int)] -> Bool
isMultiSet x = let isMultiSet_help [] _ = True
                   isMultiSet_help ((a,i):xs) l = i>0 && notElem a l && isMultiSet_help xs (a:l)
               in isMultiSet_help x []

{-H4.1.2-}
toList :: [(a,Int)] -> [a]
toList = foldl (\ z (a,i) -> z ++ replicate i a) []

{-H4.1.3-}
toSet :: Eq a => [(a, Int)] -> [a]
toSet = map fst

{-H4.1.4-}
toMultiSet :: Eq a => [a] -> [(a, Int)]
toMultiSet x = let addi x [] = [(x,1)]
                   addi x ((a,i):xs) = if x == a then (a,i+1):xs else (a,i) : addi x xs in
               let toMultiSet_help [] l = l
                   toMultiSet_help (x:xs) l = toMultiSet_help xs (addi x l)
               in toMultiSet_help x []

{-H4.1.5-}
multiplicity :: Eq a => a -> [(a, Int)] -> Int
multiplicity x a = case filter ((==x).fst) a of [] -> 0
                                                [(a',i)] -> i

{-H4.1.6-}
dotProduct :: Eq a => [(a, Int)] -> [(a, Int)] -> Int
dotProduct a [] = 0
dotProduct [] b = 0
dotProduct a ((b,i):xs) = case filter ((==b).fst) a of [] -> dotProduct a xs
                                                       [(a',i')] -> i*i' + dotProduct a xs

{-H4.1.7-}
euclidean :: Eq a => [(a, Int)] -> Float
euclidean a = sqrt $ fromIntegral (foldl (\ z (a',i) -> z + i*i) 0 a)

{-H4.1.8-}
cosine :: Eq a => [(a, Int)] -> [(a, Int)] -> Float
cosine a b =  fromIntegral (dotProduct a b) / ( euclidean a * euclidean b)

{-H4.1.9-}
vocabSimilarity :: String -> String -> Float
vocabSimilarity s1 s2= cosine (toMultiSet $ words s1)  (toMultiSet $ words s2)

{-H4.1.10-}
editDistance :: Eq a => [a] -> [a] -> Int
editDistance [] [] = 0
editDistance [] b = length b
editDistance a [] = length a
editDistance [a1] [b1] = if a1 == b1 then 0 else 1
editDistance [a1] (b1:b2:b) = if a1 == b1 then 1 + length b
                                          else minimum
                                                      [1 + editDistance [] (b1 : b2 : b),
                                                       1 + editDistance [a1] (b2 : b),
                                                       1 + editDistance [] (b2 : b)]
editDistance (a1:a2:a) [b1] = if a1 == b1 then 1 + length a
                                          else minimum
                                          [1 + editDistance (a2 : a) [b1],
                                           1 + editDistance (a1 : a2 : a) [],
                                           1 + editDistance (a2 : a) []]
editDistance (a1:a2:a) (b1:b2:b) | a1 == b1 = editDistance (a2 : a) (b2 : b)
                                 | a1 == b2 && a2 == b1 = 1 + editDistance a b
                                 | otherwise =
                                              minimum
                                                  [1 + editDistance (a2 : a) (b1 : b2 : b),
                                                   1 + editDistance (a1 : a2 : a) (b2 : b),
                                                   1 + editDistance (a2 : a) (b2 : b)]

{-H4.1.11-}
{-WETT-}
spellCorrect :: [String] -> [String] -> [[String]]
spellCorrect dict a = let spellCorrect_help [] a mini output = output
                          spellCorrect_help (x:xs) a mini output = let newD = editDistance x a in if newD < mini then spellCorrect_help xs a newD [x]
                                                                                                 else if newD == mini then spellCorrect_help xs a mini (x:output)
                                                                                                 else spellCorrect_help xs a mini output
                      in [ spellCorrect_help dict i 10000 [] | i <- a, spellCorrect_help dict i 10000 [] /= [] ]

{-TTEW-}
