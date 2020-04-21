module Exercise_7 where

{-H7.2-}
matchesPath :: String -> String -> Bool
matchesPath [] (i:input) = False
matchesPath [] [] = True
matchesPath (p:path) [] = p == '*' && matchesPath path []
matchesPath (p:path) (i:input) | p == i = matchesPath path input
                               | p == '{' = matchList path (i:input) "" []
                               | otherwise = ((p == '?') && matchesPath path input) || ((p == '*') && matchStar path (i:input) 0)

matchStar :: String -> String -> Integer -> Bool
matchStar [] [] starNum = True
matchStar (p:path) [] starNum = p == '*'
matchStar [] (i:input) starNum | i =='/' = starNum >= 1 && matchStar [] input (starNum - 1)
                               | otherwise = matchStar [] input starNum
matchStar (p:path) (i:input) starNum | p == '*' = matchStar path (i:input) (starNum + 1)
                                     | p == i && p /= '/' = matchStar (p:path) input starNum || matchesPath path input
                                     | p == i && p == '/' = if starNum >= 1 then matchStar (p:path) input (starNum - 1) || matchStar path input starNum
                                                            else matchStar path input starNum
                                     | otherwise = matchStar (p:path) input starNum


matchList :: String -> String -> String -> [String]-> Bool

matchList (p:path) input this stringStore | p == ',' = matchList path input "" (reverse this :stringStore)
                                          | p == '}' = or [matchesPath (s ++ path) input | s <- reverse this:stringStore]
                                          | otherwise = matchList path input (p:this) stringStore



{-H7.3-}
comp :: Eq b => [(Integer,(a,b))] -> [(Integer,(b,c))] -> [(Integer,(a,c))]
comp = undefined

symcl :: Eq a => [(Integer,(a,a))] -> [(Integer,(a,a))]
symcl = undefined

{-WETT-}
trancl :: Eq a => [(Integer,(a,a))] -> [(Integer,(a,a))]
trancl = undefined
{-TTEW-}
