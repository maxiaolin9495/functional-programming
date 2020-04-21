module Exercise_6 where

import Data.Ratio

{-WETT-}
traceFractran :: [Rational] -> Integer -> [Integer]
traceFractran l n = n : case [ i  | i <- l, n `mod` denominator i == 0 ] of
                    [] -> []
                    i:is -> traceFractran l $ truncate (fromIntegral n * i)
{-TTEW-}
