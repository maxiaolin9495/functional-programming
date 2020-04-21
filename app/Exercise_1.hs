module Exercise_1 where

import Test.QuickCheck

{-H1.1-}
myPair :: Integer -> Integer -> Integer
myPair a b
  | a > 0 = 2 * myPair (a-1) b
  | a == 0 = 2 * b + 1

{-H1.2-}
myFst :: Integer -> Integer
myFst a = if (div a 2  * 2) == a
    then 1 + myFst (div a 2)
    else 0

{-H1.3-}
mySnd :: Integer -> Integer
mySnd a = if (div a 2  * 2 ) == a
    then mySnd (div a 2)
    else div (a - 1) 2

{-H1.4-}
prop_myPair :: Integer -> Integer -> Property
prop_myPair a b = a > 0 && b > 0 ==> myPair a b == myPair (myFst (myPair a b))  (mySnd (myPair a b))

{-H2.1-}
equivMod :: Integer -> Integer -> Integer -> Bool
equivMod n a b = mod (a-b) n == 0

{-H2.2-}
quadRes :: Integer -> Integer -> Bool
{-WETT-}
quadRes n a = not (null [x | x <- [0..(n - 1)], mod (x ^ 2 - a) n == 0])
{-TTEW-}

{-H2.3-}
legendre :: Integer -> Integer -> Integer
legendre n a | equivMod n a 0 = 0
             | quadRes n a = 1
             | not (quadRes n a) = negate 1

{-H2.4-}
isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

prime :: Integer -> Bool
prime n | n <= 1 = False
        | otherwise = n == 2 || n == 3 || not (mod n 6 /= 1 && mod n 6 /= 5)
               && null [x | x <- [2..(isqrt n)], mod n x == 0 || mod n (x + 2) == 0]

{-H2.5-}
prop_eulersCrit :: Integer -> Integer -> Property
prop_eulersCrit p a = mod p 2 == 1 && prime p ==> let a1 = mod (legendre p a) p  in a1 == mod (a^ div (p-1) 2) p
