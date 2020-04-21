module Exercise_12 where

import Data.List (nub)
import System.IO


{-H12-}
isPrime :: Int -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = primeAux (n-1)
  where
    primeAux 1 = True
    primeAux i = n `mod` i /= 0 && primeAux (i-1)

-- returns all primes up to the passed number
primes :: Int -> [Int]
primes n = [i | i<-[2..n], isPrime i]

{-WETT-}
getNths str [] i (a,b) = (a, b ++ str)
getNths str ns i (a,b)= if head ns == i then getNths (drop 1 str) (drop 1 ns) (i+1) (a ++ [head str], b) else getNths (drop 1 str) ns (i+1) (a, b++[head str])

rebuild str [] i [] = str
rebuild str ns i fs = if head ns == i then head fs: rebuild str (drop 1 ns) (i+1) (drop 1 fs) else head str : rebuild (drop 1 str) ns (i+1) fs

encrypt :: String -> String
encrypt s = uncurry (++) $ getNths s (primes (length s)) 1 ([],[])

decrypt :: String -> String
decrypt s = let p = primes $ length s in rebuild (drop (length p) s) p 1 $ take (length p ) s

{-TTEW-}

main :: IO ()
main = doCommand
  where doCommand = do
          f <- getLine
          command <- getLine
          if command == "encrypt" then do
            content <- readFile f
            writeFile (f++".encrypt") (encrypt content)
          else do
            content <- readFile f
            writeFile (f++".decrypt") (decrypt content)


data Term = App Term Term | Abs String Term | Var String
instance Show Term where
  show (Var x) = x
  show (Abs x t) = "(\\" ++ x ++ " -> " ++ show t ++ ")"
  show (App t1 (App t2 t3)) = show t1 ++ " (" ++ show t2 ++ " " ++ show t3 ++ ")"
  show (App t1 t2) = show t1 ++ " " ++ show t2

freeVars :: Term -> [String]
freeVars = freeVarsB []
  where
    freeVarsB bs (Var x)
      | x `elem` bs = []
      | otherwise = [x]
    freeVarsB bs (App t1 t2) = nub $ freeVarsB bs t1 ++ freeVarsB bs t2
    freeVarsB bs (Abs x t) = freeVarsB (x:bs) t

substVar :: String -> Term -> Term -> Term
substVar v term (Var x)
  | v == x = term
  | otherwise = Var x
substVar v term (Abs x t)
  | v == x = Abs x t
  | otherwise = Abs x (substVar v term t)
substVar v term (App t1 t2) = App (substVar v term t1) (substVar v term t2)

instance Eq Term where
  x@(App t1 t2) == y@(App t3 t4) = t1 == t3 && t2 == t4
  x@(Var t1) == y@(Var t3) = t1 == t3
  x@(Abs t1 t2@(Var v)) == y@(Abs t3 t4@(Var v')) = (t1 == v && t3 == v') || (t1 /= v && t3 /= v' && v == v')
  x@(Abs t1 t2@(App t7 t8)) == y@(Abs t3 t4@(App t5 t6)) = (Var t1 == t7 && Var t3 == t5 && t6 == t8) || (Var t1 == t8 && Var t3 == t6 && t5==t7)
    || freeVars x == freeVars y --( Var t1 /= t7 && Var t1 /= t8 && Var t3 /= t5 && Var t3 /= t6 && t2 ==t4)
  x@(Abs t1 t2@(Abs t7 t8)) == y@(Abs t3 t4@(Abs t5 t6)) | t1 == t3 = t2 == t4
                                                         | otherwise = freeVars x == freeVars y
  x@(Abs t1 t2@(Var v)) == y@(Abs t3 t4@(Abs t5 t6)) = t1 == v && t3 == t5 && t2 == t4
  y@(Abs t3 t4@(Abs t5 t6)) == x@(Abs t1 t2@(Var v)) = t1 == v && t3 == t5 && t2 == t4
  x@(Var t1) == y@(Abs t3 t4@(Abs t5 t6)) = t1 == t3 && Var t3 == t6
  y@(Abs t3 t4@(Abs t5 t6)) == x@(Var t1) = t1 == t3 && Var t3 == t6
  _ == _ = False

betaRed :: Term -> Term
betaRed = undefined
