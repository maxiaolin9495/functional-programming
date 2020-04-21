module Exercise_11_Tut where

import System.IO
import System.Random (randomRIO)
import Text.Read (readMaybe)

{-T11.1.1-}
{- f u v = min (head u) (last (concat v))
Step 1
- u :: a, v :: b
Step 2
- head :: [c] -> c
- concat :: [[d]] -> [d]
- last :: [e] -> e
- min :: Ord f => f -> f -> f
Step 3
- from "head u" derive "a = [c]"
- from "concat v" derive "b = [[d]]"
- from "last (concat v)" derive "[d] = [e]"
- from "min (head u) (last (concat v))" derive "c = f" and "e = f"
Step 4
- apply "a = [c]" and update:
  - u :: [c]
- apply "b = [[d]]" and update:
  - v :: [[d]]
- apply "[d] = [e]" to get "d = e" and update:
  - v :: [[e]]
  - concat :: [[e]] -> [e]
- apply "c = f" and update:
  - u :: [f]
  - head :: [f] -> f
- apply "e = f" and update:
  - v :: [[f]]
  - concat :: [[f]] -> [f]
  - last :: [[f]] -> [f]
- no simplifications possible. Return:
  f :: Ord f => [f] -> [[f]] -> f
-}
{-T11.1.2-}
{- ffoldl = foldl . foldl
Step 1
no variables 
Step 2
- foldl :: (a -> b -> a) -> a -> [b] -> a
- (.) :: (d -> e) -> (c -> d) -> c -> e
- foldl :: (f -> g -> f) -> f -> [g] -> f
Step 3
- from "(.) foldl" derive "d -> e = (a -> b -> a) -> a -> [b] -> a"
- from "(.) foldl foldl" derive "c -> d = (f -> g -> f) -> f -> [g] -> f"
- also note ffoldl :: c -> e
Step 4
- from "d -> e = (a -> b -> a) -> a -> [b] -> a":
  - d = (a -> b -> a)
  - e = a -> [b] -> a
- from "c -> d = (f -> g -> f) -> f -> [g] -> f":
  - c = (f -> g -> f)
  - d = f -> [g] -> f
- from "d = (a -> b -> a)" and "d = f -> [g] -> f":
  - a = f
  - b = [g]
- apply "a = f" and "b = [g]" and update:
  - d = f -> [g] -> f
  - e = f -> [[g]] -> f
- no more simplification possible.
  ffoldl :: (f -> g -> f) -> f -> [[g]] -> f 
 
We can use this to fold a function over a list of lists, treating it as a flat list without explicit concatenation
e.g. ffoldl (+) 0 [[5], [1,2,3]] == 11 
-}
{-T11.1.3-]-}
{- f x y = y : map (++x) y
Step 1
x :: a, y :: b
Step 2
- (:) :: c -> [c] -> [c]
- map :: (d -> e) -> [d] -> [e]
- (++) :: [f] -> [f] -> [f]
Step 3
- from "++x" derive a = [f]
- from "map (++x) y" derive d -> e = [f] -> [f] and [d] = b
- from "y : map (++x) y" derive c = b and [c] = [e]
Step 4
- from "d -> e = [f] -> [f]"
  - d = [f]
  - e = [f]
- from "c = b" and "[d] = b" 
  - c = [d]
- from "d = [f]" and "c = [d]"
  - c = [[f]]
- from "c = [[f]]" and "[c] = [e]"
  - e = [[f]]
- contradiction "e = [[f]]" and "e = [f]"
  implies "f = [f]"

--> Type Error: Occurs Check
-}

{-T11.2-}
ioLoop :: (a -> Maybe b) -> IO a -> IO b
ioLoop f act = do
    x <- act
    case f x of
        Nothing -> ioLoop f act
        Just b -> return b

getInt :: IO Int
getInt = ioLoop readMaybe getLine

{-T11.3-}
guessNum :: IO Int
guessNum = do
    rnd <- randomRIO (0,100)
    putStrLn "Guess a number between 0 and 100"
    doGuessNum rnd 1
  where
    doGuessNum rnd cnt = do
      num <- getInt
      if num < rnd then do
        putStrLn "The number you are looking for is greater"
        doGuessNum rnd (cnt+1)
      else if num > rnd then do
        putStrLn "The number you are looking for is smaller"
        doGuessNum rnd (cnt+1)
      else do
        putStrLn "You found it!"
        return cnt

{-T11.4-}
g :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
g f _ (Left  x) = Left  (f x)
g _ f (Right x) = Right (f x)

-- g f1 f2 = either (Left . f1) (Right . f2)

h :: (a -> Either a b) -> a -> b

h f x = case f x of
          Left  x' -> h f x'
          Right y  -> y

-- h f = either (h f) id . f
