module Exercise_11 where
import Data.List
import System.IO
import Text.Read (readMaybe)

type Name = String
type Data = String
data FSItem = File Name Data | Directory Name [FSItem]
  deriving (Eq, Show)

getAllDirectory :: [FSItem] ->  [FSItem]
getAllDirectory [] = []
getAllDirectory (f@(File n d) :fs) = getAllDirectory fs
getAllDirectory (f@ (Directory n l) :fs) = f : getAllDirectory fs

getAllFiles :: [FSItem] ->  [FSItem]
getAllFiles [] = []
getAllFiles (f@(File n d) :fs) = f: getAllFiles fs
getAllFiles (f@(Directory n l) :fs) = getAllFiles fs

pretty :: FSItem -> String
pretty f@(File n d) = n ++ " \"" ++ d ++ "\"\n"
pretty f@(Directory n []) = n ++ "/\n"
pretty f@(Directory "" l) = "/\n" ++ intercalate "\n" (lines ("|- " ++ intercalate "|- " (sort[pretty f | f <- getAllDirectory l] ++ sort[pretty f | f <- getAllFiles l]))) ++ "\n"
pretty f@(Directory n l) =  n ++ "/\n  " ++ intercalate "\n  " (lines ("|- " ++ intercalate "|- " (sort [pretty f | f <- getAllDirectory l] ++ sort [pretty f | f <- getAllFiles l]))) ++ "\n"


getNth ::[String] -> Int -> Int -> String
getNth (s:ss) n i | i == n = s
                   | i < n = getNth ss n (i+1)


--split the input line into parts
split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

--check if contains the Item
update :: [FSItem] -> FSItem -> [FSItem]
update [] f = [f]
update (f@(File n d):fs) f'@(File n' "") = if n == n' then File n d : fs else f: update fs f'
update (f@(Directory n _):fs) f'@(File n' "") = f: update fs f'
update (f@(File n _):fs) f'@(Directory n' []) =  f: update fs f'
update (f@(Directory n l):fs) f'@(Directory n' []) =  if n == n' then Directory n l : fs else f: update fs f'

mkdir :: FSItem -> [String] -> FSItem
mkdir f@(File n d) _ = f
mkdir f@(Directory n l) [c] = Directory n (Directory c [] : l )
mkdir f@(Directory n l) (c:[c']) = if n == c then Directory n (update l (Directory c' [])) else f
mkdir f@(Directory n l) (c:cs) = if n == c then Directory n [mkdir f' cs |  f' <- l] else f

-- remove an item if the name matches
rm_help :: [FSItem] -> String -> [FSItem]
rm_help [] _ = []
rm_help (f@(File n d):fs) c = if n == c then fs else f : rm_help fs c
rm_help (f@(Directory n l):fs) c = if n == c then fs else f : rm_help fs c

rm :: FSItem -> [String] -> FSItem
rm f@(File n d) (c:cs) = f
rm f@(Directory n l) [c] = f
rm f@(Directory n l) (c:[c']) = if n == c then Directory n (rm_help l c') else f
rm f@(Directory n l) (c:cs) = if n == c then Directory n [rm f' cs |  f' <- l] else f

touch :: FSItem -> [String] -> FSItem
touch f@(File n d) _ = f
touch f@(Directory n l) [c] = Directory n (l ++ [File c []])
touch f@(Directory n l) (c:[c']) = if n == c then Directory n (update l (File c' "")) else f
touch f@(Directory n l) (c:cs) = if n == c then Directory n [touch f' cs |  f' <- l] else f

edit :: FSItem -> [String] -> String -> FSItem
edit f@(File n d) [c] t = if n == c then File n t else f
edit f@(File n d) c t = f
edit f@(Directory n l) [c] t = f
edit f@(Directory n l) (c:cs) t = if n == c then Directory n [edit f' cs t |  f' <- l] else f


readValue :: String -> String
readValue s = drop 1 (take (length s - 1) s)

main :: IO ()
main =
  doCommand (Directory "" [])
      where
        doCommand = do
          input <- getLine
          if isPrefixOf "mkdir" input then do
                putStrLn (pretty (mkdir f (split '/'(getNth (words input) 2 1))))
                doCommand (mkdir f (split '/'(getNth (words input) 2 1)))
          else if isPrefixOf "rm" input then do
                putStrLn (pretty (rm f (split '/'(getNth (words input) 2 1))))
                doCommand (rm f (split '/'(getNth (words input) 2 1)))
          else if isPrefixOf "touch" input then do
                putStrLn (pretty (touch f (split '/'(getNth (words input) 2 1))))
                doCommand (touch f (split '/'(getNth (words input) 2 1)))
          else if isPrefixOf "edit" input then do
                putStrLn (pretty (edit f (split '/'(getNth (words input) 2 1)) (readValue ( unwords (drop 2 (words input) )))))
                doCommand (edit f (split '/'(getNth (words input) 2 1)) (readValue (unwords (drop 2 (words input)) )))
          else if isPrefixOf "quit" input then putStrLn "Bye!"
          else putStrLn ""
