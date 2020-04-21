module Exercise_10 where
import Data.List
import Test.QuickCheck

{-H10.1-}
data Player = V | H -- vertical or horizontal player
  deriving (Eq,Show)
data Field = P Player | E -- a field is occupied by a player or empty
  deriving (Eq,Show)
type Row = [Field]
type Column = [Field]
type Board = [Row] -- we assume that boards are squares and encode a board row by row
data Game = Game Board Player -- stores the currenty board and player
  deriving (Eq,Show)

-- get a given row of a board
row :: Board -> Int -> Row
row = (!!)

-- get a given column of a board
column :: Board -> Int -> Column
column = row . transpose

-- width of a board
width :: Board -> Int
width [] = 0
width (x:xs) = length x

-- height of a board
height :: Board -> Int
height = length

{-H10.1.1-}
showField :: Field -> Char
showField E = '+'
showField (P V) = 'V'
showField (P H) = 'H'

prettyShowBoard :: Board -> String
prettyShowBoard [] = []
prettyShowBoard b = intercalate "\n" [ [ showField f | f<-r ] | r <- b] ++ ['\n']

{-H10.1.2-}
-- position on a board (row, column)
-- (0,0) corresponds to the top left corner
type Pos = (Int, Int)

isValidMove :: Game -> Pos -> Bool
isValidMove (Game rs V) (r, c) | r < 0 || c < 0 || r + 2 > length rs || c + 1> width rs = False
                               | otherwise = head (drop c (head (drop r rs))) == E && head (drop c (head (drop (r+1) rs))) == E
isValidMove (Game rs H) (r, c) | r < 0 || c < 0 || r + 1> length rs || c + 2 > width rs = False
                               | otherwise = head (drop c (head (drop r rs))) == E && head (drop (c+1) (head (drop r rs))) == E
{-H10.1.3-}
canMove :: Game -> Bool
canMove (Game rs p) = or[or[isValidMove (Game rs p) (r,c) | c<-[0..width rs -1]] | r<- [0..length rs - 1]]

{-H10.1.4-}
updateBoard :: Board -> Pos -> Field -> Board
updateBoard rs (r,c) p = let row = head $ drop r rs
                             beforeC = take c row
                             afterC = drop (c+1) row in
                         take r rs ++ [beforeC ++ p :afterC] ++ drop (r+1) rs

{-H10.1.5-}
playMove :: Game -> Pos -> Game
playMove (Game rs V) (r,c) = Game (updateBoard (updateBoard rs (r,c) (P V)) (r+1, c) (P V)) H
playMove (Game rs H) (r,c) = Game (updateBoard (updateBoard rs (r,c) (P H)) (r, c+1) (P H)) V

{-H10.1.6-}
-- the first paramter of a strategy is an infite list of
-- random values between (0,1) (in case you wanna go wild with
-- probabilistic methods)
type Strategy = [Double] -> Game -> Pos

{-WETT-}

getValidMoveInRow :: Game -> Int -> Maybe Pos
getValidMoveInRow (Game rs V) r = let result = filter snd [((r, c), isValidMove (Game rs V) (r,c)) | c <- [0..11]] in
                                  if null result then Nothing else Just (fst (head result))

getValidMoveInRow (Game rs H) r = let result = if even r then filter snd [((r, c), isValidMove (Game rs H) (r,c)) | c <- [9,6,3,0]] else filter snd  [((r, c), isValidMove (Game rs H) (r,c)) | c <- [10,7,4,1]] in
                                  if null result then Nothing else Just (fst (head result))

getValidMoveInColumn :: Game -> Int -> Maybe Pos
getValidMoveInColumn (Game rs H) c = let result = filter snd [((r,c), isValidMove (Game rs H) (r,c)) | r <- [0..11]] in
                                     if null result then Nothing else Just (fst (head result))

getValidMoveInColumn (Game rs V) c = let result = if even c then filter snd [((r, c), isValidMove (Game rs V) (r,c)) | r <- [0,3,6,9]] else filter snd  [((r, c), isValidMove (Game rs V) (r,c)) | r <- [1,4,7,10]] in
                                     if null result then Nothing else Just (fst (head result))

getResult :: Maybe Pos -> Pos
getResult (Just (r,c)) = (r,c)
christmasAI :: Strategy -- receives a game and plays a move for the next player
christmasAI (d:ds) (Game rs p) = case p of V ->  case filter ( /= Nothing) [ getValidMoveInColumn (Game rs p) c | c <- [0..11]] of
                                                             [] -> getResult $ head (filter ( /= Nothing) [getValidMoveInRow (Game rs p) r | r<- [0..11]])
                                                             rs -> getResult $ head rs
                                           H ->  case filter ( /= Nothing) [ getValidMoveInRow (Game rs p) c | c <- [11,10..0]] of
                                                             [] -> getResult $ head (filter ( /= Nothing) [getValidMoveInColumn (Game rs p) c | c <- [11,10..0]])
                                                             rs -> getResult $ head rs
{-TTEW-}

{-H10.1.7-}
createBoard :: Int -> Game
createBoard dim = Game [ [ E | f <- [1..dim]] | r <- [1..dim]] V
play :: [[Double]] -> Int -> Strategy -> Strategy -> ([Board],Player)
play ds dim sv sh = let play_help (d:ds) (Game b H) sv sh | canMove (Game b H) = let move = sh d (Game b H) in if isValidMove (Game b H) move then (b,H) : play_help ds (playMove (Game b H) move) sv sh
                                                                                                                                              else [(b,V)]
                                                          | otherwise = [(b, V)]
                        play_help (d:ds) (Game b V) sv sh | canMove (Game b V) = let move = sv d (Game b V) in if isValidMove (Game b V) move then (b,V) : play_help ds (playMove (Game b V) (sv d (Game b V))) sv sh
                                                                                                                                              else [(b, H)]
                                                          | otherwise = [(b, H)]
                    in
                    let result = play_help ds (createBoard dim) sv sh
                    in ([ b |(b,p) <- tail result], snd (last result))

-- generates infinite list of values between (0,1)
genRandomZeroOne :: Gen [Double]
genRandomZeroOne = mapM (const $ choose (0::Double,1)) [1..]


data Test = Essential String Bool |
            NonEssential String Bool |
            Group String [ Test ]

showFailedTests [] = []
showFailedTests (x:xs) = case x of Group s l -> map (\ a -> s ++ "/" ++ a) (showFailedTests l) ++ (showFailedTests xs)
                                   Essential s False -> s : showFailedTests xs
                                   NonEssential s False -> s: showFailedTests xs
                                   _ -> showFailedTests xs

select y = map snd . filter ((==y).fst)
-- plays a game and prints it to the console
playAndPrint :: Int -> Strategy -> Strategy -> IO ()
playAndPrint dim sh sv = do
  rss <- generate $ mapM (const $ genRandomZeroOne) [1..]
  let (bs, w) = play rss dim sh sv
  putStr $ (unlines $ map prettyShowBoard bs) ++ "\nWinner: " ++ show w ++ "\n"
