{-# OPTIONS_GHC -Wall #-}

module ConnectFour where

import Data.Map as M
import Data.List as L
import Prelude as P
import Data.Char as C

{- The first number in the key for piecesMap is the row. The heightMap keeps
   track of full columns -}
data Board = B { piecesMap :: Map (Int, Int) Player,
                 heightMap :: Map Int Int }

instance Show Board where
  show = boardString

{- This function recursively shows each row of the board plus a newline char -}
boardString :: Board -> String
boardString = show' 6
  where show' :: Int -> Board -> String
        show' 0 _ = ""
        show' n b = show (P.map getPlayer (row n 1 b)) ++ "\n" ++ 
                    (show' (n-1) b)
          where getPlayer :: Maybe Player -> String
                getPlayer (Just p) = show p
                getPlayer Nothing = " "

data Player = X | O
  deriving (Show, Eq)

{- The third argument is the column that the player wants to use. Column must
   be between 1 and 7 inclusive. If the argument is invalid or the column is
   full then the move fails and returns Nothing. -}
move :: Board -> Player -> Int -> Maybe Board
move (B pieces height) player c
  | c < 1 || c > 7              = Nothing
  | M.lookup c height == Just 6 = Nothing
  | otherwise                   = 
      case (M.lookup c height) of
        Nothing -> 
          Just $ B (M.insert (1, c) player pieces) (M.insert c 1 height)
        Just y  -> 
          Just $ B (M.insert (y+1, c) player pieces) (M.insert c (y+1) height)

{- This function checks whether p has won the game. -}
checkWinner :: Player -> Board -> Bool
checkWinner p board = checkLists p (rows 1 board) || 
                         checkLists p (cols 1 board) ||
                         checkLists p (diags 1 board)

{- This function applies checkList to each list in a list of lists-}
checkLists :: Player -> [[Maybe Player]] -> Bool
checkLists _ []     = False
checkLists p (x:xs) = checkList 0 p x || checkLists p xs

{- This function counts whether there exists a chain of four spaces that all
   contain the player of interest. -}
checkList :: Int -> Player -> [Maybe Player] -> Bool
checkList _ _ []            = False
checkList 4 _ _             = True
checkList n X ((Just X):xs) = checkList (n+1) X xs
checkList n O ((Just O):xs) = checkList (n+1) O xs
checkList _ p (_:xs)        = checkList 0 p xs

{- This function returns a list of all the lists of players, arranged by row.
   The first argument is the number of the row from top to bottom. -}
rows :: Int -> Board -> [[Maybe Player]]
rows 7 _ = []
rows n b = row n 1 b : (rows (n+1) b)

{- This function returns a list of the players in a given row. The first
   argument is the number of the row, and the second argument is the position
   within the row. -}
row :: Int -> Int -> Board -> [Maybe Player]
row _ 8 _ = []
row n x b = M.lookup (n, x) (piecesMap b) : (row n (x+1) b)

{- Same as rows. The first argument is the number of the column from left to
   right. -}
cols :: Int -> Board -> [[Maybe Player]]
cols 8 _ = []
cols n b = col n 1 b : (cols (n+1) b)

{- Same as row. -}
col :: Int -> Int -> Board -> [Maybe Player]
col _ 7 _ = []
col n x b = M.lookup (x, n) (piecesMap b) : (col n (x+1) b)

{- The first argument is the number of the diagonal from left to right. All 
   diagonals list elements starting at row 1. Only the diagonals that have at
   least four spaces are evaluated. -}
diags :: Int -> Board -> [[Maybe Player]]
diags 7 _ = []
diags n b  = diagUR n (-1) b : ((diagUL n 9 b) : (diags (n+1) b))

{- This function lists players in diagonals which go up and to the right from
   row 1. -}
diagUR :: Int -> Int -> Board -> [Maybe Player]
diagUR _ 7 _ = []
diagUR n x b = M.lookup (n, x) (piecesMap b) : (diagUR (n+1) (x+1) b)

{- This function lists players in diagonals which go up and to the left from
   row 1. -}
diagUL :: Int -> Int -> Board -> [Maybe Player]
diagUL _ 7 _ = []
diagUL n x b = M.lookup (n, x) (piecesMap b) : (diagUL (n+1) (x-1) b)

{- This function returns true if each row contains 7 players -}
full :: Board -> Bool
full b = 6 == length (takeWhile (==7) 
         $ P.map (length . (takeWhile fun)) (rows 1 b))
  where fun :: Maybe Player -> Bool
        fun Nothing  = False
        fun (Just _) = True

{- This is the starting point for the game. -}
emptyBoard :: Board
emptyBoard = B (fromList []) (fromList [])

{- First the player symbols are printed to the screen. -}
main :: IO ()
main = do 
  putStrLn "User: X"
  putStrLn "Computer: 0"
  putStr (show emptyBoard)
  playGame emptyBoard True

{- This is the method that is called after each move. It checks if there is a
   winner or if the board is full. Then it calls the appropriate move function.
   the boolean value user is true if it is the user's turn. -}
playGame :: Board -> Bool -> IO ()
playGame b user
  | checkWinner X b = putStrLn "You win!"
  | checkWinner O b = putStrLn "The Computer wins!"
  | full b          = putStrLn "The board is full. The game ends in a tie."
  | user            = playersTurn b
  | otherwise       = computersTurn b

{- This function reads input from the user and tries to move to that space. -}
playersTurn :: Board -> IO ()
playersTurn b = do
  putStrLn ""
  putStrLn "User's move. Type any column number from 1 to 7."
  attempt <- getLine
  case attempt of
    []     -> putStrLn "Invalid input." >> playersTurn b
    (x:[]) -> if C.isDigit x
                then case (move b X (read attempt)) of
                       Nothing -> putStrLn "Move failed." >> playersTurn b
                       Just b' -> putStrLn "Move successful." >>
                         putStr (show b') >> playGame b' False
    else putStrLn "Invalid input." >> playersTurn b
    _      -> putStrLn "Invalid input." >> playersTurn b

{- This function tries to move to the space given by getMove. -}
computersTurn :: Board -> IO ()
computersTurn b = do
  putStrLn ""
  attempt <- return (getMove b)
  case (move b O attempt) of
    Nothing -> putStrLn "Computer's move failed." >> computersTurn b
    Just b' -> putStrLn "Computer's move successful." >>
               putStr (show b') >> playGame b' True

{- This function zips a list of valid boards with the list of moves that 
   resulted in those boards. -}
genBoard :: Board -> Player -> [(Board, Int)]
genBoard b p = prune $ zip (P.map (move b p) [1..7]) [1..7]

{- This function takes a list of (Maybe Board, Int) and returns the list of
   (Board, Int) that results when all the elements containing Nothing are
   removed. -}
prune :: [(Maybe Board, Int)] -> [(Board, Int)]
prune [] = []
prune ((Nothing, _):xs)  = prune xs
prune (((Just b), n):xs) = (b, n) : (prune xs)

{- This function returns the move that gave the highest value in negamax. -}
getMove :: Board -> Int
getMove b = snd $ maximum $ P.map (negamax 3 X) (genBoard b O)

{- This function evaluates boards from the perspective of the computer. The
   first argument is the depth in the game tree (starting from 3 and ending at
   0). The second argument is the player who just moved. The final argument is
   the pair of the board to be evaluated and the associated move. The function
   returns a pair of (value of best move, top level move that generated that
   value). I used pseudocode from Wikipedia to help me write this function. -}
negamax :: Int -> Player -> (Board, Int) -> (Int, Int)
negamax 0 p (b, m) = case p of
                      O -> (value b, m)
                      X -> (-(value b), m)
negamax _ p (b, m)
  | full b || checkWinner X b || checkWinner O b = case p of
                                                     O -> (value b, m)
                                                     X -> (-(value b), m)
negamax d p (board, m) =
  ((maximum $ P.map ((*(-1)) . fst) $ P.map (negamax (d-1) (opp p))
    (genBoard board (opp p))), m)

{- Returns the opposite of the given player -}
opp :: Player -> Player
opp X = O
opp O = X

{- This function evaluates a given board from the perspective of the computer.
   If the game is not over, it calles score to evaluate the value of each 
   player's positions. -}
value :: Board -> Int
value b
  | checkWinner O b = 100000
  | checkWinner X b = -100000
  | full b          = 0
  | otherwise       = score b

{- This method is a placeholder since the AI still doesn't prevent the user
   from winning at matchpoint. -}
score :: Board -> Int
score b = 100*((waysToWin list O) - (waysToWin list X))
  where list = cols 1 b ++ (rows 1 b) ++ (diags 1 b)

waysToWin :: [[Maybe Player]] -> Player -> Int
waysToWin [] _ = 0
waysToWin (x:xs) p = countThrees x p + (waysToWin xs p)

countThrees :: [Maybe Player] -> Player -> Int
countThrees [] _ = 0
countThrees (x:xs) p 
  | countP p (take 4 (x:xs)) 0 = 1 + (countThrees xs p)
  | otherwise                  = countThrees xs p

countP :: Player -> [Maybe Player] -> Int -> Bool
countP _ [] 3 = True
countP _ [] _ = False
countP p (Nothing:xs) counter = countP p xs counter
countP p ((Just x):xs) counter
  | p == x    = countP p xs (counter+1)
  | otherwise = False

-------------------------------------------------------------------------------
{-
This is my second to last attempt at writing Minimax.

getMove :: Board -> Int
getMove b = snd . maximum $ (zip (Prelude.map (negamax 1 b O) [1..7]) [1..7])

-- first arg is the depth in the game tree
-- second arg is the current board
-- third arg is the player who is moving 
-- fourth arg is the move to be evaluated
-- returns the best move at this stage
negamax :: Int -> Board -> Player -> Int -> Int
negamax 4 b _ _ = value b
negamax _ b _ _
  | checkWinner X b = -100000
  | checkWinner O b = 100000
  | full b          = 0
negamax n b p x = 
  case (move x b p) of
    Nothing -> -999999
    Just b' -> 
      case p of
        O -> minimum $ Prelude.map (negamax (n+1) b' X) [1..7]
        X -> maximum $ Prelude.map (negamax (n+1) b' O) [1..7]

value :: Board -> Int
value b = score b

--always evaluates score for the user
score :: Board -> Int
score b 
  | checkWinner X b = -100000
  | checkWinner O b = 100000
  | full b          = 0
  | otherwise       = evaluate b O

-- used for testing
wonBoard :: Board
wonBoard =
  case (move 1 emptyBoard X) of
    Nothing -> emptyBoard
    Just b' -> 
      case (move 1 b' X) of
        Nothing -> b'
        Just b'' -> b''{-
          case (move 1 b'' X) of
            Nothing -> b''
            Just b''' -> b'''-}{-
              case (move 1 b''' X) of
                Nothing -> b'''
                Just b'''' -> b''''-}

evaluate :: Board -> Player -> Int
evaluate b p = 10

-}