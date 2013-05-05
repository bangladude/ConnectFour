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
  deriving Show

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
  putStrLn "Player 1: X"
  putStrLn "Player 2: 0"
  putStr (show emptyBoard)
  playGame emptyBoard True

{- This is the method that is called after each move. It checks if there is a
   winner or if the board is full. Then it calls the appropriate move function.
   The boolean value player1 is true if it is the user's turn. -}
playGame :: Board -> Bool -> IO ()
playGame b player1
  | checkWinner X b = putStrLn "Player 1 wins!"
  | checkWinner O b = putStrLn "Player 2 wins!"
  | full b          = putStrLn "The board is full. The game ends in a tie."
  | player1         = player1Turn b
  | otherwise       = player2Turn b

{- This function reads input from the user and tries to move to that space. -}
player1Turn :: Board -> IO ()
player1Turn b = do 
  putStrLn ""
  putStrLn "Player 1's move. Type any column number from 1 to 7."
  attempt <- getLine
  case attempt of
    []     -> putStrLn "Invalid input." >> player1Turn b
    (x:[]) -> if C.isDigit x
                then case (move b X (read attempt)) of
                       Nothing -> putStrLn "Move failed." >> player1Turn b
                       Just b' -> putStrLn "Move successful." >>
                         putStr (show b') >> playGame b' False
                else putStrLn "Invalid input." >> player1Turn b
    _      -> putStrLn "Invalid input." >> player1Turn b

{- This function is the same as player1Turn but it sets the booleans and
   moves differently since it is a different player. -}
player2Turn :: Board -> IO ()
player2Turn b = do 
  putStrLn ""
  putStrLn "Player 2's move. Type any column number from 1 to 7."
  attempt <- getLine
  case attempt of
    []     -> putStrLn "Invalid input." >> player2Turn b
    (x:[]) -> if C.isDigit x
                then case (move b O (read attempt)) of
                       Nothing -> putStrLn "Move failed." >> player2Turn b
                       Just b' -> putStrLn "Move successful." >>
                         putStr (show b') >> playGame b' True
                else putStrLn "Invalid input." >> player2Turn b
    _      -> putStrLn "Invalid input." >> player2Turn b