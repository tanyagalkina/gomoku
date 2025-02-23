module Main (main) where

import Lib
import System.Exit (exitSuccess)
import Text.Printf (printf)
import Data.List (findIndex)
-- import System.Random (randomRIO) cannot build this in docker
-- the piskvork protocol requires NoBuffering to be set
-- https://plastovicka.github.io/protocl2en.htm
import System.IO (hSetBuffering, hPutStrLn, BufferMode( NoBuffering ), hFlush, stdout, stderr)


-- The brain is required to process commands START, BEGIN, INFO, BOARD, TURN, END
-- START [size]
-- TURN [X],[Y]
-- BEGIN
-- 1 = own stone, 2 = opponent's stone
-- BOARD
--   10,10,1
--   10,11,2
--   11,11,1
--   9,10,2
--   DONE
-- INFO [key] [value]
-- END 
-- ABOUT


type Board = [[Int]] 
-- implement only square board right now
emptyBoard :: Int -> Board
emptyBoard size = replicate size (replicate size 0)

printBoard :: Board -> String
printBoard board =
    let size = length board
        header = "   " ++ unwords (map (printf "%d" . (`mod` 10)) [0..size-1])
        rows = unlines (map formatRow (zip [0..size-1] board))
    in unlines [header, rows]
  where
    formatRow (rowIndex, row) = printf "%2d" rowIndex ++ " " ++ unwords (map showCell row)
    showCell 0 = "."  -- Empty cell
    showCell 1 = "X"  -- Own player
    showCell 2 = "0"  -- Opponent
    showCell _ = "?"  -- Unexpected values

enterOpponentsMove :: Int -> Int -> Board -> Board
enterOpponentsMove x y board  = 
    take y board ++                                
    [replaceAtIndex x 2 (board !! y)] ++          -- Update the y-th row
    drop (y + 1) board

enterMyMove :: Int -> Int -> Board -> Board
enterMyMove x y board  = 
    take y board ++                                -- Keep rows before y
    [replaceAtIndex x 1 (board !! y)] ++          -- Update the y-th row
    drop (y + 1) board    

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i newVal row =
    take i row ++ [newVal] ++ drop (i + 1) row

-- TODO: implement random for now to test the protocol
-- myMove :: Board -> IO (Board, String)
-- myMove board = do 
--     let size = length board
--     x <- randomRIO (0, size - 1)
--     y <- randomRIO (0, size - 1)

--     if (board !! y !! x) == 0
--         then do
--             let newBoard = enterMyMove x y board
--             return (newBoard, show x ++ "," ++ show y)
--         else myMove board   

myMove :: Board -> IO (Board, String)
myMove board = return (newBoard, show x ++ "," ++ show y)
  where
    -- Find the first empty spot (0) on the board
    (x, y) = findFirstEmpty board 0 0
    
    -- Update the board with the move
    newBoard = enterMyMove x y board

findFirstEmpty :: Board -> Int -> Int -> (Int, Int)
findFirstEmpty [] _ _ = error "No empty spaces left"  -- This shouldn't happen in a normal game
findFirstEmpty (row:rows) y x = 
    case findIndex (== 0) row of
        Just x' -> (x', y)   -- Found an empty cell at (x', y)
        Nothing -> findFirstEmpty rows (y + 1) 0     

main :: IO ()
main = do
         hSetBuffering stdout NoBuffering
         gameLoop $ emptyBoard 20 

gameLoop :: Board -> IO ()
gameLoop board = do
    input <- getLine
    -- hPutStrLn stderr $ "DEBUG: Received input: " ++ input  -- Print debug info to stderr
    -- hFlush stderr  -- Flush stderr to ensure it prints immediately
    case words input of
        ["START", size] -> do
            let boardSize = read size :: Int
            newBoard <- return (emptyBoard boardSize)
            -- hPutStrLn stderr $ printBoard newBoard
            -- hFlush stderr
            putStrLn "OK"
            gameLoop newBoard
        ["BOARD"] -> do
            hPutStrLn stderr $ "DEBUG: BOARD COMMAND NOT IMPLEMENTED YET"
            hFlush stderr
            -- newBoard <- sequence reading
            -- hPutStrLn stderr $ printBoard board
            -- hFlush stderr
            gameLoop board 

        ["ABOUT"] -> do 
            putStrLn "name=\"MyUnrealPlayer\", version=\"1.0\", author=\"Me\", country=\"Wonderland\""
            gameLoop board

        ["BEGIN"] -> do
            (newBoard, replyString) <- myMove board
            -- hPutStrLn stderr $ printBoard newBoard
            -- hFlush stderr
            putStrLn $ replyString
            gameLoop newBoard  

        ["TURN", xy ] -> do
            let (xStr, yStr) = break (== ',') xy  -- Split at comma
            let x = read xStr :: Int
            let y = read (drop 1 yStr) :: Int
            let newBoard = enterOpponentsMove x y board
            -- hPutStrLn stderr $ printBoard newBoard
            -- hFlush stderr
            (newBoardAgain, replyString) <- myMove newBoard
            -- hPutStrLn stderr $ printBoard newBoardAgain
            -- hFlush stderr
            putStrLn $ replyString
            gameLoop newBoardAgain


        ["INFO", key, value] -> do
            -- putStrLn $ "key: " ++ key
            -- putStrLn $ "value: " ++ value
            gameLoop board 

        ["END"] -> do
            exitSuccess

        _       -> do
            putStrLn "UNKNOWN" 
            gameLoop board    

    -- -- putStrLn $ "ERROR message - unsupported size"
    --      else putStrLn $ "OK"

