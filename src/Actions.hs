module Actions where

import Data.List
import Data.List.Split

import Types

printGame :: Game -> IO ()
printGame game = case gameState of
                 WhitesMove -> do
                     putStrLn ""
                     mapM (putStrLn . intersperse ' ') $ reverse gameBoard
                     putStrLn ""
                     putStrLn "White's move"
                 BlacksMove -> do
                     putStrLn ""
                     mapM (putStrLn . intersperse ' ' . reverse) $ gameBoard
                     putStrLn ""
                     putStrLn "Black's move"
                 WhiteWon -> do
                     putStrLn "Game over. White won!"
                 BlackWon -> do
                     putStrLn "Game over. Black won!"
                 Tie -> do
                     putStrLn "Game over. Tie."
    where gameState = state game
          boardWithRanks = concat <$> transpose [(flip (++) "|" . show)  <$> [1..8], board game, ((++) "|" . show) <$> [1..8]]
          gameBoard = ("  " ++ ['a'..'h'] ++ "  ") : "  --------  " : boardWithRanks ++ ["  --------  ", ("  " ++ ['a'..'h'] ++ "  ")]
    