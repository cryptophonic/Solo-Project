module Actions where

import Data.List

import Types

printBoard :: Player -> Board -> IO ()
printBoard White board = do
    putStrLn ""
    mapM (putStrLn . intersperse ' ') $ reverse board
    putStrLn ""
printBoard Black board = do
    putStrLn ""
    mapM (putStrLn . intersperse ' ' . reverse) $ board
    putStrLn ""
    