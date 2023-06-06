module Actions where

import Data.List
import Data.List.Split
import Data.Maybe

import Types
import Lib

printLogo :: IO ()
printLogo = do
    putStrLn "                                                     _:_"
    putStrLn "                                                    '-.-'"
    putStrLn "                                           ()      __.'.__"
    putStrLn "                                        .-:--:-.  |_______|"
    putStrLn "                                 ()      \\____/    \\=====/"
    putStrLn "                                 /\\      {====}     )___("
    putStrLn "                      (\\=,      //\\      )__(     /_____\\"
    putStrLn "      __    |'-'-'|  //  .\\    (    )    /____\\     |   |"
    putStrLn "     /  \\   |_____| (( \\_  \\    )__(      |  |      |   |"
    putStrLn "     \\__/    |===|   ))  `\\_)  /____\\     |  |      |   |"
    putStrLn "    /____\\   |   |  (/     \\    |  |      |  |      |   |"
    putStrLn "     |  |    |   |   | _.-'|    |  |      |  |      |   |"
    putStrLn "     |__|    )___(    )___(    /____\\    /____\\    /_____\\"
    putStrLn "    (====)  (=====)  (=====)  (======)  (======)  (=======)"
    putStrLn "    }===={  }====={  }====={  }======{  }======{  }======={"
    putStrLn "   (______)(_______)(_______)(________)(________)(_________)"

printGame :: Maybe Game -> IO ()
printGame Nothing     = do
                            putStrLn "Error"
printGame (Just game) = case gs of
                        WhitesMove -> do
                            putStrLn ""
                            mapM (putStrLn . intersperse ' ') $ reverse printBoard
                            putStrLn ""
                            putStrLn "White's move"
                            putStrLn (show moves)
                        BlacksMove -> do
                            putStrLn ""
                            mapM (putStrLn . intersperse ' ' . reverse) $ printBoard
                            putStrLn ""
                            putStrLn "Black's move"
                            putStrLn (show moves)
                        WhiteWon -> do
                            mapM (putStrLn . intersperse ' ') $ reverse printBoard
                            putStrLn ""
                            putStrLn "Game over. White won!"
                            putStrLn ""
                        BlackWon -> do
                            mapM (putStrLn . intersperse ' ') $ reverse printBoard
                            putStrLn ""
                            putStrLn "Game over. Black won!"
                            putStrLn ""
                        Tie -> do
                            mapM (putStrLn . intersperse ' ') $ reverse printBoard
                            putStrLn ""
                            putStrLn "Game over. Tie."
                            putStrLn ""
    where board = gameBoard game
          gs = gameState game
          boardWithRanks = concat <$> transpose [(flip (++) "|" . show)  <$> [1..8], board, ((++) "|" . show) <$> [1..8]]
          printBoard = ("  " ++ ['a'..'h'] ++ "  ") : "  --------  " : boardWithRanks ++ ["  --------  ", ("  " ++ ['a'..'h'] ++ "  ")]
          moves = movesToPGN $ legalMoves game
          
gameLoop :: Maybe Game -> IO ()
gameLoop game@(Just (Game _ state _ _)) = do
    printGame game
    if elem state [WhitesMove, BlacksMove] then do
        putStrLn "Enter your move: "
        move <- getLine
        let newGame = applyMove game move
        if isJust newGame then
            gameLoop newGame
        else do
            putStrLn ("Invalid move: " ++ move)
            gameLoop game
    else
        return ()
          
playGame :: IO ()
playGame = do 
    printLogo
    let game = newGame
    gameLoop game
    