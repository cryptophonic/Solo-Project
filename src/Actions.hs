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
printGame (Just game) = case gameState of
                        WhitesMove -> do
                            putStrLn ""
                            mapM (putStrLn . intersperse ' ') $ reverse gameBoard
                            putStrLn ""
                            putStrLn "White's move"
                            putStrLn (show moves)
                        BlacksMove -> do
                            putStrLn ""
                            mapM (putStrLn . intersperse ' ' . reverse) $ gameBoard
                            putStrLn ""
                            putStrLn "Black's move"
                            putStrLn (show moves)
                        WhiteWon -> do
                            putStrLn "Game over. White won!"
                        BlackWon -> do
                            putStrLn "Game over. Black won!"
                        Tie -> do
                            putStrLn "Game over. Tie."
    where gameState = state game
          boardWithRanks = concat <$> transpose [(flip (++) "|" . show)  <$> [1..8], board game, ((++) "|" . show) <$> [1..8]]
          gameBoard = ("  " ++ ['a'..'h'] ++ "  ") : "  --------  " : boardWithRanks ++ ["  --------  ", ("  " ++ ['a'..'h'] ++ "  ")]
          moves = movesToPGN $ legalMoves game
          
gameLoop :: Maybe Game -> IO ()
gameLoop game = do
    printGame game
    putStrLn "Enter your move: "
    move <- getLine
    let newGame = applyMove game move
    if isJust newGame then do
        gameLoop newGame
    else do
        gameLoop game
          
playGame :: IO ()
playGame = do 
    printLogo
    let game = newGame
    gameLoop game
    