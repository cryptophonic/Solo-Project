module Types where

import Data.Maybe

type File = Char             -- File [a-h]
type Rank = Int              -- Rank [1-8]
data Coord = Coord File Rank

coordToString :: Coord -> String
coordToString (Coord f r) = [f] ++ show r

instance Show Coord where
    show c = show $ coordToString c
    
type Square = Char
type Row = [Square]
type Board = [Row]

-- any valid coord + what's at that coord
type CoordSquare = (Coord, Square)

-- move = from, to, maybe capture
data Move = Move {
      movePiece :: Square
    , moveFrom :: Coord 
    , moveTo :: Coord 
    , moveCapture :: (Maybe Square)
    , moveDups :: [Coord]
    , movePGN :: String
}

moveToString :: Move -> String
moveToString (Move _ from to _ _ pgn) = pgn ++ "(" ++ (coordToString from) ++ " " 
    ++ (coordToString to) ++ ")"
                   
instance Show Move where
    show m = moveToString m
    
data Player = Black | White
    deriving Eq

data Direction = DirUpLeft | DirUp | DirUpRight | DirRight | 
                 DirDownRight | DirDown | DirDownLeft | DirLeft
    deriving (Show, Eq)
    
data GameState = WhitesMove | BlacksMove | WhiteWon | BlackWon | Tie 

data Game = Game { 
    board      :: Board
    , state    :: GameState
    , lastMove :: Maybe Move       -- Necessary for stateful en-passant moves
}

_FILES_ = ['a'..'h']
_RANKS_ = [1..8]

-- Default board view is from White's perspective, with rows stored in
-- rank order (i.e. White's pieces are in the first and second Strings,
-- with Black's pieces in Strings 6 and 7).

_START_BOARD_ :: [String]
_START_BOARD_ = reverse [   
                    "rnbqkbnr"
                  , "pppppppp"
                  , "Q.Q....."
                  , "......R."
                  , "Q...N..."
                  , "..r....R"
                  , "PPPPPPP."
                  , ".NBQKB.."
                ]
               
newGame :: Game
newGame = Game _START_BOARD_ WhitesMove Nothing
