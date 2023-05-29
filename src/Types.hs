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

data Move = Move Coord Coord (Maybe Square)

moveToString :: Move -> String
moveToString (Move from to capture) = coordToString from ++ "->" ++ coordToString to ++ cap
    where cap = if isNothing capture then 
                   "" 
               else 
                   " (" ++ [fromJust capture] ++ ")"

instance Show Move where
    show m = moveToString m

type MoveList = (Square, [Move])

data Player = Black | White
    deriving Eq

data Direction = DirUpLeft | DirUp | DirUpRight | DirRight | 
                 DirDownRight | DirDown | DirDownLeft | DirLeft
    deriving (Show, Eq)
    
data GameState = WhiteWon | BlackWon | Tie | InProgress

data Game = Game { 
    board :: Board
    , state :: GameState
    , lastMove :: Move       -- Necessary for stateful en-passant moves
}

_FILES_ = ['a'..'h']
_RANKS_ = [1..8]

-- Default board view is from White's perspective, with rows stored in
-- rank order (i.e. White's pieces are in the first and second Strings,
-- with Black's pieces in Strings 6 and 7).

_START_GAME_ :: [String]
_START_GAME_ = reverse [   
                   "rnbqkbnr"
                 , "pppppppp"
                 , "........"
                 , "........"
                 , "........"
                 , "........"
                 , "PPPPPPPP"
                 , "RNBQKBNR"
               ]
               