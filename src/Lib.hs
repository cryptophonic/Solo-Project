module Lib where

import Data.Char
import Data.List
import Data.Maybe

import Types

-- Board operations

type IndexCoord = (Int, Int)       -- IndexCoord is used internally to index into
                                   -- the actual board lists but is never exposed
                                   -- to the outside world

-- Returns a tuple of zero-based indices in the range [0..7] for a coordinate,
-- or Nothing if the coordinate is not valid
toIndices :: Coord -> Maybe IndexCoord
toIndices (Coord file rank)
    | not (elem file ['a'..'h'])   = Nothing
    | rank < 1 || rank > 8         = Nothing 
    | otherwise                    = Just (ord file - 97, rank - 1)
    
fromIndices :: IndexCoord -> Coord
fromIndices (x, y) = Coord (chr $ 97 + x) (y + 1)

-- Returns a tuple containing the coordinate and the piece on a given coordinate
-- if the coordinate is valid
getSquare :: Board -> Coord -> Maybe CoordSquare
getSquare board coord
    | isNothing indices    = Nothing
    | otherwise            = Just (coord, board !! r !! f)
    where indices = toIndices coord
          (f, r)  = fromJust indices

replaceSquare :: IndexCoord -> Square -> Board -> Board
replaceSquare (f, r) sq board = take r board ++ newRow : drop (r+1) board
    where row = board !! r
          newRow = take f row ++ sq : drop (f+1) row
          
-- Helpful functions

file :: Coord -> File
file (Coord file _) = file

rank :: Coord -> Rank
rank (Coord _ rank) = rank

square :: CoordSquare -> Square
square cs = snd cs
          
isOpponent :: Player -> Square -> Bool
isOpponent White square = isLower square
isOpponent Black square = isUpper square

isMy :: Player -> Square -> Bool
isMy White square = isUpper square
isMy Black square = isLower square

isEmpty :: Square -> Bool
isEmpty square = square == '.'

opponent :: Player -> Player
opponent player = case player of
               White -> Black
               Black -> White
               
-- Move operations

down :: Coord -> Coord
down (Coord f r) = Coord f (pred r)   -- decrease rank

up :: Coord -> Coord
up (Coord f r) = Coord f (succ r)     -- increase rank

left :: Coord -> Coord
left (Coord f r) = Coord (pred f) r   -- decrease file

right :: Coord -> Coord
right (Coord f r) = Coord (succ f) r  -- increase file

directionalTransform :: Direction -> (Coord -> Coord)
directionalTransform dir
    | dir == DirUpLeft    = up . left
    | dir == DirUp        = up
    | dir == DirUpRight   = up . right
    | dir == DirRight     = right
    | dir == DirDownRight = down . right
    | dir == DirDown      = down
    | dir == DirDownLeft  = down . left
    | dir == DirLeft      = left
    
knightTransforms :: [(Coord -> Coord)]
knightTransforms = [ up . up . left
                   , up . up . right
                   , right . right . up
                   , right . right . down
                   , down . down . left
                   , down . down . right
                   , left . left . up
                   , left . left . down
                   ]
                    
-- Returns a directional list of squares from the provided coordinate along
-- the specified direction from that coordinate.  The list terminates when the
-- edge of the board is encountered.
directionalVector :: Board -> Coord -> Direction -> [CoordSquare]
directionalVector board coord dir = recurse [] coord transform
    where transform = directionalTransform dir
          recurse :: [CoordSquare] -> Coord -> (Coord -> Coord) -> [CoordSquare]
          recurse coords thisCoord transform
              | isNothing square = reverse coords
              | otherwise        = recurse (fromJust square : coords) nextCoord transform
              where nextCoord = transform thisCoord
                    square    = getSquare board nextCoord
                    
-- Returns a list of squares that are a knight's move from the given coordinate.
-- Off-board vectors are filtered from the list.
knightVectors :: Board -> Coord -> [CoordSquare]
knightVectors board coord = map fromJust moves
    where moves = filter isJust $ getSquare board <$> (knightTransforms <*> pure coord)

-- Returns a list of directional squares consisting empty squares, terminating
-- on the first non-empty square or the edge of the board.  For opponent pieces,
-- those pieces are included in the move list (captures).
validMoveVector :: Player -> Board -> Coord -> Direction-> [Move]
validMoveVector player board coord dir = iterate candidates []
    where candidates = directionalVector board coord dir
          iterate :: [CoordSquare] -> [Move] -> [Move]
          iterate [] moves                        = moves
          iterate ((c,sq):remain) moves 
              | isEmpty sq                        = (Move coord c Nothing)   : (iterate remain moves)
              | isOpponent player sq              = (Move coord c (Just sq)) : moves
              | otherwise                         = moves
              
validMoves :: Player -> Board -> Coord -> [Direction] -> [[Move]]
validMoves player board coord dirs = map moves dirs
    where moves :: Direction -> [Move]
          moves = validMoveVector player board coord
          
-- Pawn

pawnMoves :: Player -> Board -> Coord -> [CoordSquare]
pawnMoves player board coord = fst $ span (isEmpty . square) $ possibleMoves
    where (dir, firstRank) = case player of 
                                 White -> (DirUp, 2)
                                 Black -> (DirDown, 7)
          -- Pawns may move two squares forward on their first move only
          count = if rank coord == firstRank then 2 else 1
          possibleMoves = take count $ directionalVector board coord dir
          
pawnCaptures :: Player -> Board -> Coord -> [CoordSquare]
pawnCaptures player board coord = filter (isOpponent player . square) possibleCaptures
    where dirs = case player of
                    White -> [DirUpLeft, DirUpRight]
                    Black -> [DirDownLeft, DirDownRight]
          possibleCaptures = concat $ take 1 . directionalVector board coord <$> dirs
          
validPawnMoves :: Player -> Board -> Coord -> [Move]
validPawnMoves player board coord = (convertMove <$> pawnMoves player board coord) ++
                                    (convertCapture <$> pawnCaptures player board coord)
    where convertMove :: CoordSquare -> Move
          convertMove (c,s) = (Move coord c Nothing)
          convertCapture :: CoordSquare -> Move
          convertCapture (c,s) = (Move coord c (Just s))

-- Rook
          
rookDirs :: [Direction]
rookDirs = [DirUp, DirDown, DirLeft, DirRight]

validRookMoves :: Player -> Board -> Coord -> [Move]
validRookMoves player board coord = concat $ validMoves player board coord rookDirs

-- Knight

validKnightMoves :: Player -> Board -> Coord -> [Move]
validKnightMoves player board coord = foldr convertMove [] candidates 
    where candidates = knightVectors board coord
          convertMove :: CoordSquare -> [Move] -> [Move]
          convertMove (c, '.') moves = (Move coord c Nothing) : moves
          convertMove (c, p) moves   = if isOpponent player p then
                                           (Move coord c (Just p)) : moves
                                       else
                                           moves

-- Bishop
          
bishopDirs :: [Direction]
bishopDirs = [DirUpRight, DirDownRight, DirDownLeft, DirUpLeft]

validBishopMoves :: Player -> Board -> Coord -> [Move]
validBishopMoves player board coord = concat $ validMoves player board coord bishopDirs

-- Queen

queenDirs :: [Direction]
queenDirs = [DirUp, DirUpRight, DirRight, DirDownRight, DirDown, DirDownLeft, DirLeft, DirUpLeft]

validQueenMoves :: Player -> Board -> Coord -> [Move]
validQueenMoves player board coord = concat $ validMoves player board coord queenDirs

-- King

kingDirs :: [Direction]
kingDirs = [DirUp, DirUpRight, DirRight, DirDownRight, DirDown, DirDownLeft, DirLeft, DirUpLeft]

validKingMoves :: Player -> Board -> Coord -> [Move]
validKingMoves player board coord = concat $ take 1 <$> validMoves player board coord queenDirs

-- Game logic

iterateRow :: (Int, Row) -> [CoordSquare]
iterateRow (y, row) = zipWith zipper [0..7] row
    where zipper :: Int -> Char -> CoordSquare
          zipper a b = (fromIndices (a, y), b)

iterateBoard :: (CoordSquare -> a -> a) -> a -> Board -> a
iterateBoard f accum board = foldr f accum list
    where list = concat $ iterateRow <$> zip [0..7] board
    
allMoves :: Player -> Board -> [Move]
allMoves player board = concat $ iterateBoard f [] board
    where f :: CoordSquare -> [[Move]] -> [[Move]]
          f (c, s) acc = if isMy player s then
                              case toLower s of 
                              'p' -> validPawnMoves player board c : acc
                              'r' -> validRookMoves player board c : acc
                              'n' -> validKnightMoves player board c : acc
                              'b' -> validBishopMoves player board c : acc
                              'q' -> validQueenMoves player board c : acc
                              'k' -> validKingMoves player board c : acc
                              otherwise -> acc
                          else 
                              acc

kingCoord :: Player -> Board -> Maybe Coord
kingCoord player board = iterateBoard f Nothing board
    where kingPattern = case player of 
              White -> 'K'
              Black -> 'k'
          f :: CoordSquare -> Maybe Coord -> Maybe Coord
          f (c,s) acc = if s == kingPattern then
                            Just c
                        else 
                            acc
                            
isInCheck :: Player -> Board -> Bool
isInCheck player board = or (map testKingCapture moves)
    where moves = allMoves (opponent player) board
          testKingCapture :: Move -> Bool
          testKingCapture (Move _ _ cap) = if (isJust cap) && 
                                              ((toLower (fromJust cap)) == 'k') then True
                                           else False
                                           
applyMove :: Board -> Move -> Board
applyMove board (Move from to _) = replaceFrom . replaceTo $ board
    where (_, piece)  = fromJust $ getSquare board from
          fromI       = fromJust $ toIndices from
          toI         = fromJust $ toIndices to
          replaceFrom = replaceSquare fromI '.'
          replaceTo   = replaceSquare toI piece
                                           
-- legalMoves prunes the move list generated by allMoves to remove those that leave
-- or place the player's king in check
legalMoves :: Player -> Board -> [Move]
legalMoves player board = filter (notInCheck board) (allMoves player board)
    where notInCheck :: Board -> Move -> Bool
          notInCheck board move = not $ isInCheck player $ applyMove board move
          