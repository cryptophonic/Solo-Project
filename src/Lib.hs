module Lib where

import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe

import Types

-- Board operations

type IndexCoord = (Int, Int)       -- IndexCoord is used internally to index into
                                   -- the actual board lists but is never exposed
                                   -- to the outside world

-- Returns a tuple of zero-based indices in the range [0..7] for a coordinate
toIndices :: Coord -> IndexCoord
toIndices (Coord file rank) = (ord file - 97, rank - 1)
    
isValidCoord :: Coord -> Bool
isValidCoord (Coord file rank)
    | not (elem file ['a'..'h'])   = False
    | not (elem rank [1..8])       = False
    | otherwise                    = True
    
fromIndices :: IndexCoord -> Coord
fromIndices (x, y) = Coord (chr $ 97 + x) (y + 1)

-- Returns a tuple containing the coordinate and the piece on a given coordinate
-- if the coordinate is valid
getSquare :: Board -> Coord -> CoordSquare
getSquare board coord = (coord, board !! r !! f)
    where (f, r) = toIndices coord

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
              | not $ isValidCoord nextCoord = reverse coords
              | otherwise                    = recurse (getSquare board nextCoord : coords) nextCoord transform
              where nextCoord = transform thisCoord
                    
-- Returns a list of squares that are a knight's move from the given coordinate.
-- Off-board vectors are filtered from the list.
knightVectors :: Board -> Coord -> [CoordSquare]
knightVectors board coord = getSquare board <$> filter isValidCoord coords
    where coords = knightTransforms <*> pure coord

-- Returns a list of directional squares consisting empty squares, terminating
-- on the first non-empty square or the edge of the board.  For opponent pieces,
-- those pieces are included in the move list (captures).
validMoveVector :: Player -> Board -> Coord -> Direction-> [Move]
validMoveVector player board coord dir = iterate candidates []
    where candidates = directionalVector board coord dir
          (_, piece) = getSquare board coord
          iterate :: [CoordSquare] -> [Move] -> [Move]
          iterate [] moves                        = moves
          iterate ((c,sq):remain) moves 
              | isEmpty sq                        = (Move piece coord c Nothing Nothing [] "")   : (iterate remain moves)
              | isOpponent player sq              = (Move piece coord c (Just sq) Nothing [] "") : moves
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
validPawnMoves player board coord = foldr convertMove [] (pawnMoves player board coord) ++
                                    foldr convertCapture [] (pawnCaptures player board coord)
    where (_, piece) = getSquare board coord
          convertMove :: CoordSquare -> [Move] -> [Move]
          convertMove (c@(Coord _ rank),s) moveList
              -- pawn promotions
              | player == White && rank == 8  = moveList ++ [(Move piece coord c Nothing (Just 'R') [] ""),
                                                 (Move piece coord c Nothing (Just 'N') [] ""),
                                                 (Move piece coord c Nothing (Just 'B') [] ""),
                                                 (Move piece coord c Nothing (Just 'Q') [] "")]
              | player == Black && rank == 1  = moveList ++ [(Move piece coord c Nothing (Just 'r') [] ""),
                                                 (Move piece coord c Nothing (Just 'n') [] ""),
                                                 (Move piece coord c Nothing (Just 'b') [] ""),
                                                 (Move piece coord c Nothing (Just 'q') [] "")]
              | otherwise                     = moveList ++ [(Move piece coord c Nothing Nothing [] "")]
          convertCapture :: CoordSquare -> [Move] -> [Move]
          convertCapture (c@(Coord _ rank),s) moveList
              -- pawn promotions
              | player == White && rank == 8  = moveList ++ [(Move piece coord c (Just s) (Just 'R') [] ""),
                                                 (Move piece coord c (Just s) (Just 'N') [] ""),
                                                 (Move piece coord c (Just s) (Just 'B') [] ""),
                                                 (Move piece coord c (Just s) (Just 'Q') [] "")]
              | player == Black && rank == 1  = moveList ++ [(Move piece coord c (Just s) (Just 'r') [] ""),
                                                 (Move piece coord c (Just s) (Just 'n') [] ""),
                                                 (Move piece coord c (Just s) (Just 'b') [] ""),
                                                 (Move piece coord c (Just s) (Just 'q') [] "")]
              -- en passant (can only be a capture)
              | otherwise                     = moveList ++ [(Move piece coord c (Just s) Nothing [] "")]

-- Rook
          
rookDirs :: [Direction]
rookDirs = [DirUp, DirDown, DirLeft, DirRight]

validRookMoves :: Player -> Board -> Coord -> [Move]
validRookMoves player board coord = concat $ validMoves player board coord rookDirs

-- Knight

validKnightMoves :: Player -> Board -> Coord -> [Move]
validKnightMoves player board coord = foldr convertMove [] candidates 
    where (_, piece) = getSquare board coord
          candidates = knightVectors board coord
          convertMove :: CoordSquare -> [Move] -> [Move]
          convertMove (c, '.') moves = (Move piece coord c Nothing Nothing [] "") : moves
          convertMove (c, p) moves   = if isOpponent player p then
                                           (Move piece coord c (Just p) Nothing [] "") : moves
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
    
potentialMoves :: Player -> Board -> [Move]
potentialMoves player board = concat $ iterateBoard f [] board
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
    where moves = potentialMoves (opponent player) board
          testKingCapture :: Move -> Bool
          testKingCapture (Move _ _ _ cap _ _ _) = if (isJust cap) && ((toLower (fromJust cap)) == 'k') then True
                                                 else False
                                           
isPawnMove :: Move -> Bool
isPawnMove move = (toUpper $ movePiece move) == 'P'

applyMoveToBoard :: Board -> Move -> Board
applyMoveToBoard board (Move piece from to _ promote _ _) = replaceFrom . replaceTo $ board
    where p = if isJust promote then
              fromJust promote
          else 
              piece
          fromI       = toIndices from
          toI         = toIndices to
          replaceFrom = replaceSquare fromI '.'
          replaceTo   = replaceSquare toI p
                                           
-- legalMoves prunes the move list generated by potentialMoves to remove those that leave
-- or place the player's king in check
legalMoves :: Game -> [Move] 
legalMoves (Game board state _)
    | state == WhitesMove = filter (notInCheck White board) (potentialMoves White board)
    | state == BlacksMove = filter (notInCheck Black board) (potentialMoves Black board)
    | otherwise           = []
    where 
          notInCheck :: Player -> Board -> Move -> Bool
          notInCheck player board move = not $ isInCheck player $ applyMoveToBoard board move
          
moveToMapKey :: Move -> String
moveToMapKey (Move piece _ to _ _ _ _) = piece : coordToString to

-- need to ensure moves are unique.  if there are duplicates they need to be
-- qualified for PGN notation
moveMapping :: [Move] -> Map.Map String [Coord]
moveMapping [] = Map.empty
moveMapping (move:remaining) = Map.unionWith (++) (moveMapping remaining) 
                               (Map.singleton (moveToMapKey move) [moveFrom move])
          
-- fill out the moveDups field for moves that have duplicates. this is a two
-- pass process - first all the moves are put into a Map, then the map is 
-- checked for duplicates and the accumulated coords are added to each
-- duplicate move for use in qualifying the pgn notation.
identifyDuplicateMoves :: [Move] -> [Move]
identifyDuplicateMoves inMoves = foldr worker [] inMoves
    where moveMap = moveMapping inMoves
          worker :: Move -> [Move] -> [Move]
          worker move moves = if length list > 1 && not (isPawnMove move) then
                                  move { moveDups = list } : moves
                              else
                                  move : moves
              where list = fromJust (Map.lookup (moveToMapKey move) moveMap)
              
-- given a list of coords and my coord, return the unique qualifier needed for
-- PGN notation.  precedence is file, then rank, then the full coord
getQualifier :: [Coord] -> Coord -> String
getQualifier coordList myCoord 
    | length files == length coordList  = [file myCoord]
    | length ranks == length coordList  = [chr ((rank myCoord) + 48)]
    | otherwise                         = coordToString myCoord
    where files = nub $ head . coordToString <$> coordList
          ranks = nub $ tail . coordToString <$> coordList
          
appendPawnPromotion :: Move -> String -> String
appendPawnPromotion (Move _ _ _ _ promote _ _) str
    | isJust promote   = str ++ "=" ++ [fromJust promote]
    | otherwise        = str
          
pawnToPGN :: Move -> String
pawnToPGN move@(Move _ from to capture _ _ _)
    | isJust capture   = appendPawnPromotion move $ file from : 'x' : coordToString to
    | otherwise        = appendPawnPromotion move $ coordToString to
    
qualifiedPiece :: Move -> String
qualifiedPiece move = if length dups > 0 then
                          piece : (getQualifier dups from)
                      else
                          [piece]
    where piece = toUpper $ movePiece move
          dups = moveDups move
          from = moveFrom move
          
movesToPGN :: [Move] -> [Move]
movesToPGN moves = map addPGN $ identifyDuplicateMoves moves
    where moveToPGN :: Move -> String
          moveToPGN move
              | isPawnMove move           = pawnToPGN move
              | isJust (moveCapture move) = qualifiedPiece move ++ "x" ++ (coordToString $ moveTo move)
              | otherwise                 = qualifiedPiece move ++ (coordToString $ moveTo move)
          addPGN :: Move -> Move
          addPGN move = move { movePGN = moveToPGN move }
          
checkEndOfGame :: Maybe Game -> Maybe Game
checkEndOfGame Nothing = Nothing
checkEndOfGame (Just game@(Game board state lastMove)) = if (length $ legalMoves game) == 0 then
                                                             case state of 
                                                             WhitesMove -> if isInCheck White board then
                                                                               Just (Game board BlackWon lastMove)
                                                                           else
                                                                               Just (Game board Tie lastMove)
                                                             BlacksMove -> if isInCheck Black board then
                                                                               Just (Game board WhiteWon lastMove)
                                                                           else
                                                                               Just (Game board Tie lastMove)
                                                         else
                                                             Just game
              
applyMove :: Maybe Game -> String -> Maybe Game
applyMove Nothing _                                   = Nothing
-- we don't apply moves to ended games
applyMove (Just game@(Game _ WhiteWon _)) _           = Just game
applyMove (Just game@(Game _ BlackWon _)) _           = Just game
applyMove (Just game@(Game _ Tie _)) _                = Just game
applyMove (Just game@(Game board state lastMove)) pgn = checkEndOfGame $ if isJust move then
                                                            Just (Game (applyMoveToBoard board (fromJust move)) nextState move)
                                                        else 
                                                            Nothing
    where (player, nextPlayer, nextState) = case state of
                                            WhitesMove -> (White, Black, BlacksMove)
                                            BlacksMove -> (Black, White, WhitesMove)
          pgnMoves = movesToPGN $ legalMoves game
          move = foldr matchMove Nothing pgnMoves
          matchMove :: Move -> Maybe Move -> Maybe Move
          matchMove _ (Just m)                           = Just m
          matchMove m@(Move _ _ _ _ _ _ movePgn) Nothing = if pgn == movePgn then
                                                               Just m
                                                           else
                                                               Nothing