-- |
-- Module: Draughts.Library.Board.Internal
-- Copyright: (C) 2017 Siddhanathan Shanmugam
-- License: GPL (see the file LICENSE)
-- 
-- Maintainer: siddhanathan@gmail.com
-- Stability: experimental
-- Portability: non-portable (GHC Extensions)
--
-- English Draughts in Haskell
--

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

--------------------------------------------------------------------------------

module Draughts.Library.Board.Internal
    (
      flipSide
    , forwardMoves
    , movePiece
    , multiJumps
    , opposite
    , playerMoves
    , removePiece
    , singleJumps
    , validSingleMoves
    ) where

--------------------------------------------------------------------------------

import Draughts.Library.Types

import Control.Monad        (MonadPlus, mfilter)
import Control.Monad.Plus   (mconcatMap, mfold, mplus)
import Control.Monad.Zip    (MonadZip, mzip)
import Data.Array           ((!), (//))
import Data.Bool            (bool)
import Data.Function        (fix, on)
import Data.Maybe           (isNothing, fromJust)
import Lens.Micro.GHC       ((^.))

--------------------------------------------------------------------------------
-- Moves and Captures

-- | Find all the positions a piece can move to from a given position.
-- 
--   Avoid calling this directly.
{-@ forwardMoves :: Index -> ListNB Index 0 2 @-}
forwardMoves
    :: MonadPlus m
    => Index    -- ^ 'Index' to begin calculations from
    -> m Index  -- ^ List of possible moves
forwardMoves (Index i) =
    fmap Index . mfilter (>= 1) . mfilter (<= 32) . mfilter isValid $ vs
  where
    columnSize = 4
    graphColumn = pred i `div` columnSize

    vs = (columnSize * succ graphColumn +) <$> mfold [1..columnSize]

    isValid x =
        let v = columnSize + i in
        x == v || x == v + adjustOffset graphColumn

    adjustOffset (flip mod 2 -> 0) = 1
    adjustOffset (flip mod 2 -> 1) = -1
    adjustOffset _ = undefined


-- | Rotation of the 'Board' by τ/2.
--   The symmetry of the 'Board' allows us to compose our forward-algorithms
--   with 'flipSide' to obtain backward-algorithms. The algorithms must be
--   used in a conjugating manner. Refer to Conjugation (group theory).
-- 
--   Avoid calling this directly.
flipSide
    :: Index  -- ^ Old 'Index'
    -> Index  -- ^ Transformed 'Index'
flipSide (Index i) = Index (33 - i)


-- | Return 'forwardMoves' relative to the 'Player'
--
--   Use the natural transformation to get playerMoves for White
--
--
--                                    flipSide
--              WhiteIndex -----------------------------> BlackIndex
--                 |                                          |
--                 |                                          |
--    forwardMoves |                                          | forwardMoves
--                 |                                          |
--                 |                                          |
--                 V                                          V
--            WhitePlayerMoves <----------------------- BlackPlayerMoves
--                                  fmap flipSide
--
--   This is almost a natural transformation. The key observation is that
--   'flipSide' is its own inverse, so we may flip the arrow.
-- 
--   Avoid calling this directly.
playerMoves
    :: (MonadPlus m)
    => Piece    -- ^ Player for which moves are checked
    -> Index    -- ^ Index of the player
    -> m Index  -- ^ List of Indices which Player may move to
playerMoves (Piece Black Man) = forwardMoves
playerMoves (Piece White Man) = fmap flipSide . forwardMoves . flipSide
playerMoves (Piece _ King)    = \x ->
    (mplus `on` flip playerMoves x) (Piece Black Man) (Piece White Man)


-- | Make a move from 'playerMoves' if it is legal on the Board.
--   This prevents moving onto a piece that exists on the Board.
validSingleMoves
    :: (MonadPlus m)
    => Board    -- ^ Current Board state
    -> Piece    -- ^ The Piece that wishes to move
    -> Index    -- ^ The Index of that Piece
    -> m Index  -- ^ The Indices which that Piece can move to
validSingleMoves (Board b) p =
    mfilter (isNothing . (b!)) . playerMoves p


-- | Given a player's position on the 'Board', find all the positions the
--   player can jump onto in order to capture other players.
--   The return value consists of a list of (PlayerIndex, PositionIndex).
--
--   Avoid calling this directly.
singleJumps
    :: (MonadZip m, MonadPlus m)
    => Board            -- ^ 'Board'
    -> Index            -- ^ Starting 'Index'
    -> m (Index,Index)  -- ^ [(CapturedPlayerIndex,NewPositon)]
singleJumps (Board b) i =
    -- If a move is possible from the opponents position, and
    -- a jump to that move would capture the opponent, then that
    -- jump is a valid jump.
    mfilter (isValidJump . snd)
    . mconcatMap (\(x,ys) -> (x,) <$> ys)
    . mfilter (not . null . snd)
    $ mzip opponents opponentMovess
  where
    -- Grab the adjacent opponents
    opponents =
        mfilter (hasOpposingPieces (Board b) i) . playerMoves p $ i

    opponentMovess =
        validSingleMoves (Board b) p <$> opponents

    -- List of possible jumps which may capture an opponent.
    isValidJump x =
        x ^. index `elem` [ui+9, ui+7, ui-9, ui-7]

    p = fromJust (b ! i)
    ui = i ^. index


-- | If a piece can jump then it must jump. This handles multi-jump logic.
--
--   Avoid calling this directly.
multiJumps
    :: (MonadZip m, MonadPlus m, Foldable m)
    => (Board, Index)             -- ^ (InitialBoard, StartIndex)
    -> m (Board, [Index], Index)  -- ^ [(FinalBoard, [Captures], EndIndex)]
multiJumps xs =
    mfilter (\(_,_,c) -> c /= snd xs) . fix untilStable jumps . reshape $ xs
  where
    -- Perform a jump on the board, and remove the captured pieces.
    -- The board is threaded through this computation
    jumps (b, cs, i) =
        bool (step <$> zs) (pure (b, cs, i)) (null zs)
      where
        zs = singleJumps b i
        step (c,j) = (removePiece i (pure c) j b, c:cs, j)

    -- Repeat a computation until its return value is stable
    untilStable g f x = do
        y <- f x
        bool (g f y) (pure x) (x == y)

    -- Reshape the domain to allow performing a multi-jump computation
    reshape (b,i) = (b, mempty, i)


-- | Remove a piece from the board in a lookahead style manner.
--   This function is meant to be used to run Reader computations
--   in a modified environment.
removePiece
    :: Index    -- ^ Starting Player Position
    -> [Index]  -- ^ Opponents captured
    -> Index    -- ^ Ending Player Position
    -> Board    -- ^ Board
    -> Board    -- ^ Board
removePiece player opponents position (Board b) =
    Board $ nb // ((,Nothing) <$> opponents)
  where
    Board nb = movePiece player position (Board b)


-- | Move a piece on the board in a lookahead style manner. This does not
--   assert that the move is possible. Please validate that the move is
--   feasible before calling this function.
movePiece
    :: Index  -- ^ Input Index
    -> Index  -- ^ Output Index
    -> Board  -- ^ Board
    -> Board  -- ^ Board
movePiece i o (Board b) =
    Board $ b // [ (i, Nothing), (o, b!i) ]


--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Helpers

-- | Function to check if two indices on the Board have
--   pieces of opposing colors
hasOpposingPieces :: Board -> Index -> Index -> Bool
hasOpposingPieces (Board b) x y =
    fmap pieceColor (b ! x) == fmap (opposite . pieceColor) (b ! y)


-- | Helper function to determine the opposite player.
opposite
    :: Player  -- ^ Current Player
    -> Player  -- ^ Opposing Player
opposite Black = White
opposite White = Black

--
--------------------------------------------------------------------------------
