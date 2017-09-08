-- |
-- Module: Draughts.Library.Board
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

module Draughts.Library.Board
    (
      crownBoard
    , hasWon
    , initialBoard
    , makeMove
    , playerTransitions

    -- * Internal functions re-exported
    , removePiece
    , opposite
    ) where

--------------------------------------------------------------------------------

import Draughts.Library.Types
import Draughts.Library.Board.Internal

import Control.Monad        (MonadPlus, mfilter, guard)
import Control.Monad.Plus   (mfromMaybe, mplus)
import Control.Monad.Zip    (MonadZip)
import Data.Array           (elems, (!), listArray)
import Data.Bool            (bool)
import Data.Foldable        (find)
import Data.Function        ((&))
import Data.Maybe           (isNothing, fromJust)
import Data.Monoid          ((<>), mconcat)
import Lens.Micro.GHC       ((<&>))

--------------------------------------------------------------------------------

-- | The initial 'Board' state
initialBoard :: Board
initialBoard = Board $ listArray (Index 1, Index 32) $ mconcat
    [ replicate 12 (pure $ Piece Black Man)
    , replicate 8 Nothing
    , replicate 12 (pure $ Piece White Man)
    ]


-- | Actual function intended to be called by clients.
--   Handles all the edge cases associated with all types of jumps.
playerTransitions
    :: (MonadZip m, MonadPlus m, Foldable m)
    => Board    -- ^ The state of the Board
    -> Player   -- ^ Player Color, to double check that the move is valid
    -> Index    -- ^ Index from which the Player wants to transition
    -> m Index  -- ^ Indices to which the Player can transition
playerTransitions (Board b) p i = do
    -- Ensure that the Player is allowed to choose that piece
    p2 <- mfromMaybe (b ! i)
    guard $ p == pieceColor p2

    -- Get Multi-Jumps
    let mjs = (\(_,_,c) -> c) <$> multiJumps (Board b, i)

    -- Prefer multi-jumps, and fall back on single move on failure
    bool mjs (validSingleMoves (Board b) p2 i) (null mjs)


-- | Function to be called after 'playerTransitions'
makeMove :: Board -> Player -> Index -> Index -> Maybe (Board, [Index])
makeMove (Board b) p from to = do
    -- Validate that the correct player is attempting to move
    xp <- fmap pieceColor (b ! from)
    guard $ p == xp

    find (const True) ((mjs `mplus` pms) `asTypeOf` [])
  where
    mjs = fmap (\(x,y,_) -> (x,y))
        . mfilter (\(_,_,x) -> x == to)
        $ multiJumps (Board b, from)

    pms = fmap (const (movePiece from to (Board b), mempty))
        . mfilter (==to)
        $ playerMoves (fromJust (b!from)) from


crownBoard
    :: Board  -- ^ Game Board threaded through the computation
    -> Board  -- ^ Game Board threaded through the computation
crownBoard (Board b) =
    Board $ listArray (Index 1, Index 32) $ firstRow <> middleRows <> lastRow
  where
    ls = elems b
    firstRow   = ls & take 4  <&> promo White
    middleRows = ls & take 28  &  drop 4
    lastRow    = ls & drop 28 <&> promo Black
    promo c x = bool x (pure (Piece c King)) (fmap pieceColor x == pure c)


-- | Check if either player has won the game.
hasWon :: Board -> Maybe Player
hasWon (Board b) =
    if win Black then pure Black
    else if win White then pure White
    else Nothing
  where
    win :: Player -> Bool
    win p =
        all (\x -> fmap pieceColor x == pure p || isNothing x) (elems b)

--------------------------------------------------------------------------------
