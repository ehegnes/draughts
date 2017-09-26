-- |
-- Module: Draughts.Game
-- Copyright: (C) 2017 Siddhanathan Shanmugam
-- License: GPL (see the file LICENSE)
-- 
-- Maintainer: siddhanathan@gmail.com
-- Stability: experimental
-- Portability: non-portable (GHC Extensions)
--
-- Game
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Draughts.Game where

--------------------------------------------------------------------------------

import Draughts.Library.Board
import Draughts.Library.Types

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Data.Maybe              (fromJust)
import Data.Monoid             ((<>))
import Lens.Micro              ((&), (.~), (%~), (^.), _1, _2, _3)
import Lens.Micro.TH           (makeLenses)

--------------------------------------------------------------------------------

-- | The current Stage in the Game
data Stage
    = WaitingForOtherPlayer
    | MoveInProgress
    | ServerRequestedMove
    | NullStage
    deriving (Eq)

-- | Synchronization message between UI and Game
data ITN
    = UIRequestMove Index
    | UIMakeMove Index Index

-- | Game state
data Game = Game
    { _board    :: Board            -- ^ Board state
    , _turn     :: ClientNumber     -- ^ Next player to take a turn
    , _selected :: Index            -- ^ Keeps track of cell focus
    , _highlighted :: [Index]       -- ^ List of squares highlighted
    , _prompt   :: String           -- ^ Prompt for the user
    , _snet :: MVar ITN             -- ^ Channel for synchronization
    , _stage :: Stage               -- ^ Current stage of the game
    , _stats :: (Int, Int, Player)  -- ^ Current stats of the game
    , _entered :: Maybe Index       -- ^ Index of position entered earlier
    }

makeLenses ''Game

-- | Direction for cursor movement using a keyboard
data Direction
    = UP
    | DOWN
    | LEFT
    | RIGHT
    deriving (Eq)

initGame :: MonadIO m => m Game
initGame = do
    v <- liftIO $ newEmptyMVar
    pure Game
        { _board = initialBoard
        , _turn = Player1 "Black"
        , _selected = Index 1
        , _highlighted = mempty
        , _stats = (0, 0, Black)
        , _prompt = "INIT: Not connected to server!"
        , _snet = v
        , _stage = NullStage
        , _entered = Nothing
        }

--------------------------------------------------------------------------------

-- | Show the Player Stats
sPrompt
    :: (Int, Int, Player)  -- ^ (BlackPoints, WhitePoints, NextPersonToMove)
    -> String              -- ^ Prompt to display to the user
sPrompt (black, white, next) =
    "┌─────┬─────┬──────┐\
    \\n│BLACK│WHITE│ NEXT │\
    \\n├─────┼─────┼──────┤\
    \\n│  " <> n black <> " │  " <> n white <> " │ " <> p next <> "│\
    \\n└─────┴─────┴──────┘"
  where
    p Black = "BLACK"
    p White = "WHITE"
    n i | i < 10 = " " <> show i
        | otherwise = show i

-- | Update the prompt
updatePrompt :: String -> Game -> Game
updatePrompt v g = g & prompt .~ v

-- | Highlight squares
highlightMoves :: [Index] -> Game -> Game
highlightMoves xs g = g & highlighted .~ xs

-- | Request a move to be performed
tReqMove :: Game -> Game
tReqMove g = g & stage .~ ServerRequestedMove

-- | Perform a move
tMakeMove
    :: Index    -- ^ From Index
    -> [Index]  -- ^ Captured Pieces
    -> Index    -- ^ To Index
    -> Game     -- ^ Game state threaded through computation
    -> Game     -- ^ Game state threaded through computation
tMakeMove f cs t g =
    -- Make the move
    g & board %~ removePiece f cs t
    -- Crown any pieces that have reached the ends
      & board %~ crownBoard
    -- Update the score
      & stats . h (g ^. stats . _3) %~ (+length cs)
    -- Allow the other player to play
      & stats . _3 %~ opposite
  where
    h Black = _1
    h White = _2

-- | Try performing a move
reqMove :: MonadIO m => Game -> m Game
reqMove g = do
    -- Ask for the move
    liftIO $ putMVar (g ^. snet) (UIRequestMove (g ^. selected))
    -- Tell the UI that a move is in progress
    g & stage .~ MoveInProgress
    -- Keep track of the entered piece
      & entered .~ Just (g ^. selected)
    -- Lift value into computation
      & pure

-- | Make a move if it is possible
attemptMove :: MonadIO m => Game -> m Game
attemptMove g =
    -- Check if the move is valid using only the Client logic
    if g ^. selected `elem` g ^. highlighted
    -- If the move is valid
    then do
        -- Tell the UI about the move
        liftIO $ putMVar (g ^. snet)
            (UIMakeMove (fromJust $ g ^. entered) (g ^. selected))
        -- Switch to waiting for the other player after the move
        g & stage .~ WaitingForOtherPlayer
        -- Clear the previously entered piece
          & entered .~ Nothing
        -- Clear any highlighted pieces
          & highlighted .~ mempty
        -- Lift the value into computation
          & pure
    else pure g

-- | Move the Cursor in the UI using the keyboard
moveCursor
    :: Direction  -- ^ Direction to move the cursor
    -> Game       -- ^ Game state threaded through the computation
    -> Game       -- ^ Game state threaded through the computation
moveCursor dir g =
    g & selected %~ go dir
  where
    canGoUp    i = i >= 5
    canGoDown  i = i <= 28
    canGoLeft  i = i `notElem` fmap (succ . (4*)) [0..7]
    canGoRight i = i `notElem` fmap (4*) [1..8]
    go d (Index i)
        | d == UP    && canGoUp    i = Index (i - 4)
        | d == DOWN  && canGoDown  i = Index (i + 4)
        | d == LEFT  && canGoLeft  i = Index (i - 1)
        | d == RIGHT && canGoRight i = Index (i + 1)
        | otherwise = Index i

--------------------------------------------------------------------------------

