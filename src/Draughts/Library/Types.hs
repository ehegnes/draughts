-- |
-- Module: Draughts.Library.Types
-- Copyright: (C) 2017 Siddhanathan Shanmugam
-- License: GPL (see the file LICENSE)
-- 
-- Maintainer: siddhanathan@gmail.com
-- Stability: experimental
-- Portability: non-portable (GHC Extensions)
--
-- Types
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Draughts.Library.Types where

--------------------------------------------------------------------------------

import Draughts.Library.JSON

import Data.Aeson.TH              (deriveJSON)
import Data.Array                 (Array, Ix)
import GHC.Generics               (Generic)
import Lens.Micro.TH              (makeLenses)

import qualified Data.Text as T

--------------------------------------------------------------------------------

-- | Represents the position of a square on the 'Board'
newtype Index = Index
    { _index :: Int
    } deriving (Generic, Ord, Eq, Ix, Show)

makeLenses ''Index

-- | Represents the contents of a square on the 'Board'
data Player
    = Black  -- ^ Has a black player
    | White  -- ^ Has a white player
    deriving (Generic, Eq, Show)

-- | Represents whether a piece is a Man or a King
data Cast
    = Man   -- ^ A standard piece capable of moving forward
    | King  -- ^ A promoted piece capable of moving forward/backward
    deriving (Generic, Eq, Show)

-- | Represents a piece on the 'Board'
data Piece = Piece
    { pieceColor :: Player  -- ^ Color of the piece
    , pieceCast  :: Cast    -- ^ Promotion status of the piece
    } deriving (Generic, Eq, Show)

-- | The English Draughts 'Board'.
--   This must be accessed in a single-threaded manner.
--   Using the 'State' Monad to access the 'Board' can capture
--   this invariant.
--
--
--   Only 32 of the available squares on the board can be occupied.
--   We capture this invariant by only allowing indexing into a
--   single-dimensional array of length 32. Each of the 32 items
--   in the array can consist of a black player, a white player,
--   or it may be empty.
--
--   @
--   ===========================
--   ||  | 1|  | 2|  | 3|  | 4||
--   ||-----------------------||
--   || 5|  | 6|  | 7|  | 8|  ||
--   ||-----------------------||
--   ||  | 9|  |10|  |11|  |12||
--   ||-----------------------||
--   ||13|  |14|  |15|  |16|  ||
--   ||-----------------------||
--   ||  |17|  |18|  |19|  |20||
--   ||-----------------------||
--   ||21|  |22|  |23|  |24|  ||
--   ||-----------------------||
--   ||  |25|  |26|  |27|  |28||
--   ||-----------------------||
--   ||29|  |30|  |31|  |32|  ||
--   ===========================
--   @
--
newtype Board = Board
    { _unBoard :: Array Index (Maybe Piece)
    } deriving (Eq, Show)

makeLenses ''Board

-- | Client Identification Token
data ClientNumber
    = Player1 { blackName :: !T.Text }  -- ^ Black Player
    | Player2 { whiteName :: !T.Text }  -- ^ White Player
    deriving (Generic, Show, Eq)

-- | Handshake between Server and Client
data Handshake
    = ClientHandshake { unHandshake :: ClientNumber }  -- Successful Handshake
    | TooManyClientsConnected                          -- Failed Handshake
    deriving (Generic, Show)

-- | Messages sent from the Server to the Client
data ServerToClient
    = Acknowledge { ack :: Handshake }
        -- ^ Acknowledge the Client
    | RequestMove
        -- ^ Ask the Client to make a move
    | ListOfValidMoves { allowedMoves :: [Index] }
        -- ^ List of moves the Client is allowed to make
    | MovePerformed
        { mFromIndex :: Index   -- ^ From Index before the move
        , mCaptured :: [Index]  -- ^ Pieces Captured during the move
        , mToIndex :: Index     -- ^ To Index after the move
        }
        -- ^ Move performed by either the current or opposing Client
    | InvalidMove
        -- ^ The move requested by the Client was invalid
    | GameOver { winner :: Player }
        -- ^ Game has been won
    deriving (Generic, Show)

-- | Messages sent from the Client to the Server
data ClientToServer
    = Hello { rClientName :: !T.Text }
        -- ^ Tell the Server that you have connected
    | RequestListOfValidMoves { rFromIndex :: Index }
        -- ^ Request the server for allowed moves
    | MakeMove
        { fromPosition :: Index  -- ^ Index to move from
        , toPosition :: Index    -- ^ Index to move to
        }
        -- ^ Make a move
    | Quit
        -- ^ Quit playing
    deriving (Generic, Show)

--------------------------------------------------------------------------------

deriveJSON jsonOptions ''Index
deriveJSON jsonOptions ''Player
deriveJSON jsonOptions ''ClientNumber
deriveJSON jsonOptions ''Handshake
deriveJSON jsonOptions ''ServerToClient
deriveJSON jsonOptions ''ClientToServer

--------------------------------------------------------------------------------
