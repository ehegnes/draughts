-- |
-- Module: Main
-- Copyright: (C) 2017 Siddhanathan Shanmugam
-- License: GPL (see the file LICENSE)
-- 
-- Maintainer: siddhanathan@gmail.com
-- Stability: experimental
-- Portability: non-portable (GHC Extensions)
--
-- TUI
--

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

--------------------------------------------------------------------------------

import Draughts.Game
import Draughts.Library.Types

import Control.Concurrent  (threadDelay, forkIO, takeMVar)
import Control.Monad       (void)
import Control.Monad.Trans (liftIO)
import Data.Aeson          (encode, decodeStrict')
import Data.Function       (on)
import Data.List           (transpose)
import Data.Maybe          (fromJust)
import Data.Monoid         ((<>))
import GHC.Generics        (Generic)
import Lens.Micro.GHC      ((^.), (&), (^?), ix, (^?!), _Just)
import Network.Socket      (withSocketsDo)

import Options.Generic     ( ParseRecord(..), Wrapped, unwrapRecord, (:::)
                           , Unwrapped, type (<?>)(..) )

import qualified Brick                 as B
import qualified Brick.BChan           as B
import qualified Brick.Widgets.Border  as B
import qualified Brick.Widgets.Center  as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text             as T
import qualified Graphics.Vty          as V
import qualified Network.WebSockets    as WS


--------------------------------------------------------------------------------

-- | Command line arguments
data ClientArgs w = ClientArgs
    { hostName   :: w ::: String <?> "Host Name"
    , portNumber :: w ::: Int    <?> "Port Number"
    , clientName :: w ::: T.Text <?> "Client Name"
    } deriving (Generic)

instance ParseRecord (ClientArgs Wrapped)

--------------------------------------------------------------------------------
-- Types

-- | Named resources
type Name = ()


-- | Custom Events
data Tick
    = Prompt { unPrompt :: String }
        -- ^ Message shown to the Client
    | TRequestMove
        -- ^ Request the Client to make a move
    | ValidMoves { unValidMoves :: [Index] }
        -- ^ List of Valid moves sent back to the Client
    | TMove
        { tMoveFrom :: Index        -- ^ The index from which the Client started
        , tMoveCaptures :: [Index]  -- ^ The pieces captured as a result
        , tMoveTo :: Index          -- ^ The index to which the Client is moving
        }
        -- ^ Move made by the Client


-- | App definition
app :: B.App Game Tick Name
app = B.App
    { B.appDraw         = drawUI
    , B.appChooseCursor = B.neverShowCursor
    , B.appHandleEvent  = handleEvent
    , B.appStartEvent   = pure
    , B.appAttrMap      = const theMap
    }


-- | Map for the Style attributes
theMap :: B.AttrMap
theMap = B.attrMap V.defAttr
    [ (blackSquare, V.white `B.on` V.black     `V.withStyle` V.bold)
    , (redSquare,   V.white `B.on` V.brightRed `V.withStyle` V.bold)
    , (cursorAttr,  V.white `B.on` V.green     `V.withStyle` V.bold)
    , (hlAttr,      V.white `B.on` V.blue      `V.withStyle` V.bold)
    ]


-- | Style attribute names
blackSquare, cursorAttr, hlAttr, redSquare :: B.AttrName
blackSquare = "blackSquare"
cursorAttr  = "cursorAttr"
hlAttr      = "hlAttr"
redSquare   = "redSquare"

--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Event Handling

-- | Helper function to pattern match on 'handleEvent' arguments
vkey :: B.BrickEvent Name Tick -> V.Key
vkey (B.VtyEvent (V.EvKey y [])) = y
-- Handle non-exhaustive patterns
vkey _ = V.KChar '$'


-- | Handle input events to the UI
handleEvent :: Game -> B.BrickEvent Name Tick -> B.EventM Name (B.Next Game)
-- Tick
handleEvent g (B.AppEvent (Prompt v))      = g & updatePrompt v    & B.continue
handleEvent g (B.AppEvent TRequestMove)    = g & tReqMove          & B.continue
handleEvent g (B.AppEvent (ValidMoves xs)) = g & highlightMoves xs & B.continue
handleEvent g (B.AppEvent (TMove f cs t))  = g & tMakeMove f cs t  & B.continue
-- Movement Keys
handleEvent g (vkey -> V.KUp   )      = g & moveCursor UP    & B.continue
handleEvent g (vkey -> V.KDown )      = g & moveCursor DOWN  & B.continue
handleEvent g (vkey -> V.KLeft )      = g & moveCursor LEFT  & B.continue
handleEvent g (vkey -> V.KRight)      = g & moveCursor RIGHT & B.continue
handleEvent g (vkey -> (V.KChar 'k')) = g & moveCursor UP    & B.continue
handleEvent g (vkey -> (V.KChar 'j')) = g & moveCursor DOWN  & B.continue
handleEvent g (vkey -> (V.KChar 'h')) = g & moveCursor LEFT  & B.continue
handleEvent g (vkey -> (V.KChar 'l')) = g & moveCursor RIGHT & B.continue
-- User interaction
handleEvent g (vkey -> V.KEnter)      = do
    let effect
            | g ^. stage == ServerRequestedMove = liftIO . reqMove
            | g ^. stage == MoveInProgress      = liftIO . attemptMove
            | otherwise                         = pure . id
    g & effect >>= B.continue
--handleEvent g (vkey -> V.KEsc)        = g & clearState & B.continue
-- Quit
handleEvent g (vkey -> (V.KChar 'q')) = g & B.halt
-- Handle non-exhaustive patterns
handleEvent g (vkey -> (V.KChar '$')) = g & B.continue
handleEvent g _                       = g & B.continue

--
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- UI

-- | Draw the UI
drawUI :: Game -> [B.Widget Name]
drawUI = pure . drawBoard


-- | Generate Draughts Board
boardNumbers :: Game -> [[B.Widget Name]]
boardNumbers g =
    [ zl oddRowBg  (oddExtras 1 4) (oddRow  1 4)
    , zl evenRowBg (evenExtras 5 8) (evenRow 5 8)
    , zl oddRowBg  (oddExtras 9 12) (oddRow  9 12)
    , zl evenRowBg (evenExtras 13 16) (evenRow 13 16)
    , zl oddRowBg  (oddExtras 17 20) (oddRow  17 20)
    , zl evenRowBg (evenExtras 21 24) (evenRow 21 24)
    , zl oddRowBg  (oddExtras 25 28) (oddRow  25 28)
    , zl evenRowBg (evenExtras 29 32) (evenRow 29 32)
    ]
  where
    (.:) = (.) . (.)

    zl = zipWith3 (\a b c -> B.withAttr (b <> a) c)

    ap f = take 8 .: zipC `on` f

    zipC = (concat . transpose) .: mappend `on` pure

    oddRow  a b = ap id blanks (labels a b)
    evenRow a b = ap id (labels a b) blanks

    oddRowBg  = ap repeat reds blacks
    evenRowBg = ap repeat blacks reds

    oddExtras a b  = ap id (repeat mempty) (extras a b)
    evenExtras a b = ap id (extras a b) (repeat mempty)

    extras = fmap extra .: enumFromTo

    extra i =
                if (g ^. selected == Index i) then cursorAttr
                else if (Index i `elem` g ^. highlighted) then hlAttr
                else mempty

    labels = fmap label .: enumFromTo

    label i =
        case g ^. board . unBoard ^? ix (Index i) ^?! _Just of
            Just (Piece Black Man)  -> cstr $ "⛂"
            Just (Piece White Man)  -> cstr $ "⛀"
            Just (Piece Black King) -> cstr $ "⛃"
            Just (Piece White King) -> cstr $ "⛁"
            Nothing -> cstr $ " "

    reds   = redSquare
    blacks = blackSquare
    blanks = repeat $ cstr " "

    cstr = B.center . B.str


-- | Stylized Title
title :: String
title =
    "  _____  _____           _    _  _____ _    _ _______ _____ \
    \\n |  __ \\|  __ \\     /\\  | |  | |/ ____| |  | |__   __/ ____|\
    \\n | |  | | |__) |   /  \\ | |  | | |  __| |__| |  | | | (___  \
    \\n | |  | |  _  /   / /\\ \\| |  | | | |_ |  __  |  | |  \\___ \\ \
    \\n | |__| | | \\ \\  / ____ \\ |__| | |__| | |  | |  | |  ____) |\
    \\n |_____/|_|  \\_\\/_/    \\_\\____/ \\_____|_|  |_|  |_| |_____/ "


-- | Draw the UI
drawBoard :: Game -> B.Widget Name
drawBoard g = B.vBox
    [ B.vLimit 7 . B.hBox . fmap B.hCenter $
        [B.str title, B.hBox . fmap B.hCenter $
            [ B.borderWithLabel (B.str "Debug Messages") (B.str (g ^. prompt))
            , B.str (sPrompt (g ^. stats)) ]]
    , ( B.border . B.vBox . fmap B.hBox ) (boardNumbers g)
    ]

--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- App

networkApp
    :: B.BChan Tick     -- ^ Bounded Channel receiving Custom Events
    -> Game             -- ^ Game State threaded through the UI
    -> T.Text           -- ^ Name of the 'Client'
    -> WS.ClientApp ()  -- ^ Computation for the Websocket Client Application
networkApp chan g name conn = do
    -- Send Hello message
    WS.sendBinaryData conn (encode . Hello $ name)

    -- Receive Handshake
    hs :: Handshake <- ack . fromJust . decodeStrict' <$> WS.receiveData conn

    -- Check if the handshake succeeded
    case hs of
        -- If the server stated that there are too many clients connected,
        -- then quit.
        TooManyClientsConnected ->
            B.writeBChan chan $
                Prompt "QUIT: Only two people can play on the server!"
        -- Otherwise, the handshake succeded
        ClientHandshake chs -> case chs of
            -- Tell the Player whether he is Player 1
            Player1 _ ->
                B.writeBChan chan $ Prompt "WAIT: Connected as Player 1"
            -- Tell the Player whether he is Player 2
            Player2 _ ->
                B.writeBChan chan $ Prompt "WAIT: Connected as Player 2"

        
    let
        -- Computation to perform when the Server needs a move
        requestComp = do
            -- Initialize the Request Move Phase
            B.writeBChan chan $ TRequestMove
            -- Ask the user to make a move
            B.writeBChan chan . Prompt $ "PLAY: It's your turn!"
            UIRequestMove v <- takeMVar (g ^. snet)
            -- Send the move to the server
            WS.sendBinaryData conn
                (encode . RequestListOfValidMoves $ v)

        -- Client Computation Step
        loop = do
            -- Receive a message from the server
            msg <- fromJust . decodeStrict' <$> WS.receiveData conn

            -- Check the type of message the Server sent
            case (msg :: ServerToClient) of
                -- The server needs a move for the game to proceed
                RequestMove -> do
                    requestComp
                    loop

                -- The server replied with a list of valid moves as requested
                ListOfValidMoves xs -> do
                    -- Tell the user that he is midway in making a move
                    B.writeBChan chan . Prompt $
                        "PLAY: Where do you want to move this piece?"
                    -- Initialize the Make Move Phase
                    B.writeBChan chan . ValidMoves $ xs
                    UIMakeMove f t <- takeMVar (g ^. snet)
                    -- Tell the server about the move the Client wishes to make
                    WS.sendBinaryData conn (encode $ MakeMove f t)
                    loop

                -- The server deemed the requested move invalid
                InvalidMove -> do
                    -- Tell the user that the move was invalid
                    B.writeBChan chan . Prompt $ "ERROR: Invalid move"
                    -- Give the user 2 seconds to see the message
                    threadDelay (2000)
                    -- Ask the user to make a different move
                    requestComp
                    loop

                -- The server accepted a move performed by either the same
                -- or opposing Client
                MovePerformed f cs t -> do
                    -- Render the received Move
                    B.writeBChan chan $ TMove f cs t
                    -- Tell the user that it is the opponent's turn
                    B.writeBChan chan . Prompt $ "WAIT: Opponent's turn"
                    loop

                -- The server announced the winner of the game
                GameOver w ->
                    -- Announce the winner to the user
                    B.writeBChan chan . Prompt $
                        "DONE: " <> show w <> " won!"

                -- Be optimistic about the server message types
                _ -> undefined

    -- Perform computation
    loop

    -- Disconnect from the server once the computation has terminated
    WS.sendClose conn ("Bye!" :: BC.ByteString)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    chan <- B.newBChan 100
    args :: ClientArgs Unwrapped <- unwrapRecord "Client Program"
    g <- initGame
    _ <- forkIO $ withSocketsDo $
        WS.runClient (hostName args) (portNumber args) "/" $
            networkApp chan g (clientName args)
    void $ B.customMain (V.mkVty V.defaultConfig) (Just chan) app g

--------------------------------------------------------------------------------

