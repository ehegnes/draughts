-- |
-- Module: Main
-- Copyright: (C) 2017 Siddhanathan Shanmugam
-- License: GPL (see the file LICENSE)
-- 
-- Maintainer: siddhanathan@gmail.com
-- Stability: experimental
-- Portability: non-portable (GHC Extensions)
--
-- Server
--

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

--------------------------------------------------------------------------------

import Draughts.Library.Board
import Draughts.Library.Types

import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import Control.Concurrent.QSem (QSem, newQSem, waitQSem, signalQSem)
import Control.Monad           (when)
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.Loops     (untilJust)
import Data.Aeson              (decodeStrict', ToJSON)
import Data.Aeson.Text         (encodeToLazyText)
import Data.Function           ((&))
import Data.Maybe              (fromJust, isNothing, isJust)
import Data.Monoid             ((<>))
import GHC.Generics            (Generic)
import Lens.Micro              ((.~), (^.), (?~), (^?!), _Just, (%~))
import Lens.Micro.TH           (makeLenses)
import Options.Generic         ( ParseRecord(..), Wrapped, unwrapRecord
                               , (:::), Unwrapped, type (<?>)(..) )

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Network.WebSockets    as WS

--------------------------------------------------------------------------------

-- | Command line arguments
data ServerArgs w = ServerArgs
    { hostName   :: w ::: String <?> "Host Name"
        -- ^ Host Name (use 127.0.0.1 for localhost)
    , portNumber :: w ::: Int    <?> "Port Number"
        -- ^ Port Number (use 80 when connecting to naive Websocket clients)
    } deriving (Generic)

instance ParseRecord (ServerArgs Wrapped)

-- | Client connected to the Server
data Client = Client
    { _clientDetails :: ClientNumber  -- ^ Client identification
    , _connection :: WS.Connection    -- ^ Client connection details
    }

makeLenses ''Client

-- | Internal state used by Server while playing the game
data ServerState = ServerState
    { _blackClient :: Maybe Client  -- ^ Player1 connection details
    , _whiteClient :: Maybe Client  -- ^ Player2 cennection details
    , _serverBoard :: Board         -- ^ State of the Board
    , _serverTurn  :: Player        -- ^ Next person to play
    }

makeLenses ''ServerState

-- | Initial server state
newServerState :: ServerState
newServerState = ServerState
    { _blackClient = Nothing
    , _whiteClient = Nothing
    , _serverBoard = initialBoard
    , _serverTurn = Black
    }

-- | Number of 'Client' connected to the Server
numClients
    :: ServerState  -- ^ 'ServerState' to get 'Client' info from
    -> Int          -- ^ Number of 'Client' connected to the Server
numClients s =
    length . filter isJust $ [(s ^. blackClient), (s ^. whiteClient)]

-- | Send a message to a specific 'Client'
sendMessage
    :: (ToJSON a, MonadIO m)
    => Client  -- ^ 'Client' to send message to
    -> a       -- ^ Message to send to 'Client'
    -> m ()
sendMessage client =
    liftIO . WS.sendTextData (client ^. connection) . encodeToLazyText

-- | Send a message to both 'Client'
broadcastMessage
    :: (ToJSON a, MonadIO m)
    => ServerState  -- ^ 'ServerState' to get 'Client' info from
    -> a            -- ^ Message to send to all 'Client'
    -> m ()
broadcastMessage state x = do
    liftIO $ sendMessage (state ^. blackClient ^?! _Just) x
    liftIO $ sendMessage (state ^. whiteClient ^?! _Just) x

-- | Get 'Client' color from identification
clientColor
    :: ClientNumber  -- ^ 'Client' indentity
    -> Player        -- ^ 'Client' color
clientColor (Player1 _) = Black
clientColor (Player2 _) = White

-- | Function to get 'Client' connection details, given the 'Client' color
getClient
    :: Functor f
    => Player  -- ^ 'Client' color
    -> (Maybe Client -> f (Maybe Client)) -> ServerState -> f ServerState
getClient Black = blackClient
getClient White = whiteClient

--------------------------------------------------------------------------------

-- | Websocket Server Application
application
    :: MVar ServerState  -- ^ Internal Server State
    -> QSem              -- ^ Quantity Semaphore for number of 'Client'
    -> WS.ServerApp      -- ^ Computation for the Websocket Server
application state players pending = do
    -- Connect with the 'Client'
    conn <- WS.acceptRequest pending

    -- Keep the connection alive
    WS.forkPingThread conn 30

    -- Establish handshake with 'Client'
    p <- connectClient conn

    -- XXX The server only supports two clients.
    -- If an additional client connects, reject that client.
    -- The server must be restarted if a client quits/disconnects.
    when (isNothing p) $ do
        let ackF = Acknowledge TooManyClientsConnected
        WS.sendTextData conn . encodeToLazyText $ ackF
        BC.putStrLn "An extra client connected. Please restart the server"

    -- XXX The signals to the QSem allow Thread 1 to proceed, but
    -- prevent Thread 2 or newer from proceeding. This prevents
    -- race conditions in the server.
    --
    -- How does this work?
    --
    -- Thread 1 (by Player 1) can signal the first QSem.
    -- Thread 2 (by Player 2) can signal the second QSem.
    -- Any additional thread is not allowed to signal.
    -- Even though Thread 2 is waiting for 2 QSems, it will only get one.
    -- This prevents Thread 2 from executing the server again.
    --
    waitQSem players -- Wait for Player 1
    waitQSem players -- Wait for Player 2

    -- Play the game until one of the two players win
    gameWinner <- untilJust (playGame state)

    -- Congratulate the winner!
    putStrLn $ show gameWinner <> " won!"
  where
    -- | Handshake with 'Client'
    connectClient
        :: (MonadIO m)
        => WS.Connection            -- ^ 'Client' connection info
        -> m (Maybe ClientNumber)  -- ^ Success or Failure on connecting
    connectClient conn = do
        -- Receive Hello from the 'Client'
        msg :: B.ByteString <- liftIO $ WS.receiveData conn
        let Hello name = fromJust . decodeStrict' $ msg

            -- Establish computation to do handshake
            handshake p = do
                let ackH = Acknowledge . ClientHandshake $ p
                    client = Client p conn
                    z = getClient . clientColor
                liftIO $ modifyMVar_ state (pure . (& z p ?~ client))
                liftIO $ signalQSem players
                liftIO $ WS.sendTextData conn . encodeToLazyText $ ackH
                pure (Just p)

        -- Read clients list from server state
        clients <- liftIO $ readMVar state
        -- Handshake depending on number of already connected users
        case numClients clients of
            0 -> handshake (Player1 name)
            1 -> handshake (Player2 name)
            _ -> pure Nothing

    -- | Step through the game
    playGame
        :: MonadIO m
        => MVar ServerState   -- ^ Internal Game State
        -> m (Maybe Player)  -- ^ The winning Player
    playGame s = do
        -- Request a Move from the Client
        player <- getPlayer
        sendMessage player $ RequestMove

        -- Wait for Client to move
        r <- loop

        -- Swap turns
        liftIO $ modifyMVar_ s (pure . (& serverTurn %~ opposite))

        -- Announce the results
        pure r
      where
        -- | Get 'Client' info
        getPlayer :: MonadIO m => m Client
        getPlayer = do
            mst <- liftIO $ readMVar s
            pure $ mst ^. getClient (mst^.serverTurn) ^?! _Just

        -- | One step of the Game computation
        loop :: MonadIO m => m (Maybe Player)
        loop = do
            -- Get Environment details
            mst <- liftIO $ readMVar s
            player <- getPlayer

            -- Get message from 'Client'
            msg <- liftIO $ WS.receiveData (player ^. connection)
            case decodeStrict' (msg :: B.ByteString) of

                -- Client requests back a list of valid moves
                Just (RequestListOfValidMoves i) -> do
                    let color = player ^. clientDetails & clientColor
                        mvs = playerTransitions (mst ^. serverBoard) color i
                        xs = if null mvs
                             then InvalidMove
                             else ListOfValidMoves mvs

                    -- Send client Valid moves
                    sendMessage player $ xs
                    loop

                -- Client makes a move
                Just (MakeMove f t) -> do
                    let p = mst ^. serverTurn
                        b = mst ^. serverBoard
                        movesBoard = makeMove b p f t

                    case movesBoard of
                        -- If the server accepts the move, then proceed
                        Just (nb, cs) -> do
                            -- The move needs to be sent to both 'Client'
                            -- This allows the opponent to know the move
                            let xs = MovePerformed f cs t
                            broadcastMessage mst $ xs
                            liftIO $ modifyMVar_ s
                                ( pure
                                . (& serverBoard %~ crownBoard)
                                . (& serverBoard .~ nb) )
                            -- Check if the user has won after making the move
                            case hasWon nb of
                                Just w -> do
                                    broadcastMessage mst $ GameOver w
                                    pure . pure $ w
                                Nothing ->
                                    pure Nothing
                        -- If the server rejects the move, then tell the client
                        Nothing -> do
                            sendMessage player $ InvalidMove
                            loop

                -- Be optimistic about the Client implementation
                _ -> undefined

--------------------------------------------------------------------------------

main :: IO ()
main = do
    args :: ServerArgs Unwrapped <- unwrapRecord "Server Program"
    state <- newMVar newServerState
    players <- newQSem 0
    WS.runServer (hostName args) (portNumber args) $ application state players

--------------------------------------------------------------------------------

