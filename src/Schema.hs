-- |
-- Module: Main
-- Copyright: (C) 2017 Siddhanathan Shanmugam
-- License: GPL (see the file LICENSE)
-- 
-- Maintainer: siddhanathan@gmail.com
-- Stability: experimental
-- Portability: non-portable (GHC Extensions)
--
-- JSON Schema generator
--

module Main where

--------------------------------------------------------------------------------

import Draughts.Library.Types

import Data.JSON.Schema.Generator (generate)
import Data.Proxy                 (Proxy(..))

import qualified Data.ByteString.Lazy.Char8 as BLC

--------------------------------------------------------------------------------

instance JSONSchemaGen Index
instance JSONSchemaGen Player
instance JSONSchemaGen ClientNumber
instance JSONSchemaGen Handshake
instance JSONSchemaGen ServerToClient
instance JSONSchemaGen ClientToServer


-- | Main computation to generate the JSON schema and output to stdout.
main :: IO ()
main = do
    BLC.putStrLn $ generate (Proxy :: Proxy ServerToClient)
    BLC.putStrLn $ generate (Proxy :: Proxy ClientToServer)

--------------------------------------------------------------------------------

