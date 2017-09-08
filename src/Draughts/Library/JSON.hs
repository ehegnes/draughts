-- |
-- Module: Draughts.Library.JSON
-- Copyright: (C) 2017 Siddhanathan Shanmugam
-- License: GPL (see the file LICENSE)
-- 
-- Maintainer: siddhanathan@gmail.com
-- Stability: experimental
-- Portability: non-portable (GHC Extensions)
--
-- JSON
--

module Draughts.Library.JSON
    (jsonOptions) where

--------------------------------------------------------------------------------

import Data.Aeson       ()
import Data.Aeson.Types (Options(..), defaultOptions)

--------------------------------------------------------------------------------

jsonOptions :: Options
jsonOptions = defaultOptions
    { unwrapUnaryRecords = True
    }

--------------------------------------------------------------------------------

