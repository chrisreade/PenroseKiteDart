{-|
Module      : PKD
Description : A wrapper module including Tgraphs and TileLib
Copyright   : (c) Chris Reade, 2024
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This is the main module to re-export both Tgraphs and TileLib (which includes Halftile) modules.
However it does not export the data constructor Forced (only the newtype operator).
Thers is a about using makeUncheckedTgraph.
-}

 
module PKD ( module Tgraphs
           , module TileLib
           , makeUncheckedTgraph
           ) where

import TileLib
import Tgraphs hiding (makeUncheckedTgraph, Forced(..)) -- hides data constructor
import Tgraphs (Forced) -- import Type only
import qualified Tgraphs as Unchecked (makeUncheckedTgraph)

{-# WARNING makeUncheckedTgraph "Bypasses checks for required Tgraph properties. Use makeTgraph instead" #-}
-- |Now has a warning.
makeUncheckedTgraph :: [TileFace] -> Tgraph
makeUncheckedTgraph = Unchecked.makeUncheckedTgraph
