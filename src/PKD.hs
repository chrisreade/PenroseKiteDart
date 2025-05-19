{-|
Module      : PKD
Description : A wrapper module including Tgraphs and TileLib
Copyright   : (c) Chris Reade, 2024
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This is the main module to re-export both Tgraphs and TileLib (which includes Halftile) modules.
However it does not export the data constructor Forced (only the newtype operator).
It issues a warning when using makeUncheckedTgraph.
-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE FlexibleContexts          #-}
-- {-# LANGUAGE TypeFamilies              #-}
-- {-# LANGUAGE FlexibleInstances         #-} -- needed for Drawable Patch
-- {-# LANGUAGE TypeOperators             #-} -- needed for type equality constraints ~
 
module PKD ( module Tgraphs
           , module TileLib
           , makeUncheckedTgraph
           ) where

import TileLib
import Tgraphs hiding (makeUncheckedTgraph, Forced(..)) -- hides data constructor
import Tgraphs (Forced) -- import Type only
import qualified Tgraphs as Unchecked (makeUncheckedTgraph)

{-# WARNING makeUncheckedTgraph "Bypasses checks for required Tgraph properties. Use makeTgraph instead" #-}
-- |Now issues a warning.
-- Bypasses checks for required Tgraph properties. Use makeTgraph instead
makeUncheckedTgraph :: [TileFace] -> Tgraph
makeUncheckedTgraph = Unchecked.makeUncheckedTgraph
