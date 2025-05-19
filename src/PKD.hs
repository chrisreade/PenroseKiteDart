{-|
Module      : PKD
Description : A wrapper module including Tgraphs and TileLib
Copyright   : (c) Chris Reade, 2024
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This is the main module to import Tgraphs and TileLib (which includes Halftile) modules.
-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE FlexibleContexts          #-}
-- {-# LANGUAGE TypeFamilies              #-}
-- {-# LANGUAGE FlexibleInstances         #-} -- needed for Drawable Patch
-- {-# LANGUAGE TypeOperators             #-} -- needed for type equality constraints ~
 
module PKD ( module Tgraphs
           , module TileLib
           , makeUncheckedTgraph
           , uncheckedCompose
           , uncheckedPartCompose
           ) where


import TileLib
import Tgraphs hiding (makeUncheckedTgraph, uncheckedCompose, uncheckedPartCompose)
import qualified Tgraphs as Unchecked (makeUncheckedTgraph, uncheckedCompose, uncheckedPartCompose)

{-# WARNING makeUncheckedTgraph "Bypasses checks for required Tgraph properties. Use makeTgraph instead" #-}
-- |Now issues a warning.
-- Bypasses checks for required Tgraph properties. Use makeTgraph instead
makeUncheckedTgraph :: [TileFace] -> Tgraph
makeUncheckedTgraph = Unchecked.makeUncheckedTgraph

{-# WARNING uncheckedCompose "Only safe when applied to a forced Tgraph. Use compose instead" #-}
-- |Now issues a warning
-- Only safe when applied to a forced Tgraph. Use compose instead
uncheckedCompose :: Tgraph -> Tgraph
uncheckedCompose = Unchecked.uncheckedCompose

{-# WARNING uncheckedPartCompose "Only safe when applied to a forced Tgraph. Use partCompose instead" #-}
-- |Now issues a warning
-- Only safe when applied to a forced Tgraph. Use partCompose instead
uncheckedPartCompose :: Tgraph -> ([TileFace], Tgraph)
uncheckedPartCompose = Unchecked.uncheckedPartCompose
