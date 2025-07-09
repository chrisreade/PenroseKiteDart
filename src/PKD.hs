{-|
Module      : PKD
Description : The main module which re-exports everything except TgraphExamples
Copyright   : (c) Chris Reade, 2024
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This is the main module (PKD for PenroseKiteDart) which re-exports
    TileLib
and all the Tgraph modules
    Tgraph.Prelude, 
    Tgraph.Decompose, 
    Tgraph.Compose, 
    Tgraph.Force, 
    Tgraph.Relabelling 
    Tgraph.Extras

(Everything except TgraphExamples)
-}


module PKD
  ( -- * Pieces, Patches and Drawable Class
    module TileLib
    -- * Tgraphs, TileFaces, VPatches ... plus HalfTile and Try Modules
  , module Tgraph.Prelude
    -- * Decomposing Tgraphs
  , module Tgraph.Decompose
    -- * Composing Tgraphs
  , module Tgraph.Compose
    -- * Forcing Tgraphs
  , module Tgraph.Force
    -- * Relabelling Tgraphs plus guided equality, union, common faces
  , module Tgraph.Relabelling
    -- * Smart drawing of Tgraphs and other extras 
  , module Tgraph.Extras
    -- * Converting to P3 (rhombus) tilings and drawing
  , module TileLibP3

  ) where

import TileLib
import Tgraph.Prelude
import Tgraph.Decompose
import Tgraph.Compose
import Tgraph.Relabelling
import Tgraph.Force
import Tgraph.Extras
import TileLibP3

