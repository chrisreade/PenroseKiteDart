{-|
Module      : PKD
Description : A wrapper module including Tgraphs and TileLib
Copyright   : (c) Chris Reade, 2024
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This is the main module to import Tgraphs and TileLib (whichh includes Halftile) modules.
-}
-- {-# OPTIONS_HADDOCK ignore-exports #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE FlexibleContexts          #-}
-- {-# LANGUAGE TypeFamilies              #-}
-- {-# LANGUAGE FlexibleInstances         #-} -- needed for Drawable Patch
-- {-# LANGUAGE TypeOperators             #-} -- needed for type equality constraints ~

module PKD ( module Tgraphs
           , module TileLib
           ) where

import Tgraphs
import TileLib

