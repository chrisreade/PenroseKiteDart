{-|
Module      : CheckBackend
Description : Introduces a class synonym OKBackend for requirements of a backend
Copyright   : (c) Chris Reade, 2024
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module introduces a class synonym OKBackend to abbreviate requirements of a backend for drawing tilings.
The instance declaration requires UndecidableInstances to be enabled.

-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE TypeOperators             #-}

module CheckBackend 
  ( OKBackend
  ) where

import Diagrams.Prelude
import Diagrams.TwoD.Text (Text)


-- |Class OKBackend is a class synonym for suitable constraints on Backends for drawing tilings.
class (V b ~ V2, N b ~ Double, Renderable (Path V2 Double) b, Renderable (Text Double) b) =>
      OKBackend b where {}

-- |Instance declaration for OKBackend requires UndecidableInstances to be enabled,
-- but allows a suitable backend B to be recognised as an instance without explicitly writing
-- instance OKBackend B
-- Note B will be declared by user of this library and is not declared in the library
instance (V b ~ V2, N b ~ Double, Renderable (Path V2 Double) b, Renderable (Text Double) b) =>
         OKBackend b where {}
