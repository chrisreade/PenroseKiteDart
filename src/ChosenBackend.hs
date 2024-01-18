{-|
Module      : ChosenBackend
Description : Exports mainWith and B for the chosen backend 
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

Export the chosen backend
Currently  either
SVG (Diagrams.Backend.SVG.CmdLine) or
Postscript (Diagrams.Backend.Postscript.CmdLine)
-}
module ChosenBackend (module Backend) where

{- Options:
    for svg output
import Diagrams.Backend.SVG.CmdLine as Backend

    for eps output
import Diagrams.Backend.Postscript.CmdLine as Backend

    for gif/animated gif/png/jpg/tif/bmp/pdf output NOT svg
import Diagrams.Backend.Rasterific.CmdLine as Backend
-}

import Diagrams.Backend.SVG.CmdLine as Backend

