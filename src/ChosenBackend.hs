{-|
Module      : ChosenBackend
Description : Exports mainWith and B for the chosen backend 
Copyright   : (c) Chris Reade, 2021
License     : MIT
Maintainer  : chrisreade@mac.com
Stability   : experimental

Export mainWith and B for the chosen backend
Currently  either
SVG (Diagrams.Backend.SVG.CmdLine) or
Postscript (Diagrams.Backend.Postscript.CmdLine)
-}
module ChosenBackend (B, mainWith) where

import Diagrams.Backend.SVG.CmdLine
-- import Diagrams.Backend.Postscript.CmdLine
-- import Diagrams.Backend.SVG.CmdLine