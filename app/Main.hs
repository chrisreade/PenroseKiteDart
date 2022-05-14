{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
    
import Diagrams.Prelude
import ChosenBackend (B, mainWith)

import TileFigExamples
-- import DecompExplain (used for the single figure decompExplainFig)

{- e.g
markedTiles, componentsFig, componentsFig2, fourDecomps, fiveInflate, fiveAlternatives, 
sun6Fig, sun5Over6Fig, filledSun6, filledSun6' , experimentFig, 
-}
{- and colour examples: swatches and samples
e.g. showSample (darkcyan, darkblue, wheat)
-}

import GraphFigExamples

-- imported for testing / debugging only
import TileLib
import Tgraphs
import Tgraph.Convert

                
fig::Diagram B
fig = kkEmpsFig
--halfWholeFig --drawForceEmplace sunPlus3Dart' --maxShapesFig -- newDartPlusFig -- maxEmplaceFig --halfWholeFig

main = mainWith fig
