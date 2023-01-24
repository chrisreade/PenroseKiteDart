{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
    
import Diagrams.Prelude
import ChosenBackend (B, mainWith)
import DecompExplain --(used for the single figure decompExplainFig)

import TileFigExamples
{- includes e.g
markedTiles, markedtiles2, piecesFig, piecesFig2, newPiecesFig, tileIntro
fourDecomps, fiveInflate, fiveAlternatives, 
sun6Fig, sun5Over6Fig, filledSun6, filledSun6' , experimentFig. twoLevelsFig

dartWingFig, 

Also with Colours:

e.g. filledSun6, newFillSun6
exampleTriple1, exampleTriple2

Also swatches and samples:
e.g. swatchFig0, sampleFig0
-}

import GraphFigExamples

-- imported for testing / debugging only
import TileLib
import Tgraphs

import ArtWork
               


{-

-- To produce executable which can output multiple diagrams, use
figs::[(String,Diagram B)]
figs = trial2

main = mainWith figs

-}



-- normal executable generating single figure
fig::Diagram B
fig = drawEmpire1 kingGraph--testLoops2--boundaryEdgeCaseTrees --starFlakeCardsSmall --padBorder $ drawEmpire2 jackGraph
 --curioPic --kingEmpire--testCommonFacesFig --testKingEmpire--kingEmpireCheck 

main = mainWith fig





{-
-- For profiling - non diagram output

main = putStrLn $ "Number of Faces of force (dartDs!!6) is: " ++ show (length $ faces $ force (dartDs!!6))
-}
