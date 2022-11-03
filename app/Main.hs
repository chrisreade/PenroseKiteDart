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

-- To produce executable which can output multiple diagrams 
figs::[(String,Diagram B)]
figs = trial 

main = mainWith figs

-}


-- normal executable
fig::Diagram B
fig = cdMistake1Fig  -- dartChoiceLimeGold --forceFig --rocket5Fig --curioPic --halfWholeFig --boundaryGap5Fig

main = mainWith fig



{-
-- For profiling - non diagram output

main = putStrLn $ "Number of Faces of force (dartDs!!6) is: " ++ show (length $ faces $ force (dartDs!!6))
-}
