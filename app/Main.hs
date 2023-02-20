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
fig = incorrectExample --forcedBContextsFig
--dartWingContextsFig--kiteWingContextsFig--kiteOppContextsFig--kiteOriginContextsFig--dartOriginContextsFig --sunVContextsFig--checkFool--forcedExtendedContexts--compBoundaryEdgeCaseTrees--boundaryEdgeCaseTrees --starFlakeCardsSmall --padBorder $ drawEmpire2 jackGraph
 --curioPic --kingEmpire--testCommonFacesFig --testKingEmpire--kingEmpireCheck 

main = mainWith fig


{-
-- For profiling - non diagram output

main = putStrLn $ "Number of Faces of force (dartDs!!6) is: " ++ show (length $ faces $ force (dartDs!!6))
-}
incorrectExample:: Diagram B
incorrectExample = padBorder $ lw ultraThin $ vsep 1 $ fmap drawSmartVGraph [okForced,incorrectExtension,successful] where

  successful = force $ addHalfDart (217,218) okForced
  incorrectExtension = addHalfKite (217,218) okForced -- fails on forcing
  okForced = force $ addHalfDart (196,200) $ force $ addHalfDart (97,104) $ force $ startGraph  
  startGraph = makeTgraph
    [RD (42,35,41),LK (33,41,35),RK (33,40,41),LK (33,39,40),RK (33,38,39),LK (33,37,38),RK (33,36,37),LK (33,32,36)
    ,RD (31,36,32),RK (33,35,34),LD (24,34,35),RD (24,30,34),LK (33,34,30),RK (33,30,32),LD (31,32,30),RK (30,28,31)
    ,LK (26,31,28),LK (30,29,28),LK (30,24,23),RK (30,23,29),LD (20,29,23),RD (20,28,29),RK (26,28,27),LD (20,27,28)
    ,RD (20,22,27),LK (26,27,22),RK (26,22,25),LK (18,25,22),RK (17,23,24),LK (17,21,23),RD (20,23,21),RK (18,22,19)
    ,LD (20,19,22),LD (20,21,16),RK (17,16,21),RD (20,16,10),LD (20,10,9),RD (20,9,19),LK (18,19,9),RK (18,9,1)
    ,LK (17,8,16),LK (6,10,16),RK (6,16,8),RK (4,15,5),LK (4,14,15),RK (4,13,14),LK (4,12,13),RK (4,11,12),LK (4,7,11)
    ,RD (8,11,7),RK (6,9,10),LK (6,1,9),LD (8,7,6),RK (4,6,7),LK (4,3,6),RD (1,6,3),LK (4,5,2),RK (4,2,3),LD (1,3,2)
    ]


