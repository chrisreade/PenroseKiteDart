{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TupleSections             #-}

{-|
Module      : TgraphExamples
Description : Examples of tilings represented with Tgraphs and their diagrams 
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

-}
module TgraphExamples where

import Diagrams.Prelude
import Diagrams.TwoD.Text (Text)
-- import ChosenBackend (B)
import TileLib
import Tgraphs

{-*
Some Layout tools
-}

-- |used for most diagrams to give border padding
-- padBorder:: Diagram B -> Diagram B
padBorder :: Renderable (Path V2 Double) b => Diagram2D b -> Diagram2D b
padBorder = pad 1.2 . centerXY

-- |chunks n l -  split a list l into chunks of length n (n>0)
chunks::Int -> [a] -> [[a]]
chunks n
  | n < 1 = error "chunks: argument <1\n"
  | otherwise = ch where
      ch [] = []
      ch as = take n as : ch (drop n as)

-- |arrangeRows n diags - arranges diags into n per row, centering each row horizontally.
-- The result is a single diagram (seperation is 1 unit vertically and horizontally).
-- arrangeRows :: Int -> [Diagram B] -> Diagram B
arrangeRows :: Renderable (Path V2 Double) b => 
               Int -> [Diagram2D b] -> Diagram2D b
arrangeRows n = centerY . vsep 1 . fmap (centerX . hsep 1) . chunks n

-- |add a given label at a given point offset from the centre of the given diagram
labelAt :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => 
           Point V2 Double -> String -> Diagram2D b -> Diagram2D b
labelAt p l d = baselineText l # fontSize (output 15) # moveTo p <> d
--labelAt p l d = baselineText l # fontSize (normalized 0.02) # moveTo p <> d

{-*
Basic Tgraphs with Figures
-}
fool, foolD, foolDminus:: Tgraph
-- |fool: fool's kite - also called an ace.
fool = makeTgraph [RK (5,2,7),LK (5,6,4),RK (5,4,3),LK (5,3,2),RD (1,2,3),LD (1,3,4)]
-- fool = makeTgraph [ RD (1,2,3), LD (1,3,4), RK (6,2,5), LK (6,3,2), RK (6,4,3), LK (6,7,4)]

-- |a once decomposed fool (= foolDs!!1)
foolD = decompose fool

-- |foolDminus: 3 faces removed from foolD - still a valid Tgraph
foolDminus = removeFaces [RD (5,15,13), LD (5,16,15), RK (7,11,2)] foolD
-- foolDminus = removeFaces [RD (6,15,13), LD (6,17,15), RK (5,11,2)] foolD

-- | an infinite list of decompositions of fool
foolDs :: [Tgraph]
foolDs = decompositions fool

-- | diagram of just fool.
-- foolFig :: Diagram B
foolFig :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
foolFig = padBorder $ labelLarge drawj fool

-- |diagram of fool with foolD.
-- foolAndFoolD :: Diagram B
foolAndFoolD :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
foolAndFoolD = padBorder $ hsep 1 [scale phi $ labelled drawj fool, labelled drawj foolD]

-- |Tgraph for a sun
sunGraph :: Tgraph
sunGraph = makeTgraph
             [ RK (1,2,11), LK (1,3,2)
             , RK (1,4,3) , LK (1,5,4)
             , RK (1,6,5) , LK (1,7,6)
             , RK (1,8,7) , LK (1,9,8)
             , RK (1,10,9), LK (1,11,10)
             ]
-- | an infinite list of decompositions of sunGraph
sunDs :: [Tgraph]
sunDs =  decompositions sunGraph

-- |Figure for a 3 times decomposed sun with a 2 times decomposed sun
figSunD3D2 :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
figSunD3D2 = padBorder $ hsep 1 [labelled drawj $ sunDs !! 3, scale phi $ labelled drawj $ sunDs !! 2]

-- |Tgraph for kite
kiteGraph :: Tgraph
kiteGraph = makeTgraph [ RK (1,2,4), LK (1,3,2)]

-- | an infinite list of decompositions of a kite
kiteDs :: [Tgraph]
kiteDs = decompositions kiteGraph

-- |Tgraph for a dart
dartGraph :: Tgraph
dartGraph =  makeTgraph [ RD (1,2,3), LD (1,3,4)]

-- | an infinite list of decompositions of a dart
dartDs :: [Tgraph]
dartDs =  decompositions dartGraph

-- |Tgraph of 4 times decomposed dartGraph (used in several examples)
dartD4 :: Tgraph
dartD4 = dartDs!!4

{-* Partial Composition figures
-}

pCompFig1,pCompFig2,pCompFig :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
-- |diagram showing partial composition of a forced 3 times decomposed dart (with remainder faces in pale green)
pCompFig1 = lw ultraThin $ hsep 5 $ rotations [1,1] [draw fd3, drawPCompose fd3]
            where fd3 = force $ dartDs!!3
-- |diagram showing partial composition of a forced 3 times decomposed kite (with remainder faces in pale green)
pCompFig2 = lw ultraThin $ hsep 5 [draw fk3, drawPCompose fk3]
            where fk3 = force $ kiteDs!!3
-- |diagram showing two partial compositions (with remainder faces in pale green)
pCompFig = padBorder $ vsep 3 [center pCompFig1, center pCompFig2]

{-* Forced Tgraph figures
-}

-- |diagram of foolDminus and the result of forcing              
forceFoolDminus :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
forceFoolDminus = padBorder $ hsep 1 $ fmap (labelled drawj) [foolDminus, force foolDminus]


-- |diagrams of forced graphs (5 times decomposed kite or dart or sun)           
forceDartD5Fig,forceKiteD5Fig,forceSunD5Fig,forceFig :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
forceDartD5Fig = padBorder $ drawForce $ dartDs !! 5
forceKiteD5Fig = padBorder $ rotate (ttangle 1) $ drawForce $ kiteDs!!5
forceSunD5Fig =  padBorder $ drawForce $ sunDs !! 5
forceFig = hsep 1 [forceDartD5Fig,forceKiteD5Fig]

-- |an example showing a 4 times forceDecomp pair of darts (sharing a long edge),
-- with the maximal compForce Tgraph (a kite) overlaid in red
maxExampleFig :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
maxExampleFig = padBorder $ lw ultraThin $ drawWithMax $ allForceDecomps dartPlusDart !! 4 where
                 dartPlusDart = addHalfDart (1,5) $ addHalfDart (1,2) dartGraph

-- |showing 4 emplaceChoices for foolD 
-- Uses revised emplaceChoices.
emplaceChoicesFoolD :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
emplaceChoicesFoolD = padBorder $ hsep 1 $
        fmap (addFoolD . lw ultraThin . draw) vpChoices where
        (vpFoolD:vpChoices) = alignAll (1,6) $ fmap makeVP (foolD:emplaceChoices foolD)
        addFoolD fig = (lc red . lw thin . drawj) vpFoolD <> fig

{-*
Removed faces (forcing and composing)
-}
-- |brokenDart is a 4 times decomposed dart (dartD4) with 5 halftile faces removed.
-- Forcing will repair to produce the same Tgraph as force dartD4.
-- This graph can also be repeatedly composed (without forcing) to get a maximal Tgraph.
brokenDart :: Tgraph
brokenDart = removeFaces deleted dartD4 where
  deleted = [RK (2,16,33),LD (15,33,16),RK (16,66,15),LK (16,67,66),LK (5,15,66)]

{-| badlyBrokenDart has more faces removed from brokenDart.
This will also get repaired by forcing (to produce the same as force dartD4).
However it will fail to produce a valid Tgraph if composed twice without forcing. 
-}
badlyBrokenDart :: Tgraph
badlyBrokenDart = removeFaces deleted bbd where
  deleted = [RK (6,28,54)]
  bbd = removeVertices [63,37] brokenDart
--  deleted = RK(6,28,54):filter (isAtV 63) (faces brokenDart)

-- |brokenDartFig shows the faces removed from dartD4 to make brokenDart and badlyBrokenDart
brokenDartFig  :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
brokenDartFig = padBorder $ lw thin $ hsep 1 $ fmap (labelled drawj) [dartD4, brokenDart, badlyBrokenDart]

-- |badlyBrokenDartFig shows badlyBrokenDart, followed by its composition, followed by the faces 
-- that would result from an unchecked second composition which are not tile-connected.
-- (Simply applying compose twice to badlyBrokenDart will raise an error).
badlyBrokenDartFig :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
badlyBrokenDartFig = padBorder $ lw thin $ hsep 1 $ fmap (labelled drawj) [vp, vpComp, vpFailed] where
    vp = makeVP badlyBrokenDart
    comp = compose badlyBrokenDart
    vpComp = restrictVP vp $ faces $ comp
    vpFailed  = restrictVP vp $ composedFaces comp

-- |figure showing the result of removing incomplete tiles (those that do not have their matching halftile)
-- to a 3 times decomposed sun.
removeIncompletesFig :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
removeIncompletesFig = padBorder $ drawj $ removeFaces (boundaryJoinFaces g) g where 
    g = sunDs !! 3

{-*
Incorrect Tgraphs (and other problem Tgraphs)
-}

-- |mistake is a legal but incorrect Tgraph - a kite with 2 darts on its long edges
mistake:: Tgraph
mistake = makeTgraph [RK (1,2,4), LK (1,3,2), RD (3,1,5), LD (4,6,1), LD (3,5,7), RD (4,8,6)]

-- |mistake1 is a kite bordered by 2 half darts (subgraph of mistake and still incorrect)
mistake1:: Tgraph
mistake1 = makeTgraph [RK (1,2,4), LK (1,3,2), RD (3,1,5), LD (4,6,1)]

-- |Figure showing an incorrect tiling (left) with a false queen vertex at 2.
-- and (right) the result of forcing before the queen rules were generalised.
-- Forcing will now identify the left Tgraph as stuck/incorrect.
falseQueenFig :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
falseQueenFig = padBorder $ hsep 1 $ [smart (labelled draw) g, labelled draw fg] where
  g = makeTgraph [LK(1,2,3),RK(4,3,2),RK(1,3,5),LK(4,6,3),RK(1,7,2),LK(4,2,8)]
  fg = makeTgraph [LK (4,21,17),RK (4,20,21),LK (4,19,20),RK (4,18,19),LK (4,10,18)
                  ,RK (4,17,6),LD (9,6,17),LK (1,16,7),RK (1,15,16),LK (1,14,15)
                  ,RK (1,13,14),LK (1,12,13),RK (1,11,12),LK (1,5,11),RD (9,11,5)
                  ,RK (4,8,10),LD (9,5,3),RD (9,3,6),LK (1,2,3),RK (4,3,2),RK (1,3,5)
                  ,LK (4,6,3),RK (1,7,2),LK (4,2,8)]
{-*
Figures for 7 vertex types
-}
{-| vertexTypesFig is 7 vertex types single diagram as a row -}
vertexTypesFig :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
vertexTypesFig = padBorder $ hsep 1 lTypeFigs
 where
 lTypeFigs = zipWith (labelAt (p2 (0,-2.2))) ["sun","star","jack","queen","king","ace","deuce"] vTypeFigs
 vTypeFigs = zipWith drawVertex
               [sunGraph, starGraph, jackGraph, queenGraph, kingGraph, aceGraph,  deuceGraph]
               [(1,2),    (1,2),     (1,2),     (1,2),      (1,2),     (3,6),     (2,6)] -- alignments
 drawVertex g alm = alignBefore (lw thin . showOrigin . drawj) alm g

{-|Graphs for the vertex types (other than sunGraph previously declared)
    7 vertex types are, sunGraph, starGraph, jackGraph, queenGraph, kingGraph, aceGraph=fool, deuceGraph
-}
jackGraph,kingGraph,queenGraph,aceGraph,deuceGraph,starGraph::Tgraph
jackGraph = makeTgraph
  [LK (1,9,11),RK (1,11,2),LK (7,8,1),RD (9,1,8),RK (1,3,4)
  ,LK (1,2,3),RK (7,1,5),LD (4,5,1),LD (9,8,10),RD (4,6,5)
  ]
kingGraph = makeTgraph
  [LD (1,2,3),RD (1,11,2),LD (1,4,5),RD (1,3,4),LD (1,10,11)
  ,RD (1,9,10),LK (9,1,7),RK (9,7,8),RK (5,7,1),LK (5,6,7)
  ]
queenGraph = makeTgraph
  [LK (7,1,5),RK (3,5,1),LD (1,2,3),RK (7,9,1),LK (11,1,9)
  ,RD (1,11,2),RK (7,5,6),LK (7,8,9),LK (3,4,5),RK (11,9,10)
  ]

aceGraph = fool -- centre 3

deuceGraph = makeTgraph
  [LK (7,8,2),RK (7,2,6),RK (5,2,4),LK (5,6,2),LD (1,4,2)
  ,RD (1,2,8),RD (1,3,4),LD (1,8,9)
  ] -- centre 2

starGraph = makeTgraph
  [LD (1,2,3),RD (1,11,2),LD (1,10,11),RD (1,9,10),LD (1,8,9)
  ,RD (1,7,8),LD (1,6,7),RD (1,5,6),LD (1,4,5),RD (1,3,4)
  ]

{-|forceVFigures is a list of 7 diagrams - force of 7 vertex types -}
forceVFigures :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => [Diagram2D b]
forceVFigures = rotations [0,0,9,5,0,0,1] $
                fmap (center . drawForce) [sunGraph,starGraph,jackGraph,queenGraph,kingGraph,aceGraph,deuceGraph]

{-*
Sun with darts and superForce examples
-}

sun1Dart,sun2AdjDart,sun2Dart,sun3AdjDart,sun3Dart :: Tgraph
-- |A sun with a single complete dart on the boundary
sun1Dart = addHalfDart (3,4) $ addHalfDart (2,3) sunGraph
-- |A sun with 2 darts adjacent on the boundary
sun2AdjDart = addHalfDart (5,6) $ addHalfDart (4,5) sun1Dart
-- |A sun with 2 darts NOT adjacent on the boundary
sun2Dart = addHalfDart (7,8) $ addHalfDart (6,7) sun1Dart
-- |A sun with 3 darts adjacent on the boundary
sun3AdjDart = addHalfDart (7,8) $ addHalfDart (6,7) sun2AdjDart
-- |A sun with 3 darts on the boundary NOT all adjacent
sun3Dart = addHalfDart (9,10) $ addHalfDart (8,9) sun2AdjDart


-- |Diagram showing superForce with initial Tgraph g (red), force g (red and black),
-- and superForce g (red and black and blue).
superForceFig :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
superForceFig = padBorder $ rotate (ttangle 1) $ drawSuperForce g where
    g = addHalfDart (220,221) $ force $ decompositions fool !!3

-- |Diagram showing 4 rockets formed by applying superForce to successive decompositions
-- of sun3Dart. The decompositions are in red with normal force additions in black and superforce additions in blue.
superForceRocketsFig :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
superForceRocketsFig = padBorder $ lw veryThin $ vsep 1 $ rotations [8,9,9,8] $
   fmap drawSuperForce decomps where
      decomps = take 4 $ decompositions sun3Dart


{-*
Other miscellaneous Tgraphs and Diagrams
-}

-- |graphs of the boundary faces only of forced graphs (dartDs!!4 and dartDs!!5)
boundaryFDart4, boundaryFDart5 :: Tgraph
boundaryFDart4 = checkedTgraph $ boundaryFaces $ force $ makeBoundaryState dartD4
boundaryFDart5 = checkedTgraph $ boundaryFaces $ force $ makeBoundaryState (dartDs!!5)

-- |figures of the boundary faces only of a forced graph
boundaryFDart4Fig,boundaryFDart5Fig :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
boundaryFDart4Fig = padBorder $ lw ultraThin $ labelSmall drawj boundaryFDart4
boundaryFDart5Fig = padBorder $ lw ultraThin $ labelSmall drawj boundaryFDart5

-- |graphs of the boundary faces only of a forced graph - with extra faces removed to make a gap
boundaryGapFDart4, boundaryGapFDart5 :: Tgraph
boundaryGapFDart4 = removeVertices [354] boundaryFDart4
    -- checkedTgraph $ filter ((/=354).originV)  (faces boundaryFDart4)
boundaryGapFDart5 = removeVertices [1467] boundaryFDart5
    -- checkedTgraph $ filter ((/=1467).originV) (faces boundaryFDart5)

-- |figures for the boundary gap graphs boundaryGapFDart4, boundaryGapFDart5
boundaryGap4Fig, boundaryGap5Fig  :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
boundaryGap4Fig = padBorder $ lw ultraThin $ labelSmall drawj boundaryGapFDart4
boundaryGap5Fig = padBorder $ lw ultraThin $ labelSmall drawj boundaryGapFDart5


{-*
Boundary coverings and empires
-}

-- | boundaryVCoveringFigs bd - produces a list of diagrams for the boundaryVCovering of bd 
-- (with the Tgraph represented by bd shown in red in each case)
boundaryVCoveringFigs :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) =>
                         BoundaryState -> [Diagram2D b]
boundaryVCoveringFigs bd =
    fmap (lw ultraThin . (redg <>) . alignBefore draw alig . recoverGraph) $ boundaryVCovering bd
      where redg = lc red $ draw g --alignBefore draw alig g
            alig = defaultAlignment g
            g = recoverGraph bd

-- | boundaryECoveringFigs bd - produces a list of diagrams for the boundaryECovering of bd  
-- (with the Tgraph represented by bd shown in red in each case)
boundaryECoveringFigs :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) =>
                         BoundaryState -> [Diagram2D b]
boundaryECoveringFigs bd =
    fmap (lw ultraThin . (redg <>) . alignBefore draw alig . recoverGraph) $ boundaryECovering bd
      where redg = lc red $ draw g
            alig = defaultAlignment g
            g = recoverGraph bd

-- | diagram showing the boundaryECovering of a forced kingGraph
kingECoveringFig,kingVCoveringFig :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
kingECoveringFig = padBorder $ arrangeRows 3 $ boundaryECoveringFigs $ force $ makeBoundaryState kingGraph
-- | diagram showing the boundaryVCovering of a forced kingGraph
kingVCoveringFig = padBorder $ arrangeRows 3 $ boundaryVCoveringFigs $ force  $ makeBoundaryState kingGraph

-- | figures showing King's empires (1 and 2)
kingEmpiresFig, kingEmpire1Fig, kingEmpire2Fig :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
kingEmpiresFig = padBorder $ hsep 10 [kingEmpire1Fig, kingEmpire2Fig]
kingEmpire1Fig = drawEmpire1 kingGraph
kingEmpire2Fig = drawEmpire2 kingGraph
