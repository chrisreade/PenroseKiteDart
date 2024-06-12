
{-|
Module      : TgraphExamples
Description : Examples of tilings represented with Tgraphs and their diagrams 
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TupleSections             #-}

module TgraphExamples
  (-- * Some Layout tools
    padBorder
  , chunks
  , arrangeRowsGap
  , arrangeRows
  , labelAt
    -- *  Tgraphs for 7 vertex types
  , sunGraph 
  , jackGraph
  , kingGraph
  , queenGraph
  , aceGraph
  , deuceGraph
  , starGraph
    -- *  Further Basic Tgraphs
  , kiteGraph
  , dartGraph
  , fool
  , foolD
  , foolDminus
  , foolDs
  , sunDs
  , kiteDs
  , dartDs
  , dartD4  
  , sun3Dart 
    -- *  Some Simple Figures
  , foolFig
  , foolAndFoolD
  , figSunD3D2
    -- *  Figures for 7 vertex types
  , vertexTypesFig
  , forceVFigures
    -- * Partial Composition figures
  , pCompFig1
  , pCompFig2
  , pCompFig
    -- * Forced Tgraph figures
  , forceFoolDminus
  , forceDartD5Fig
  , forceKiteD5Fig
  , forceSunD5Fig
  , forceFig
    -- *  Removed faces (forcing and composing)
  , brokenDart
  , badlyBrokenDart
  , brokenDartFig
  , badlyBrokenDartFig
  , removeIncompletesFig
    -- *  Incorrect Tgraphs
  , mistake
  , mistake1
    -- * superForce Figure
  , superForceFig
  , superForceRocketsFig
    -- *  Tgraphs with Boundary faces
  , boundaryFDart4
  , boundaryFDart5
  , boundaryFDart4Fig
  , boundaryFDart5Fig
  , boundaryGapFDart4
  , boundaryGapFDart5
  , boundaryGap4Fig
  , boundaryGap5Fig
    -- *  Boundary coverings and empires
 , boundaryVCoveringFigs
 , boundaryECoveringFigs
 , kingECoveringFig
 , kingVCoveringFig 
 , kingEmpiresFig
 , kingEmpire1Fig
 , kingEmpire2Fig
    -- *  Emplace Choices
 , emplaceChoices
 , emplaceChoicesFig
 
  ) where

import Diagrams.Prelude
import Diagrams.TwoD.Text (Text)
-- import ChosenBackend (B)
import TileLib
import Tgraphs

import Data.List (intersect,find)      -- for emplaceChoices


-- |used for most diagrams to give border padding
-- 
--  When a specific Backend B is in scope, padBorder:: Diagram B -> Diagram B
padBorder :: Diagram2D b -> Diagram2D b
padBorder = pad 1.2 . centerXY

-- |chunks n l -  split a list l into chunks of length n (n>0)
chunks::Int -> [a] -> [[a]]
chunks n
  | n < 1 = error "chunks: argument <1\n"
  | otherwise = ch where
      ch [] = []
      ch as = take n as : ch (drop n as)

-- |arrangeRowsGap s n diags - arranges diags into n per row, centering each row horizontally,
-- with a seperation gap (horizontally and vertically) of s.
-- The result is a single diagram.
-- 
--  When a specific Backend B is in scope, arrangeRowsGap :: Double -> Int -> [Diagram B] -> Diagram B
arrangeRowsGap :: Double -> Int -> [Diagram2D b] -> Diagram2D b
arrangeRowsGap s n = centerY . vsep s . fmap (centerX . hsep s) . chunks n

-- |arrangeRows n diags - arranges diags into n per row, centering each row horizontally.
-- The result is a single diagram (seperation is 1 unit vertically and horizontally).
-- 
--  When a specific Backend B is in scope, arrangeRows :: Int -> [Diagram B] -> Diagram B
arrangeRows :: Int -> [Diagram2D b] -> Diagram2D b
arrangeRows = arrangeRowsGap 1.0

-- |add a given label at a given point offset from the centre of the given diagram.
-- 
-- When a specific Backend B is in scope, labelAt :: Point V2 Double -> String -> Diagram B -> Diagram B
labelAt :: Renderable (Text Double) b => 
           Point V2 Double -> String -> Diagram2D b -> Diagram2D b
labelAt p l d = baselineText l # fontSize (output 15) # moveTo p <> d
--labelAt p l d = baselineText l # fontSize (normalized 0.02) # moveTo p <> d


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
--
-- When a specific Backend B is in scope, foolFig :: Diagram B
foolFig :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
foolFig = padBorder $ labelSize normal drawj fool

-- |diagram of fool with foolD.
-- 
-- When a specific Backend B is in scope, foolAndFoolD :: Diagram B
foolAndFoolD :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
foolAndFoolD = padBorder $ hsep 1 [scale phi $ labelled drawj fool, labelled drawj foolD]

-- |Tgraph for a sun (sun vertex type)
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

-- |Figure for a 3 times decomposed sun with a 2 times decomposed sun.
-- 
-- When a specific Backend B is in scope, figSunD3D2 :: Diagram B
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




pCompFig1,pCompFig2,pCompFig :: Renderable (Path V2 Double) b => Diagram2D b
-- |diagram showing partial composition of a forced 3 times decomposed dart (with remainder faces in pale green).
-- 
-- When a specific Backend B is in scope, pCompFig1 :: Diagram B
pCompFig1 = lw veryThin $ hsep 5 $ rotations [1,1] [draw fd3, drawPCompose fd3]
            where fd3 = force $ dartDs!!3
-- |diagram showing partial composition of a forced 3 times decomposed kite (with remainder faces in pale green).
-- 
-- When a specific Backend B is in scope, pCompFig2 :: Diagram B
pCompFig2 = lw veryThin $ hsep 5 [draw fk3, drawPCompose fk3]
            where fk3 = force $ kiteDs!!3
-- |diagram showing two partial compositions (with remainder faces in pale green).
-- 
-- When a specific Backend B is in scope, pCompFig :: Diagram B
pCompFig = padBorder $ vsep 3 [center pCompFig1, center pCompFig2]


-- |diagram of foolDminus and the result of forcing.  
-- 
-- When a specific Backend B is in scope, forceFoolDminus :: Diagram B
forceFoolDminus :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
forceFoolDminus = padBorder $ hsep 1 $ fmap (labelled drawj) [foolDminus, force foolDminus]


forceDartD5Fig,forceKiteD5Fig,forceSunD5Fig,forceFig :: Renderable (Path V2 Double) b => Diagram2D b
-- |diagram of forced 5 times decomposed dart.
-- 
-- When a specific Backend B is in scope, forceDartD5Fig :: Diagram B
forceDartD5Fig = padBorder $ lw ultraThin $ drawForce $ dartDs !! 5
-- |diagram of forced 5 times decomposed kite.
-- 
-- When a specific Backend B is in scope, forceKiteD5Fig :: Diagram B
forceKiteD5Fig = padBorder $ lw ultraThin $ rotate (ttangle 1) $ drawForce $ kiteDs!!5
-- |diagram of forced 5 times decomposed sun.
-- 
-- When a specific Backend B is in scope, forceSunD5Fig :: Diagram B
forceSunD5Fig =  padBorder $ lw ultraThin $ drawForce $ sunDs !! 5
-- |diagram of forced 5 times decomposed dart (left) and kite (right).
-- 
-- When a specific Backend B is in scope, forceFig :: Diagram B
forceFig = hsep 1 [forceDartD5Fig,forceKiteD5Fig]

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

-- |brokenDartFig shows the faces removed from dartD4 to make brokenDart and badlyBrokenDart.
-- 
-- When a specific Backend B is in scope, brokenDartFig :: Diagram B
brokenDartFig  :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
brokenDartFig = padBorder $ lw thin $ hsep 1 $ fmap (labelled drawj) [dartD4, brokenDart, badlyBrokenDart]

-- |badlyBrokenDartFig shows badlyBrokenDart, followed by its composition, followed by the faces 
-- that would result from an unchecked second composition which are not tile-connected.
-- (Simply applying compose twice to badlyBrokenDart will raise an error).
-- 
-- When a specific Backend B is in scope, badlyBrokenDartFig :: Diagram B
badlyBrokenDartFig :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
badlyBrokenDartFig = padBorder $ lw thin $ hsep 1 $ fmap (labelled drawj) [vp, vpComp, vpFailed] where
    vp = makeVP badlyBrokenDart
    comp = compose badlyBrokenDart
    vpComp = restrictVP vp $ faces $ comp
    vpFailed  = restrictVP vp $ composedFaces comp

-- |figure showing the result of removing incomplete tiles (those that do not have their matching halftile)
-- to a 3 times decomposed sun.
-- 
-- When a specific Backend B is in scope, removeIncompletesFig :: Diagram B
removeIncompletesFig :: Renderable (Path V2 Double) b => Diagram2D b
removeIncompletesFig = padBorder $ drawj $ removeFaces (boundaryJoinFaces g) g where 
    g = sunDs !! 3


-- |mistake is a legal but incorrect Tgraph - a kite with 2 darts on its long edges
mistake:: Tgraph
mistake = makeTgraph [RK (1,2,4), LK (1,3,2), RD (3,1,5), LD (4,6,1), LD (3,5,7), RD (4,8,6)]

-- |mistake1 is a kite bordered by 2 half darts (subgraph of mistake and still incorrect)
mistake1:: Tgraph
mistake1 = makeTgraph [RK (1,2,4), LK (1,3,2), RD (3,1,5), LD (4,6,1)]

-- *  Figures for 7 vertex types

-- | vertexTypesFig is 7 vertex types in a single diagram as a row.
-- 
-- When a specific Backend B is in scope, vertexTypesFig :: Diagram B
vertexTypesFig :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
vertexTypesFig = padBorder $ hsep 1 lTypeFigs
 where
 lTypeFigs = zipWith (labelAt (p2 (0,-2.2))) ["sun","star","jack","queen","king","ace","deuce"] vTypeFigs
 vTypeFigs = zipWith drawVertex
               [sunGraph, starGraph, jackGraph, queenGraph, kingGraph, aceGraph,  deuceGraph]
               [(1,2),    (1,2),     (1,2),     (1,2),      (1,2),     (3,6),     (2,6)] -- alignments
 drawVertex g alm = alignBefore (lw thin . showOrigin . drawj) alm g

jackGraph,kingGraph,queenGraph,aceGraph,deuceGraph,starGraph::Tgraph
-- |Tgraph for vertex type jack.
jackGraph = makeTgraph
  [LK (1,9,11),RK (1,11,2),LK (7,8,1),RD (9,1,8),RK (1,3,4)
  ,LK (1,2,3),RK (7,1,5),LD (4,5,1),LD (9,8,10),RD (4,6,5)
  ] -- centre 1
-- |Tgraph for vertex type king.
kingGraph = makeTgraph
  [LD (1,2,3),RD (1,11,2),LD (1,4,5),RD (1,3,4),LD (1,10,11)
  ,RD (1,9,10),LK (9,1,7),RK (9,7,8),RK (5,7,1),LK (5,6,7)
  ] -- centre 1
-- |Tgraph for vertex type queen.
queenGraph = makeTgraph
  [LK (7,1,5),RK (3,5,1),LD (1,2,3),RK (7,9,1),LK (11,1,9)
  ,RD (1,11,2),RK (7,5,6),LK (7,8,9),LK (3,4,5),RK (11,9,10)
  ] -- centre 1
-- |Tgraph for vertex type ace (same as fool).
aceGraph = fool -- centre 3
-- |Tgraph for vertextype deuce.
deuceGraph = makeTgraph
  [LK (7,8,2),RK (7,2,6),RK (5,2,4),LK (5,6,2),LD (1,4,2)
  ,RD (1,2,8),RD (1,3,4),LD (1,8,9)
  ] -- centre 2
-- |Tgraph for vertex type star.
starGraph = makeTgraph
  [LD (1,2,3),RD (1,11,2),LD (1,10,11),RD (1,9,10),LD (1,8,9)
  ,RD (1,7,8),LD (1,6,7),RD (1,5,6),LD (1,4,5),RD (1,3,4)
  ] -- centre 1

-- |forceVFigures is a list of 7 diagrams - force of 7 vertex types.
--
-- When a specific Backend B is in scope, forceVFigures :: [Diagram B]
forceVFigures :: Renderable (Path V2 Double) b => [Diagram2D b]
forceVFigures = rotations [0,0,9,5,0,0,1] $
                fmap (center . drawForce) [sunGraph,starGraph,jackGraph,queenGraph,kingGraph,aceGraph,deuceGraph]


sun3Dart :: Tgraph
-- |A sun with 3 darts on the boundary NOT all adjacent
-- (Used in superForceRocketsFig).
sun3Dart = addHalfDart (9,10) $ addHalfDart (8,9) $ addHalfDart (5,6) $ addHalfDart (4,5) $ addHalfDart (3,4) $ addHalfDart (2,3) sunGraph
-- sun3Dart = addHalfDart (9,10) $ addHalfDart (8,9) sun2AdjDart


-- |Diagram showing superForce with initial Tgraph g (red), force g (red and black),
-- and superForce g (red and black and blue).
-- 
-- When a specific Backend B is in scope, superForceFig :: Diagram B
superForceFig :: Renderable (Path V2 Double) b => Diagram2D b
superForceFig = padBorder $ lw thin $ rotate (ttangle 1) $ drawSuperForce g where
    g = addHalfDart (220,221) $ force $ decompositions fool !!3

-- |Diagram showing 4 rockets formed by applying superForce to successive decompositions
-- of sun3Dart. The decompositions are in red with normal force additions in black and superforce additions in blue.
-- 
-- When a specific Backend B is in scope, superForceRocketsFig :: Diagram B
superForceRocketsFig :: Renderable (Path V2 Double) b => Diagram2D b
superForceRocketsFig = padBorder $ lw veryThin $ vsep 1 $ rotations [8,9,9,8] $
   fmap drawSuperForce decomps where
      decomps = take 4 $ decompositions sun3Dart


boundaryFDart4, boundaryFDart5 :: Tgraph
-- |graph of the boundary faces only of a forced graph (dartDs!!4)
boundaryFDart4 = checkedTgraph $ boundaryFaces $ force $ makeBoundaryState dartD4
-- |graph of the boundary faces only of a forced graph (dartDs!!5)
boundaryFDart5 = checkedTgraph $ boundaryFaces $ force $ makeBoundaryState (dartDs!!5)

boundaryFDart4Fig,boundaryFDart5Fig :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
-- |figure of the boundary faces only of a forced graph (dartDs!!4).
-- 
-- When a specific Backend B is in scope, boundaryFDart4Fig :: Diagram B
boundaryFDart4Fig = padBorder $ lw ultraThin $ labelSize tiny drawj boundaryFDart4
-- |figure of the boundary faces only of a forced graph (dartDs!!5).
-- 
-- When a specific Backend B is in scope, boundaryFDart5Fig :: Diagram B
boundaryFDart5Fig = padBorder $ lw ultraThin $ labelSize (normalized 0.006) drawj boundaryFDart5

boundaryGapFDart4, boundaryGapFDart5 :: Tgraph
-- |graph of the boundary faces only of a forced graph - with extra faces removed to make a gap
boundaryGapFDart4 = removeVertices [354] boundaryFDart4
    -- checkedTgraph $ filter ((/=354).originV)  (faces boundaryFDart4)
-- |graph of the boundary faces only of a forced graph - with extra faces removed to make a gap
boundaryGapFDart5 = removeVertices [1467] boundaryFDart5
    -- checkedTgraph $ filter ((/=1467).originV) (faces boundaryFDart5)

boundaryGap4Fig, boundaryGap5Fig  :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Diagram2D b
-- |figure for the boundary gap graph boundaryGapFDart4.
-- 
-- When a specific Backend B is in scope, boundaryGap4Fig :: Diagram B
boundaryGap4Fig = padBorder $ lw ultraThin $ labelSize tiny drawj boundaryGapFDart4
-- |figure for the boundary gap graph boundaryGapFDart5.
-- 
-- When a specific Backend B is in scope, boundaryGap5Fig :: Diagram B
boundaryGap5Fig = padBorder $ lw ultraThin $ labelSize (normalized 0.006) drawj boundaryGapFDart5


-- | boundaryVCoveringFigs bd - produces a list of diagrams for the boundaryVCovering of bd 
-- (with the Tgraph represented by bd shown in red in each case).
-- 
-- When a specific Backend B is in scope, boundaryVCoveringFigs :: BoundaryState -> [Diagram B]
boundaryVCoveringFigs :: Renderable (Path V2 Double) b =>
                         BoundaryState -> [Diagram2D b]
boundaryVCoveringFigs bd =
    fmap (lw ultraThin . (redg <>) . alignBefore draw alig . recoverGraph) $ boundaryVCovering bd
      where redg = lc red $ draw g --alignBefore draw alig g
            alig = defaultAlignment g
            g = recoverGraph bd

-- | boundaryECoveringFigs bd - produces a list of diagrams for the boundaryECovering of bd  
-- (with the Tgraph represented by bd shown in red in each case).
-- 
-- When a specific Backend B is in scope, boundaryECoveringFigs :: BoundaryState -> [Diagram B]
boundaryECoveringFigs :: Renderable (Path V2 Double) b =>
                         BoundaryState -> [Diagram2D b]
boundaryECoveringFigs bd =
    fmap (lw ultraThin . (redg <>) . alignBefore draw alig . recoverGraph) $ boundaryECovering bd
      where redg = lc red $ draw g
            alig = defaultAlignment g
            g = recoverGraph bd

kingECoveringFig,kingVCoveringFig :: Renderable (Path V2 Double) b => Diagram2D b
-- | diagram showing the boundaryECovering of a forced kingGraph.
-- 
-- When a specific Backend B is in scope, kingECoveringFig :: Diagram B
kingECoveringFig = padBorder $ arrangeRows 3 $ boundaryECoveringFigs $ force $ makeBoundaryState kingGraph
-- | diagram showing the boundaryVCovering of a forced kingGraph.
-- 
-- When a specific Backend B is in scope, kingVCoveringFig :: Diagram B
kingVCoveringFig = padBorder $ arrangeRows 3 $ boundaryVCoveringFigs $ force  $ makeBoundaryState kingGraph

kingEmpiresFig, kingEmpire1Fig, kingEmpire2Fig :: Renderable (Path V2 Double) b => Diagram2D b
-- | figure showing King's empires (1 and 2).
-- 
-- When a specific Backend B is in scope, kingEmpiresFig :: Diagram B
kingEmpiresFig = padBorder $ hsep 10 [kingEmpire1Fig, kingEmpire2Fig]
-- | figure showing King's empires 1.
-- 
-- When a specific Backend B is in scope, kingEmpire1Fig :: Diagram B
kingEmpire1Fig = showEmpire1 kingGraph
-- | figure showing King's empire 2.
-- 
-- When a specific Backend B is in scope, kingEmpire2Fig :: Diagram B
kingEmpire2Fig = showEmpire2 kingGraph


-- |emplaceChoices forces then maximally composes. At this top level it
-- produces a list of forced choices for each of the unknowns of this top level Tgraph.
-- It then repeatedly forceDecomps back to the starting level to return a list of Tgraphs.
-- This version relies on compForce theorem and related theorems
emplaceChoices:: Tgraph -> [Tgraph]
emplaceChoices g = emplaceChoicesForced $ recoverGraph $ force $ makeBoundaryState g where

  emplaceChoicesForced:: Tgraph -> [Tgraph]
  emplaceChoicesForced g0 | nullGraph g' = chooseUnknowns [(unknowns $ getDartWingInfo g0, g0)]
                          | otherwise    = forceDecomp <$> emplaceChoicesForced g'
                          where g' = compose g0

  chooseUnknowns :: [([Vertex],Tgraph)] -> [Tgraph]
  chooseUnknowns [] = []
  chooseUnknowns (([],g0):more) = g0:chooseUnknowns more
  chooseUnknowns (((u:unks),g0): more)
     =  chooseUnknowns (map (remainingunks unks) newgs ++ more)
        where newgs = map recoverGraph $ atLeastOne $ tryDartAndKiteForced (findDartLongForWing u bd) bd
              bd = makeBoundaryState g0
              remainingunks startunks g' = (startunks `intersect` graphBoundaryVs g', g')

  findDartLongForWing :: Vertex -> BoundaryState -> Dedge
  findDartLongForWing v bd 
      = case find isDart (facesAtBV bd v) of
        Just d -> longE d
        Nothing -> error $ "findDartLongForWing: dart not found for dart wing vertex " ++ show v

-- |Example showing emplaceChoices for foolD with foolD shown in red in each choice
-- 
-- When a specific Backend B is in scope, emplaceChoicesFig :: Diagram B
emplaceChoicesFig :: Renderable (Path V2 Double) b => Diagram2D b
emplaceChoicesFig =  lw thin $ hsep 1 $ map overlayg $ emplaceChoices g
    where g = foolD
          overlayg g' = smartAlignBefore draw algmnt g # lc red <> alignBefore draw algmnt g'
          algmnt = defaultAlignment g

