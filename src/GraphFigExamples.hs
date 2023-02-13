{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

{-|
Module      : GraphFigExamples
Description : Examples of tilings represented with Tgraphs and their diagrams 
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

-}
module GraphFigExamples where

-- used for testing
-- import qualified Data.IntMap.Strict as VMap (IntMap, lookup, insert, empty, fromList, union)

import Data.List (intersect,foldl',(\\))      
import Diagrams.Prelude
import Data.Tree (Tree(..),levels) -- used for boundaryEdgeCaseTrees    

import ChosenBackend (B)
import TileLib
import Tgraphs


-- |used for most diagrams to give border padding
padBorder:: Diagram B -> Diagram B
padBorder = pad 1.2 . centerXY



    
{-*
Example Tgraphs with Figures
-}
fool, foolD, foolDminus:: Tgraph
-- |fool: fool's kite - a decomposed left and right kite back-to-back (i.e. not sharing join edge)
fool = makeTgraph
          [ RD(1,2,3), LD(1,3,4), RK(6,2,5), LK(6,3,2), RK(6,4,3), LK(6,7,4)]

-- |a once decomposed fool (= foolDs!!1)
foolD = decompose fool
-- | a list of all decompositions of fool
foolDs :: [Tgraph]
foolDs = decompositions fool

-- |foolDminus: 3 faces removed from foolD - still a valid Tgraph
foolDminus = removeFaces [RD(6,15,13), LD(6,17,15), RK(5,11,2)] foolD

-- | diagram of just fool
foolFig :: Diagram B
foolFig = padBorder $ dashJVGraph fool

-- |diagram of fool with foolD
foolAndFoolD :: Diagram B
foolAndFoolD = padBorder $ hsep 1 [scale phi $ dashJVGraph fool, dashJVGraph foolD]

{-|touchErrorFaces is an addition of 2 faces to those of foolD which contains touching vertices.
These will be caught by makeTgraph which raises an error.
The error is not picked up by checkedTgraph. It can be fixed using tryCorrectTouchingVs.

*** Exception: makeTgraph: touching vertices [(19,7)]

> checkedTgraph touchErrorFaces
Tgraph {maxV = 19, faces = ...}

> tryCorrectTouchingVs touchErrorFaces
Right (Tgraph {maxV = 18, faces = [..., LK (7,17,18)]})

test with:
padBorder $ dashJVGraph $ runTry $ tryCorrectTouchingVs touchErrorFaces
-}
touchErrorFaces::[TileFace]
touchErrorFaces = faces foolD ++ [RD(6,18,17),LK(19,17,18)]

-- |Tgraph for a sun
sunGraph :: Tgraph
sunGraph = makeTgraph
             [ RK(1,2,11), LK(1,3,2)
             , RK(1,4,3) , LK(1,5,4)
             , RK(1,6,5) , LK(1,7,6)
             , RK(1,8,7) , LK(1,9,8)
             , RK(1,10,9), LK(1,11,10)
             ]
-- |All decompositions of sunGraph
sunDs :: [Tgraph]
sunDs =  decompositions sunGraph

-- |Figure for a 3 times decomposed sun with a 2 times decomposed sun
figSunD3D2:: Diagram B
figSunD3D2 = padBorder $ hsep 1 [dashJVGraph $ sunDs !! 3, scale phi $ dashJVGraph $ sunDs !! 2]

-- |Tgraph for kite
kiteGraph :: Tgraph
kiteGraph = makeTgraph [ RK(1,2,4), LK(1,3,2)]

-- |All decompositions of a kite
kiteDs :: [Tgraph]
kiteDs = decompositions kiteGraph

-- |Tgraph for a dart
dartGraph :: Tgraph
dartGraph =  makeTgraph [ RD(1,2,3), LD(1,3,4)]

-- |All decompositions of a dart
dartDs :: [Tgraph]
dartDs =  decompositions dartGraph

-- |Tgraph of 4 times decomposed dartGraph (used in several examples)
dartD4 :: Tgraph
dartD4 = dartDs!!4


{-* Partial Composition figures
-} 
pCompFig1,pCompFig2,pCompFig:: Diagram B
-- |diagram showing partial composition of a forced 3 times decomposed dart (with ignored faces in pale green)
pCompFig1 = lw ultraThin $ hsep 5 $ rotations [1,1] [drawGraph fd3, drawPCompose fd3]
            where fd3 = force $ dartDs!!3
-- |diagram showing partial composition of a forced 3 times decomposed kite (with ignored faces in pale green)
pCompFig2 = lw ultraThin $ hsep 5 $ rotations [] [drawGraph fk3, drawPCompose fk3]
            where fk3 = force $ kiteDs!!3
-- |diagram showing two partial compositions (with ignored faces in pale green)
pCompFig = padBorder $ vsep 3 [center pCompFig1, center pCompFig2]


{-|
This example illustrates that an experimental version of composition (composeK)
which defaults to kites when there are choices (unknowns) can produce incorrect Tgraphs.
(A counter example to composeK being correct)
The second Tgraph shown (force queenGraph) is correct and its normal (partial) composition is shown to the left.
The third Tgraph is the result of applying composeK and the last Tgraph is the result of applying composeK again
- the mistake1 Tgraph.
Both these last 2 Tgraphs are incorrect (and will fail when forced).
-}
counterK :: Diagram B
counterK = padBorder $ lw thin $ hsep 1 $ 
             drawPCompose g : (rotations [0,6,5] $ phiScales $ fmap drawSmartGraph [g,kg,kkg])
        where g = force queenGraph
              kg = composeK g
              kkg = composeK kg
-- An experimental version of composition which defaults to kites when there are choices (unknowns).
-- This is unsafe in that it can create an incorrect Tgraph from a correct Tgraph.
-- composeK :: Tgraph -> Tgraph
              composeK = snd . partComposeK
-- partComposeK:: Tgraph -> ([TileFace],Tgraph)
              partComposeK g = (remainder,newGraph) where
                 newGraph = makeTgraph newfaces
                 dwInfo = getDartWingInfo g
                 changedInfo = dwInfo{ largeKiteCentres = largeKiteCentres dwInfo ++ unknowns dwInfo
                                     , unknowns = []
                                     }
                 compositions = composedFaceGroups changedInfo
                 newfaces = map fst compositions
                 groups = map snd compositions
                 remainder = faces g \\ concat groups
{-
{-|
This example illustrates that an experimental version of composition (composeK)
which defaults to kites when there are choices (unknowns) can produce incorrect Tgraphs.
The first 3 Tgraphs are correct. The second is composeK of the first and the third is force applied to the second
(a forced queen vertex).  
The fourth Tgraph is a further composeK and this is clearly an incorrect Tgraph (applying force to this fails).
In one further composition (either composeK or compose) the incomplete mistake1 graph is produced - fifth graph.
-}
counterK :: Diagram B
counterK = padBorder $ hsep 1 $ rotations [8,0,0,6,5] $ scales [1,phi,phi,1+phi,1+2*phi] $ 
           fmap dashJVGraph [g,cg,fcg, cfcg, compose cfcg]
        where g = sunPlus3Dart'
              cg = composeK g
              fcg = force cg
              cfcg = composeK fcg
-- An experimental version of composition which defaults to kites when there are choices (unknowns).
-- This is unsafe in that it can create an incorrect Tgraph from a correct Tgraph.
-- composeK :: Tgraph -> Tgraph
              composeK = snd . partComposeK
-- partComposeK:: Tgraph -> ([TileFace],Tgraph)
              partComposeK g = (remainder,newGraph) where
                 newGraph = makeTgraph newfaces
                 dwInfo = getDartWingInfo g
                 changedInfo = dwInfo{ largeKiteCentres = largeKiteCentres dwInfo ++ unknowns dwInfo
                                     , unknowns = []
                                     }
                 compositions = composedFaceGroups changedInfo
                 newfaces = map fst compositions
                 groups = map snd compositions
                 remainder = faces g \\ concat groups
-}


{-* Forced Tgraph figures
-}
-- |diagram of foolDminus and the reult of forcing              
forceFoolDminus :: Diagram B              
forceFoolDminus = padBorder $ hsep 1 $ fmap dashJVGraph [foolDminus, force foolDminus]


-- |diagrams of forced graphs (3 or 5 times decomposed kite or dart or sun)           
forceDartD3Fig,forceDartD5Fig,forceKiteD3Fig,forceKiteD5Fig,forceSunD5Fig,forceFig:: Diagram B
forceDartD3Fig = padBorder $ lw thin $ rotate (ttangle 1) $ drawForce $ dartDs!!3
forceDartD5Fig = padBorder $ lw ultraThin $ drawForce $ dartDs !! 5
forceKiteD3Fig = padBorder $ lw thin $ drawForce $ kiteDs !! 3
forceKiteD5Fig = padBorder $ lw ultraThin $ rotate (ttangle 1) $ drawForce $ kiteDs!!5
forceSunD5Fig =  padBorder $ lw ultraThin $ drawForce $ sunDs  !! 5
forceFig = hsep 1 [forceDartD5Fig,forceKiteD5Fig]


 
-- |an example showing a 4 times forcedDecomp pair of darts,
-- with the maximal compForced Tgraph (a kite) overlaid in red
maxExampleFig :: Diagram B
maxExampleFig = padBorder $ lw ultraThin $ drawWithMax $ allForcedDecomps dartPlusDart !! 4

{-*
Emplace with choices
-}
-- |four choices for composing fool
foolChoices :: Diagram B
foolChoices = padBorder $ vsep 1 
              [hsep 1 $ fmap ((redFool <>) . dashJGraph) choices
              ,hsep 1 $ rotations [1,1,9,4] $ scale phi $ fmap (dashJGraph . compose) choices
              ] where choices = makeChoices fool
                      redFool = dashJGraph fool # lc red
                         
-- |showing 4 emplacement choices for foolD 
emplaceFoolDChoices :: Diagram B
emplaceFoolDChoices = padBorder $ hsep 1 $
        fmap (addFoolD . lw ultraThin . drawPatch . dropLabels) vpChoices where
        (vpFoolD:vpChoices) = alignments [(1,6),(1,6),(1,6),(1,6),(28,6)] --(29,6)] 
                                         (fmap makeVPinned (foolD:emplaceChoices foolD))
        addFoolD fig = (lc red . lw thin . dashJPatch . dropLabels) vpFoolD <> fig
--  WARNING: relies on numbering which can change with changes to forcing
--  Vertex 1 is not present in the final choice (hence 28)

{-*
Removed faces (forcing and composing)
-}
-- |brokenDart is a 4 times decomposed dart (dartD4) with 5 halftile faces removed.
-- Forcing will repair to produce the same Tgraph as force dartD4.
-- This graph can also be repeatedly composed (without forcing) to get a maximal Tgraph.
brokenDart :: Tgraph
brokenDart = removeFaces deleted dartD4 where
  deleted = [RK(2,16,33),LD(15,33,16),RK(16,66,15),LK(16,67,66),LK(5,15,66)]

{-| badlyBrokenDart has more faces removed from brokenDart.
This will also get repaired by forcing (to produce the same as force dartD4).
However it will fail to produce a valid Tgraph if composed twice without forcing. 
-}
badlyBrokenDart :: Tgraph
badlyBrokenDart = removeFaces deleted bbd where
  deleted = [RK(6,28,54)]
  bbd = removeVertices [63,37] brokenDart
--  deleted = RK(6,28,54):filter (isAtV 63) (faces brokenDart)
 
-- |brokenDartFig shows the faces removed from dartD4 to make brokenDart and badlyBrokenDart
brokenDartFig :: Diagram B
brokenDartFig = padBorder $ lw thin $ hsep 1 $ fmap dashJVGraph [dartD4, brokenDart, badlyBrokenDart]

-- |badlyBrokenDartFig shows badlyBrokenDart, followed by its composition, followed by the faces 
-- that would result from an unchecked second composition which are not tile-connected.
-- (Simply applying compose twice to badlyBrokenDart will raise an error).
badlyBrokenDartFig :: Diagram B
badlyBrokenDartFig = padBorder $ hsep 1 $ fmap draw [badlyBrokenDart, compBBD, failed] where
    vp = makeVPinned badlyBrokenDart
    draw g = dashJPatch $ subPatch vp $ faces g
    compBBD = compose badlyBrokenDart
    failed  = snd $ uncheckedPartCompose $ compBBD

-- |figure showing force badlyBrokenDart (which is the same as force dartD4)
checkBrokenDartFig  :: Diagram B
checkBrokenDartFig = padBorder $ lw thin $ drawGraph $ force badlyBrokenDart


-- |2 adjacent kites decomposed then the top half kite components removed (3 of them)
brokenKites::Tgraph
brokenKites = removeFaces deleted kPlusKD where
                      kPlusKD = decompose kitePlusKite
                      deleted = filter (hasVIn [6,14]) (faces kPlusKD)
{-|
Figure showing a decomposed pair of adjacent kites, followed by
brokenKites (3 faces removed from decomposed pair of kites), followed by
compositon of brokenKites (a kite) which is also the composition of forced brokenKites, followed by
the emplacement of brokenKites (which is the same as force brokenKites).
-}
brokenKitesDFig :: Diagram B
brokenKitesDFig = padBorder $ hsep 1 $ fmap dashJVPinned $ alignAll (1,3) $ scales [1,1,phi] $ fmap makeVPinned 
                  [decompose kitePlusKite, brokenKites, compose brokenKites, emplace brokenKites]

-- |diagram illustrating touching vertex situation and forced result.
-- The faces shown in lime are removed from a twice decomposed sun.
-- These are reconstructed by force (with other additional faces). The touching vertex restriction blocks
-- the bottom 4 face additions initially. 
touchingTestFig::  Diagram B
touchingTestFig = 
  padBorder $ lw thin $ hsep 1 $
    [ dashJVPinned vpLeft <> (dashJPatch (dropLabels vpGone) # lc lime)
    , dashJVPinned $  alignXaxis (8,3) $ makeVPinned $ force touchGraph
    ] where    
      touchGraph = graphFromVP vpLeft
      vpLeft = removeFacesVP deleted vp
      vpGone = selectFacesVP deleted vp
      vp = makeVPinned sunD2
      sunD2 = sunDs!!2
      deleted = filter ((==1).originV) (faces sunD2) ++
                [LD(29,41,31),RK(31,79,29),LK(10,29,79),RK(10,79,75)]


-- |A function to remove halftile faces that do not have their matching halftile
-- This weill raise an error if the result is not a valid Tgraph.
removeIncompleteTiles:: Tgraph -> Tgraph
removeIncompleteTiles g = removeFaces halfTiles g
       where bdry = makeBoundaryState g
             halfTiles = fmap snd $ incompleteHalves bdry $ boundary bdry

-- |figure showing the result of applying removeIncompleteTiles to a 3 times decomposed sun.
removeIncompletesFig::Diagram B
removeIncompletesFig = padBorder $ dashJGraph $ removeIncompleteTiles  $ sunDs !! 3

{-*
Incorrect Tgraphs (and other problem Tgraphs)
-}  
-- |faces removed from foolD to illustrate crossing boundary and non tile-connected faces
-- (using VPinned to draw). Crossing boundary at 4 in first case (but still tile-connected),
-- Crossing boundary at 11 in second case and not tile-connected.
crossingBdryFig :: Diagram B
crossingBdryFig = padBorder $ hsep 1 [d1,d2]
       where d1 = dashJVPinned $ removeFacesGtoVP [RK(3,11,13), LK(3,13,15), RK(3,15,4)] foolD
             d2 = dashJVPinned $ removeFacesGtoVP [RK(5,11,2), LD(6,13,11), RD(6,15,13), LD(6,17,15)] foolD

-- |mistake is a legal but incorrect Tgraph - a kite with 2 darts on its long edges
mistake:: Tgraph
mistake = makeTgraph [RK(1,2,4), LK(1,3,2), RD(3,1,5), LD(4,6,1), LD(3,5,7), RD(4,8,6)]

-- |figure showing mistake Tgraph and the point at which forcing fails                
pfMistakeFig :: Diagram B
pfMistakeFig  = padBorder $ hsep 1 [dashJVGraph mistake, dashJVGraph partForcedMistake] where
   partForcedMistake = makeTgraph 
                       [RK (9,1,11),LK (9,10,7),RK (9,7,5),LK (9,5,1),RK (1,2,4)
                       ,LK (1,3,2),RD (3,1,5),LD (4,6,1),LD (3,5,7),RD (4,8,6)
                       ]
   
-- |decompose mistake and the point at which forcing fails  with  RK (6,26,1)              
forcingDmistakeFig :: Diagram B
forcingDmistakeFig = padBorder $ hsep 1 [dashJVGraph (decompose mistake), dashJVGraph part] where
    part = makeTgraph
             [RK (26,24,1),RK (5,24,25),LK (5,1,24),RK (3,23,2),LK (3,22,23)
             ,RK (3,21,22),LK (3,15,21),LK (4,2,20),RK (4,20,19),LK (4,19,18),RK (4,18,17)
             ,LK (4,17,16),RK (4,16,12),LD (8,12,16),RK (3,14,15),LK (3,11,14),RD (7,14,11)
             ,RK (4,13,2),LK (4,9,13),RD (1,13,9),LK (3,2,13),RK (3,13,10),LD (1,10,13)
             ,LK (3,10,5),RD (1,5,10),RK (4,6,9),LD (1,9,6),RK (3,5,11),LD (7,11,5)
             ,LK (4,12,6),RD (8,6,12)
             ]

{-|  forcingD2mistakeFig
    Figure showing a stuck graph with error at vertex 35 
    This is involves a twice decomposed mistake which fails when forced.
    The figure shows the graph when the error is discovered.
-}
forcingD2mistakeFig :: Diagram B
forcingD2mistakeFig = padBorder $ dashJVGraph partF where
  partF = makeTgraph
            [LK (78,46,35),LK (78,47,45),RK (78,45,46),LK (7,77,73),RK (7,76,77),LK (7,75,76),RK (7,74,75)
            ,LK (7,47,74),RK (7,73,43),LD (44,43,73),RK (8,72,67),LK (8,71,72),RK (8,70,71),LK (8,69,70)
            ,RK (8,68,69),LK (8,42,68),RD (49,68,42),RK (62,40,67),LK (8,67,40),RD (66,35,39),LD (66,39,38)
            ,RD (66,38,65),LK (63,65,38),LD (37,64,63),RD (37,62,64),RK (63,38,37),LK (62,41,40),LK (62,37,36)
            ,RK (62,36,41),LK (30,61,59),RK (30,60,61),LK (30,58,60),LK (48,4,59),RK (30,59,4),RD (58,30,34)
            ,LD (58,34,33),RD (58,33,57),LK (55,57,33),LD (32,56,55),RD (32,54,56),RK (55,33,32),LK (54,53,52)
            ,LK (54,32,31),RK (54,31,53),LD (3,53,31),RD (3,52,53),RK (50,52,51),LD (3,51,52),RD (3,29,51)
            ,LK (50,51,29),RK (50,29,44),LD (49,42,12),RK (12,48,49),LK (12,23,48),RD (4,48,23),RK (7,45,47)
            ,LD (5,46,45),RD (5,35,46),LK (7,22,45),RD (5,45,22),LK (11,44,29),RD (44,11,43),LK (7,43,11)
            ,RK (8,12,42),LD (6,41,36),RD (6,40,41),RK (8,40,24),LD (6,24,40),LK (1,39,35),RK (1,38,39)
            ,LK (1,37,38),RK (1,36,37),RD (6,36,20),LK (1,20,36),RK (1,35,19),LD (5,19,35),LK (2,34,30)
            ,RK (2,33,34),LK (2,32,33),RK (2,31,32),RD (3,31,17),LK (2,17,31),RK (2,30,14),LD (4,14,30)
            ,RK (11,29,21),LD (3,21,29),RK (2,25,13),LK (2,14,25),RD (4,25,14),LK (9,13,25),RK (9,25,15)
            ,LD (4,15,25),LK (1,16,9),RD (13,9,16),LK (2,13,26),RK (2,26,17),LD (3,17,26),RK (10,26,13)
            ,LK (10,18,26),RD (3,26,18),RK (1,10,16),LD (13,16,10),LK (10,5,27),RK (10,27,18),LD (3,18,27)
            ,LK (1,19,10),RD (5,10,19),RK (9,28,6),LK (9,15,28),RD (4,28,15),RK (1,9,20),LD (6,20,9)
            ,RK (11,27,5),LK (11,21,27),RD (3,27,21),RK (7,11,22),LD (5,22,11),LK (12,6,28),RK (12,28,23)
            ,LD (4,23,28),LK (8,24,12),RD (6,12,24)
            ]


-- |mistake1 is a kite bordered by 2 half darts (subgraph of mistake and still incorrect)
mistake1:: Tgraph
mistake1 = makeTgraph [RK(1,2,4), LK(1,3,2), RD(3,1,5), LD(4,6,1)]

-- |partially forced mistake1 (at the point of discovery of incorrect graph
partFMistake1Fig:: Diagram B
partFMistake1Fig = padBorder $ dashJVGraph partF where
  partF = makeTgraph [RK (8,1,6),LK (7,5,1),RK (1,2,4),LK (1,3,2),RD (3,1,5),LD (4,6,1)]

-- |decomposed mistake1 is no longer incorrect and can be forced and recomposed
cdMistake1Fig :: Diagram B
cdMistake1Fig = padBorder $ hsep 1 $ fmap dashJVPinned $ scales [phi,1,1,phi] $ alignAll (1,2) $ fmap makeVPinned
               [ mistake1 , mistake1D, force mistake1D, compose mistake1D]
               where mistake1D = decompose mistake1


{-*
Figures fof 7 vertex types
-} 
{-| vertexTypesFig is 7 vertex types single diagram as a row -}
vertexTypesFig:: Diagram B
vertexTypesFig = padBorder $ hsep 1 lTypeFigs
 where
 lTypeFigs = zipWith (labelAt (p2(0,-2.2))) ["sun","star","jack","queen","king","ace","deuce"] vTypeFigs
 vTypeFigs = zipWith drawVertex 
               [sunGraph, starGraph, jackGraph, queenGraph, kingGraph, aceGraph,  deuceGraph]
               [(1,2),    (1,2),     (1,2),     (1,2),      (1,2),     (3,6),     (2,6)] -- alignments
 drawVertex g alm = lw thin $ showOrigin $ dashJPatch $ makeAlignedPatch alm g

-- |add a given label at a given point offset from the centre of the given diagram
labelAt :: Point V2 Double -> String -> Diagram B -> Diagram B
labelAt p l d = baselineText l # fontSize (output 15) # moveTo p <> d 
--labelAt p l d = baselineText l # fontSize (local 0.5) # fc blue # moveTo p <> d 

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
forceVFigures :: [Diagram B]
forceVFigures = rotations [0,0,9,5,0,0,1] $
                fmap (center . drawForce) [sunGraph,starGraph,jackGraph,queenGraph,kingGraph,aceGraph,deuceGraph]


{-| forceVsFig shows force of the 7 vertex types in a row as single diagram -}
forceVsFig :: Diagram B
forceVsFig = padBorder $ hsep 1 forceVFigures


{-| relatedVType0 lays out figures from forceVFigures plus a kite as single diagram with 3 columns
    without arrows (used by relatedVTypeFig which adds arrows).
-}
relatedVType0:: Diagram B
relatedVType0 = lw thin $
 atPoints [p2(0,20),p2(0,12),  p2(8,20),p2(8,12),p2(8,1),  p2(18,20),p2(18,12),p2(18,1) ] $
          [sunF,    starF,      aceF,    jackF,    kingF,     kite,     deuceF,   queenF]
 where kite   = labelAt (p2(-4,2)) "force kite (= kite)" $ named "kite" $ center $ lc red $ drawGraph kiteGraph
       sunF   = labelAt (p2(-4,2)) "force sun (= sun)" $ named "sunF" $ forceVFigures!!0
       starF  = labelAt (p2(-4,2.1)) "force star" $ named "starF" $ forceVFigures!!1
       jackF  = labelAt (p2(-4,2.1)) "force jack" $ named "jackF" $ forceVFigures!!2
       queenF = labelAt (p2(-4,4)) "force queen" $ named "queenF" $ forceVFigures!!3
       kingF  = labelAt (p2(-4,4)) "force king" $ named "kingF" $ forceVFigures!!4
       aceF   = labelAt (p2(-4,2)) "force ace (= ace)" $ named "aceF" $ forceVFigures!!5
       deuceF = labelAt (p2(-4,2.1)) "force deuce" $ named "deuceF" $ forceVFigures!!6

{-| relatedVTypeFig lays out figures from forceVFigures plus a kite as a single diagram with 3 columns
    showing relationships - forcedDecomp (blue arrows) and compose (green arrows)
 -}
relatedVTypeFig:: Diagram B
relatedVTypeFig = (key # rotate (90@@deg) # moveTo (p2(-10,-10))) <>  mainfig where
  mainfig = padBorder relatedVType0
    # forceDecArrow "sunF" "starF"
    # composeArcUp "starF" "sunF"
    # forceDecArrow "aceF" "jackF"
    # composeArcUp "jackF" "aceF"
    # forceDecArrow "kite" "deuceF"
    # composeArcUp "deuceF" "kite"
    # forceDecArrow "jackF" "kingF"
    # composeArcUp "kingF" "jackF"
    # forceDecArrow "deuceF" "queenF"
    # composeArcUp "queenF" "deuceF"
  key = (a|||box|||b) 
         # labelAt (p2(1,-1)) "force . decompose" 
         # labelAt (p2(2,1.3)) "compose" 
         # forceDecArrow "B" "A" 
         # composeArcRight "A" "B" where
            box = rect 7 3.8 
            a = named "A" (rect 0 0.1 # lw none)
            b = named "B" (rect 0 0.1 # lw none)

{-*
Other miscelaneous Tgraphs and Diagrams
-}
-- |graphs of the boundary faces only of forced graphs (dartDs!!4 and dartDs!!5)
boundaryFDart4, boundaryFDart5 :: Tgraph
boundaryFDart4 = checkedTgraph $ boundaryFaces $ makeBoundaryState $ force (dartD4)
boundaryFDart5 = checkedTgraph $ boundaryFaces $ makeBoundaryState $ force (dartDs!!5)

-- | figure to check that force can complete a hole
forceHoleTest = padBorder $ lw ultraThin $ drawGraph $ force boundaryFDart5

-- |figures of the boundary faces only of a forced graph
boundaryFDart4Fig,boundaryFDart5Fig:: Diagram B
boundaryFDart4Fig = padBorder $ lw ultraThin $ dashJVGraph boundaryFDart4
boundaryFDart5Fig = padBorder $ lw ultraThin $ dashJVGraph boundaryFDart5

-- |graphs of the boundary faces only of a forced graph - with extra faces removed to make a gap
boundaryGapFDart4, boundaryGapFDart5 :: Tgraph
boundaryGapFDart4 = removeVertices [354] boundaryFDart4
    -- checkedTgraph $ filter ((/=354).originV)  (faces boundaryFDart4)
boundaryGapFDart5 = removeVertices [1467] boundaryFDart5
    -- checkedTgraph $ filter ((/=1467).originV) (faces boundaryFDart5)

-- |figures for the boundary gap graphs boundaryGapFDart4, boundaryGapFDart5
boundaryGap4Fig, boundaryGap5Fig :: Diagram B
boundaryGap4Fig = lw ultraThin $ dashJVGraph boundaryGapFDart4
boundaryGap5Fig = lw ultraThin $ dashJVGraph boundaryGapFDart5

-- |showing intermediate state of filling the inlet and closing the gap of boundaryGapFDart5
-- using stepForce 2000
gapProgress5 :: Diagram B
gapProgress5 = lw ultraThin $ vsep 1 $ center <$> rotations [1,1]
    [ drawSmartGraph g
    , drawSmartGraph $ recoverGraph $ boundaryState $ stepForce g 2000
    ] where g = boundaryGapFDart5

-- |showing intermediate state of filling the inlet and closing the gap of boundaryGapFDart4
-- using stepForce 600 (finished at 820)
gapProgress4 :: Diagram B
gapProgress4 = lw ultraThin $ hsep 1 $ center <$> rotations [5,5]
    [ drawSmartGraph g
    , drawSmartGraph $ recoverGraph $ boundaryState $ stepForce g 600 --finished at 820
    ] where g = boundaryGapFDart4

{-| bigPic is a diagram illustrating force/emplacement relationships for decomposed darts
     bigPic0 is main diagram for bigPic without the arrows
-}
bigPic0,bigPic :: Diagram B
bigPic0 = padBorder $ lw ultraThin $ position $ concat
          [ zip pointsR1 $ rotations [0,1,1] partComps
          , zip pointsR2 $ zipWith named ["a4", "a3","a2","a1","a0"] (dots : rotations [1,1] forceDs)
          , zip pointsR3 $ zipWith named ["b4", "b3","b2","b1","b0"] (dots : rotations [1,1] drts)
          ]
          where
              partComps = phiScales $ fmap drawPCompose $ reverse $ take 5 $ allForcedDecomps $ force dartGraph
              forceDs = fmap center $ phiScaling phi $ reverse $ take 4 $ fmap drawForce dartDs
              drts  = fmap center $ phiScaling phi $ reverse $ take 4 $ fmap drawSmartGraph dartDs
              dots = center $ hsep 1 $ replicate 4 (circle 0.5 # fc gray # lw none)
              pointsR1 = map p2 [ (0, 70), (52, 70), (100, 70), (150, 70), (190, 70)]
              pointsR2 = map p2 [ (0, 40), (42, 40), (95, 40), (140, 40), (186, 40)]
              pointsR3 = map p2 [ (0, 0),  (42, 0),  (95, 0),  (140, 0),  (186, 0) ]    
bigPic = 
    bigPic0  # composeArcRight "a3" "a2"
             # composeArcRight "a2" "a1"
             # composeArcRight "a1" "a0"
             # composeArcRight "a4" "a3"
             # composeArcRight "b4" "b3"
             # composeArcRight "b3" "b2"
             # composeArcRight "b2" "b1"
             # composeArcRight "b1" "b0"
             # decompArrow "b3" "b4"
             # decompArrow "b2" "b3"
             # decompArrow "b1" "b2"
             # decompArrow "b0" "b1"
             # forceDecArrow "a3" "a4"
             # forceDecArrow "a2" "a3"
             # forceDecArrow "a1" "a2"
             # forceDecArrow "a0" "a1"
             # forceArrow "b0" "a0"
             # forceArrow "b1" "a1"
             # forceArrow "b2" "a2"
             # forceArrow "b3" "a3"

-- |add a force arrow (black) from named parts of 2 diagrams
forceArrow :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B -> Diagram B
forceArrow = connectOutside' arrowStyleF where
  arrowStyleF = with & headLength .~ verySmall 
                     & headGap .~ small & tailGap .~ large

-- |add a (force . decompose) arrow (blue) from named parts of 2 diagrams
forceDecArrow :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B -> Diagram B
forceDecArrow = connectOutside' arrowStyleFD where
  arrowStyleFD = with & headLength .~ verySmall & headStyle %~ fc blue 
                      & shaftStyle %~ lc blue 
                      & headGap .~ small & tailGap .~ small
               
-- |add a decompose arrow (ddashed blue) from named parts of 2 diagrams
decompArrow :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B -> Diagram B
decompArrow = connectOutside' arrowStyleD where
  arrowStyleD  = with & headLength .~ verySmall & headStyle %~ fc blue  
                      & shaftStyle %~ dashingG [1.5, 1.5] 0  
                      & shaftStyle %~ lc blue & headGap .~ large & tailGap .~ large

-- |add a compose arrow (green) from named parts of 2 diagrams
composeArrow :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B -> Diagram B
composeArrow = connectOutside' arrowStyleC where
  arrowStyleC = with & headLength .~ verySmall 
                      & headStyle %~ fc green & shaftStyle %~ lc green 
                      & headGap .~ large & tailGap .~ large

-- |add a compose arrow arc (green) from named parts of 2 diagrams and start/end angles
composeArc :: (IsName n1, IsName n2) => n1 -> n2 -> Angle Double -> Angle Double -> Diagram B -> Diagram B
composeArc a b t1 t2 = connectPerim' arrowStyleC a b t1 t2 where
  arrowStyleC = with & arrowShaft .~ arc xDir (-1/10 @@ turn) & headLength .~ verySmall 
                     & headStyle %~ fc green & shaftStyle %~ lc green 
                     & headGap .~ large & tailGap .~ large

-- |add a compose arrow arc (green) from named parts of 2 diagrams (horizontal left to right)
composeArcRight :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B -> Diagram B
composeArcRight a b = composeArc a b (1/10 @@ turn) (4/10 @@ turn)

-- |add a compose arrow arc (green) from named parts of 2 diagrams (vertical up)
composeArcUp :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B -> Diagram B
composeArcUp a b = composeArc a b (3/10 @@ turn) (6/10 @@ turn)
    

{-| curioPic0 is diagram curioPic without arrows
-}
curioPic0 :: Diagram B
curioPic0 = padBorder $ lw ultraThin $ position $ concat
  [ zip pointsRa $ zipWith named ["a4","a3","a2","a1","a0"] (dots : fmap center forceDs)
  , zip pointsRb $ zipWith named ["b4", "b3","b2","b1"] (dots : fmap center forceXDs)
  , zip pointsRc $ zipWith named ["c4", "c3","c2","c1","c0"] (dots : fmap center xDs)
  ] where
    forceDs  = rotations [1,1]  $ phiScaling phi $ reverse $ take 4 $ fmap (drawGraph . force) dartDs
    forceXDs = rotations [9,9,8]  $ phiScaling phi $ reverse $ take 3 $ fmap drawForce xDGraphs
    xDGraphs = decompositions sunPlus3Dart'
    xDs  = rotations [9,9,8] $  phiScaling phi $ reverse $
           drawGraph dartGraph : (drawGraph sunPlus3Dart' # lc red # lw thin): 
           take 2  (drop 1 $ fmap drawSmartGraph xDGraphs)
    dots = center $ hsep 1 $ replicate 4 (circle 0.5 # fc gray # lw none)
    pointsRa = map p2 [ (0, 80), (42, 80), (95, 80), (150, 80), (200, 80)]
    pointsRb = map p2 [ (0, 40), (42, 40), (95, 40), (150, 40)]
    pointsRc = map p2 [ (0, 0),  (42, 0),  (95, 0),  (150, 0), (200, 0)]

{-| curioPic is a diagram illustrating where compose loses information not recovered by force
  with sunPlus3Dart' third item in bottom row, (curioPic0 is diagram without arrows)
-}
curioPic :: Diagram B
curioPic = 
  curioPic0  # composeArcRight "a3" "a2"
             # composeArcRight "a2" "a1"
             # composeArcRight "a1" "a0"
             # composeArcRight "a4" "a3"
             # composeArcRight "b3" "b2"
             # composeArcRight "b2" "b1"
             # composeArcRight "b4" "b3"
             # composeArrow "b1" "a0"
             # forceDecArrow "a3" "a4"
             # forceDecArrow "a2" "a3"
             # forceDecArrow "a1" "a2"
             # forceDecArrow "a0" "a1"
             # forceDecArrow "b3" "b4"
             # forceDecArrow "b2" "b3"
             # forceDecArrow "b1" "b2"
             # decompArrow "c3" "c4"
             # decompArrow "c2" "c3"
             # decompArrow "c1" "c2"
             # forceArrow "c1" "b1"
             # forceArrow "c2" "b2"
             # forceArrow "c3" "b3"
             # forceArrow "c0" "a0"
             # composeArcRight "c4" "c3"
             # composeArcRight "c3" "c2"
             # composeArcRight "c2" "c1"
             # composeArcRight "c1" "c0"

-- |figure showing ordering of a decomposed kite (bottom), a test graph with an extra LK(3,6,8),
-- and forced figure at the top and composition of all 3 = kite on the right
graphOrder1 = padBorder $ hsep 2 [center $ vsep 1 [ft,t,dcft], cft] where
              [cft,dcft,ft,t] = fmap dashJVPinned $ scales [phi] $ alignAll (1,2) $ fmap makeVPinned 
                                [cftest, dcftest, ftest, test]
              dcftest = decompose cftest
              cftest = compose ftest
              ftest = force test
              test = makeTgraph [RK (4,7,2),LK (4,5,7),RD (1,7,5),LK (3,2,7)
                                ,RK (3,7,6),LD (1,6,7), LK(3,6,8)
                                ]


{-*
Testing (functions and figures and experiments)
-}          
-- |diagrams of forced graphs for boundaryGapFDart4 and boundaryGapFDart5
testForce4, testForce5 :: Diagram B
testForce4 = padBorder $ lw ultraThin $ dashJVGraph $ force boundaryGapFDart4
testForce5 = padBorder $ lw ultraThin $ dashJVGraph $ force boundaryGapFDart5        

  
{-| testViewBoundary is a testing tool to inspect the boundary vertex locations of some (intermediate) BoundaryState
-- (used in conjunction with stepForce to get an intermediate BoundaryState)
-- The boundary edges of a BoundaryState are shown in lime - using the BoundaryState positions of vertices.
-- This is overlaid on the full graph drawn with vertex labels.
-}
testViewBoundary :: BoundaryState -> Diagram B
testViewBoundary bd =  lc lime (drawEdges vpMap bdE) <> dashJVGraph g where 
    g = recoverGraph bd
    vpMap = bvLocMap bd
    bdE = boundary bd

-- |used to discover accuracy problem of older thirdVertexLoc
-- view tha boundary after n steps of forcing (starting with boundaryGapFDart5)
-- e.g. n = 1900 for inspectForce5 or 200 for inspectForce3
inspectForce5,inspectForce3 :: Int -> Diagram B
inspectForce5 n = padBorder $ lw ultraThin $
                  testViewBoundary $ boundaryState $ stepForce boundaryGapFDart5 n

inspectForce3 n = padBorder $ lw ultraThin $
                  testViewBoundary $ boundaryState $ stepForce (dartDs!!3) n


-- |figures showing boundary edges of the boundary gap graphs boundaryGapFDart4 and boundaryGapFDart5 
testBoundary4, testBoundary5 :: Diagram B
testBoundary4 =  padBorder $ lw ultraThin $ drawGBoundary boundaryGapFDart4 
testBoundary5 =  padBorder $ lw ultraThin $ drawGBoundary boundaryGapFDart5 

-- |Example for testing crossing boundary detection e.g. using 
-- checkedTgraph testCrossingBoundary, or by using
-- force (makeUncheckedTgraph testCrossingBoundary)
-- This produces a non-valid Tgraph.
testCrossingBoundary :: [TileFace]
testCrossingBoundary = [LK (1,8,3),RD (2,3,8),RK (1,3,9),LD (4,9,3),LK (5,10,13),RD (6,13,10)
                       ,LK (3,2,13),RK (3,13,11),RK (3,14,4),LK (3,11,14),LK (7,4,14),RK (7,14,12)
                       ]

-- |test wholeTiles (which adds missing second halves of each face)
checkCompleteFig:: Diagram B
checkCompleteFig =  padBorder $ hsep 1 $ fmap dashJGraph [sunD4, wholeTiles sunD4] where sunD4 = sunDs !! 4

-- |test graphFromVP
checkGraphFromVP :: Diagram B
checkGraphFromVP = padBorder $ (drawGraph . graphFromVP . makeVPinned) dartD4

-- |figure testing selectFacesGtoVP by removing all kites
dartsOnlyFig :: Diagram B
dartsOnlyFig = padBorder $ lw thin $ drawPatch $ dropLabels $ selectFacesGtoVP darts g where
    g = force $ sunDs !! 5
    darts = filter isDart $ faces g

{-*
Using SubTgraphs
-}
-- |hollowgraph illustrates an essential use of SubTgraphs.
-- Starting with fd2 = force (dartDs!!2) we want to make 3 further decompositions,
-- but also make 3 forced decompositions and then subtract the former faces from the latter.
-- The result happens to be a valid Tgraph but this is not generally the case.
-- SubTgraphs are essential to ensure numbering of new vertices in decompositions
-- match up in the 2 cases which they would not do if treated separately.
hollowGraph::Tgraph
hollowGraph = removeFaces (head (tracked exampleSub)) (tgraph exampleSub) where
  exampleSub = iterate (forceSub . decomposeSub) (pushFaces (newSubTgraph fd2)) !!3
  fd2 = force (dartDs!!2)

-- |figure showing hollowGraph and result of forcing
forceHollowFig:: Diagram B
forceHollowFig = padBorder $  lw ultraThin $ hsep 1 $ fmap drawGraph [hollowGraph, force hollowGraph]


{-
N.B.  Changes to forcing (or decomposing) can affect the vertex numbers chosen in twoChoices...
They should be the long edge of the left dart on the left of a group of 3 darts
Middle of top edge of dartDs!!4.  Use e.g
checkChoiceEdge (force $ dartDs !!4)
to view the vertex numbers
-}
-- |for Tgraph g produce diagram of forced version showing vertices (to inspect edges for selection)
checkChoiceEdge g = padBorder $ lw ultraThin $ drawVGraph $ force g

-- |given a boundary directed edge of a forced graph (either direction)
-- construct two SubTgraphs with half dart/kite addition on the edge respectively
-- track the resulting faces and also the singleton new face, then force both SubTgraphs
trackTwoChoices:: Dedge -> Tgraph -> [SubTgraph]
trackTwoChoices de g = [sub1,sub2] where
          sub1 = forceSub $ pushFaces $ addHalfDartSub de $ newSubTgraph g
          sub2 = forceSub $ pushFaces $ addHalfKiteSub de $ newSubTgraph g

-- |forced 4 times decomposed dart (used for identifying particular boundary
-- edges in twoChoices and moreChoices)
forceDartD4Fig:: Diagram B
forceDartD4Fig = padBorder $ lw ultraThin $ dashJVGraph $ force $ dartD4          
-- |Take a forced, 4 times decomposed dart, then track the two choices
twoChoices:: [SubTgraph]
twoChoices = trackTwoChoices (223,255) (force $ dartD4) --(233,201) 

-- |show the result of (tracked) two choices
-- with tracked faces in red, new face filled black. 
drawChoice:: SubTgraph -> Diagram B
drawChoice = drawSubTgraph [drawPatch, lc red . drawPatch, drawPatchWith (fillDK black black)]
         
-- |show the (tracked) twoChoices with (tracked faces in red, new face filled black)  
twoChoicesFig:: Diagram B
twoChoicesFig  = padBorder $ lw ultraThin $ hsep 1 $ fmap drawChoice $ twoChoices
{-
-- |show the (tracked) twoChoices with (tracked faces in red, new face filled black)  
twoChoicesFig:: Diagram B
twoChoicesFig  = padBorder $ lw ultraThin $ hsep 1 $ fmap drawSub twoChoices where
    drawSub = drawSubTgraph [drawPatch, lc red . drawPatch, drawPatchWith (fillDK black black)]
-}

-- |track two further choices with the first of twoChoices (fullgraph)  
moreChoices0:: [SubTgraph]
moreChoices0 = trackTwoChoices (200,241) (tgraph $ twoChoices !! 0) --(178,219)

-- |track two further choices with the second of twoChoices (fullgraph)  
moreChoices1:: [SubTgraph]
moreChoices1 = trackTwoChoices (200,241) (tgraph $ twoChoices !! 1) --(178,219)

-- |figures for 4 further choices
moreChoicesFig0,moreChoicesFig1,moreChoicesFig:: Diagram B
moreChoicesFig0 =  padBorder $ lw ultraThin $ hsep 10 $ fmap drawChoice moreChoices0
moreChoicesFig1 =  padBorder $ lw ultraThin $ hsep 1 $ fmap drawChoice moreChoices1
moreChoicesFig  =  vsep 1 [moreChoicesFig0,moreChoicesFig1]  


-- |What happens if you take the first result of twoChoices, then 
-- select only the faces that were added by force, then force just these faces?
-- It does not quite complete the original faces    
forcedNewFaces:: Diagram B
forcedNewFaces = padBorder $ lw thin $ drawForce g2 where
    g1 = addHalfDart (223,255) (force $ dartD4) --(233,201)
    g2 = removeFaces (faces g1) (force g1)
                       
-- |Trying to find which extensions to the starting dart correspond to the twoChoicesFig
dartHalfDart,dartHalfKite,dartPlusDart,dartPlusKite :: Tgraph
-- |a dart with another half dart on a long edge
dartHalfDart = addHalfDart (1,2) dartGraph
-- |a dart with a half kite on a long edge
dartHalfKite = addHalfKite (1,2) dartGraph
-- |two darts sharing a long edge
dartPlusDart = addHalfDart (1,5) dartHalfDart
-- |a dart and a kite sharing a long edge
dartPlusKite = addHalfKite (2,5) dartHalfKite
-- |two kites sharing a long edge
kitePlusKite = addHalfKite (1,5) $ addHalfKite (1,3) kiteGraph

sunPlusDart,sunPlus2Dart,sunPlus2Dart',sunPlus3Dart,sunPlus3Dart' :: Tgraph
-- |A sun with a single complete dart on the boundary
sunPlusDart = addHalfDart (3,4) $ addHalfDart (2,3) sunGraph
-- |A sun with 2 darts adjacent on the boundary
sunPlus2Dart = addHalfDart (5,6) $ addHalfDart (4,5) sunPlusDart
-- |A sun with 2 darts NOT adjacent on the boundary
sunPlus2Dart' = addHalfDart (7,8) $ addHalfDart (6,7) sunPlusDart
-- |A sun with 3 darts adjacent on the boundary
sunPlus3Dart = addHalfDart (7,8) $ addHalfDart (6,7) sunPlus2Dart
-- |A sun with 3 darts on the boundary NOT all adjacent
-- This example has an emplacement that does not include the original but is still a correct Tgraph
sunPlus3Dart' = addHalfDart (9,10) $ addHalfDart (8,9) sunPlus2Dart

-- |halfWholeFig shows that a whole dart/kite needs to be added to get the same result as twoChoicesFig
-- Adding a half tile has no effect on the forced decomposition
halfWholeFig:: Diagram B
halfWholeFig =  padBorder $ lw ultraThin $ vsep 1 $ fmap (hsep 1) [take 2 figs, drop 2 figs]
  where                        
    figs = zipWith redEmbed scaledCases forcedD4Cases
    cases = [dartPlusDart, dartPlusKite, dartHalfDart, dartHalfKite]
    scaledCases = alignAll (1,3) $ fmap (scale (phi^4) . makeVPinned) cases
    forcedD4Cases = alignAll (1,3) $ fmap (makeVPinned . force . decomp4) cases
    decomp4 g = decompositions g !! 4
    redEmbed g1 g2 = lc red (lw medium $ dashJPatch $ dropLabels g1) <> lw ultraThin (drawPatch $ dropLabels g2)

-- | two kites (force decomp twice) figure
kkEmpsFig:: Diagram B
kkEmpsFig = padBorder $ lw ultraThin $ vsep 1 $ rotations [0,9,9] $ 
            fmap drawGraph  [kk, kkD, kkD2] where
              kk = kitePlusKite
              kkD = force $ decompose kk
              kkD2 = force $ decompose kkD
             
-- | two kites added to related vertex types figure
maxShapesFig:: Diagram B
maxShapesFig = relatedVTypeFig ||| kkEmpsFig

-- |compareForceEmplace g is a diagram showing g embedded (in red) in force g, followed by emplace g
compareForceEmplace :: Tgraph -> Diagram B
compareForceEmplace g = padBorder $ hsep 1 $
                        [ drawForce g
                        , drawGraph $ emplace g
                        ]

-- |sunPlus3Dart' is a sun with 3 darts on the boundary NOT all adjacent
-- This example has an emplacement that does not include the original but is still a correct Tgraph.
-- The figure shows the force and emplace difference.
emplaceProblemFig:: Diagram B
emplaceProblemFig = compareForceEmplace sunPlus3Dart'

-- | force after adding half dart (rocket cone) to sunPlus3Dart'.
-- Adding a kite half gives an incorrect graph discovered by forcing.
rocketCone1:: Tgraph
rocketCone1 =  force $ addHalfDart (59,60) $ forcedDecomp sunPlus3Dart'

-- | figure for rocketCone
rocketCone1Fig:: Diagram B
rocketCone1Fig = padBorder $ lw thin $ hsep 1 $ fmap dashJVGraph [r1,rc1] where
  r1 = forcedDecomp sunPlus3Dart'
  rc1 = force $ addHalfDart (59,60) r1
  
-- | figure for rocket5 showing its maximal forced composition
rocket5Fig:: Diagram B
rocket5Fig = padBorder $ lw ultraThin  drawWithMax rocket5

-- | rocket5 is the result of a chain of 5 forcedDecomps, each after
-- adding a dart (cone) to the tip of the previous rocket starting with sunPlus3Dart'.
-- As a quick check rocket5 was extends with both choices on a randomly chosen boundary edge (8414,8415)
-- drawGraph $ force $ addHalfKite rocket5 (8414,8415)
-- drawGraph $ force $ addHalfDart rocket5 (8414,8415)
rocket5:: Tgraph
rocket5 = forcedDecomp rc4 where
  rc0 = sunPlus3Dart'
  rc1 = force $ addHalfDart (59,60) (forcedDecomp rc0)
  rc2 = force $ addHalfDart (326,327) (forcedDecomp rc1)
  rc3 = force $ addHalfDart (1036,1037) (forcedDecomp rc2)
  rc4 = force $ addHalfDart (3019,3020) (forcedDecomp rc3)

-- |6 times forced and decomposed kingGraph. Has 53574 faces (now builds more than 60 times faster after profiling)
-- There are 2906 faces for kingD6 before forcing.
kingFD6:: Diagram B
kingFD6 = padBorder $ lw ultraThin $ colourDKG (darkmagenta, indigo, gold) $ makePatch $
          allForcedDecomps kingGraph !!6

-- | displays some test cases for boundary edge types using boundaryECover
testCasesE = padBorder $ lw ultraThin $ vsep 1 $ fmap (testcase (1,2) . makeTgraph . (:[])) examples where
    examples = [LD(1,3,2),LK(2,1,3),LK(3,2,1)]
    testcase alig g = hsep 1 $ 
      fmap (((t <> fbdes) <>) . drawPatch . makeAlignedPatch alig . recoverGraph) $ boundaryECover $ bd 
      where t = seeOrigin $ drawPatchWith (fillDK black black) $ makeAlignedPatch alig g
            seeOrigin = ((circle 0.25 # fc red # lw none) <>) 
            bd = runTry $ tryForceBoundary $ makeBoundaryState g
            vp = alignXaxis alig $ makeVPinned $ recoverGraph bd
            fbdes = drawEdgesWith vp (boundary bd) # lw thin

-- | displays some test cases for boundary edge types using boundaryVCover
testCasesV = padBorder $ lw ultraThin $ vsep 1 $ fmap (testcase (1,2) . makeTgraph . (:[])) examples where
    examples = [LD(1,3,2),LK(2,1,3),LK(3,2,1)]
    testcase alig g = hsep 1 $ 
      fmap (((t <> fbdes) <>) . drawPatch . makeAlignedPatch alig . recoverGraph) $ boundaryVCover bd 
      where t = seeOrigin $ drawPatchWith (fillDK black black) $ makeAlignedPatch alig g
            seeOrigin = ((circle 0.25 # fc red # lw none) <>)
            bd = runTry $ tryForceBoundary $ makeBoundaryState g
            vp = alignXaxis alig $ makeVPinned $ recoverGraph bd
            fbdes = drawEdgesWith vp (boundary bd) # lw thin

{- OLD non-tree version
-- | displays all cases for boundary edges by adding faces at either end and forcing.
-- There are only 3 (left) starting points we need to consider with the edge shown in red
-- (right versions will be symmetric, and joins and dart short edges have unique cases so not considered).
-- In each case, whenever there is a graph where the red edge is still on the boundary,
-- that graph appears extended with both a kite and a dart on the red edge amongst the diagrams beyond it.
-- This provides a completeness argument for forcing.
boundaryEdgeCases = pad 1.02 $ centerXY $ lw ultraThin $ vsep 5 $ fmap caseRows examples where
    examples = fmap  (makeTgraph . (:[])) [LD(1,3,2),LK(2,1,3),LK(3,2,1)]
    edge = (1,2)
    caseRows g = vsep (-1) $ fmap (hsep 1) $ chunks 7 $ fmap drawCase $ growBothEnds fbd 
      where fbd = runTry $ tryForceBoundary $ makeBoundaryState g
            fvp = alignXaxis edge $ makeVPinned $ recoverGraph fbd
            fbdes = ((drawEdgeWith fvp edge # lc red) <> drawEdges (vLocs fvp) (boundary fbd)) # lw thin
            drawCase = (fbdes <>) . drawPatch . makeAlignedPatch edge . recoverGraph
    addOnRight bd = -- add dart/kite on boundary edge starting at v then force each case
      case filter ((==(snd edge)). fst) (boundary bd) of
          [] -> []
          [de] -> bothDartAndKite de bd
    addOnLeft bd = -- add dart/kite on boundary edge ending at v then force each case
      case filter ((==(fst edge)). snd) (boundary bd) of
          [] -> []
          [de] -> bothDartAndKite de bd
    growBothEnds bd = bd: goBoth (filter continue [bd]) where
      continue bd = edge `elem` boundary bd
-- to avoid repetitions, goBoth produces right and left cases but then recurses to the right only,
-- using goLeft to deal with left cases recursively.
      goBoth [] = []
      goBoth bds = let left = concatMap addOnLeft bds
                       right  = concatMap addOnRight bds
                   in left ++ goLeft (filter continue left) ++ right ++ goBoth (filter continue right)                  
      goLeft [] = []
      goLeft bds =  left ++ goLeft (filter continue left) where left = concatMap addOnLeft bds
-}

-- | Figure displaying all cases for boundary edges of forced Tgraphs.
-- These are produced as trees (but only the levels of the trees are displayed).
-- We start with an edge (shown red) of a face on the boundary after forcing the face.
-- There are only 3 (left-hand) starting face edges we need to consider, so there are 3 trees
-- (Right versions will be symmetric, and joins and dart short edges are immediately covered by forcing so not shown).
-- Each tree is grown by adding a kite/dart face at either end of the boundary edge
-- and forcing, terminating as a leaf node when the red edge is no longer on the boundary.
-- In each case, whenever there is a Tgraph where the red edge is still on the boundary,
-- that Tgraph appears extended with both a kite and a dart on the red edge amongst the diagrams below it in the tree.
-- This provides a completeness argument for forcing.
-- [The edge must be on the boundary if both additions are possible.]
boundaryEdgeCaseTrees:: Diagram B
boundaryEdgeCaseTrees = pad 1.02 $ centerXY $ lw ultraThin $ vsep 13 $ fmap caseRows examples where
    examples = fmap  (makeTgraph . (:[])) [LD(1,3,2),LK(2,1,3),LK(3,2,1)]
    edge = (1,2)
    caseRows g = vsep 3 $ fmap (centerX . hsep 1 # composeAligned alignT) $ levels $ treeFor g
    treeFor g = fmap drawCase $ growBothEnds fbd where
      fbd = runTry $ tryForceBoundary $ makeBoundaryState g
      drawCase bd = fbdes <> drawg where
                    g = recoverGraph bd
                    vp = alignXaxis edge $ makeVPinned g
                    drawg = drawPatch (dropLabels vp)
                    fbdes = ((drawEdgeWith vp edge # lc red) <> drawEdgesWith vp (boundary fbd)) # lw thin

    addOnRight bd = -- add dart/kite on boundary edge starting at v then force each case
      case filter ((==(snd edge)). fst) (boundary bd) of
          [] -> []
          [de] -> bothDartAndKite de bd
    addOnLeft bd = -- add dart/kite on boundary edge ending at v then force each case
      case filter ((==(fst edge)). snd) (boundary bd) of
          [] -> []
          [de] -> bothDartAndKite de bd
-- growBothEnds:: BoundaryState -> Tree BoundaryState
    growBothEnds bd = goB bd where
      continue bd = edge `elem` boundary bd
-- to avoid repetitions, goB produces right and left cases but then recurses to the right only,
-- using goL to deal with left cases recursively.
      goB bd = if continue bd
               then Node{ rootLabel=bd, subForest = fmap goL (addOnLeft bd) ++ fmap goB (addOnRight bd)}
               else Node{ rootLabel=bd, subForest = []}
      goL bd = if continue bd
               then Node{ rootLabel=bd, subForest = fmap goL (addOnLeft bd)}
               else Node{ rootLabel=bd, subForest = []}

-- | boundaryVCoverFigs g - produces a list of diagrams for the boundaryVCover of g  (with g shown in red in each case)
boundaryVCoverFigs g = 
    fmap (lw ultraThin . (redg <>) . drawPatch . makeAlignedPatch alig . recoverGraph) $ 
    boundaryVCover $ makeBoundaryState g 
      where redg = lc red $ drawPatch $ makeAlignedPatch alig g
            alig = lowestJoin (faces g)

-- | boundaryECoverFigs g - produces a list of diagrams for the boundaryECover of g  (with g shown in red in each case)
boundaryECoverFigs g = 
    fmap (lw ultraThin . (redg <>) . drawPatch . makeAlignedPatch alig . recoverGraph) $ 
    boundaryECover $ makeBoundaryState g 
      where redg = lc red $ drawPatch $ makeAlignedPatch alig g
            alig = lowestJoin (faces g)

-- | diagram showing the boundaryECover of a forced kingGraph
kingECoverFig = padBorder $ vsep 1 $ fmap (hsep 1) $ chunks 3 $ boundaryECoverFigs $ force kingGraph
-- | diagram showing the boundaryVCover of a forced kingGraph
kingVCoverFig = padBorder $ vsep 1 $ fmap (hsep 1) $ chunks 3 $ boundaryVCoverFigs $ force kingGraph


{-*
Boundary Edge Contexts
-}

-- |A directed edge with 3 lists of (left handed) contexts for the edge on a forced Tgraph boundary,
-- This simple version does not extend cases with an empty composition.
forcedBSimpleContexts:: (Dedge,([BoundaryState],[BoundaryState],[BoundaryState]))
forcedBSimpleContexts = (edge, (dartLong,kiteLong,kiteShort)) where
  edge = (1,2)
  dartLong = casesFor $ force $ makeTgraph [LD(1,3,2)]
  kiteLong = casesFor $ force $ makeTgraph [LK(2,1,3)] 
  kiteShort = casesFor $ force $ makeTgraph [LK(3,2,1)]
  casesFor g = boundaryEdgeContexts edge (makeBoundaryState g)

{-
A directed edge with  3 lists of (left handed) contexts for the edge on the boundary of a forced Tgraph.
This extends forcedBSimpleContexts by adding further contexts for cases where the composition is empty.
For each row, we consider both dart and kite additions either side of the edge of the leftmost starting Tgraph,
plus further additions either side of the red edge in the results,
and remove any Tgraphs where the red edge is no longer on the boundary.
The composition of each Tgraph is shown filled yellow (no yellow means empty composition).
We can check that both ends of the red line are on a normal (non-crossed) boundary or not present in the composition.
(N.B. Some repeated cases removed)  
-}
forcedBExtendedContexts:: (Dedge,([BoundaryState],[BoundaryState],[BoundaryState]))
forcedBExtendedContexts = (edge, (dartLongE,kiteLongE,kiteShortE) ) where
  (edge , (dartLong,kiteLong,kiteShort)) = forcedBSimpleContexts
  dartLongE = extendContexts edge dartLong 
  kiteLongE  = extendContexts edge kiteLong 
  kiteShortE = extendContexts edge kiteShort 

{-
Diagram of all (extended) contexts for an edge on the boundary of a forced Tgraph (using forcedBExtendedContexts).
The edge is shown in red in each case.
There are 3 groups for the 3 edge types (right-hand variants are not shown).
For each group, we consider both dart and kite additions either side of the red edge of the first Tgraph in the group.
There are further dart and kite additions either side of the red edge in the results, and further additions on other boundary edges when
the composition is empty.
We remove any Tgraphs where the red edge is no longer on the boundary and remove any repeated cases.
The composition of each Tgraph is shown filled yellow (no yellow means empty composition).
We can check that both ends of the red line are on a normal (non-crossed) boundary or not present in the composition.
-}
forcedBContextsFig :: Diagram B
forcedBContextsFig = padBorder $ lw ultraThin $ vsep 5 
                          [vsep 1 $ fmap (hsep 1) $ chunks 6 dartLongDiags
                          ,hsep 1 kiteLongDiags
                          ,vsep 1 $ fmap (hsep 1) $ chunks 6 kiteShortDiags
                          ] where
  (edge, (dartLongE,kiteLongE,kiteShortE) ) = forcedBExtendedContexts
  dartLongDiags = fmap (drawContext edge) dartLongE 
  kiteLongDiags = fmap (drawContext edge) kiteLongE
  kiteShortDiags = fmap (drawContext edge) kiteShortE

  -- drawContext::Dedge -> BoundaryState -> Diagram B
  drawContext edge bd = drawe <> drawg <> drawComp where
    g = recoverGraph bd
    vp = alignXaxis edge $ makeVPinned g
    drawg = drawPatch (dropLabels vp)
    drawe = drawEdgeWith vp edge # lc red # lw medium
    drawComp = lw none $ drawPatchWith (fillDK yellow yellow) $ subPatch vp (faces (compose g))




-- |chunks n l -  split a list l into chunks of length n (n>0)
chunks::Int -> [a] -> [[a]]
chunks n 
  | n < 1 = error "chunks: argument <1\n"
  | otherwise = ch where 
      ch [] = []
      ch as = take n as : ch (drop n as)

-- |boundaryLoopFill tests the calculation of boundary loops of a Tgraph and conversion to a (Diagrams) Path, using
-- boundaryLoopsG and pathFromBoundaryLoops. The conversion of the Path to a Diagram allows
-- a fill colour to be used for the entire internal part of the Tgraph - i.e. not by filling the individual tilefaces.
-- It also associates vertex labels with the respective positions in the diagram.
boundaryLoopFill:: Colour Double -> Tgraph -> Diagram B
boundaryLoopFill c g = dg # lw ultraThin <> d # fc c where
    vp = makeVPinned g
    dg = drawPatch $ dropLabels vp
    vlocs = vLocs vp
    bdLoops = boundaryLoopsG g
    d = strokeP' (with & vertexNames .~ bdLoops) $ pathFromBoundaryLoops vlocs bdLoops
--    d = strokeP' (with & vertexNames .~ bdLoops) $ toPath $ map (glueTrail . trailFromVertices . map getPoint) bdLoops

testLoops1,testLoops2:: Diagram B
-- | diagram using boundaryLoopFill with a single boundary loop
testLoops1 = padBorder $ boundaryLoopFill honeydew boundaryGapFDart4

-- | diagram using boundaryLoopFill with two boundary loops (i.e. a single hole)
testLoops2 = padBorder $ lw ultraThin $ boundaryLoopFill honeydew g where
         g = removeFaces (faces $ recoverGraph bs1) (recoverGraph bs2)
         bs2 = head $ boundaryVCover bs1 
         bs1 = head $ boundaryVCover bs0
         bs0 = runTry $ tryForceBoundary $ makeBoundaryState kingGraph
--         bs0 = makeBoundaryState $ force kingGraph

{-*
Testing Relabelling (fullUnion, commonFaces)
-}

{-|A diagram testing matchByEdges with a single tile-connected overlap.
The top 2 graphs g1 and g2 have possible matching overlaps except for labelling.
The next row has: (left) a relabelling of g2 leaving (1,10) 
which is a preparation step to avoid accidental clashes with g1,
(middle) a further relabelling of g2 by matching against g1 using (1,10)
as the edge to match with (1,15),
(right) the union of this relabelled graph with g1.
The bottom row is as for the row above but using (1,18) as the edge to match with (1,15)
resulting in a different union.
-}
testRelabellingFig:: Diagram B
testRelabellingFig = padBorder $ lw ultraThin $ vsep 1 
                       [ hsep 1 $ center <$> take 2 eight
                       , hsep 1 $ center <$> take 3 $ drop 2 eight
                       , hsep 1 $ center <$> drop 5 eight
                       ] where
     eight = fmap dashJVGraph [ g1
                              , g2
                              , g2_A
                              , matchByEdges (g1, (1,15)) (g2,(1,10))
                              , fullUnion (g1, (1,15)) (g2,(1,10))
                              , g2_B
                              , matchByEdges (g1, (1,15)) (g2,(1,18))
                              , fullUnion (g1, (1,15)) (g2,(1,18))
                              ]
     sunD2 = sunDs!!2
     fsunD2 = force sunD2
     g1 = removeFaces [RK(1,31,41)] (removeVertices [74,79,29] sunD2)
     reduced2 = removeVertices [8,7,6] fsunD2
     g2 = relabelContig reduced2
     g2_A = prepareFixAvoid [1,10] (vertices g1) g2
     g2_B = prepareFixAvoid [1,18] (vertices g1) g2

{-| Example showing match relabelling failing as well as a successful fullUnion of graphs.
The top right graph g2 is matched against the top left graph g1 
with g2 edge (1,10) matching g1 edge (1,15).
The bottom left shows the relabelling to match, but this is not correct because the overlap of
g2 and g1 is not a simple tile connected region.
(In the bottom left relabelled graph, vertex 41 does not get matched to 22 in g1, for example)
A call to relabelTouching is essential to produce a valid Tgraph.
-}
incorrectAndFullUnionFig:: Diagram B
incorrectAndFullUnionFig = padBorder $ lw ultraThin $ vsep 1 
                            [ hsep 1 $ center <$> take 2 thelist
                            , hsep 1 $ center <$> drop 2 thelist
                            ] where
     thelist = fmap dashJVPinned $ rotations [0,7] $ fmap makeVPinned 
                 [ g1
                 , g2
                 , matchByEdges (g1, (1,15)) (g2,(1,10))
                 , fullUnion  (g1, (1,15)) (g2,(1,10))
                 ]
     sunD2 = sunDs!!2
     fsunD2 = force sunD2
     g1 = removeFaces [RK(1,31,41)] (removeVertices [74,79,29] sunD2)
         --removeFaces [RK(1,16,36)] (removeVertices [20,48,49,35,37] sunD2)
     reduced2 = removeVertices [8,7,6,23] fsunD2
     g2 = relabelContig reduced2

{-| Example showing the use of commonFaces.
 This is applied to the pairs from forcedKingChoicesFig
-}
testCommonFacesFig :: Diagram B
testCommonFacesFig = padBorder $ vsep 1 $ fmap edgecase [(57,58),(20,38),(16,23),(49,59)] where
    fk = force $ kingGraph
    drawSub = drawSubTgraph [drawPatch, lc red . drawPatch, drawPatchWith (fillDK black black)]
    edgecase e = hsep 1 $ fmap (lw ultraThin) [drawSub sub1, drawSub sub2, drawCommonFaces (g1,(1,2)) (g2,(1,2))] 
      where
        [sub1, sub2] = trackTwoChoices e fk
        g1 = tgraph sub1
        g2 = tgraph sub2

-- | Diagram comparing two choices at 4 boundary edges of a forced kingGraph
forcedKingChoicesFig :: Diagram B
forcedKingChoicesFig = padBorder $ lw ultraThin $ vsep 1 $ fmap example [(57,58),(20,38),(16,23),(49,59)] where
    fk = force $ kingGraph
    drawSub = drawSubTgraph [drawPatch, lc red . drawPatch, drawPatchWith (fillDK black black)]
    example e = hsep 1 $ fmap drawSub $ trackTwoChoices e fk 

-- | testing drawing of king empire based on forcedKingChoicesFig
testKingEmpire :: Diagram B
testKingEmpire =  padBorder $ drawCommonFaces (g4,(1,2)) (g3,(1,2)) where
  fk = force $ kingGraph
  g1 = addHalfKite (20,38) fk
  g2 = addHalfKite (16,23) fk
  g3 = fullUnion (g1,(1,2)) (g2,(1,2))
  g4 = addHalfKite (49,59) fk

{-|
Diagram showing a calculation of some of the kings empire level 1 (done by hand - not using empire1).
The top left graph shows the intersection faces of the the other 6 graphs (emphasised) with the second graph as background.
The latter 6 graphs show all the possible ways of extending a forced kingGraph (forced kinGraph shown red)
round its boundary.
-}
kingEmpire:: Diagram B
kingEmpire = padBorder $ lw ultraThin $ vsep 1 $ 
             [ hsep 10 $ [emphasizeFaces fcs g1, drawChoice sub1]
             , hsep 1 $ fmap drawChoice [sub2,sub3,sub4]
             , hsep 1 $ fmap drawChoice [sub5,sub6]
             ]  where
    fkSub = newSubTgraph $ force $ kingGraph
    sub1 = forceSub $ pushFaces $ addHalfKiteSub (49,59) fkSub
    sub2 = forceSub $ pushFaces $ unionTwoSub $ addHalfKiteSub (16,23) $ addHalfKiteSub (20,38) fkSub
    sub3 = forceSub $ pushFaces $ unionTwoSub $ addHalfDartSub (16,23) $ addHalfKiteSub (20,38) fkSub
    sub4 = forceSub $ pushFaces $ unionTwoSub $ addHalfKiteSub (16,23) $ addHalfDartSub (20,38) fkSub
    subX = unionTwoSub $ unionTwoSub $ addHalfDartSub (49,59) $ addHalfDartSub (16,23) $ addHalfDartSub (20,38) fkSub
    sub5 = forceSub $ pushFaces $ unionTwoSub $ addHalfDartSub (56,57) subX
    sub6 = forceSub $ pushFaces $ unionTwoSub $ addHalfKiteSub (56,57) subX
    g1 = tgraph sub1
    g1Intersect sub = commonFaces (g1,(1,2)) (tgraph sub,(1,2))
    fcs = foldl' intersect (faces g1) $
           fmap g1Intersect [sub2,sub3,sub4,sub5,sub6]

-- | Diagram showing 5 embeddings of a forced kingGraph in a forced decomposed kingGraph
forcedKingEmbedding:: Diagram B
forcedKingEmbedding = padBorder $ lw ultraThin $ vsep 1 
  [ hsep 1 [drawGraph fk, drawGraph fdk]
  , hsep 1 [cases!!0 , cases!!1]
  , hsep 1 [cases!!2, cases!!3]
  , cases!!4
  ] where
    fk = force kingGraph
    fdk = forcedDecomp fk
    fkVP = makeVPinned fk
    backVP = makeVPinned fdk
    embed de = drawPatchWith (fillDK yellow yellow) a <> drawPatch b 
                where [a,b] = fmap dropLabels $ alignments [(1,7), de] [fkVP, backVP]
    cases = fmap center $ rotations [8,2,1,9] $ fmap embed [(43,205),(33,188),(9,107),(5,91),(12,119)]

-- | Diagram to check vertex numbering for a forced kingGraph, a forcedDecomp forced kingGraph, and a
-- twice forceDecomp forced kingGraph
kingEmpireCheck = padBorder $ lw ultraThin $ vsep 1 $ fmap drawVGraph [fk, fdfk, fdfdfk] where
    fk = force kingGraph
    fdfk = forcedDecomp fk
    fdfdfk = forcedDecomp fdfk







{-*
Testing Try functions
-}

-- | testing Try  - try arguments 0..5  only 4 succeeds
reportFailTest1 a = runTry $ onFail "reportFailTest1:\n" $ do
  b <- maybeExample a `nothingFail` ("maybeExample: produced Nothing when applied to "++show a ++"\n")
  c <- onFail "first call:\n"  $ eitherExample (b-1)
  d <- onFail "second call:\n" $ eitherExample (c-1)
  e <- onFail "third call:\n"  $ eitherExample (d-1)
  return (a,b,c,d,e)

-- | testing Try  - try arguments 0..3   only 2 succeeds
reportFailTest2 a = runTry $ onFail "reportFailTest2:\n" $ do
  b <- eitherMaybeExample a
  c <- b `nothingFail` "eitherMaybeExample produced Nothing\n"
  d <- onFail "trying eitherExample:\n" $ eitherExample (c-1)
  return (a,b,c,d)

-- | for testing in reportFailTest1 and reportFailTest2
eitherExample :: Int -> Try Int
eitherExample a = if a==0 then Left "eitherExample: arg is zero\n" else Right a 

-- | for testing in reportFailTest1
maybeExample :: Int -> Maybe Int
maybeExample a = if a<5 then Just a else Nothing

-- | for testing in reportFailTest1 and reportFailTest2
eitherMaybeExample :: Int -> Try (Maybe Int)
eitherMaybeExample a | a<1 = Right Nothing
                     | a<3 = Right (Just a) 
                     | otherwise = Left "eitherMaybeExample: arg >=3\n"



