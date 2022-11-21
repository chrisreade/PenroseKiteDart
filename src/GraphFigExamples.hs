{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

{-|
Module      : GraphFigExamples
Description : Examples of tilings represented with Tgraphs with diagrams 
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

-}
module GraphFigExamples where

-- used for testing
import qualified Data.IntMap.Strict as VMap (IntMap, lookup, insert, empty, fromList, union)

-- partition find used for testing (relabelWatchStep)
import Data.List ((\\), partition,find, intersect)      
import Diagrams.Prelude

import ChosenBackend (B)
import TileLib
import Tgraphs


-- |used for most diagrams to give border padding
padBorder:: Diagram B -> Diagram B
padBorder = pad 1.2 . centerXY

{- *
Advanced drawing tools for Tgraphs
-}

-- |same as drawGraph except adding dashed lines on boundary join edges. 
drawGraphSmart :: Tgraph -> Diagram B
drawGraphSmart g = subDrawGraph g $ createVPoints $ faces g

-- |same as drawVGraph except adding dashed lines on boundary join edges.
drawVGraphSmart :: Tgraph -> Diagram B
drawVGraphSmart g = subDrawVGraph g $ createVPoints $ faces g

-- |Auxiliary function for drawGraphSmart. It needs to be passed a suitable vertex location map.
-- This can be used instead of drawGraphSmart when such a map is already available.
subDrawGraph:: Tgraph -> VertexLocMap -> Diagram B
subDrawGraph g vpMap = (drawPatchWith dashJ $ subPatch (boundaryJoinFaces g) vpMap) 
                       <> drawPatch (subPatch (faces g) vpMap)

-- |Auxiliary function for drawVGraphSmart. It needs to be passed a suitable vertex location map.
-- This can be used instead of drawVGraphSmart when such a map is already available.
subDrawVGraph:: Tgraph -> VertexLocMap -> Diagram B
subDrawVGraph g vpMap = (drawPatchWith dashJ $ subPatch (boundaryJoinFaces g) vpMap) 
                        <> drawVPatch (subVPatch (faces g) vpMap)

-- |applies partCompose to a Tgraph g, then draws the composed graph with the remainder faces (in lime).
-- (Relies on the vertices of the composition and remainder being subsets of the vertices of g.)
drawPCompose ::  Tgraph -> Diagram B
drawPCompose g = (drawPatch $ subPatch (faces g') vpMap)
                 <> (lw thin $ lc lime $ dashJPatch $ subPatch fcs vpMap)
  where (fcs,g') = partCompose g
        vpMap = createVPoints $ faces g

-- |drawForce g is a diagram showing the argument g in red overlayed on force g
-- It adds dashed join edges on the boundary of g
drawForce:: Tgraph -> Diagram B
drawForce g = (dg # lc red) <> dfg where
    fg = force g
    vpMap = createVPoints (faces fg)
    dfg = drawPatch $ subPatch (faces fg) vpMap 
    dg = subDrawGraph g vpMap

{- |
drawWithMax g - draws g and overlays the maximal forced composition of g in red
-}
drawWithMax :: Tgraph -> Diagram B
drawWithMax g =  (dmax # lc red # lw thin) <> dg where
    vpMap = createVPoints (faces g)
    dg = drawPatch $ subPatch (faces g) vpMap
    maxg = maxCompForced g
    dmax = drawPatch $ subPatch (faces maxg) vpMap

-- |displaying the boundary of a Tgraph in lime (overlaid on the Tgraph drawn with labels)
drawGBoundary :: Tgraph -> Diagram B
drawGBoundary g =  (drawEdges vpMap bd # lc lime) <> (drawVPatch $ subVPatch (faces g) vpMap) where
    vpMap  = createVPoints (faces g)
    bd = boundaryDedges g

-- |drawCommonFaces (g1,e1) (g2,e2) uses commonFaces to find the common faces of g1 and g2
-- and emphasizes the common faces on the background g1
drawCommonFaces:: (Tgraph,Dedge) -> (Tgraph,Dedge) -> Diagram B
drawCommonFaces (g1,e1) (g2,e2) = emphasizeFaces (commonFaces (g1,e1) (g2,e2)) g1
{-
  (drawPatch commonPatch # lw thin) <> (drawPatch g1Patch # lw ultraThin) where
    fcs = commonFaces (g1,e1) (g2,e2)
    vpMap = createVPoints (faces g1)
    g1Patch = subPatch (faces g1) vpMap
    commonPatch = subPatch fcs vpMap
-}

-- |emphasizeFaces fcs g emphasizes the given faces (that are in g) overlaid on the background g.
emphasizeFaces:: [TileFace] -> Tgraph -> Diagram B
emphasizeFaces fcs g =  (drawPatch emphPatch # lw thin) <> (drawPatch gPatch # lw ultraThin) where
    vpMap = createVPoints (faces g)
    gPatch = subPatch (faces g) vpMap
    emphPatch = subPatch (fcs `intersect` faces g) vpMap
    
{- *
Example Tgraphs with Figures
-}
fool, foolD, foolDminus:: Tgraph
-- |fool: fool's kite - a decomposed left and right kite back-to-back (i.e. not sharing join edge)
fool = makeTgraph
          [ RD(1,2,3), LD(1,3,4), RK(6,2,5), LK(6,3,2), RK(6,4,3), LK(6,7,4)]

-- |a once decomposed fool (= foolDs!!1)
foolD = decomposeG fool
-- | a list of all decompositions of fool
foolDs :: [Tgraph]
foolDs = decompositionsG fool

-- |foolDminus: 3 faces removed from foolD - still a valid Tgraph
foolDminus = removeFaces [RD(6,15,13), LD(6,17,15), RK(5,11,2)] foolD
-- [RD(6,12,11), LD(6,14,12), RK(5,10,2)] foolD --removeFaces [RD(6,14,11), LD(6,12,14), RK(5,13,2)] foolD

-- | diagram of just fool
foolFig :: Diagram B
foolFig = padBorder $ dashJVGraph fool

-- |diagram of fool with foolD
foolAndFoolD :: Diagram B
foolAndFoolD = padBorder $ hsep 1 [(dashJVPatch . scale phi . makeVPatch) fool, dashJVGraph foolD]

{-|touchErrorFaces is an addition of 2 faces to those of foolD which contains touching vertices.
These will be caught by makeTgraph which raises an error.
The error is not picked up by checkedTgraph. It can be fixed using correctTouchingVs.

*** Exception: makeTgraph: touching vertices [(19,7)]

> checkedTgraph touchErrorFaces
Tgraph {maxV = 19, faces = ...}

> correctTouchingVs touchErrorFaces
Right (Tgraph {maxV = 18, faces = [..., LK (7,17,18)]})
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
sunDs =  decompositionsG sunGraph

figSunD3D2,figSunD2D:: Diagram B
-- |Figure for a 3 times decomposed sun with a 2 times decomposed sun
figSunD3D2 = padBorder $ hsep 1 [dashJVGraph $ sunDs !! 3, scale phi $ dashJVGraph $ sunDs !! 2]
-- |Fifure for a 2 times decomposed sun with a once decomposed sun
figSunD2D = padBorder  $ hsep 1 [dashJVGraph $ sunDs !! 2, scale phi $ dashJVGraph $ sunDs !! 1]

-- |Tgraph for kite
kiteGraph :: Tgraph
kiteGraph = makeTgraph [ RK(1,2,4), LK(1,3,2)]
-- |All decompositions of a kite
kiteDs :: [Tgraph]
kiteDs = decompositionsG kiteGraph

-- |Tgraph for a dart
dartGraph :: Tgraph
dartGraph =  makeTgraph [ RD(1,2,3), LD(1,3,4)]
-- |All decompositions of a dart
dartDs :: [Tgraph]
dartDs =  decompositionsG dartGraph

-- |Tgraph of 4 times decomposed dartGraph (used in several examples)
dartD4 :: Tgraph
dartD4 = dartDs!!4


{- * Partial Compositions figures
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


{- |
This example illustrates that the experimental composeK does not always make correct choices.
composeK composes by treating unknowns as large kite centres (= deuce vertices), so favouring kites when there is a choice.
The first 3 Tgraphs are correct. The second is composeK of the first and the third is force applied to the second
(a forced queen vertex).  
The fourth Tgraph is a further composeK and this is clearly an incorrect Tgraph (applying force to this fails).
In one further composition (either composeK or composeG) the incomplete mistake1 graph is produced - fifth graph.
-}
counterK :: Diagram B
counterK = padBorder $ hsep 1 $ rotations [8,0,0,6,5] $ scales [1,phi,phi,1+phi,1+2*phi] $ 
           fmap dashJVGraph [g,cg,fcg, cfcg, composeG cfcg]
        where g = sunPlus3Dart'
              cg = composeK g
              fcg = force cg
              cfcg = composeK fcg


{- * Forced Tgraph figures
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

{- *
Emplace with choices
-}

-- |four choices for composing fool
foolChoices :: Diagram B
foolChoices = padBorder $ vsep 1 
              [hsep 1 $ fmap ((redFool <>) . dashJGraph) choices
              ,hsep 1 $ rotations [1,1,9,4] $ scale phi $ fmap (dashJGraph . composeG) choices
              ] where choices = makeChoices fool
                      redFool = dashJGraph fool # lc red
                         
-- |showing 4 emplacement choices for foolD 
emplaceFoolDChoices :: Diagram B
emplaceFoolDChoices = padBorder $ hsep 1 $
        fmap (addFoolD . lw ultraThin . drawPatch . dropVertices) vpChoices where
        (vpFoolD:vpChoices) = alignments [(1,6),(1,6),(1,6),(1,6),(28,6)] --(29,6)] 
                                         (fmap makeVPatch (foolD:emplaceChoices foolD))
        addFoolD fig = (lc red . lw thin . dashJPatch . dropVertices) vpFoolD <> fig
--  WARNING: relies on numbering which can change with changes to forcing
--  Vertex 1 is not present in the final choice (hence 28)

{- *
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
     *** Exception: checkedTgraph: crossing boundaries found at [3]
     in
     Tgraph {vertices = [4,6,3,1,5], faces = [LD (4,6,3),LK (1,5,3)]}
-}
badlyBrokenDart :: Tgraph
badlyBrokenDart = removeFaces deleted brokenDart where
  deleted = RK(6,28,54):filter (isAtV 63) (faces brokenDart)
 
-- |brokenDartFig shows the faces removed from dartD4 to make brokenDart and badlyBrokenDart
brokenDartFig :: Diagram B
brokenDartFig = padBorder $ lw thin $ hsep 1 $ fmap dashJVGraph [dartD4, brokenDart, badlyBrokenDart]

-- |figure showing force badlyBrokenDart (which is the same as forceD4)
checkBrokenDartFig  :: Diagram B
checkBrokenDartFig = drawGraph $ force badlyBrokenDart


-- |2 adjacent kites decomposed then the top half kite components removed (3 of them)
brokenKites::Tgraph
brokenKites = removeFaces deleted kPlusKD where
                      kPlusKD = decomposeG kitePlusKite
                      deleted = filter (hasVIn [6,14]) (faces kPlusKD)
{-|
Figure showing a decomposed pair of adjacent kites, followed by
brokenKites (3 faces removed from decomposed pair of kites), followed by
compositon of brokenKites (a kite) which is also composition of forced brokenKites, followed by
the emplacement of brokenKites (which is the same as force brokenKites).
The figure illustrates that brokenKites is included in emplace brokenKites
even though the missing part is not repaired.
-}
brokenKitesDFig :: Diagram B
brokenKitesDFig = padBorder $ hsep 1 $ fmap dashJVPatch $ alignAll (1,3) $ scales [1,1,phi] $ fmap makeVPatch 
                  [decomposeG kitePlusKite, brokenKites, composeG brokenKites, emplace brokenKites]

-- |diagram illustrating touching vertex situation and forced result.
-- The faces shown in lime are removed from a twice decomposed sun.
-- These are reconstructed by force (with other additional faces). The touching vertex restriction blocks many
-- of the face additions initially. 
touchingTestFig::  Diagram B
touchingTestFig = 
  padBorder $ lw thin $ hsep 1 $
    [ dashJVPatch vpLeft <> (dashJPatch (dropVertices vpGone) # lc lime)
    , dashJVPatch $  alignXaxis (8,3) $ makeVPatch $ force touchGraph
    ] where    
      touchGraph = graphFromVP vpLeft
      vpLeft = removeFacesVP deleted vp
      vpGone = selectFacesVP deleted vp
      vp = makeVPatch $ sunD2
      sunD2 = sunDs!!2
      deleted = filter ((==1).originV) (faces sunD2) ++
                [LD(29,41,31),RK(31,79,29),LK(10,29,79),RK(10,79,75)]

-- |A function to remove halftile faces that do not have their matching halftile
-- This weill raise an error if the result is not a valid Tgraph.
removeIncompleteTiles:: Tgraph -> Tgraph
removeIncompleteTiles g = removeFaces halfTiles g
       where bdry = makeBoundary g
             halfTiles = fmap snd $ incompleteHalves bdry $ bDedges bdry

-- |figure showing the result of applying removeIncompleteTiles to a 3 times decomposed sun.
removeIncompletesFig::Diagram B
removeIncompletesFig = padBorder $ dashJGraph $ removeIncompleteTiles  $ sunDs !! 3

{- *
Incorrect Tgraphs (and other problem Tgraphs)
-}
  
-- |faces removed from foolD to illustrate crossing boundary and non tile-connected VPatches
crossingBdryFig :: Diagram B
crossingBdryFig = padBorder $ hsep 1 [d1,d2]
       where d1 = dashJVPatch $ removeFacesGtoVP [RK(3,11,13), LK(3,13,15), RK(3,15,4)] foolD
             d2 = dashJVPatch $ removeFacesGtoVP [RK(5,11,2), LD(6,13,11), RD(6,15,13), LD(6,17,15)] foolD

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
   
-- |decomposeG mistake and the point at which forcing fails  with  RK (6,26,1)              
forcingDmistakeFig :: Diagram B
forcingDmistakeFig = padBorder $ hsep 1 [dashJVGraph (decomposeG mistake), dashJVGraph part] where
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
cdMistake1Fig = padBorder $ hsep 1 $ fmap dashJVPatch $ scales [phi,1,1,phi] $ alignAll (1,2) $ fmap makeVPatch
               [ mistake1 , mistake1D, force mistake1D, composeG mistake1D]
               where mistake1D = decomposeG mistake1
{- Alternative using subVPatch is messier
cdMistake1Fig :: Diagram B
cdMistake1Fig = padBorder $ hsep 1 $ fmap draw
               [ mistake1 , mistake1D, fmistake1D, composeG mistake1D]
               where mistake1D = decomposeG mistake1
                     fmistake1D = force mistake1D
                     vpMap = createVPoints $ faces fmistake1D
                     draw g = relevantVPatchWith dashJPiece $ rotate (ttangle 1) $ subVPatch (faces g) vpMap
-}



{- *
Figures fof 7 vertex types
-}
   
{-| vertexTypesFig is 7 vertex types single diagram as a row -}
vertexTypesFig = padBorder $ hsep 1 lTypeFigs
 where
 lTypeFigs = zipWith labelD ["sun","star","jack","queen","king","ace","deuce"] vTypeFigs
 vTypeFigs = zipWith drawVertex 
               [sunGraph, starGraph, jackGraph, queenGraph, kingGraph, aceGraph,  deuceGraph]
               [(1,2),    (1,2),     (1,2),     (1,2),      (1,2),     (3,6),     (2,6)] -- alignments
 drawVertex g alm = lw thin $ showOrigin $ dashJPatch $ dropVertices $ alignXaxis alm $ makeVPatch g

-- |add a given label below and to right of centre of a diagram
labelD :: String -> Diagram B -> Diagram B
labelD l d = baselineText l # fontSize (local 0.5) # fc blue <> d # moveTo (p2(0,2.2))

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
                fmap drawForce [sunGraph,starGraph,jackGraph,queenGraph,kingGraph,aceGraph,deuceGraph]


{-| forceVsFig shows force of the 7 vertex types in a row as single diagram -}
forceVsFig :: Diagram B
forceVsFig = padBorder $ hsep 1 forceVFigures

{-| relatedVTypeFig lays out figures from forceVFigures plus a kite as single diagram with 3 columns -}
relatedVTypeFig = padBorder $ lw thin $
 atPoints [p2(0,15),p2(0,10),   p2(8,15),p2(9,10),p2(9,1),  p2(18,15),p2(18,10),p2(20,1) ]
          [sunF,    starF,      aceF,    jackF,    kingF,     kite,     deuceF,   queenF]
 where kite = drawGraph kiteGraph
       sunF = forceVFigures!!0
       starF = forceVFigures!!1
       jackF = forceVFigures!!2
       queenF = forceVFigures!!3
       kingF = forceVFigures!!4
       aceF = forceVFigures!!5
       deuceF = forceVFigures!!6

{- *
Other miscelaneous Tgraphs and Diagrams
-}


-- |graphs of the boundary faces only of forced graphs (dartDs!!4 and dartDs!!5)
boundaryFDart4, boundaryFDart5 :: Tgraph
boundaryFDart4 = checkedTgraph $ boundaryFaces $ makeBoundary $ force (dartD4)
boundaryFDart5 = checkedTgraph $ boundaryFaces $ makeBoundary $ force (dartDs!!5)

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
    [ dashJGraph g
    , drawGraph $ recoverGraph $ boundaryState $ stepForce g 2000
    ] where g = boundaryGapFDart5

-- |showing intermediate state of filling the inlet and closing the gap of boundaryGapFDart4
-- using stepForce 600 (finished at 820)
gapProgress4 :: Diagram B
gapProgress4 = lw ultraThin $ hsep 1 $ center <$> rotations [5,5]
    [ dashJGraph g
    , drawGraph $ recoverGraph $ boundaryState $ stepForce g 820 --600
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
              drts  = fmap center $ phiScaling phi $ reverse $ take 4 $ fmap drawGraphSmart dartDs
              dots = center $ hsep 1 $ replicate 4 (circle 0.5 # fc gray # lw none)
              pointsR1 = map p2 [ (0, 70), (52, 70), (100, 70), (150, 70), (190, 70)]
              pointsR2 = map p2 [ (0, 40), (42, 40), (95, 40), (140, 40), (186, 40)]
              pointsR3 = map p2 [ (0, 0),  (42, 0),  (95, 0),  (140, 0),  (186, 0) ]    
bigPic = 
    bigPic0  # composeArc "a3" "a2"
             # composeArc "a2" "a1"
             # composeArc "a1" "a0"
             # composeArc "a4" "a3"
             # composeArc "b4" "b3"
             # composeArc "b3" "b2"
             # composeArc "b2" "b1"
             # composeArc "b1" "b0"
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

-- |add a compose arrow arc (green) from named parts of 2 diagrams
composeArc :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B -> Diagram B
composeArc a b = connectPerim' arrowStyleC a b (1/10 @@ turn) (4/10 @@ turn) where
  arrowStyleC = with & arrowShaft .~ arc xDir (-1/10 @@ turn) & headLength .~ verySmall 
                     & headStyle %~ fc green & shaftStyle %~ lc green 
                     & headGap .~ large & tailGap .~ large
    

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
    xDGraphs = decompositionsG sunPlus3Dart'
    xDs  = rotations [9,9,8] $  phiScaling phi $ reverse $
           drawGraph dartGraph : (drawGraph sunPlus3Dart' # lc red): 
           take 2  (drop 1 $ fmap drawGraphSmart xDGraphs)
    dots = center $ hsep 1 $ replicate 4 (circle 0.5 # fc gray # lw none)
    pointsRa = map p2 [ (0, 80), (42, 80), (95, 80), (150, 80), (200, 80)]
    pointsRb = map p2 [ (0, 40), (42, 40), (95, 40), (150, 40)]
    pointsRc = map p2 [ (0, 0),  (42, 0),  (95, 0),  (150, 0), (200, 0)]

{-| curioPic is a diagram illustrating where compose loses information not recovered by force
  with sunPlus3Dart' third item in bottom row, (curioPic0 is diagram without arrows)
-}
curioPic :: Diagram B
curioPic = 
  curioPic0  # composeArc "a3" "a2"
             # composeArc "a2" "a1"
             # composeArc "a1" "a0"
             # composeArc "a4" "a3"
             # composeArc "b3" "b2"
             # composeArc "b2" "b1"
             # composeArc "b4" "b3"
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
             # composeArc "c4" "c3"
             # composeArc "c3" "c2"
             # composeArc "c2" "c1"
             # composeArc "c1" "c0"

-- |figure showing ordering of a decomposed kite (bottom), a test graph with an extra LK(3,6,8),
-- and forced figure at the top and composition of all 3 = kite on the right
graphOrder1 = padBorder $ hsep 2 [center $ vsep 1 [ft,t,dcft], cft] where
              [cft,dcft,ft,t] = fmap dashJVPatch $ scales [phi] $ alignAll (1,2) $ fmap makeVPatch 
                                [cftest, dcftest, ftest, test]
              dcftest = decomposeG cftest
              cftest = composeG ftest
              ftest = force test
              test = makeTgraph [RK (4,7,2),LK (4,5,7),RD (1,7,5),LK (3,2,7)
                                   ,RK (3,7,6),LD (1,6,7), LK(3,6,8)
                                   ]


{- *
Testing (functions and figures and experiments)
-}
          
{-
Testing newest force with 10 rules   
Fixed BUG filling in boundary of a forced graph  
testForce4 ok but 
testForce5 failed (introducing touching vertices) with previous version of thirdVertexLoc
because of accumulated discrepancies in vertex position calculations.

Now works with signum introduced in thirdVertexLoc,
dramatically improving accuracy of position calculation
-}

-- |diagrams of forced graphs for boundaryGapFDart4 and boundaryGapFDart5
testForce4, testForce5 :: Diagram B
testForce4 = padBorder $ lw ultraThin $ dashJVGraph $ force boundaryGapFDart4
testForce5 = padBorder $ lw ultraThin $ dashJVGraph $ force boundaryGapFDart5        




  
{-| testViewBoundary is a testing tool to inspect the boundary vertex locations of some (intermediate) Boundary
-- (used in conjunction with stepForce to get an intermediate Boundary)
-- The boundary edges of a Boundary are shown in lime - using the Boundary positions of vertices.
-- This is overlaid on the full graph drawn with vertex labels.
-}
testViewBoundary :: Boundary -> Diagram B
testViewBoundary bd =  lc lime (drawEdges vpMap bdE) <> dashJVGraph g where 
    g = recoverGraph bd
    vpMap = bvLocMap bd
    bdE = bDedges bd

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
checkGraphFromVP = padBorder $ (drawGraph . graphFromVP . makeVPatch) dartD4

-- |figure testing selectFacesGtoVP by removing all kites
dartsOnlyFig :: Diagram B
dartsOnlyFig = padBorder $ lw thin $ drawPatch $ dropVertices $ selectFacesGtoVP darts g where
    g = force $ sunDs !! 5
    darts = filter isDart $ faces g

{- *
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
    scaledCases = alignAll (1,3) $ fmap (scale (phi^4) . makeVPatch) cases
    forcedD4Cases = alignAll (1,3) $ fmap (makeVPatch . force . decomp4) cases
    decomp4 g = decompositionsG g !! 4
    redEmbed g1 g2 = lc red (lw medium $ dashJPatch $ dropVertices g1) <> lw ultraThin (drawPatch $ dropVertices g2)

-- | two kites (force decomp twice) figure
kkEmpsFig:: Diagram B
kkEmpsFig = padBorder $ lw ultraThin $ vsep 1 $ rotations [0,9,9] $ 
            fmap drawGraph  [kk, kkD, kkD2] where
              kk = kitePlusKite
              kkD = force $ decomposeG kk
              kkD2 = force $ decomposeG kkD
             
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
rocketCone1Fig = padBorder $ lw thin $ dashJVGraph rocketCone1

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



{- *
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
     thelist = fmap dashJVPatch $ rotations [0,7] $ fmap makeVPatch 
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

{- |
Diagram showing a calculation of some of the kings empire.
The top left graph shows the intersection faces of the the other 6 graphs (emphasised) with the second graph as background.
The latter 6 graphs show all the possible ways of extending a forced kingGraph (forced kinGraph shown red)
round its boundaru.
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
    fcs = foldl Data.List.intersect (faces g1) $
           fmap g1Intersect [sub2,sub3,sub4,sub5,sub6]


{-
empire g = 
    fg = force g
    commonDE = lowestJoin (faces fg)
-}

test5 = padBorder $ drawVGraph $ tgraph $ forceSub $ pushFaces $ unionTwoSub $ unionTwoSub 
           $ addHalfDartSub (49,59) $ addHalfDartSub (16,23)
           $ addHalfDartSub (20,38) $ newSubTgraph $ force $ kingGraph

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
    fkVP = makeVPatch fk
    backVP = makeVPatch fdk
    embed de = drawPatchWith (fillDK yellow yellow) a <> drawPatch b 
                where [a,b] = fmap dropVertices $ alignments [(1,7), de] [fkVP, backVP]
    cases = fmap center $ rotations [8,2,1,9] $ fmap embed [(43,205),(33,188),(9,107),(5,91),(12,119)]

-- | Diagram to check vertex numbering for a forced kingGraph, a forcedDecomp forced kingGraph, and a
-- twice forceDecomp forced kingGraph
kingEmpireCheck = padBorder $ lw ultraThin $ vsep 1 $ fmap drawVGraph [fk, fdfk, fdfdfk]where
    fk = force kingGraph
    fdfk = forcedDecomp fk
    fdfdfk = forcedDecomp fdfk


{- *
Testing ReportFail functions
-}

-- | testing ReportFail  - try arguments 0..5  only 4 succeeds
reportFailTest1 a = getResult $ onFail "reportFailTest1:\n" $ do
  b <- maybeExample a `nothingFail` ("maybeExample: produced Nothing when applied to "++show a ++"\n")
  c <- onFail "first call:\n"  $ eitherExample (b-1)
  d <- onFail "second call:\n" $ eitherExample (c-1)
  e <- onFail "third call:\n"  $ eitherExample (d-1)
  return (a,b,c,d,e)

-- | testing ReportFail  - try arguments 0..3   only 2 succeeds
reportFailTest2 a = getResult $ onFail "reportFailTest2:\n" $ do
  b <- eitherMaybeExample a
  c <- b `nothingFail` "eitherMaybeExample produced Nothing\n"
  d <- onFail "trying eitherExample:\n" $ eitherExample (c-1)
  return (a,b,c,d)

-- | for testing in reportFailTest1 and reportFailTest2
eitherExample :: Int -> ReportFail Int
eitherExample a = if a==0 then Left "eitherExample: arg is zero\n" else Right a 

-- | for testing in reportFailTest1
maybeExample :: Int -> Maybe Int
maybeExample a = if a<5 then Just a else Nothing

-- | for testing in reportFailTest1 and reportFailTest2
eitherMaybeExample :: Int -> ReportFail (Maybe Int)
eitherMaybeExample a | a<1 = Right Nothing
                     | a<3 = Right (Just a) 
                     | otherwise = Left "eitherMaybeExample: arg >=3\n"



