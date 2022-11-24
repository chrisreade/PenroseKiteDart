{-|
Module      : Tgraphs
Description : Collects and exports the various Tgraph modules 
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This is the main module for Tgraph operations which collects and exports the other Tgraph modules. 
It exports makeTgraph for constructing checked Tgraphs and excludes data constructor Tgraph.
The module includes several functions for producing overlaid diagrams for graphs and
a definition of emplace and other experimental combinations.
-}
module Tgraphs ( module Tgraphs
               , module Tgraph.Prelude -- excludes data constructor Tgraph
               , module Tgraph.Decompose
               , module Tgraph.Compose
               , module Tgraph.Force
               , module Tgraph.Convert
               , module Tgraph.Relabelling
               ) where

import Tgraph.Prelude hiding (Tgraph(Tgraph)) -- hides Tgraph as type and data constructor
import Tgraph.Prelude (Tgraph) -- re-includes Tgraph as type constructor only
import Tgraph.Decompose
import Tgraph.Compose
import Tgraph.Force
import Tgraph.Convert
import Tgraph.Relabelling

import Diagrams.Prelude
import ChosenBackend (B)
import TileLib

import Data.List (intersect)      

{- *
Making valid Tgraphs (with a check for no touching vertices).
-}


{-|
makeTgraph performs a no touching vertex check as well as using checkTgraphProps for other required properties.
It produces an error if either check fails.
Note that the other Tgraph properties are checked first, to ensure that calculation of 
vertex locations can be done for a touching vertex check.
-}
makeTgraph :: [TileFace] -> Tgraph
makeTgraph fcs = getResult $ onFail "makeTgraph: (failed):\n" $ touchCheckProps fcs

{-|
touchCheckProps performs the same checks for Tgraph properties as checkTgraphProps but in addition
it also checks that there are no touching vertices (distinct labels for the same vertex)
using Tgraph.Convert.touchingVertices (which calculates vertex locations).
It produces Left ... if either check fails and Right g otherwise where g is the Tgraph.
Note that the other Tgraph properties are checked first, to ensure that calculation of 
vertex locations can be done.
-}
touchCheckProps :: [TileFace] -> ReportFail Tgraph
touchCheckProps fcs =
 do g <- checkTgraphProps fcs
    let touchVs = touchingVertices (faces g)
    if null touchVs 
    then Right g 
    else Left ("Found touching vertices: " 
               ++ show touchVs
               ++ "\n(To fix, use: correctTouchingVs)\n"
              )

{- *
Advanced drawing tools for Tgraphs
-}

-- |same as drawGraph except adding dashed lines on boundary join edges. 
drawSmartGraph :: Tgraph -> Diagram B
drawSmartGraph g = drawSmartSub g $ makeVPinned g

-- |same as drawVGraph except adding dashed lines on boundary join edges.
drawSmartVGraph :: Tgraph -> Diagram B
drawSmartVGraph g = drawSmartVSub g $ makeVPinned g

-- |drawSmartSub g vp converts g to a diagram (without vertex labels).
-- It requires vp to contain a suitable vertex location map for drawing g.
-- This can be used instead of drawSmartGraph when such a map is already available.
drawSmartSub:: Tgraph -> VPinned -> Diagram B
drawSmartSub g vp = (drawPatchWith dashJ $ subPatch (boundaryJoinFaces g) vp) 
                        <> drawPatch (subPatch (faces g) vp)

-- |drawSmartVSub g vp converts g to a diagram with vertex labels.
-- It requires vp to contain a suitable vertex location map for drawing g.
-- This can be used instead of drawSmartVGraph when a suitable VPinned is already available.
drawSmartVSub:: Tgraph -> VPinned -> Diagram B
drawSmartVSub g vp = (drawPatchWith dashJ $ subPatch (boundaryJoinFaces g) vp) 
                        <> drawVPinned (subVPinned (faces g) vp)

-- |select the halftile faces of a Tgraph with a join edge on the boundary.
-- Useful for drawing join edges only on the boundary.
boundaryJoinFaces :: Tgraph -> [TileFace]
boundaryJoinFaces g = fmap snd $ incompleteHalves bdry $ bDedges bdry where
    bdry = makeBoundary g

-- |applies partCompose to a Tgraph g, then draws the composed graph with the remainder faces (in lime).
-- (Relies on the vertices of the composition and remainder being subsets of the vertices of g.)
drawPCompose ::  Tgraph -> Diagram B
drawPCompose g = (drawPatch $ subPatch (faces g') vp)
                 <> (lw thin $ lc lime $ dashJPatch $ subPatch fcs vp)
  where (fcs,g') = partCompose g
        vp = makeVPinned g

-- |drawForce g is a diagram showing the argument g in red overlayed on force g
-- It adds dashed join edges on the boundary of g
drawForce:: Tgraph -> Diagram B
drawForce g = (dg # lc red) <> dfg where
    fg = force g
    vp = makeVPinned fg
    dfg = drawPatch $ dropLabels vp
    dg = drawSmartSub g vp

{- |
drawWithMax g - draws g and overlays the maximal forced composition of g in red
-}
drawWithMax :: Tgraph -> Diagram B
drawWithMax g =  (dmax # lc red # lw thin) <> dg where
    vp = makeVPinned g
    dg = drawPatch $ dropLabels vp
    maxg = maxCompForced g
    dmax = drawPatch $ subPatch (faces maxg) vp

-- |displaying the boundary of a Tgraph in lime (overlaid on the Tgraph drawn with labels)
drawGBoundary :: Tgraph -> Diagram B
drawGBoundary g =  (drawEdges (vLocs vp) bd # lc lime) <> drawVPinned vp where
    vp  = makeVPinned g
    bd = boundaryDedges g

-- |drawCommonFaces (g1,e1) (g2,e2) uses commonFaces to find the common faces of g1 and g2
-- and emphasizes the common faces on the background g1
drawCommonFaces:: (Tgraph,Dedge) -> (Tgraph,Dedge) -> Diagram B
drawCommonFaces (g1,e1) (g2,e2) = emphasizeFaces (commonFaces (g1,e1) (g2,e2)) g1

-- |emphasizeFaces fcs g emphasizes the given faces (that are in g) overlaid on the background g.
emphasizeFaces:: [TileFace] -> Tgraph -> Diagram B
emphasizeFaces fcs g =  (drawPatch emphPatch # lw thin) <> (drawPatch gPatch # lw ultraThin) where
    vp = makeVPinned g
    gPatch = dropLabels vp
    emphPatch = subPatch (fcs `intersect` faces g) vp


{- *
Combining force, composeG, decomposeG
-}

-- |compForced does a force then composeG. 
-- (the connectedNoCross check may be redundant on the composed graph because the argument was forced.)
compForced:: Tgraph -> Tgraph
compForced = composeG . force

-- |force after a decomposition
forcedDecomp:: Tgraph -> Tgraph
forcedDecomp = force . decomposeG
        
-- |allCompForced g produces a list of all forced compositions starting from g up to but excluding the empty graph
allCompForced:: Tgraph -> [Tgraph]
allCompForced = takeWhile (not . nullGraph) . iterate compForced

-- | produces an infinite list of forced decompositions
allForcedDecomps:: Tgraph -> [Tgraph]
allForcedDecomps = iterate forcedDecomp

-- |maxCompForced produces a maximally composed forced graph.
maxCompForced:: Tgraph -> Tgraph
maxCompForced = force . last . allCompForced


{- *
Emplacements
-}

-- |emplace does maximal composing with force and composeG, 
-- then applies decomposeG and force repeatedly back to the starting level.
-- It produces the emplacement of influence of the argument graph.   
emplace:: Tgraph -> Tgraph
emplace g | nullGraph g' = fg
          | otherwise = (forcedDecomp . emplace) g'
  where fg = force g
        g' = composeG fg 
            
{-
-- |emplacements is best supplied with a maximally composed or near maximally composed graph
-- It produces an infinite list of emplacements of the starting graph and its decompositions.
emplacements :: Tgraph -> [Tgraph]
emplacements = iterate forcedDecomp . emplace -- was .force
-}

-- |a version of emplace using makeChoices at the top level.
-- after makeChoices we use emplace to attempt further compositions but with no further choices
emplaceChoices:: Tgraph -> [Tgraph]
emplaceChoices g | nullGraph g' = emplace <$> makeChoices fg
                 | otherwise = forcedDecomp <$> emplaceChoices g'
  where fg = force g
        g' = composeG fg 
                                 
{-| makeChoices should only be used on a forced Tgraph.
It is a temporary tool which does not attempt to analyse choices for correctness.
It can thus create some choices which will be incorrect.
The unknowns returned from classifyDartWings can become largeKiteCentres or largeDartBases.
This produces 2^n choices where n is the number of unknowns.
(There will not be any dart wing tips with valency 2 in a forced graph).
-}
makeChoices :: Tgraph -> [Tgraph]
makeChoices g = choices unks [g] where
    unks = unknowns (classifyDartWings g) -- g not forced may allow solitary wing tips which will fail
    choices [] gs = gs
    choices (v:more) gs = choices more (fmap (forceLKC v) gs ++ fmap (forceLDB v) gs)







 