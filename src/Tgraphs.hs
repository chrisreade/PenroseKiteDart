{-# OPTIONS_HADDOCK ignore-exports #-}
{-|
Module      : Tgraphs
Description : Collects and exports the various Tgraph modules plus some experimental ones
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
import qualified Tgraph.Prelude as Local (Tgraph(Tgraph)) -- Allows Tgraph data constructor to be used in this module
import Tgraph.Decompose
import Tgraph.Compose
import Tgraph.Force
import Tgraph.Convert
import Tgraph.Relabelling

import Diagrams.Prelude hiding (union)
import ChosenBackend (B)
import TileLib

import Data.List (intersect, union, (\\))      

-- * Making valid Tgraphs (with a check for no touching vertices).

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

{-*
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
drawForce g = (dg # lc red # lw thin) <> dfg where
    fg = force g
    vp = makeVPinned fg
    dfg = drawPatch $ dropLabels vp
    dg = drawSmartSub g vp

{-|
drawWithMax g - draws g and overlays the maximal composition of g in red
-}
drawWithMax :: Tgraph -> Diagram B
drawWithMax g =  (dmax # lc red # lw thin) <> dg where
    vp = makeVPinned g
    dg = drawPatch $ dropLabels vp
    maxg = maxComp g
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

{-*
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

-- |maxComp may produce a maximally composed graph, but may raise an error if any intermediate composition 
-- is not a valid Tgraph.
maxComp:: Tgraph -> Tgraph
maxComp = last . allComp

-- |allComp g may produce a list of all compositions starting from g up to but excluding the empty graph,
-- but it may raise an error if any composition is not a valid Tgraph.
allComp:: Tgraph -> [Tgraph]
allComp = takeWhile (not . nullGraph) . iterate composeG

{-*
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

{-*
Boundary Covers and Empires
-}

{-| forcedBoundaryCover g - profuces a list of all possible ways of extending (force g)
so that the boundary of force g is entirely internal edges.
The  common faces of the resulting list of graphs constitute the empire (level 1) of g
-}
forcedBoundaryCover:: Tgraph -> [Tgraph]
forcedBoundaryCover g = fmap recoverGraph $ boundaryCover $ makeBoundary $ force g

{-| boundaryCover bd - profuces a list of all possible ways of extending the Boundary bd
so that the boundary of bd is entirely internal edges.
-}
boundaryCover:: Boundary -> [Boundary]
boundaryCover bd = continue [] [(bd, bDedges bd)] where
--continue::([Boundary], [(Boundary,[Dedge])]) -> [Boundary]
  continue complete [] = complete
  continue complete ((open,[]):opens) = continue (open:complete) opens
  continue complete ((open,de:bdes):opens) = 
      continue complete (fmap (remainder bdes) (tryDartAndKite de open) ++ opens)  
--remainder:: [Dedge] -> Boundary -> (Boundary, [Dedge])
  remainder bds b = (b, bDedges b `intersect` bds)
--tryDartAndKite:: Dedge -> Boundary -> [Boundary]
  tryDartAndKite de bd = ignoreFails 
    [ tryAddHalfDartBoundary de bd >>= tryForceBoundary defaultAllUGen
    , tryAddHalfKiteBoundary de bd >>= tryForceBoundary defaultAllUGen
    ]


-- | test function to draw a column of the list of graphs resulting from forcedBoundaryCover g
drawFBCover:: Tgraph -> Diagram B
drawFBCover g = lw ultraThin $ vsep 1 $ 
     fmap drawGraph $ forcedBoundaryCover g

-- | empire1 g - produces a SubTgraph representing the level 1 empire of g
-- The tgraph is an arbitrarily chosen extension of (force g) covering its boundary,
-- and the tracked faces are the common faces of all possible extensions covering the boundary.
empire1:: Tgraph -> SubTgraph
empire1 g = makeSubTgraph g1 [fcs] where
    (g1:others) = forcedBoundaryCover g
    fcs = foldl intersect (faces g1) $ fmap g1Intersect others
    de = lowestJoin (faces g)
    g1Intersect g2 = commonFaces (g1,de) (g2,de)

-- | empire2 g - produces a SubTgraph representing the level 2 empire of g
-- The tgraph is an arbitrarily chosen (double) extension of (force g),
-- and the tracked faces are the common faces of all possible (double) extensions.
empire2:: Tgraph -> SubTgraph
empire2 g = makeSubTgraph g1 [fcs] where
    covers1 = boundaryCover $ makeBoundary $ force g
    covers2 = concatMap boundaryCover covers1
    (g1:others) = fmap recoverGraph covers2
    fcs = foldl intersect (faces g1) $ fmap g1Intersect others
    de = lowestJoin (faces g)
    g1Intersect g2 = commonFaces (g1,de) (g2,de)

-- | drawEmpire1 g - produces a diagram emphasising the common faces of all boundary covers of force g.
-- This is drawn over one of the possible boundary covers.
drawEmpire1:: Tgraph -> Diagram B
drawEmpire1 g = emphasizeFaces (head $ tracked sub) (tgraph sub) where
    sub = empire1 g

-- | drawEmpire2 g - produces a diagram emphasising the common faces of a double boundary cover of force g.
-- This is drawn over one of the possible double boundary covers.
drawEmpire2:: Tgraph -> Diagram B
drawEmpire2 g = emphasizeFaces (head $ tracked sub) (tgraph sub) where
    sub = empire2 g

{-*
SubTgraphs
-}
{-|
 SubTgraph - introduced to allow tracking of subsets of faces
 in both force and decompose oerations.
 A SubTgraph has a main Tgraph (fullgraph) and a list of subsets of faces (tracked).
 The list allows for tracking different subsets of faces at the same time.
-}
data SubTgraph = SubTgraph{ tgraph:: Tgraph, tracked::[[TileFace]]} deriving Show

-- |newSubTgraph g creates a SubTgraph from a Tgraph g with an empty tracked list
newSubTgraph :: Tgraph -> SubTgraph
newSubTgraph g = makeSubTgraph g []

-- |makeSubTgraph g trackedlist creates a SubTgraph from a Tgraph g
-- from trackedlist where each list in trackedlist is a subset of the faces of g.
-- Any faces not in g are ignored.
makeSubTgraph :: Tgraph -> [[TileFace]] -> SubTgraph
makeSubTgraph g trackedlist = SubTgraph{ tgraph = g, tracked = fmap (`intersect` faces g) trackedlist}

-- |pushFaces sub - pushes the maingraph tilefaces onto the stack of tracked subsets of sub
pushFaces:: SubTgraph -> SubTgraph
pushFaces sub = makeSubTgraph g newTracked where
    g = tgraph sub
    newTracked = faces g:tracked sub

-- |unionTwoSub sub - combines the top two lists of tracked tilefaces replacing them with the list union.
unionTwoSub:: SubTgraph -> SubTgraph
unionTwoSub sub = makeSubTgraph g newTracked where
    g = tgraph sub
    (a:b:more) = tracked sub
    newTracked = case tracked sub of
                   (a:b:more) -> a `union` b:more
                   _ -> error $ "unionTwoSub: Two tracked lists of faces not found: " ++ show sub ++"\n"


{-*
Forcing and Decomposing SubTgraphs
-}
-- |force applied to a SubTgraph - has no effect on tracked subsets but applies force to the full Tgraph.
forceSub :: SubTgraph -> SubTgraph
forceSub sub = makeSubTgraph (force $ tgraph sub) (tracked sub)

-- |aaddHalfDartSub sub e - add a half dart to the tgraph of sub on the given edge e,
-- and push the new singleton face list onto the tracked list.
addHalfDartSub:: Dedge -> SubTgraph -> SubTgraph
addHalfDartSub e sub =
    makeSubTgraph g' (fcs:tracked sub) where
    g = tgraph sub
    g' = addHalfDart e g
    fcs = faces g' \\ faces g

-- |addHalfKiteSub sub e - add a half kite to the tgraph of sub on the given edge e,
-- and push the new singleton face list onto the tracked list.
addHalfKiteSub:: Dedge -> SubTgraph -> SubTgraph
addHalfKiteSub e sub =
    makeSubTgraph g' (fcs:tracked sub) where
    g = tgraph sub
    g' = addHalfKite e g
    fcs = faces g' \\ faces g

-- |decompose a SubTgraph - applies decomposition to all tracked subsets as well as the full Tgraph.
-- Tracked subsets get the same numbering of new vertices as the main Tgraph. 
decomposeSub :: SubTgraph -> SubTgraph
decomposeSub sub = makeSubTgraph g' tlist where
   g = tgraph sub
   g' = Local.Tgraph{ maxV = newMax
                    , faces = newFaces
                    }
   (newMax , newVFor) = maxAndPhiVMap g
   newFaces = concatMap (decompFace newVFor) (faces g)
   tlist = fmap (concatMap (decompFace newVFor)) (tracked sub)

{-*  Drawing with SubTgraphs
-}                                          
{-|
    To draw a SubTgraph without vertex labels, we use a list of functions each turning a patch into a diagram.
    The first function is applied to a patch for untracked faces
    Subsequent functions are applied to patches for the respective tracked subsets.
    Each diagram is atop earlier ones, so the diagram for the untracked patch is at the bottom.
    The patches will all have been made from the same VPinned vertex location map so will be aligned/scaled
    appropriately.
-}
drawSubTgraph:: [Patch -> Diagram B] -> SubTgraph -> Diagram B
drawSubTgraph drawList sub = drawAll drawList (pUntracked:pTrackedList) where
          vp = makeVPinned (tgraph sub)
          fcsFull = vpFaces vp    
          pTrackedList = fmap (\fcs -> subPatch fcs vp) (tracked sub)
          pUntracked = subPatch (fcsFull \\ concat (tracked sub)) vp
          drawAll fs ps = mconcat $ reverse $ zipWith ($) fs ps

-- |drawing non tracked faces only
drawWithoutTracked:: SubTgraph -> Diagram B
drawWithoutTracked sub = drawSubTgraph [drawPatch] sub

{-|
    To draw a SubTgraph with possible vertex labels, we use a list of functions each turning a VPinned into a diagram.
    The first function is applied to a VPinned for untracked faces.
    Subsequent functions are applied to VPinned for respective tracked subsets.
    Each diagram is atop earlier ones, so the diagram for the untracked VPinned is at the bottom.
    The VPinned all use the same vertex location map so will be aligned/scaled
    appropriately.
    The angle argument is used to rotate the common vertex location map before drawing
    (to ensure labels are not rotated).
-}
drawSubTgraphV:: [VPinned -> Diagram B] -> Angle Double -> SubTgraph -> Diagram B
drawSubTgraphV drawList a sub = drawAll drawList (vpUntracked:vpTrackedList) where
          vp = rotate a $ makeVPinned (tgraph sub)
          fcsFull = vpFaces vp    
          vpTrackedList = fmap (\fcs -> subVPinned fcs vp) (tracked sub)
          vpUntracked = subVPinned (fcsFull \\ concat (tracked sub)) vp
          drawAll fs vps = mconcat $ reverse $ zipWith ($) fs vps




 