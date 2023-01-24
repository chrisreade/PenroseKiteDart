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
               , module Tgraph.Prelude -- export excludes data constructor Tgraph
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

import Data.List (intersect, union, (\\), find, foldl',nub)      
import qualified Data.Set as Set  (Set,fromList,member,null,intersection,deleteFindMin,map)-- used for boundary covers
import qualified Data.IntMap.Strict as VMap (delete, fromList, findMin, null, lookup, (!)) -- used for boundary loops, boundaryLoops

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
 do g <- checkTgraphProps fcs -- must be checked first
    let touchVs = touchingVertices (faces g)
    if null touchVs 
    then Right g 
    else Left ("Found touching vertices: " 
               ++ show touchVs
               ++ "\n(To fix, use: correctTouchingVs)\n"
              )

{-| correctTouchingVs fcs finds touching vertices by calculating locations for vertices in the faces fcs,
    then renumbers to remove touching vertices (renumbers higher to lower numbers),
    then checks for Tgraph properties of the resulting faces to produce a Tgraph.
    NB fcs needs to be tile-connected before the renumbering and
    the renumbering need not be 1-1 (hence Relabelling is not used)      
-}
correctTouchingVs ::  [TileFace] -> ReportFail Tgraph
correctTouchingVs fcs = 
    onFail ("correctTouchingVs:\n" ++ show touchVs) $ 
    checkTgraphProps $ nub $ renumberFaces touchVs fcs
--    checkTgraphProps $ nub $ fmap (relabelFace $ newRelabelling touchVs) fcs
    where touchVs = touchingVertices fcs -- uses non-generalised version of touchingVertices
        -- renumberFaces allows for a non 1-1 relabelling represented by a list 

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
boundaryJoinFaces g = fmap snd $ incompleteHalves bdry $ boundary bdry where
    bdry = makeBoundaryState g

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
drawWithMax g - draws g and overlays the maximal composition of g in red.
This may raise an error if any of the compositions of g upto the maximal one are invalid Tgraphs
(e.g. not tile connected).
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
    bd = graphBoundary g

-- |drawCommonFaces (g1,e1) (g2,e2) uses commonFaces (g1,e1) (g2,e2) to find the common faces
-- and emphasizes them on the background g1
drawCommonFaces:: (Tgraph,Dedge) -> (Tgraph,Dedge) -> Diagram B
drawCommonFaces (g1,e1) (g2,e2) = emphasizeFaces (commonFaces (g1,e1) (g2,e2)) g1

-- |emphasizeFaces fcs g emphasizes the given faces (that are in g) overlaid on the background g.
emphasizeFaces:: [TileFace] -> Tgraph -> Diagram B
emphasizeFaces fcs g =  (drawPatch emphPatch # lw thin) <> (drawPatch gPatch # lw ultraThin) where
    vp = makeVPinned g
    gPatch = dropLabels vp
    emphPatch = subPatch (fcs `intersect` faces g) vp


 
      

{-*
Combining force, compose, decompose
-}
-- |compForced does a force then compose. 
-- (the connectedNoCross check may be redundant on the composed graph because the argument was forced.)
compForced:: Tgraph -> Tgraph
compForced = compose . force

-- |force after a decomposition
forcedDecomp:: Tgraph -> Tgraph
forcedDecomp = force . decompose
        
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
allComp = takeWhile (not . nullGraph) . iterate compose

{-*
Emplacements
-}
-- |emplace does maximal composing with force and compose, 
-- then applies decompose and force repeatedly back to the starting level.
-- It produces the emplacement of influence of the argument graph.   
emplace:: Tgraph -> Tgraph
emplace g | nullGraph g' = fg
          | otherwise = (forcedDecomp . emplace) g'
  where fg = force g
        g' = compose fg 

-- |a version of emplace using makeChoices at the top level.
-- after makeChoices we use emplace to attempt further compositions but with no further choices
emplaceChoices:: Tgraph -> [Tgraph]
emplaceChoices g | nullGraph g' = emplace <$> makeChoices fg
                 | otherwise = forcedDecomp <$> emplaceChoices g'
  where fg = force g
        g' = compose fg 
                                 
{-| makeChoices should only be used on a forced Tgraph.
It is a temporary tool which does not attempt to analyse choices for correctness.
It can thus create some choices which will be incorrect.
The unknowns returned from classifyDartWings can become largeKiteCentres or largeDartBases.
This produces 2^n choices where n is the number of unknowns.
(There will not be any dart wing tips with valency 2 in a forced graph).
-}
makeChoices :: Tgraph -> [Tgraph]
makeChoices g = choices unks [g] where
    unks = unknowns (getDartWingInfo g) -- g not forced may allow solitary wing tips which will fail
--    unks = unknowns (classifyDartWings g) -- g not forced may allow solitary wing tips which will fail
    choices [] gs = gs
    choices (v:more) gs = choices more (fmap (forceLKC v) gs ++ fmap (forceLDB v) gs)

{-*
Boundary Covers and Empires
-}

{-| forcedBoundaryECover g - produces a list of all boundary covers of force g, each of which
extends force g to cover the entire boundary directed edges in (force g).
(So the boundary of force g is entirely internal edges in each cover).
The covers include all possible ways faces can be added on the boundary that are correct.
The common faces of the covers constitute the empire (level 1) of g.
This will raise an error if the initial force fails with a stuck graph.
-}
forcedBoundaryECover:: Tgraph -> [Tgraph]
forcedBoundaryECover g = fmap recoverGraph $ boundaryECover gforcedBdry where
     gforcedBdry = getResult $ onFail "forcedBoundaryECover:Initial force failed (incorrect graph)\n" $
                             tryForceBoundary $ makeBoundaryState g

{-| forcedBoundaryVCover g - produces a list of all boundary covers of force g as with
forcedBoundaryECover g but covering all boundary vertices rather than just boundary edges.                        
-}
forcedBoundaryVCover:: Tgraph -> [Tgraph]
forcedBoundaryVCover g = fmap recoverGraph $ boundaryVCover gforcedBdry where
     gforcedBdry = getResult $ onFail "forcedBoundaryVCover:Initial force failed (incorrect graph)\n" $
                             tryForceBoundary $ makeBoundaryState g

{-| boundaryECover bd - produces a list of all possible covers of the boundary directed edges in bd.
A cover is an extension (of bd) such that the original boundary directed edges of bd are all internal edges.
Extensions are made by repeatedly adding a face to any edge on the original boundary that is still on the boundary
and forcing, repeating this until the orignal boundary is all internal edges.
The resulting covers account for all possible ways the boundary can be extended that do not produce a (stuck graph) failure.
-}
boundaryECover:: BoundaryState -> [BoundaryState]
boundaryECover bd = covers [(bd, Set.fromList (boundary bd))] where
--covers:: [(BoundaryState, Set.Set Dedge)] -> [BoundaryState]
  covers [] = []
  covers ((open,es):opens) | Set.null es = open:covers opens
  covers ((open,es):opens) | otherwise = 
      covers (fmap (\b -> (b, onBoundary des b)) (bothDartAndKite de open) ++ opens)
      where (de,des) = Set.deleteFindMin es

-- | onBoundary des b - returns those directed edges in des that are boundary directed edges of bd
onBoundary:: Set.Set Dedge -> BoundaryState -> Set.Set Dedge
onBoundary des b = des `Set.intersection` Set.fromList (boundary b)

{-| boundaryVCover bd - similar to boundaryECover, but produces a list of all possible covers of 
    the boundary vertices in bd (rather than just boundary edges).
-}
boundaryVCover:: BoundaryState -> [BoundaryState]
boundaryVCover bd = covers [(bd, startbds)] where
  startbds = Set.fromList $ boundary bd
  startbvs = Set.map fst startbds
--covers:: [(BoundaryState,Set.Set Dedge)] -> [BoundaryState]
  covers [] = []
  covers ((open,es):opens) | Set.null es
    = case find (\(a,_) -> Set.member a startbvs) (boundary open) of
        Nothing -> open:covers opens
        Just de -> covers (fmap (\b -> (b, es))  (bothDartAndKite de open) ++opens)
  covers ((open,es):opens) | otherwise = 
      covers (fmap (\b -> (b, onBoundary des b)) (bothDartAndKite de open) ++opens)  
      where (de,des) = Set.deleteFindMin es
                  
-- | anyDartAndKite de b - returns the list of successful cases after adding a dart (respectively kite)
-- to edge de on boundary state b and forcing. (A list of 0 to 2 new boundary states)
anyDartAndKite:: Dedge -> BoundaryState -> [BoundaryState]
anyDartAndKite de b = ignoreFails $ tryDartAndKite de b

-- | bothDartAndKite de b - returns the list of (2) cases after adding a dart (respectively kite)
-- to edge de on boundary state b and forcing. It will raise an error if either case fails.
bothDartAndKite:: Dedge -> BoundaryState -> [BoundaryState]
bothDartAndKite de b = getResult $ concatFail $ tryDartAndKite de b

-- | tryDartAndKite de b - returns the list of (2) results after adding a dart (respectively kite)
-- to edge de on boundary state b and forcing. Each result is a ReportFail.
tryDartAndKite:: Dedge -> BoundaryState -> [ReportFail BoundaryState]
tryDartAndKite de b = 
    [ tryAddHalfDartBoundary de b >>= tryForceBoundary
    , tryAddHalfKiteBoundary de b >>= tryForceBoundary
    ]

-- | test function to draw a column of the list of graphs resulting from forcedBoundaryVCover g
drawFBCover:: Tgraph -> Diagram B
drawFBCover g = lw ultraThin $ vsep 1 $ 
     fmap drawGraph $ forcedBoundaryVCover g

-- | empire1 g - produces a SubTgraph representing the level 1 empire of g.
-- The tgraph of the result is an arbitrarily chosen boundary vertex cover of force g,
-- and the tracked list of the result has the common faces of all the boundary vertex covers (of force g)
-- at the head, followed by the original faces of g.
empire1:: Tgraph -> SubTgraph
empire1 g = makeSubTgraph g0 [fcs,faces g] where
    (g0:others) = forcedBoundaryVCover g
    fcs = foldl' intersect (faces g0) $ fmap g0Intersect others
    de = lowestJoin (faces g)
    g0Intersect g1 = commonFaces (g0,de) (g1,de)

-- | empire2 g - produces a SubTgraph representing the level 2 empire of g.
-- NB since very large graphs can be generated with boundary vertex covers, we use boundary edge covers only.
-- That is, after finding all boundary edge covers of force g, 
-- boundary edge covers are then found for each boundary edge cover to form a list of doubly-extended
-- boundary edge covers.
-- The tgraph  of the result is an arbitrarily chosen (doubly-extended) boundary edge cover (of force g),
-- and the tracked list of the result has the common faces of all the (doubly-extended) boundary edge covers
-- at the head, followed by the original faces of g.
empire2:: Tgraph -> SubTgraph
empire2 g = makeSubTgraph g0 [fcs, faces g] where
    covers1 = boundaryECover $ getResult $ onFail "empire2:Initial force failed (incorrect graph)\n" 
              $ tryForceBoundary $ makeBoundaryState g
    covers2 = concatMap boundaryECover covers1
    (g0:others) = fmap recoverGraph covers2
    fcs = foldl intersect (faces g0) $ fmap g0Intersect others
    de = lowestJoin (faces g)
    g0Intersect g1 = commonFaces (g0,de) (g1,de)

-- | empire2Plus g - produces a SubTgraph representing an extended level 2 empire of g
-- similar to empire2, but using boundaryVCovers insrtead of boundaryECovers.
-- On a kinGraph this currently takes about 4 hours 20 minutes.
empire2Plus:: Tgraph -> SubTgraph
empire2Plus g = makeSubTgraph g0 [fcs, faces g] where
    covers1 = boundaryVCover $ getResult $ onFail "empire2:Initial force failed (incorrect graph)\n" 
              $ tryForceBoundary $ makeBoundaryState g
    covers2 = concatMap boundaryVCover covers1
    (g0:others) = fmap recoverGraph covers2
    fcs = foldl intersect (faces g0) $ fmap g0Intersect others
    de = lowestJoin (faces g)
    g0Intersect g1 = commonFaces (g0,de) (g1,de)

-- | drawEmpire1 g - produces a diagram emphasising the common faces of all boundary covers of force g.
-- This is drawn over one of the possible boundary covers and the faces of g are shown in red.
drawEmpire1:: Tgraph -> Diagram B
drawEmpire1 g = drawSubTgraph [ lw ultraThin . drawPatch
                              , lw thin . drawPatch
                              , lw thin . lc red . drawPatch
                              ] $ empire1 g

-- | drawEmpire2 g - produces a diagram emphasising the common faces of a doubly-extended boundary cover of force g.
-- This is drawn over one of the possible doubly-extended boundary covers and the faces of g are shown in red.
drawEmpire2:: Tgraph -> Diagram B
drawEmpire2 g = drawSubTgraph [ lw ultraThin . drawPatch
                              , lw thin . drawPatch
                              , lw thin . lc red . drawPatch
                              ] $ empire2 g


{-*
Boundary loops
-}

-- | Returns a list of (looping) vertex trails for the boundary of a Tgraph.
-- There will usually be a single trail, but more than one indicates the presence of boundaries round holes.
-- Each trail starts with the lowest numbered vertex in that trail, and ends with the same vertex.
-- The trails will have disjoint sets of vertices because of the no-crossing-boundaries condition of Tgraphs.
boundaryLoopsG:: Tgraph -> [[Vertex]] 
boundaryLoopsG = findLoops . graphBoundary

-- | Returns a list of (looping) vertex trails for a BoundaryState.
-- There will usually be a single trail, but more than one indicates the presence of boundaries round holes.
-- Each trail starts with the lowest numbered vertex in that trail, and ends with the same vertex.
boundaryLoops:: BoundaryState -> [[Vertex]]
boundaryLoops = findLoops . boundary

-- | When applied to a boundary edge list this returns a list of (looping) vertex trails.
-- I.e. if we follow the boundary edges of a Tgraph recording vertices visited as a list returning to the starting vertex
-- we get a looping trail.
-- There will usually be a single trail, but more than one indicates the presence of boundaries round holes.
-- Each trail starts with the lowest numbered vertex in that trail, and ends with the same vertex.
findLoops:: [Dedge] -> [[Vertex]]
findLoops = collectLoops . VMap.fromList where

    -- Make a vertex to vertex map from the directed edges then delete items from the map as a trail is followed
    -- from the lowest numbered vertex.
    -- Vertices are collected in reverse order, then the list is reversed when a loop is complete.
    -- This is repeated until the map is empty, to collect all boundary trials.
   collectLoops vmap -- 
     | VMap.null vmap = []
     | otherwise = chase start vmap [start] 
         where
         (start,_) = VMap.findMin vmap
         chase a vm sofar -- sofar is the collected trail in reverse order.
            = case VMap.lookup a vm of
                Just b -> chase b (VMap.delete a vm) (b:sofar)
                Nothing -> if a == start 
                           then reverse sofar: collectLoops vm -- look for more loops
                           else error $ "boundaryLoops: non looping boundary component, starting at "
                                        ++show start++
                                        " and finishing at "
                                        ++ show a ++ 
                                        "\nwith loop vertices "++ show (reverse sofar) ++"\n"


-- | Given a suitable vertex to location map and boundary loops (represented as a list of lists of vertices),
-- this will return a (Diagrams) Path for the boundary.  It will raise an error if any vertex listed is not a map key.
-- (The resulting path can be filled when converted to a diagram.)
pathFromBoundaryLoops:: VertexLocMap -> [[Vertex]] -> Path V2 Double
pathFromBoundaryLoops vlocs loops = toPath $ map (locateLoop . map (vlocs VMap.!)) loops where 
    locateLoop pts = (`at` head pts) $ glueTrail $ trailFromVertices pts


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
pushFaces sub = sub{ tracked = faces (tgraph sub):tracked sub } where
{-
pushFaces sub = makeSubTgraph g newTracked where
    g = tgraph sub
    newTracked = faces g:tracked sub
-}

-- |unionTwoSub sub - combines the top two lists of tracked tilefaces replacing them with the list union.
unionTwoSub:: SubTgraph -> SubTgraph
unionTwoSub sub = sub{ tracked = newTracked } where
    newTracked = case tracked sub of
                   (a:b:more) -> a `union` b:more
                   _ -> error $ "unionTwoSub: Two tracked lists of faces not found: " ++ show sub ++"\n"
                   
{-*
Forcing and Decomposing SubTgraphs
-}
-- |force applied to a SubTgraph - has no effect on tracked subsets but applies force to the tgraph.
forceSub :: SubTgraph -> SubTgraph
forceSub sub = sub{ tgraph = force $ tgraph sub }
--forceSub sub = makeSubTgraph (force $ tgraph sub) (tracked sub)

-- |addHalfDartSub sub e - add a half dart to the tgraph of sub on the given edge e,
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

{-
-- |drawing non tracked faces only
drawWithoutTracked:: SubTgraph -> Diagram B
drawWithoutTracked sub = drawSubTgraph [drawPatch] sub
-}

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




 