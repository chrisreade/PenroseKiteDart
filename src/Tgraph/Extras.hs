{-|
Module      : Tgraph.Extras
Description : Additional Tgraph functions
Copyright   : (c) Chris Reade, 2024
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module defines several functions for producing overlaid diagrams for Tgraphs
(including smart drawing) and combinations such as compForce,
experimental combinations such as
boundaryECovering, boundaryVCovering, empire1, empire2, superForce, boundaryLoopsG.

It also defines experimental TrackedTgraphs (used for tracking subsets of faces of a Tgraph).

-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-} -- needed for Drawable Patch
{-# LANGUAGE TupleSections             #-}

module Tgraph.Extras
  ( smart
  , boundaryJoinFaces
  , drawBoundaryJoins
  , drawJoinsFor
  , smartdraw
  , restrictSmart
  , smartRotateBefore
  , smartAlignBefore
    -- * Overlaid drawing tools for Tgraphs
  , drawPCompose
  , drawForce
  , drawSuperForce
  , drawWithMax
  , addBoundaryAfter
  , drawCommonFaces
  , emphasizeFaces
    -- * Combining force, compose, decompose
  , composeK
  , compForce
  , allCompForce
  , maxCompForce
--  , forceDecomp
  , allForceDecomps
    -- * Boundary Covering and Empires
  , forcedBoundaryECovering
  , forcedBoundaryVCovering
  , boundaryECovering
  , boundaryVCovering
  , tryDartAndKite
  , tryDartAndKiteForced
  , tryDartAndKiteF
  , tryCheckCasesDKF
  , checkCasesDKF
  , boundaryEdgeSet
  , commonBdry
  , boundaryVertexSet
  , internalVertexSet
  , drawFBCovering
  , empire1
  , empire2
  , empire2Plus
  , drawEmpire
  , showEmpire1
  , showEmpire2
    -- * Super Force with boundary edge covers
  , superForce
  , trySuperForce
  , singleChoiceEdges
    -- * Boundary face graph
  , tryBoundaryFaceGraph
    -- * Boundary loops
  , boundaryLoops
  -- , findLoops
  , pathFromBoundaryLoops
    -- * TrackedTgraphs
  , TrackedTgraph(..)
  , newTrackedTgraph
  , makeTrackedTgraph
  , trackFaces
  , unionTwoTracked
    -- * Forcing and Decomposing TrackedTgraphs
  , addHalfDartTracked
  , addHalfKiteTracked
  , decomposeTracked
    -- *  Drawing TrackedTgraphs
  , drawTrackedTgraph
  , drawTrackedTgraphRotated
  , drawTrackedTgraphAligned
  ) where

import TileLib
import Tgraph.Prelude
import Tgraph.Decompose
import Tgraph.Compose
import Tgraph.Relabelling
import Tgraph.Force

import Diagrams.Prelude hiding (union)
import Data.List (intersect, union, (\\), find, foldl', transpose)
import qualified Data.Set as Set  (Set,fromList,null,intersection,deleteFindMin)-- used for boundary covers
import qualified Data.IntSet as IntSet (fromList,member,(\\)) -- for boundary vertex set
import qualified Data.IntMap.Strict as VMap (delete, fromList, findMin, null, lookup, (!)) -- used for boundary loops, boundaryLoops
import qualified Data.Maybe (fromMaybe)

-- |smart dr g - uses VPatch drawing function dr after converting g to a VPatch
-- It will add boundary joins regardless of the drawing function.
-- Examples:
-- 
-- smart draw g
--
-- smart (labelled draw) g
--
-- smart (labelSize normal draw) g
smart :: OKBackend b =>
         (VPatch -> Diagram b) -> Tgraph -> Diagram b
smart dr g = drawBoundaryJoins g vp <> dr vp
  where vp = makeVP g

-- |select the halftile faces of a Tgraph with a join edge on the boundary.
-- Useful for drawing join edges only on the boundary.
boundaryJoinFaces :: Tgraph -> [TileFace]
boundaryJoinFaces g = map snd $ incompleteHalves bdry $ boundary bdry where
    bdry = makeBoundaryState g

-- |draw boundary join edges of a Tgraph using a given VPatch
drawBoundaryJoins :: OKBackend b => Tgraph -> VPatch -> Diagram b
drawBoundaryJoins g vp = drawEdgesVP vp (map joinE $ boundaryJoinFaces g) # joinDashing

-- |Given a list of faces and a VPatch with suitable locations, draw just the dashed joins for those faces.
-- Will raise an error if any vertex in the faces does not have a location in the VPatch.
drawJoinsFor::  OKBackend b =>
                [TileFace] -> VPatch -> Diagram b
drawJoinsFor fcs vp = drawWith dashjOnly (restrictVP vp fcs)

-- |same as draw except adding dashed lines on boundary join edges. 
smartdraw :: OKBackend b => Tgraph -> Diagram b
smartdraw = smart draw

-- |restrictSmart g dr vp - assumes vp has locations for vertices in g.
-- It uses the VPatch drawing function dr to draw g and adds dashed boundary joins.
-- This can be used instead of smart when an appropriate vp is already available.
restrictSmart :: OKBackend b =>
                 Tgraph -> (VPatch -> Diagram b) -> VPatch -> Diagram b
restrictSmart g dr vp = drawBoundaryJoins g rvp <> dr rvp
                        where rvp = restrictVP vp $ faces g

-- |smartRotateBefore vfun a g - a tricky combination of smart with rotateBefore.
-- Uses vfun to produce a Diagram after converting g to a rotated VPatch but also adds the dashed boundary join edges of g.
--
-- Example: smartRotateBefore (labelled draw) angle g
smartRotateBefore :: OKBackend b =>
                     (VPatch -> Diagram b) -> Angle Double -> Tgraph -> Diagram b
smartRotateBefore vfun angle g = rotateBefore (restrictSmart g vfun) angle g

-- |smartAlignBefore vfun (a,b) g - a tricky combination of smart with alignBefore.
-- Uses vfun to produce a Diagram after converting g to an aligned VPatch but also adds the dashed boundary join edges of g.
-- 
-- Example: smartAlignBefore (labelled draw) (a,b) g
smartAlignBefore :: OKBackend b =>
                    (VPatch -> Diagram b) -> (Vertex,Vertex) -> Tgraph -> Diagram b
smartAlignBefore vfun (a,b) g = alignBefore (restrictSmart g vfun) (a,b) g

-- |applies partCompose to a Tgraph g, then draws the composed graph along with the remainder faces (in lime).
-- (Relies on the vertices of the composition and remainder being subsets of the vertices of g.)
-- This will raise an error if the composed faces have a crossing boundary or are disconnected.
drawPCompose :: OKBackend b =>
                Tgraph -> Diagram b
drawPCompose g =
    restrictSmart g' draw vp
    <> drawj (subVP vp remainder) # lw medium # lc lime
    where (remainder,g') = partCompose g
          vp = makeVP g

-- |drawForce g is a diagram showing the argument g in red overlayed on force g.
-- It adds dashed join edges on the boundary of g.
-- It will raise an error if the force fails with an incorrect/stuck Tgraph
drawForce :: OKBackend b =>
             Tgraph -> Diagram b
drawForce g =
    restrictSmart g draw vp # lc red # lw medium
    <> draw vp
    where vp = makeVP $ force g

-- |drawSuperForce g is a diagram showing the argument g in red overlayed on force g in black
-- overlaid on superForce g in blue.
-- It adds dashed join edges on the boundary of g.
-- It will raise an error if the initial force fails with an incorrect/stuck Tgraph
drawSuperForce :: OKBackend b =>
                  Tgraph -> Diagram b
drawSuperForce g = (dg # lc red) <> dfg <> (dsfg # lc blue) where
    fg = force g
    sfg = superForce fg
    vp = makeVP sfg
    dfg = draw $ selectFacesVP vp (faces fg \\ faces g) -- restrictSmart (force g) draw vp
    dg = restrictSmart g draw vp
    dsfg = draw $ selectFacesVP vp (faces sfg \\ faces fg)

-- | drawWithMax g - draws g and overlays the maximal composition of force g in red.
-- This relies on g and all compositions of force g having vertices in force g.
-- It will raise an error if forcing fails (g is an incorrect Tgraph).
drawWithMax :: OKBackend b =>
               Tgraph -> Diagram b
drawWithMax g =  (dmax # lc red # lw medium) <> dg where
    vp = makeVP $ force g -- duplicates force to get the locations of vertices in the forced Tgraph
    dg = restrictSmart g draw vp
    maxg = maxCompForce g
    dmax = draw $ subVP vp $ faces $ forgetF maxg

-- |addBoundaryAfter f g - displaying the boundary of a Tgraph g in lime (overlaid on g drawn with f).
addBoundaryAfter :: OKBackend b =>
                    (VPatch ->  Diagram b) -> Tgraph ->  Diagram b
addBoundaryAfter f g =  (drawEdgesVP vp edges # lc lime) <> f vp where
    vp = makeVP g
    edges = boundary g

-- |drawCommonFaces (g1,e1) (g2,e2) uses commonFaces (g1,e1) (g2,e2) to find the common faces
-- and emphasizes them on the background g1.
drawCommonFaces :: OKBackend b =>
                   (Tgraph,Dedge) -> (Tgraph,Dedge) -> Diagram b
drawCommonFaces (g1,e1) (g2,e2) = emphasizeFaces (commonFaces (g1,e1) (g2,e2)) g1

-- |emphasizeFaces fcs g emphasizes the given faces (that are in g) overlaid on the background draw g.
emphasizeFaces :: OKBackend b =>
                  [TileFace] -> Tgraph -> Diagram b
emphasizeFaces fcs g =  (drawj emphvp # lw thin) <> (draw vp # lw ultraThin) where
    vp = makeVP g
    emphvp = subVP vp (fcs `intersect` faces g)


-- | For illustrating an unsound version of composition which defaults to kites when there are unknown
-- dart wings on the boundary.
-- This is unsound in that it can create an incorrect Tgraph from a correct Tgraph.
-- E.g. when applied to force queenGraph.
composeK :: Tgraph -> Tgraph
composeK g = runTry $ tryConnectedNoCross newfaces where
    dwInfo = getDartWingInfo g
    changedInfo = dwInfo{ largeKiteCentres = largeKiteCentres dwInfo ++ unknowns dwInfo
                        , unknowns = []
                        }
    compositions = composedFaceGroups changedInfo
    newfaces = map fst compositions

-- |compForce is a partial function similar to (compose . force),
-- i.e it does a force then compose (raising an error if the force fails with an incorrect Tgraph).
-- However it produces an explicitly Forced Tgraph, 
-- and is more efficient because it omits the check for connected, and no crossing boundaries
-- (and uses getDartWingInfoForced instead of getDartWingInfo)
-- This relies on a proof that composition does not need to be checked for a forced Tgraph.
-- (We also have a proof that the result must be a forced Tgraph when the initial force succeeds.)
-- This will raise an error if the initial force fails with an incorrect Tgraph.
compForce:: Tgraph -> Forced Tgraph
compForce = composeF . forceF


-- |allCompForce g produces a list of the non-null iterated (forced) compositions of force g.
-- It will raise an error if the initial force fails with an incorrect Tgraph.
-- The list will be [] if g is the emptyTgraph, otherwise the list begins with force g (when the force succeeds).
-- The definition relies on (1) a proof that the composition of a forced Tgraph is forced  and
-- (2) a proof that composition does not need to be checked for a forced Tgraph.
allCompForce:: Tgraph -> [Forced Tgraph]
allCompForce = takeWhile (not . nullFaces . forgetF) . iterate composeF . forceF


-- |maxCompForce g produces the maximally composed (non-null) Tgraph starting from force g, provided g is not the emptyTgraph
-- and just the emptyTgraph otherwise.
-- It will raise an error if the initial force fails with an incorrect Tgraph.
maxCompForce:: Tgraph -> Forced Tgraph
maxCompForce g | nullFaces g = labelAsForced g
               | otherwise = last $ allCompForce g


-- | allForceDecomps g - produces an infinite list (starting with g) 
-- of forced decompositions of g (raising an error if a force fails with an incorrect Tgraph).
allForceDecomps:: Tgraph -> [Tgraph]
allForceDecomps = iterate (force . decompose)

{-| forcedBoundaryECovering g - produces a list of all boundary covers of force g.
Each boundary cover is a forced Tgraph that extends force g and covers the entire boundary directed edges of force g.
(So the boundary of force g is entirely internal edges in each boundary cover).
The covers include all possible ways faces can be added on the boundary of force g
except combinations which are found to be incorrect.
The common faces of the covers constitute an empire (level 1) of g.
This will raise an error if the initial force fails with an incorrect/stuck Tgraph.
-}
forcedBoundaryECovering:: Tgraph -> [Forced Tgraph]
forcedBoundaryECovering g = recoverGraphF <$> boundaryECovering gforcedBdry where
     gforcedBdry = runTry $ onFail "forcedBoundaryECovering:Initial force failed (incorrect Tgraph)\n" $
                             tryForceF $ makeBoundaryState g

{-| forcedBoundaryVCovering g - produces a list of all boundary covers of force g as with
forcedBoundaryECovering g but covering all boundary vertices rather than just boundary edges.
This will raise an error if the initial force fails with an incorrect/stuck Tgraph.                      
-}
forcedBoundaryVCovering:: Tgraph -> [Forced Tgraph]
forcedBoundaryVCovering g = recoverGraphF <$> boundaryVCovering gforcedBdry where
     gforcedBdry = runTry $ onFail "forcedBoundaryVCovering:Initial force failed (incorrect Tgraph)\n" $
                             tryForceF $ makeBoundaryState g

{-| boundaryECovering - for an explicitly Forced BoundaryState fbd,
produces a list of all possible covers of the boundary directed edges in fbd.
A cover is an explicitly Forced extension (of fbd) such that the original boundary directed edges of fbd are all internal edges.
Extensions are made by repeatedly adding a face to any edge on the original boundary that is still on the boundary
and forcing, repeating this until the orignal boundary is all internal edges.
The resulting covers account for all possible ways the boundary can be extended.
This can raise an error if both choices on a boundary edge fail when forced (using atLeastOne).

In which case, fbd represents an important counter example to the hypothesis that
successfully forced forcibles are correct.
-}
boundaryECovering:: Forced BoundaryState -> [Forced BoundaryState]
boundaryECovering forcedbs = covers [(forcedbs, boundaryEdgeSet (forgetF forcedbs))] where
  covers:: [(Forced BoundaryState, Set.Set Dedge)] -> [Forced BoundaryState]
  covers [] = []
  covers ((fbs,es):opens)
    | Set.null es = fbs:covers opens -- bs is a completed cover
    | otherwise = covers (newcases ++ opens)
       where (de,des) = Set.deleteFindMin es
             newcases = map (\b -> (b, commonBdry des (forgetF b)))
                             (runTry $ tryCheckCasesDKF de fbs)


-- |Make a set of the directed boundary edges from tilefaces
boundaryEdgeSet:: HasFaces a => a -> Set.Set Dedge
boundaryEdgeSet = Set.fromList . boundary

-- | commonBdry des a - returns those directed edges in des that are boundary directed edges of a
commonBdry:: HasFaces a => Set.Set Dedge -> a -> Set.Set Dedge
commonBdry des a = des `Set.intersection` boundaryEdgeSet a

{-| boundaryVCovering fbd - similar to boundaryECovering, but produces a list of all possible covers of 
    the boundary vertices in fbd (rather than just boundary edges).
    This can raise an error if both choices on a boundary edge fail when forced (using atLeastOne).
 -}
boundaryVCovering:: Forced BoundaryState -> [Forced BoundaryState]
boundaryVCovering fbd = covers [(fbd, startbds)] where
  startbds = boundaryEdgeSet $ forgetF fbd
  startbvs = boundaryVertexSet $ forgetF fbd
--covers:: [(Forced BoundaryState,Set.Set Dedge)] -> [Forced BoundaryState]
  covers [] = []
  covers ((open,es):opens)
    | Set.null es = case find (\(a,_) -> IntSet.member a startbvs) (boundary $ forgetF open) of
        Nothing -> open:covers opens
        Just dedge -> covers $ map (,es) (runTry $ tryCheckCasesDKF dedge open) ++opens
    | otherwise =  covers $ map (\b -> (b, commonBdry des (forgetF b))) (atLeastOne $  tryDartAndKiteF de (forgetF open)) ++opens
                   where (de,des) = Set.deleteFindMin es


-- | returns the set of boundary vertices of a tilefaces
boundaryVertexSet :: HasFaces a => a -> VertexSet
boundaryVertexSet = IntSet.fromList . boundaryVs

-- | returns the set of internal vertices of a tilefaces
internalVertexSet :: HasFaces a => a -> VertexSet
internalVertexSet a = vertexSet a IntSet.\\ boundaryVertexSet a


-- | tryDartAndKite de b - returns the list of (2) results after adding a dart (respectively kite)
-- to edge de of a Forcible b. Each of the results is a Try.
tryDartAndKite:: Forcible a => Dedge -> a -> [Try a]
tryDartAndKite de b =
    [ onFail ("tryDartAndKite: Dart on edge: " ++ show de ++ "\n") $
        tryAddHalfDart de b
    , onFail ("tryDartAndKite: Kite on edge: " ++ show de ++ "\n") $
        tryAddHalfKite de b
    ]

-- | tryDartAndKiteF de b - returns the list of (2) results after adding a dart (respectively kite)
-- to edge de of a Forcible b and then tries forcing.
-- Each of the results is a Try of an explicitly Forced type.
tryDartAndKiteF:: Forcible a => Dedge -> a -> [Try (Forced a)]
tryDartAndKiteF de b =
    [ onFail ("tryDartAndKiteF: Dart on edge: " ++ show de ++ "\n") $
        tryAddHalfDart de b >>= tryForceF
    , onFail ("tryDartAndKiteF: Kite on edge: " ++ show de ++ "\n") $
        tryAddHalfKite de b >>= tryForceF
    ]

-- | tryDartAndKiteForced de b - returns the list of (2) results after adding a dart (respectively kite)
-- to edge de of a Forcible b and then tries forcing.
-- Each of the results is a Try.
tryDartAndKiteForced:: Forcible a => Dedge -> a -> [Try a]
tryDartAndKiteForced de b = 
    [ onFail ("tryDartAndKiteForced: Dart on edge: " ++ show de ++ "\n") $
        tryAddHalfDart de b >>= tryForce
    , onFail ("tryDartAndKiteForced: Kite on edge: " ++ show de ++ "\n") $
        tryAddHalfKite de b >>= tryForce
    ]

-- | tryCheckCasesDKF dedge fb (where fb is an explicitly forced Forcible
-- and dedge is a directed boundary edge of fb) tries to add both a half kite and a half dart to the edge
-- then tries forcing each result.
-- It returns the list of only the successful Try results provided there is AT LEAST ONE.
-- If there are no successes, this may be an important counter example 
-- and it will return Left with a failure report describing the counter example
-- to the following:
--
-- Hypothesis: A successfully forced Tgraph is correct (a correct tiling).
--
-- (If both legal additions to a boundary edge are incorrect,
-- then the (Forced) Forcible must be incorrect).
tryCheckCasesDKF :: (Forcible a, Show a) => Dedge -> Forced a -> Try [Forced a]
tryCheckCasesDKF dedge fb = 
    onFail ("tryCheckCasesDKF: <<< Counter Example Found!! >>>\n"
            ++ "\nBoth legal extensions to directed edge " ++ show dedge
            ++ " \nare incorrrect for a successfully forced Forcible.\n"
            ++ "This shows a successfully forced forcible can still be incorrect\n"
            ++ "which is a counter example to the hypothesis that successful forcing\n"
            ++ "returns correct tilings.\n\n"
            ++ "The incorrect but forced forcible is:\n"
            ++ show fb
           )
    $ tryAtLeastOne $ tryDartAndKiteF dedge (forgetF fb)

-- | checkCasesDKF dedge fb (where fb is an explicitly forced Forcible
-- and dedge is a directed boundary edge of fb) tries to add both a half kite and a half dart to the edge
-- then tries forcing each result.
-- It returns the list of only the successful results provided there is AT LEAST ONE.
-- If there are no successes, this may be an important counter example 
-- and it will raise an error describing the counter example
-- to the following:
--
-- Hypothesis: A successfully forced Tgraph is correct (a correct tiling).
--
-- (If both legal additions to a boundary edge are incorrect,
-- then the (Forced) Forcible must be incorrect).
checkCasesDKF :: (Forcible a, Show a) => Dedge -> Forced a -> [Forced a]
checkCasesDKF dedge = runTry . tryCheckCasesDKF dedge
 
-- |A test function to draw (as a column) the list of covers resulting from forcedBoundaryVCovering
-- for a given Tgraph.
drawFBCovering :: OKBackend b =>
                  Tgraph -> Diagram b
drawFBCovering g = lw ultraThin $ vsep 1 (draw . forgetF <$> forcedBoundaryVCovering g)


-- | empire1 g - produces a TrackedTgraph representing the level 1 empire of g.
-- Raises an error if force g fails with a stuck/incorrect Tgraph.
-- The tgraph of the result is the first boundary vertex cover of force g
-- which is arbitrarily chosen amongst the covers as the background setting,
-- and the tracked list of the result has the common faces of all the boundary vertex covers (of force g)
-- at the head, followed by the original faces of g.
empire1 :: Tgraph -> TrackedTgraph
empire1 g = 
    case forcedBoundaryVCovering g of
     [] -> error "empire1 : no forced boundary covers found\n"
     (fg0:others) -> makeTrackedTgraph g0 [fcs,faces g] where
          g0 = forgetF fg0
          fcs = foldl' intersect (faces g0) $ map g0Intersect others
          de = defaultAlignment g
          g0Intersect fg1 = commonFaces (g0,de) (forgetF fg1,de)

-- | empire2 g - produces a TrackedTgraph representing a level 2 empire of g.
-- Raises an error if force g fails with a stuck/incorrect Tgraph.
-- After finding all boundary edge covers of force g, 
-- boundary edge covers are then found for each boundary edge cover to form a list of doubly-extended
-- boundary edge covers.
-- The tgraph of the result is the first (doubly-extended) boundary edge cover (of force g)
-- which is arbitrarily chosen amongst the (doubly-extended) covers as the background setting,
-- and the tracked list of the result has the common faces of all the (doubly-extended) boundary edge covers
-- at the head, followed by the original faces of g.
empire2:: Tgraph -> TrackedTgraph
empire2 g = 
  case map (recoverGraph . forgetF) covers2 of
    [] -> error "empire2: empty list of secondary boundary covers found"
    (g0:others) -> makeTrackedTgraph g0 [fcs, faces g]
      where fcs = foldl' intersect (faces g0) $ map g0Intersect others
            g0Intersect g1 = commonFaces (g0,de) (g1,de)
  where
     covers1 = boundaryECovering $ runTry $ onFail "empire2:Initial force failed (incorrect Tgraph)\n"
              $ tryForceF $ makeBoundaryState g
     covers2 = concatMap boundaryECovering covers1
     de = defaultAlignment g
     

-- | empire2Plus g - produces a TrackedTgraph representing an extended level 2 empire of g
-- similar to empire2, but using boundaryVCovering instead of boundaryECovering.
-- Raises an error if force g fails with a stuck/incorrect Tgraph.
empire2Plus:: Tgraph -> TrackedTgraph
empire2Plus g = 
  case map (recoverGraph . forgetF) covers2 of
    [] -> error "empire2: empty list of secondary boundary covers found"
    (g0:others) -> makeTrackedTgraph g0 [fcs, faces g]
      where fcs = foldl' intersect (faces g0) $ map g0Intersect others
            g0Intersect g1 = commonFaces (g0,de) (g1,de)
  where
     covers1 = boundaryVCovering $ runTry $ onFail "empire2:Initial force failed (incorrect Tgraph)\n"
              $ tryForceF $ makeBoundaryState g
     covers2 = concatMap boundaryVCovering covers1
     de = defaultAlignment g
     

-- | drawEmpire e - produces a diagram for an empire e represented as a TrackedTgraph
-- as calcultaed by e.g. empire1 or empire2 or empire2Plus.
-- The diagram draws the underlying Tgraph (the background setting), with the first tracked faces
-- (the starting Tgraph) shown red, and emphasising the second tracked faces
-- (the common  faces).
drawEmpire :: OKBackend b =>
               TrackedTgraph -> Diagram b
drawEmpire =
    drawTrackedTgraph  [ lw ultraThin . draw
                       , lw thin . fillDK lightgrey lightgrey
                       , lw thin . lc red . draw
                       ]

-- | showEmpire1 g - produces a diagram emphasising the common faces of all boundary covers of force g.
-- This is drawn over one of the possible boundary covers and the faces of g are shown in red.
showEmpire1 :: OKBackend b =>
               Tgraph -> Diagram b
showEmpire1 = drawEmpire . empire1

-- | showEmpire2 g - produces a diagram emphasising the common faces of a doubly-extended boundary cover of force g.
-- This is drawn over one of the possible doubly-extended boundary covers and the faces of g are shown in red.
showEmpire2 :: OKBackend b =>
               Tgraph -> Diagram b
showEmpire2 = drawEmpire . empire2

-- |superForce g - after forcing g this looks for single choice boundary edges.
-- That is a boundary edge for which only a dart or only a kite addition occurs in all boundary edge covers.
-- If there is at least one such edge, it makes the choice for the first such edge and recurses,
-- otherwise it returns the forced result.
-- This will raise an error if force encounters a stuck (incorrect) tiling or if
-- both forced extensions fail for some boundary edge.
-- Otherwise, the result has exactly two correct possible extensions for each boundary edge.
superForce:: Forcible a => a -> a
superForce g = runTry $ trySuperForce g

-- |trySuperForce g - this looks for single choice edges after trying to force g.
-- If there is at least one, it makes that choice and recurses.
-- It returns a Left s if force fails or if both choices fail for some edge (where s is a failure report).
-- Otherwise Right g' is returned where g' is the super forced g.
trySuperForce:: Forcible a => a -> Try a
trySuperForce = tryFSOp trySuperForceFS where
    -- |trySuperForceFS - implementation of trySuperForce for force states only
    trySuperForceFS :: ForceState -> Try ForceState
    trySuperForceFS fs =
        do forcedFS <- onFail "trySuperForceFS: force failed (incorrect Tgraph)\n" $
                       tryForceF fs
           case singleChoiceEdges $ boundaryStateF forcedFS of
              [] -> return $ forgetF forcedFS
              (elpr:_) -> do extended <- addSingle elpr $ forgetF forcedFS
                             trySuperForceFS extended
    addSingle (e,l) fs = if isDart l then tryAddHalfDart e fs else tryAddHalfKite e fs

-- |singleChoiceEdges bd - if bd is an explicitly Forced boundary state (of a forced Tgraph) this finds those boundary edges of bd
-- which have a single choice (i.e. the other choice is incorrect), by inspecting boundary edge covers of bd.
-- The result is a list of pairs of (edge,label) where edge is a boundary edge with a single choice
-- and label indicates the choice as the common face label.
singleChoiceEdges :: Forced BoundaryState -> [(Dedge,HalfTileLabel)]
singleChoiceEdges bstate = commonToCovering (forgetF <$> boundaryECovering bstate) (boundary $ forgetF bstate)
  where
-- commonToCovering bds edgeList - when bds are all the boundary edge covers of some forced Tgraph
-- whose boundary edges were edgeList, this looks for edges in edgeList that have the same tile label added in all covers.
-- This indicates there is a single choice for such an edge (the other choice is incorrect).
-- The result is a list of pairs: edge and a common tile label.
-- commonToCovering :: [BoundaryState] -> [Dedge] -> [(Dedge,HalfTileLabel)]
    commonToCovering bds edgeList = common edgeList (transpose labellists) where
      labellists = map (`reportCover` edgeList) bds
      common [] [] = []
      common [] (_:_) = error "singleChoiceEdges:commonToCovering: label list is longer than edge list"
      common (_:_) [] = error "singleChoiceEdges:commonToCovering: label list is shorter than edge list"
      common (_:_) ([]:_) = error "singleChoiceEdges:commonToCovering: empty list of labels"
      common (e:more) ((l:ls):lls) = if all (==l) ls
                                     then (e,l):common more lls
                                     else common more lls
      
-- |reportCover bd edgelist - when bd is a boundary edge cover of some forced Tgraph whose boundary edges are edgelist,
-- this returns the tile label for the face covering each edge in edgelist (in corresponding order).
-- reportCover :: BoundaryState -> [Dedge] -> [HalfTileLabel]
    reportCover bd des = map (tileLabel . getf) des where
      efmap = dedgesFacesMap des (faces bd) -- more efficient than using graphEFMap?
--      efmap = graphEFMap (recoverGraph bd)
      getf e = Data.Maybe.fromMaybe (error $ "singleChoiceEdges:reportCover: no face found with directed edge " ++ show e)
                                    (faceForEdge e efmap)

-- |Tries to create a new Tgraph from all faces with a boundary vertex in a Tgraph.
-- The resulting faces could have a crossing boundary and also could be disconnected if there is a hole in the starting Tgraph
-- so these conditions are checked for, producing a Try result.
tryBoundaryFaceGraph :: Tgraph -> Try Tgraph
tryBoundaryFaceGraph = tryConnectedNoCross . boundaryFaces . makeBoundaryState


{- -- | Returns a list of (looping) vertex trails for the boundary of a Tgraph.
-- There will usually be a single trail, but more than one indicates the presence of boundaries round holes.
-- Each trail starts with the lowest numbered vertex in that trail, and ends with the same vertex.
-- The trails will have disjoint sets of vertices because of the no-crossing-boundaries condition of Tgraphs.
boundaryLoopsG:: Tgraph -> [[Vertex]]
boundaryLoopsG = findLoops . boundary
 -}
-- | Returns a list of (looping) vertex trails for a BoundaryState.
-- There will usually be a single trail, but more than one indicates the presence of boundaries round holes.
-- Each trail starts with the lowest numbered vertex in that trail, and ends with the same vertex.
-- The trails will have disjoint sets of vertices because of the no-crossing-boundaries condition of Tgraphs (and hence BoundaryStates).
boundaryLoops:: HasFaces a => a -> [[Vertex]]
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
     | otherwise = chase startV vmap [startV]
         where
         (startV,_) = VMap.findMin vmap
         chase a vm sofar -- sofar is the collected trail in reverse order.
            = case VMap.lookup a vm of
                Just b -> chase b (VMap.delete a vm) (b:sofar)
                Nothing -> if a == startV
                           then reverse sofar: collectLoops vm -- look for more loops
                           else error $ "findLoops (collectLoops): non looping boundary component, starting at "
                                        ++show startV++
                                        " and finishing at "
                                        ++ show a ++
                                        "\nwith loop vertices "++ show (reverse sofar) ++"\n"


-- | Given a suitable vertex to location map and boundary loops (represented as a list of lists of vertices),
-- this will return a (Diagrams) Path for the boundary.  It will raise an error if any vertex listed is not a map key.
-- (The resulting path can be filled when converted to a diagram.)
pathFromBoundaryLoops:: VertexLocMap -> [[Vertex]] -> Path V2 Double
pathFromBoundaryLoops vlocs loops = toPath $ map (locateLoop . map (vlocs VMap.!)) loops where
    locateLoop [] = error "pathFromBoundaryLoops: empty loop found\n"
    locateLoop (p:pts) = (`at` p) $ glueTrail $ trailFromVertices (p:pts)


-- * TrackedTgraphs

{-|
 TrackedTgraph - introduced to allow tracking of subsets of faces
 in both force and decompose operations.
 Mainly used for drawing purposes but also for empires.
 A TrackedTgraph has a main Tgraph (tgraph) and a list of subsets (sublists) of faces (tracked).
 The list allows for tracking different subsets of faces at the same time.
-}
data TrackedTgraph = TrackedTgraph{ tgraph:: Tgraph, tracked::[[TileFace]]} deriving Show

-- |newTrackedTgraph g creates a TrackedTgraph from a Tgraph g with an empty tracked list
newTrackedTgraph :: Tgraph -> TrackedTgraph
newTrackedTgraph g = makeTrackedTgraph g []

-- |makeTrackedTgraph g trackedlist creates a TrackedTgraph from a Tgraph g
-- from trackedlist where each list in trackedlist is a subset of the faces of g.
-- Any faces not in g are ignored.
makeTrackedTgraph :: Tgraph -> [[TileFace]] -> TrackedTgraph
makeTrackedTgraph g trackedlist = TrackedTgraph{ tgraph = g, tracked = map (`intersect` faces g) trackedlist}

-- |trackFaces ttg - pushes the maingraph tilefaces onto the stack of tracked subsets of ttg
trackFaces:: TrackedTgraph -> TrackedTgraph
trackFaces ttg = ttg{ tracked = faces ttg : tracked ttg }

-- |unionTwoTracked ttg - combines the top two lists of tracked tilefaces replacing them with the list union.
unionTwoTracked:: TrackedTgraph -> TrackedTgraph
unionTwoTracked ttg = ttg{ tracked = newTracked } where
    newTracked = case tracked ttg of
                   (a:b:more) -> a `union` b:more
                   _ -> error $ "unionTwoTracked: Two tracked lists of faces not found: " ++ show ttg ++"\n"

{-*
Forcing and Decomposing TrackedTgraphs
-}

-- | TrackedTgraphs are Forcible    
instance Forcible TrackedTgraph where
    tryFSOpWith ugen f ttg = do
        g' <- tryFSOpWith ugen f $ tgraph ttg
        return ttg{ tgraph = g' }
    tryInitFSWith ugen ttg = tryInitFSWith ugen (tgraph ttg)
    tryChangeBoundaryWith ugen f ttg = do
        g' <- tryChangeBoundaryWith ugen f $ tgraph ttg
        return ttg{ tgraph = g' }
--    boundaryState = boundaryState . tgraph

-- |TrackedTgraph is in class HasFaces
instance HasFaces TrackedTgraph where
    faces  = faces . tgraph
    boundary = boundary . faces . tgraph
    maxV = maxV . faces . tgraph

-- |addHalfDartTracked ttg e - add a half dart to the tgraph of ttg on the given edge e,
-- and push the new singleton face list onto the tracked list.
addHalfDartTracked:: Dedge -> TrackedTgraph -> TrackedTgraph
addHalfDartTracked e ttg =
  TrackedTgraph{ tgraph = g' , tracked = newfcs:tracked ttg}
  where
    g = tgraph ttg
    g' = addHalfDart e g
    newfcs = faces g' \\ faces g

-- |addHalfKiteTracked ttg e - add a half kite to the tgraph of ttg on the given edge e,
-- and push the new singleton face list onto the tracked list.
addHalfKiteTracked:: Dedge -> TrackedTgraph -> TrackedTgraph
addHalfKiteTracked e ttg =
  TrackedTgraph{ tgraph = g' , tracked = newfcs:tracked ttg}
  where
    g = tgraph ttg
    g' = addHalfKite e g
    newfcs = faces g' \\ faces g

-- |decompose a TrackedTgraph - applies decomposition to all tracked subsets as well as the full Tgraph.
-- Tracked subsets get the same numbering of new vertices as the main Tgraph. 
decomposeTracked :: TrackedTgraph -> TrackedTgraph
decomposeTracked ttg =
  TrackedTgraph{ tgraph = g' , tracked = tlist}
  where
--    makeTrackedTgraph g' tlist where
    g = tgraph ttg
    g' = makeUncheckedTgraph newFaces
    newVFor = phiVMap g
    newFaces = concatMap (decompFace newVFor) (faces g)
    tlist = map (concatMap (decompFace newVFor)) (tracked ttg)

{-*  Drawing TrackedTgraphs
-}

{-|
    To draw a TrackedTgraph, we use a list of functions each turning a VPatch into a diagram.
    The first function is applied to a VPatch for untracked faces.
    Subsequent functions are applied to VPatches for the respective tracked subsets.
    Each diagram is beneath later ones in the list, with the diagram for the untracked VPatch at the bottom.
    The VPatches are all restrictions of a single VPatch for the Tgraph, so consistent.
    (Any extra draw functions are applied to the VPatch for the main tgraph and the results placed atop.)
-}
drawTrackedTgraph :: OKBackend b => [VPatch -> Diagram b] -> TrackedTgraph -> Diagram b
drawTrackedTgraph drawList ttg = mconcat $ reverse $ zipWith ($) drawList vpList where
    vp = makeVP (tgraph ttg)
    untracked = faces vp \\ concat (tracked ttg)
    vpList = map (restrictVP vp) (untracked:tracked ttg) ++ repeat vp

{-|
    To draw a TrackedTgraph rotated.
    Same as drawTrackedTgraph but with additional angle argument for the rotation.
    This is useful when labels are being drawn.
    The angle argument is used to rotate the common vertex location map (anticlockwise) before drawing
    to ensure labels are not rotated.
-}
drawTrackedTgraphRotated :: OKBackend b => [VPatch -> Diagram b] -> Angle Double -> TrackedTgraph -> Diagram b
drawTrackedTgraphRotated drawList a ttg = mconcat $ reverse $ zipWith ($) drawList vpList where
    vp = rotate a $ makeVP (tgraph ttg)
    untracked = faces vp \\ concat (tracked ttg)
    vpList = map (restrictVP vp) (untracked:tracked ttg) ++ repeat vp

{-|
    To draw a TrackedTgraph aligned.
    Same as drawTrackedTgraph but with additional vertex pair argument for the (x-axis) alignment.
    This is useful when labels are being drawn.
    The vertex pair argument is used to align the common vertex location map before drawing
    (to ensure labels are not rotated).
    This will raise an error if either of the pair of vertices is not a vertex of (the tgraph of) the TrackedTgraph
-}
drawTrackedTgraphAligned :: OKBackend b => [VPatch -> Diagram b] -> (Vertex,Vertex) -> TrackedTgraph -> Diagram b
drawTrackedTgraphAligned drawList (a,b) ttg = mconcat $ reverse $ zipWith ($) drawList vpList where
    vp = makeAlignedVP (a,b) (tgraph ttg)
    untracked = faces vp \\ concat (tracked ttg)
    vpList = map (restrictVP vp) (untracked:tracked ttg) ++ repeat vp



