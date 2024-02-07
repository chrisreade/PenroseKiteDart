{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-} -- needed for Drawable Patch
-- {-# LANGUAGE TypeOperators             #-} -- needed for type equality constraints ~

{-|
Module      : Tgraphs
Description : Collects and exports the various Tgraph modules plus extra operations, including makeTgraph
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This is the main module for Tgraph operations which collects and exports the other Tgraph modules. 
It exports makeTgraph for constructing checked Tgraphs and excludes data constructor Tgraph.
The module also defines several functions for producing overlaid diagrams for Tgraphs (including smart drawing) and
experimental combinations such as boundaryECovering, boundaryVCovering, empire1, empire2, superForce, boundaryLoopsG.
It also defines experimental TrackedTgraphs (used for tracking subsets of faces of a Tgraph).
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
import Tgraph.Decompose
import Tgraph.Compose
import Tgraph.Force
import Tgraph.Convert
import Tgraph.Relabelling

import Diagrams.Prelude hiding (union)
-- import ChosenBackend (B)
import TileLib

import Data.List (intersect, union, (\\), find, foldl',nub, transpose)      
import qualified Data.Set as Set  (Set,fromList,null,intersection,deleteFindMin,delete,toList)-- used for boundary covers
import qualified Data.IntSet as IntSet (fromList,member,(\\)) -- for boundary vertex set
import qualified Data.IntMap.Strict as VMap (delete, fromList, findMin, null, lookup, (!)) -- used for boundary loops, boundaryLoops

-- * Making valid Tgraphs (with a check for no touching vertices).

{-|
makeTgraph performs a no touching vertex check as well as using checkTgraphProps for other required properties.
It produces an error if either check fails.
Note that the other Tgraph properties are checked first, to ensure that calculation of 
vertex locations can be done for a touching vertex check.
-}
makeTgraph :: [TileFace] -> Tgraph
makeTgraph fcs = runTry $ onFail "makeTgraph: (failed):\n" $ tryMakeTgraph fcs

{-|
tryMakeTgraph performs the same checks for Tgraph properties as checkTgraphProps but in addition
it also checks that there are no touching vertices (distinct labels for the same vertex)
using Tgraph.Convert.touchingVertices (which calculates vertex locations).
It produces Left ... if either check fails and Right g otherwise where g is the Tgraph.
Note that the other Tgraph properties are checked first, to ensure that calculation of 
vertex locations can be done.
-}
tryMakeTgraph :: [TileFace] -> Try Tgraph
tryMakeTgraph fcs =
 do g <- checkTgraphProps fcs -- must be checked first
    let touchVs = touchingVertices (faces g)
    if null touchVs 
    then Right g 
    else Left ("Found touching vertices: " 
               ++ show touchVs
               ++ "\nwith faces:\n"
               ++ show fcs
               ++ "\n\n(To fix, use: tryCorrectTouchingVs)\n\n"
              )

{-| tryCorrectTouchingVs fcs finds touching vertices by calculating locations for vertices in the faces fcs,
    then renumbers to remove touching vertices (renumbers higher to lower numbers),
    then checks for Tgraph properties of the resulting faces to produce a Tgraph.
    NB fcs needs to be tile-connected before the renumbering and
    the renumbering need not be 1-1 (hence Relabelling is not used)      
-}
tryCorrectTouchingVs ::  [TileFace] -> Try Tgraph
tryCorrectTouchingVs fcs = 
    onFail ("tryCorrectTouchingVs:\n" ++ show touchVs) $ 
    checkTgraphProps $ nub $ renumberFaces touchVs fcs
        -- renumberFaces allows for a non 1-1 relabelling represented by a list 
    where touchVs = touchingVertices fcs -- uses non-generalised version of touchingVertices

{-*
Smart drawing of Tgraphs
-}

-- |smart dr g - uses VPatch drawing function dr after converting g to a VPatch
-- It will add boundary joins regardless of the drawing function.
-- Example: smart (labelSmall draw) g
-- Example: smart (rotateBefore (labelled draw) angle) g
-- Note smart must come after labelling as e.g. labelled (smart draw) will not typecheck.
-- smart :: (VPatch -> Diagram B) -> Tgraph -> Diagram B
smart :: Renderable (Path V2 Double) b => 
         (VPatch -> Diagram2D b) -> Tgraph -> Diagram2D b
smart dr g = drawJoinsFor (boundaryJoinFaces g) vp <> dr vp
  where vp = makeVP g

-- |select the halftile faces of a Tgraph with a join edge on the boundary.
-- Useful for drawing join edges only on the boundary.
boundaryJoinFaces :: Tgraph -> [TileFace]
boundaryJoinFaces g = fmap snd $ incompleteHalves bdry $ boundary bdry where
    bdry = makeBoundaryState g

-- |given a list of faces and a VPatch with suitable locations, draw just the dashed joins for those faces.
-- drawJoinsFor:: [TileFace] -> VPatch -> Diagram B
drawJoinsFor::  Renderable (Path V2 Double) b => 
                [TileFace] -> VPatch -> Diagram2D b
drawJoinsFor fcs vp = drawWith dashjOnly (subVP vp fcs)

-- |same as draw except adding dashed lines on boundary join edges. 
-- smartdraw :: Tgraph -> Diagram B
smartdraw :: Renderable (Path V2 Double) b => Tgraph -> Diagram2D b
smartdraw = smart draw

-- |restrictSmart g dr vp - assumes vp has locations for vertices in g.
-- It uses the VPatch drawing function dr to draw g and adds dashed boundary joins.
-- This can be used instead of smart when an appropriate vp is already available.
-- restrictSmart:: Tgraph -> (VPatch -> Diagram B) -> VPatch -> Diagram B
restrictSmart :: Renderable (Path V2 Double) b =>
                 Tgraph -> (VPatch -> Diagram2D b) -> VPatch -> Diagram2D b
restrictSmart g dr vp = drawJoinsFor (boundaryJoinFaces g) vp 
                        <> 
                        dr (restrictVP vp $ faces g)

-- |smartRotateBefore vfun a g - a tricky combination of smart with rotateBefore.
-- Uses vfun to produce a Diagram after converting g to a rotated VPatch but also adds the dashed boundary join edges of g.
-- smartRotateBefore::  (VPatch -> Diagram B) -> Angle Double -> Tgraph -> Diagram B
smartRotateBefore :: Renderable (Path V2 Double) b =>
                     (VPatch -> Diagram2D b) -> Angle Double -> Tgraph -> Diagram2D b
smartRotateBefore vfun angle g = rotateBefore (restrictSmart g vfun) angle g

-- |smartAlignBefore vfun (a,b) g - a tricky combination of smart with alignBefore.
-- Uses vfun to produce a Diagram after converting g to n aligned VPatch but also adds the dashed boundary join edges of g.
-- smartAlignBefore::  (VPatch -> Diagram B) -> (Vertex,Vertex) -> Tgraph -> Diagram B
smartAlignBefore :: Renderable (Path V2 Double) b =>
                    (VPatch -> Diagram2D b) -> (Vertex,Vertex) -> Tgraph -> Diagram2D b
smartAlignBefore vfun (a,b) g = alignBefore (restrictSmart g vfun) (a,b) g

{-*
Overlaid drawing tools for Tgraphs
-}

-- |applies partCompose to a Tgraph g, then draws the composed graph with the remainder faces (in lime).
-- (Relies on the vertices of the composition and remainder being subsets of the vertices of g.)
-- drawPCompose ::  Tgraph -> Diagram B
drawPCompose :: Renderable (Path V2 Double) b =>
                Tgraph -> Diagram2D b
drawPCompose g = 
    restrictSmart g' draw vp # lw ultraThin
    <> drawj (subVP vp remainder) # lw thin # lc lime
    where (remainder,g') = partCompose g
          vp = makeVP g

-- |drawForce g is a diagram showing the argument g in red overlayed on force g
-- It adds dashed join edges on the boundary of g
-- drawForce:: Tgraph -> Diagram B
drawForce :: Renderable (Path V2 Double) b =>
             Tgraph -> Diagram2D b
drawForce g = 
    restrictSmart g draw vp # lc red # lw thin 
    <> draw vp  # lw ultraThin
    where vp = makeVP $ force g

-- |drawSuperForce g is a diagram showing the argument g in red overlayed on force g in black
-- overlaid on superForce g in blue.
-- It adds dashed join edges on the boundary of g.
-- drawSuperForce:: Tgraph -> Diagram B
drawSuperForce :: Renderable (Path V2 Double) b =>
                  Tgraph -> Diagram2D b
drawSuperForce g = (dg # lc red # lw veryThin) <> dfg  # lw veryThin <> (dsfg # lc blue # lw thin) where
    sfg = superForce g
    fg = force g
    vp = makeVP $ superForce g
    dfg = draw $ selectFacesVP (faces fg \\ faces g) vp -- restrictSmart (force g) draw vp
    dg = restrictSmart g draw vp
    dsfg = draw $ selectFacesVP (faces sfg \\ faces fg) vp

{-|
drawWithMax g - draws g and overlays the maximal composition of force g in red.
This relies on g and all compositions of force g having vertices in force g.
  drawWithMax :: Tgraph -> Diagram B
-}
drawWithMax :: Renderable (Path V2 Double) b =>
              Tgraph -> Diagram2D b
drawWithMax g =  (dmax # lc red # lw thin) <> dg # lw ultraThin where
    vp = makeVP $ force g -- duplicates force to get the locations of vertices in the forced Tgraph
    dg = restrictSmart g draw vp
    maxg = maxCompForce g
    dmax = restrictSmart maxg draw vp

-- |displaying the boundary of a Tgraph in lime (overlaid on the Tgraph drawn with f).
-- addBoundaryAfter :: (VPatch -> Diagram B) -> Tgraph -> Diagram B
addBoundaryAfter :: Renderable (Path V2 Double) b =>
                    (VPatch ->  Diagram2D b) -> Tgraph ->  Diagram2D b
addBoundaryAfter f g =  (drawEdgesIn vp edges # lc lime) <> f vp where
    vp = makeVP g
    edges = graphBoundary g

-- |drawCommonFaces (g1,e1) (g2,e2) uses commonFaces (g1,e1) (g2,e2) to find the common faces
-- and emphasizes them on the background g1.
-- drawCommonFaces:: (Tgraph,Dedge) -> (Tgraph,Dedge) -> Diagram B
drawCommonFaces :: Renderable (Path V2 Double) b =>
                   (Tgraph,Dedge) -> (Tgraph,Dedge) -> Diagram2D b
drawCommonFaces (g1,e1) (g2,e2) = emphasizeFaces (commonFaces (g1,e1) (g2,e2)) g1

-- |emphasizeFaces fcs g emphasizes the given faces (that are in g) overlaid on the background g.
-- emphasizeFaces:: [TileFace] -> Tgraph -> Diagram B
emphasizeFaces :: Renderable (Path V2 Double) b =>
                  [TileFace] -> Tgraph -> Diagram2D b
emphasizeFaces fcs g =  (drawj emphvp # lw thin) <> (draw vp # lw ultraThin) where
    vp = makeVP g
    emphvp = subVP vp (fcs `intersect` faces g)


 
      

{-*
Combining force, compose, decompose
-}


-- | An unsound version of composition which defaults to kites when there are choices (unknowns).
-- This is unsound in that it can create an incorrect Tgraph from a correct Tgraph.
composeK :: Tgraph -> Tgraph
composeK g = runTry $ checkConnectedNoCross newfaces where
    dwInfo = getDartWingInfo g
    changedInfo = dwInfo{ largeKiteCentres = largeKiteCentres dwInfo ++ unknowns dwInfo
                        , unknowns = []
                        }
    compositions = composedFaceGroups changedInfo
    newfaces = map fst compositions

-- |compForce does a force then compose.
-- It omits the check for connected, and no crossing boundaries because the argument is forced first.
-- This relies on a proof that composition does not need to be checked for a forced Tgraph.
-- It may raise an error if the initial force fails with an incorrect Tgraph.
compForce:: Tgraph -> Tgraph
compForce = uncheckedCompose . force 
        
-- |allCompForce g produces a list of all forced compositions starting from g up to but excluding the empty Tgraph.
-- (The list will be empty if g is the emptyGraph).
-- This definition relies on (1) a proof that the composition of a forced Tgraph is forced  and
-- (2) a proof that composition does not need to be checked for a forced Tgraph.
-- It may raise an error if the initial force fails with an incorrect Tgraph.
allCompForce:: Tgraph -> [Tgraph]
allCompForce g = takeWhile (not . nullGraph) $ g: iterate uncheckedCompose (compForce g)

-- |maxCompForce g produces the maximally composed (non-empty) Tgraph from force g, provided g is non-empty
-- and just the emptyGraph otherwise.
-- It may raise an error if the initial force fails with an incorrect Tgraph.
maxCompForce:: Tgraph -> Tgraph
maxCompForce g | nullGraph g = g
               | otherwise = last $ allCompForce g


-- |force after a decomposition
forceDecomp:: Tgraph -> Tgraph
forceDecomp = force . decompose

-- | allForceDecomps g - produces an infinite list of forced decompositions of g
allForceDecomps:: Tgraph -> [Tgraph]
allForceDecomps = iterate forceDecomp

{-*
Emplace Choices
-}

-- |emplaceChoices forces then maximally composes. At this top level it
-- produces a list of forced choices for the unknowns.
-- It then repeatedly forceDecomps back to the starting level to return a list of Tgraphs.
-- This version relies on compForce theorem and related theorems
emplaceChoices:: Tgraph -> [Tgraph]
emplaceChoices g = emplaceChoices' $ force $ makeBoundaryState g

-- |emplaceChoices' bd - assumes bd is forced. It maximally composes. At this top level it
-- produces a list of forced choices for the unknowns.
-- It then repeatedly forceDecomps back to the starting level to return a list of Tgraphs.
-- This version relies on compForce theorem and related theorems
emplaceChoices':: BoundaryState -> [Tgraph]
emplaceChoices' startbd | nullGraph g' = recoverGraph <$> choices [startbd]
                   | otherwise = forceDecomp <$> emplaceChoices' (makeBoundaryState g')
  where   
   g' = compose $ recoverGraph startbd
   startunknowns = unknowns $ getDartWingInfo $ recoverGraph startbd
   choices [] = []
   choices (bd:bds) 
        = case  startunknowns `intersect` unknowns (getDartWingInfo $ recoverGraph bd) of
             [] -> bd:choices bds
             (u:_) -> choices (atLeastOne (tryDartAndKiteForced (findDartLongForWing u bd) bd)++bds)
   findDartLongForWing v bd 
        = case find isDart (facesAtBV bd v) of
            Just d -> longE d
            Nothing -> error $ "emplaceChoices': dart not found for dart wing vertex " ++ show v

{-*
Boundary Covering and Empires
-}

{-| forcedBoundaryECovering g - produces a list of all boundary covers of force g, each of which
extends force g to cover the entire boundary directed edges in (force g).
(So the boundary of force g is entirely internal edges in each cover).
The covers include all possible ways faces can be added on the boundary that are correct.
The common faces of the covers constitute the empire (level 1) of g.
This will raise an error if the initial force fails with a stuck graph.
-}
forcedBoundaryECovering:: Tgraph -> [Tgraph]
forcedBoundaryECovering g = recoverGraph <$> boundaryECovering gforcedBdry where
     gforcedBdry = runTry $ onFail "forcedBoundaryECovering:Initial force failed (incorrect Tgraph)\n" $
                             tryForce $ makeBoundaryState g

{-| forcedBoundaryVCovering g - produces a list of all boundary covers of force g as with
forcedBoundaryECovering g but covering all boundary vertices rather than just boundary edges.                        
-}
forcedBoundaryVCovering:: Tgraph -> [Tgraph]
forcedBoundaryVCovering g = recoverGraph <$> boundaryVCovering gforcedBdry where
     gforcedBdry = runTry $ onFail "forcedBoundaryVCovering:Initial force failed (incorrect Tgraph)\n" $
                             tryForce $ makeBoundaryState g

{-| boundaryECovering bd - produces a list of all possible covers of the boundary directed edges in bd.
[bd should be a boundary state resulting from forcing].
A cover is a forced extension (of bd) such that the original boundary directed edges of bd are all internal edges.
Extensions are made by repeatedly adding a face to any edge on the original boundary that is still on the boundary
and forcing, repeating this until the orignal boundary is all internal edges.
The resulting covers account for all possible ways the boundary can be extended.
This can raise an error if bd is a boundary state of an unforced Tgraph.
It will raise an error if both choices on a boundary edge fail when forced (using atLeastOne).
-}
boundaryECovering:: BoundaryState -> [BoundaryState]
boundaryECovering bs = covers [(bs, boundaryEdgeSet bs)] where
-- covers:: [(BoundaryState, Set.Set Dedge)] -> [BoundaryState]
  covers [] = []
  covers ((bs,es):opens) 
    | Set.null es = bs:covers opens -- bs is a completed cover
    | otherwise = covers (newcases ++ opens)
       where (de,des) = Set.deleteFindMin es
             newcases = fmap (\b -> (b, commonBdry des b))
                             (atLeastOne $ tryDartAndKiteForced de bs)

-- |Make a set of the directed boundary edges of a BoundaryState
boundaryEdgeSet:: BoundaryState -> Set.Set Dedge
boundaryEdgeSet = Set.fromList . boundary

-- | commonBdry des b - returns those directed edges in des that are boundary directed edges of bd
commonBdry:: Set.Set Dedge -> BoundaryState -> Set.Set Dedge
commonBdry des b = des `Set.intersection` boundaryEdgeSet b

{-| boundaryVCovering bd - similar to boundaryECovering, but produces a list of all possible covers of 
    the boundary vertices in bd (rather than just boundary edges).
    [bd should be a boundary state resulting from forcing].
    This can raise an error if bd is a boundary state of an unforced Tgraph.
-}
boundaryVCovering:: BoundaryState -> [BoundaryState]
boundaryVCovering bd = covers [(bd, startbds)] where
  startbds = boundaryEdgeSet bd
  startbvs = boundaryVertexSet bd
--covers:: [(BoundaryState,Set.Set Dedge)] -> [BoundaryState]
  covers [] = []
  covers ((open,es):opens) 
    | Set.null es = case find (\(a,_) -> IntSet.member a startbvs) (boundary open) of
        Nothing -> open:covers opens
        Just de -> covers $ fmap (\b -> (b, es))  (atLeastOne $ tryDartAndKiteForced de open) ++opens
    | otherwise =  covers $ fmap (\b -> (b, commonBdry des b)) (atLeastOne $ tryDartAndKiteForced de open) ++opens  
                   where (de,des) = Set.deleteFindMin es

-- | returns the set of boundary vertices of a BoundaryState
boundaryVertexSet :: BoundaryState -> VertexSet
boundaryVertexSet bd = IntSet.fromList $ fmap fst (boundary bd)

-- | returns the set of internal vertices of a BoundaryState
internalVertexSet :: BoundaryState -> VertexSet
internalVertexSet bd = vertexSet (recoverGraph bd) IntSet.\\ boundaryVertexSet bd

                  
-- | tryDartAndKiteForced de b - returns the list of (2) results after adding a dart (respectively kite)
-- to edge de a forcible b and then tries forcing. Each of the result is a Try.
tryDartAndKiteForced:: Forcible a => Dedge -> a -> [Try a]
tryDartAndKiteForced de b = 
    [ onFail ("tryDartAndKiteForced: Dart on edge: " ++ show de ++ "\n") $ 
        tryAddHalfDart de b >>= tryForce
    , onFail ("tryDartAndKiteForced: Kite on edge: " ++ show de ++ "\n") $ 
        tryAddHalfKite de b >>= tryForce
    ]

-- | tryDartAndKite de b - returns the list of (2) results after adding a dart (respectively kite)
-- to edge de of a Forcible b. Each of the result is a Try.
tryDartAndKite:: Forcible a => Dedge -> a -> [Try a]
tryDartAndKite de b = 
    [ onFail ("tryDartAndKite: Dart on edge: " ++ show de ++ "\n") $ 
        tryAddHalfDart de b
    , onFail ("tryDartAndKite: Kite on edge: " ++ show de ++ "\n") $ 
        tryAddHalfKite de b
    ]


-- | test function to draw a column of the list of graphs resulting from forcedBoundaryVCovering g.
-- drawFBCovering:: Tgraph -> Diagram B
drawFBCovering :: Renderable (Path V2 Double) b =>
                  Tgraph -> Diagram2D b
drawFBCovering g = lw ultraThin $ vsep 1 (draw <$> forcedBoundaryVCovering g)

-- | empire1 g - produces a TrackedTgraph representing the level 1 empire of g.
-- The tgraph of the result is an arbitrarily chosen boundary vertex cover of force g,
-- and the tracked list of the result has the common faces of all the boundary vertex covers (of force g)
-- at the head, followed by the original faces of g.
empire1:: Tgraph -> TrackedTgraph
empire1 g = makeTrackedTgraph g0 [fcs,faces g] where
    (g0:others) = forcedBoundaryVCovering g
    fcs = foldl' intersect (faces g0) $ fmap g0Intersect others
    de = defaultAlignment g
    g0Intersect g1 = commonFaces (g0,de) (g1,de)

-- | empire2 g - produces a TrackedTgraph representing the level 2 empire of g.
-- NB since very large graphs can be generated with boundary vertex covers, we use boundary edge covers only.
-- That is, after finding all boundary edge covers of force g, 
-- boundary edge covers are then found for each boundary edge cover to form a list of doubly-extended
-- boundary edge covers.
-- The tgraph  of the result is an arbitrarily chosen (doubly-extended) boundary edge cover (of force g),
-- and the tracked list of the result has the common faces of all the (doubly-extended) boundary edge covers
-- at the head, followed by the original faces of g.
empire2:: Tgraph -> TrackedTgraph
empire2 g = makeTrackedTgraph g0 [fcs, faces g] where
    covers1 = boundaryECovering $ runTry $ onFail "empire2:Initial force failed (incorrect Tgraph)\n" 
              $ tryForce $ makeBoundaryState g
    covers2 = concatMap boundaryECovering covers1
    (g0:others) = fmap recoverGraph covers2
    fcs = foldl' intersect (faces g0) $ fmap g0Intersect others
    de = defaultAlignment g
    g0Intersect g1 = commonFaces (g0,de) (g1,de)

-- | empire2Plus g - produces a TrackedTgraph representing an extended level 2 empire of g
-- similar to empire2, but using boundaryVCovering insrtead of boundaryECovering.
empire2Plus:: Tgraph -> TrackedTgraph
empire2Plus g = makeTrackedTgraph g0 [fcs, faces g] where
    covers1 = boundaryVCovering $ runTry $ onFail "empire2:Initial force failed (incorrect Tgraph)\n" 
              $ tryForce $ makeBoundaryState g
    covers2 = concatMap boundaryVCovering covers1
    (g0:others) = fmap recoverGraph covers2
    fcs = foldl' intersect (faces g0) $ fmap g0Intersect others
    de = defaultAlignment g
    g0Intersect g1 = commonFaces (g0,de) (g1,de)

-- | drawEmpire1 g - produces a diagram emphasising the common faces of all boundary covers of force g.
-- This is drawn over one of the possible boundary covers and the faces of g are shown in red.
-- drawEmpire1:: Tgraph -> Diagram B
drawEmpire1 :: Renderable (Path V2 Double) b =>
               Tgraph -> Diagram2D b
drawEmpire1 g = 
    drawTrackedTgraph  [ lw ultraThin . draw
                       , lw thin . fillDK lightgrey lightgrey
                       , lw thin . lc red . draw
                       ]  (empire1 g)

-- | drawEmpire2 g - produces a diagram emphasising the common faces of a doubly-extended boundary cover of force g.
-- This is drawn over one of the possible doubly-extended boundary covers and the faces of g are shown in red.
-- drawEmpire2:: Tgraph -> Diagram B
drawEmpire2 :: Renderable (Path V2 Double) b =>
               Tgraph -> Diagram2D b
drawEmpire2 g =
     drawTrackedTgraph  [ lw ultraThin . draw
                        , lw thin . fillDK lightgrey lightgrey
                        , lw thin . lc red . draw
                        ]  (empire2 g)


{-*
Super Force with boundary edge covers
-}

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
                       tryForce fs
           case singleChoiceEdges $ boundaryState forcedFS of
              [] -> return forcedFS
              (pr:_) -> do extended <- addSingle pr forcedFS
                           trySuperForceFS extended
    addSingle (e,l) fs = if isDart l then tryAddHalfDart e fs else tryAddHalfKite e fs

-- |singleChoiceEdges bd - if bd is a boundary state of a forced Tgraph this finds those boundary edges of bd
-- which have a single choice (i.e. the other choice is incorrect), by inspecting boundary edge covers of bd.
-- The result is a list of pairs of (edge,label) where edge is a boundary edge with a single choice
-- and label indicates the choice as the common face label.
singleChoiceEdges :: BoundaryState -> [(Dedge,HalfTileLabel)]
singleChoiceEdges bd = commonToCovering (boundaryECovering bd) (boundary bd)  
  where
-- |commonToCovering bds edgeList - when bds are all the boundary edge covers of some forced Tgraph
-- whose boundary edges were edgeList, this looks for edges in edgeList that have the same tile label added in all covers.
-- This indicates there is a single choice for such an edge (the other choice is incorrect).
-- The result is a list of pairs: edge and a common tile label.
-- commonToCovering :: [BoundaryState] -> [Dedge] -> [(Dedge,HalfTileLabel)]
    commonToCovering bds edgeList = common edgeList (transpose labellists) where
      labellists = fmap (`reportCover` edgeList) bds
      common [] [] = []
      common [] (_:_) = error "singleChoiceEdges:commonToCovering: label list is longer than edge list"
      common (_:_) [] = error "singleChoiceEdges:commonToCovering: label list is shorter than edge list"
      common (e:more) (ls:lls) = if matching ls 
                                 then (e,head ls):common more lls
                                 else common more lls
      matching [] = error "singleChoiceEdges:commonToCovering: empty list of labels" 
      matching (l:ls) = all (==l) ls

-- |reportCover bd edgelist - when bd is a boundary edge cover of some forced Tgraph whose boundary edges are edgelist,
-- this returns the tile label for the face covering each edge in edgelist (in corresponding order).
-- reportCover :: BoundaryState -> [Dedge] -> [HalfTileLabel]
    reportCover bd des = fmap (tileLabel . getf) des where
      efmap = dedgesFacesMap des (allFaces bd) -- more efficient than using graphEFMap?
--      efmap = graphEFMap (recoverGraph bd)
      getf e = maybe (error $ "singleChoiceEdges:reportCover: no face found with directed edge " ++ show e)
                     id
                     (faceForEdge e efmap)
      
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
-- The trails will have disjoint sets of vertices because of the no-crossing-boundaries condition of Tgraphs (and hence BoundaryStates).
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
                           else error $ "findLoops (collectLoops): non looping boundary component, starting at "
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
TrackedTgraphs
-}
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
makeTrackedTgraph g trackedlist = TrackedTgraph{ tgraph = g, tracked = fmap (`intersect` faces g) trackedlist}

-- |trackFaces ttg - pushes the maingraph tilefaces onto the stack of tracked subsets of ttg
trackFaces:: TrackedTgraph -> TrackedTgraph
trackFaces ttg = ttg{ tracked = faces (tgraph ttg):tracked ttg }

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
--    getBoundaryState = getBoundaryState . tgraph
              
-- |addHalfDartTracked ttg e - add a half dart to the tgraph of ttg on the given edge e,
-- and push the new singleton face list onto the tracked list.
addHalfDartTracked:: Dedge -> TrackedTgraph -> TrackedTgraph
addHalfDartTracked e ttg =
  TrackedTgraph{ tgraph = g' , tracked = fcs:tracked ttg}
  where
    g = tgraph ttg
    g' = addHalfDart e g
    fcs = faces g' \\ faces g

-- |addHalfKiteTracked ttg e - add a half kite to the tgraph of ttg on the given edge e,
-- and push the new singleton face list onto the tracked list.
addHalfKiteTracked:: Dedge -> TrackedTgraph -> TrackedTgraph
addHalfKiteTracked e ttg =
  TrackedTgraph{ tgraph = g' , tracked = fcs:tracked ttg}
  where
    g = tgraph ttg
    g' = addHalfKite e g
    fcs = faces g' \\ faces g

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
    tlist = fmap (concatMap (decompFace newVFor)) (tracked ttg)

{-*  Drawing TrackedTgraphs
-}                                          

{-|
    To draw a TrackedTgraph, we use a list of functions each turning a VPatch into a diagram.
    The first function is applied to a VPatch for untracked faces.
    Subsequent functions are applied to VPatches for the respective tracked subsets.
    Each diagram is atop earlier ones, so the diagram for the untracked VPatch is at the bottom.
    The VPatches are all restrictions of a single VPatch for the Tgraph, so consistent.
    drawTrackedTgraph:: [VPatch -> Diagram B] -> TrackedTgraph -> Diagram B
-}
drawTrackedTgraph :: Renderable (Path V2 Double) b =>
                     [VPatch -> Diagram2D b] -> TrackedTgraph -> Diagram2D b
drawTrackedTgraph drawList ttg = mconcat $ reverse $ zipWith ($) drawList vpList where
    vp = makeVP (tgraph ttg)
    untracked = vpFaces vp \\ concat (tracked ttg)
    vpList = fmap (restrictVP vp) (untracked:tracked ttg)

{-|
    To draw a TrackedTgraph rotated.
    Same as drawTrackedTgraph but with additional angle argument for the rotation.
    This is useful for when labels are being drawn.
    The angle argument is used to rotate the common vertex location map before drawing
    (to ensure labels are not rotated).
    drawTrackedTgraphRotated:: [VPatch -> Diagram B] -> Angle Double -> TrackedTgraph -> Diagram B
-}
drawTrackedTgraphRotated :: Renderable (Path V2 Double) b =>
                            [VPatch -> Diagram2D b] -> Angle Double -> TrackedTgraph -> Diagram2D b
drawTrackedTgraphRotated drawList a ttg = mconcat $ reverse $ zipWith ($) drawList vpList where
    vp = rotate a $ makeVP (tgraph ttg)
    untracked = vpFaces vp \\ concat (tracked ttg)
    vpList = fmap (restrictVP vp) (untracked:tracked ttg)




 