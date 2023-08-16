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
experimental combinations such as emplace, boundary covers, boundary loops.
It also includes experimental SubTgraphs (used for tracking subsets of faces of a Tgraph).
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

import Data.List (intersect, union, (\\), find, foldl',nub, transpose)      
import qualified Data.Set as Set  (Set,fromList,member,null,intersection,deleteFindMin,map,delete,insert,toList)-- used for boundary covers
import qualified Data.IntSet as IntSet (IntSet,fromList,isSubsetOf,intersection,null,member,notMember,(\\)) -- for boundary vertex set

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


-- |same as draw except adding dashed lines on boundary join edges. 
drawSmart :: Tgraph -> Diagram B
drawSmart g = drawSmartSub g $ makeVP g

-- |same as drawLabelled except adding dashed lines on boundary join edges.
drawSmartLabelled :: Tgraph -> Diagram B
drawSmartLabelled g = drawSmartLabelledSub g $ makeVP g

-- |same as drawLabelled except adding dashed lines on boundary join edges.
drawSmartLabelledRotated :: Tgraph -> Angle Double -> Diagram B
drawSmartLabelledRotated g a = drawSmartLabelledSub g $ rotate a $ makeVP g

-- |drawSmartAligned (v1,v2) g - same as drawSmart g except except aligning with centre on v1 and v2
-- on positive x axis. This will raise an error if v1 or v2 are not vertices of g.
drawSmartAligned :: (Vertex,Vertex) -> Tgraph -> Diagram B
drawSmartAligned vs g = drawSmartSub g $ alignXaxis vs $ makeVP g

-- |drawSmartLabelledAligned (v1,v2) g - same as drawSmartLabelled g except except aligning with centre on v1 and v2
-- on positive x axis. This will raise an error if v1 or v2 are not vertices of g.
drawSmartLabelledAligned :: (Vertex,Vertex) -> Tgraph -> Diagram B
drawSmartLabelledAligned vs g = drawSmartLabelledSub g $ alignXaxis vs $ makeVP g


-- |drawSmartSub g vp converts g to a diagram (without vertex labels).
-- It requires vp to contain a suitable vertex location map for drawing g.
-- This can be used instead of drawSmart when such a map is already available.
drawSmartSub:: Tgraph -> VPatch -> Diagram B
drawSmartSub g vp = (drawWith dashjOnly $ subVP vp $ boundaryJoinFaces g) 
                    <> 
                    draw (subVP vp (faces g))

-- |drawSmartLabelledSub g vp converts g to a diagram with vertex labels.
-- It requires vp to contain a suitable vertex location map for drawing g.
-- This can be used instead of drawSmartLabelled when a suitable VPatch is already available.
drawSmartLabelledSub:: Tgraph -> VPatch -> Diagram B
drawSmartLabelledSub g vp = (drawWith dashjOnly $ subVP vp $ boundaryJoinFaces g) 
                            <> 
                            drawLabelled (subVP vp (faces g))

-- |select the halftile faces of a Tgraph with a join edge on the boundary.
-- Useful for drawing join edges only on the boundary.
boundaryJoinFaces :: Tgraph -> [TileFace]
boundaryJoinFaces g = fmap snd $ incompleteHalves bdry $ boundary bdry where
    bdry = makeBoundaryState g


{-*
Overlaid drawing tools for Tgraphs
-}

-- |applies partCompose to a Tgraph g, then draws the composed graph with the remainder faces (in lime).
-- (Relies on the vertices of the composition and remainder being subsets of the vertices of g.)
drawPCompose ::  Tgraph -> Diagram B
drawPCompose g = (drawSmartSub g' vp)
                 <> (lw thin $ lc lime $ drawj $ subVP vp remainder)
  where (remainder,g') = partCompose g
        vp = makeVP g

-- |drawForce g is a diagram showing the argument g in red overlayed on force g
-- It adds dashed join edges on the boundary of g
drawForce:: Tgraph -> Diagram B
drawForce g = (dg # lc red # lw thin) <> dfg where
    fg = force g
    vp = makeVP fg
    dfg = draw vp
    dg = drawSmartSub g vp

-- |drawSuperForce g is a diagram showing the argument g in red overlayed on force g in black
-- overlaid on superForce g in blue.
-- It adds dashed join edges on the boundary of g
drawSuperForce:: Tgraph -> Diagram B
drawSuperForce g = (dg # lc red) <> dfg <> (dsfg # lc blue) where
    vp = makeVP $ superForce g
    dfg = drawSmartSub (force g) vp
    dg = drawSmartSub g vp
    dsfg = draw vp
{-|
drawWithMax g - draws g and overlays the maximal composition of g in red.
This may raise an error if any of the compositions of g upto the maximal one are invalid Tgraphs
(e.g. not tile connected).
-}
drawWithMax :: Tgraph -> Diagram B
drawWithMax g =  (dmax # lc red # lw thin) <> dg where
    vp = makeVP g
    dg = draw vp
    maxg = maxComp g
    dmax = draw $ subVP vp $ faces maxg

-- |displaying the boundary of a Tgraph in lime (overlaid on the Tgraph drawn with labels)
drawGBoundary :: Tgraph -> Diagram B
drawGBoundary g =  (drawEdgesIn vp edges # lc lime) <> drawLabelled vp where
    vp  = makeVP g
    edges = graphBoundary g

-- |drawCommonFaces (g1,e1) (g2,e2) uses commonFaces (g1,e1) (g2,e2) to find the common faces
-- and emphasizes them on the background g1
drawCommonFaces:: (Tgraph,Dedge) -> (Tgraph,Dedge) -> Diagram B
drawCommonFaces (g1,e1) (g2,e2) = emphasizeFaces (commonFaces (g1,e1) (g2,e2)) g1

-- |emphasizeFaces fcs g emphasizes the given faces (that are in g) overlaid on the background g.
emphasizeFaces:: [TileFace] -> Tgraph -> Diagram B
emphasizeFaces fcs g =  (drawj emphvp # lw thin) <> (draw vp # lw ultraThin) where
    vp = makeVP g
    emphvp = subVP vp (fcs `intersect` faces g)


 
      

{-*
Combining force, compose, decompose
-}


-- | An experimental version of composition which defaults to kites when there are choices (unknowns).
-- This is unsafe in that it can create an incorrect Tgraph from a correct Tgraph.
composeK :: Tgraph -> Tgraph
composeK = snd . partComposeK where
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

-- |compForced does a force then compose.
-- It omits the check for connected, and no crossing boundaries because the argument is forced first.
-- This relies on a proof that composition does not need to be checked for a forced Tgraph.
compForced:: Tgraph -> Tgraph
compForced = snd . uncheckedPartCompose . force 
--compForced = compose . force

-- |force after a decomposition
forcedDecomp:: Tgraph -> Tgraph
forcedDecomp = force . decompose
        
-- |allCompForced g produces a list of all forced compositions starting from g up to but excluding the empty Tgraph.
-- This definition relies on (1) a proof that the composition of a forced Tgraph is forced  and
-- (2) a proof that composition does not need to be checked for a forced Tgraph.
allCompForced:: Tgraph -> [Tgraph]
allCompForced g = takeWhile (not . nullGraph) $ g: (iterate (snd . uncheckedPartCompose) $ compForced g) 
-- allCompForced = takeWhile (not . nullGraph) . iterate compForced

-- | produces an infinite list of forced decompositions
allForcedDecomps:: Tgraph -> [Tgraph]
allForcedDecomps = iterate forcedDecomp

-- |maxCompForced produces a maximally composed forced graph.
maxCompForced:: Tgraph -> Tgraph
maxCompForced = last . allCompForced

-- |maxComp may produce a maximally composed graph, but may raise an error if any intermediate composition 
-- is not a valid Tgraph.
maxComp:: Tgraph -> Tgraph
maxComp = last . allComp

-- |allComp g may produce a list of all compositions starting from g up to but excluding the empty Tgraph,
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
forcedBoundaryECovering g = fmap recoverGraph $ boundaryECovering gforcedBdry where
     gforcedBdry = runTry $ onFail "forcedBoundaryECovering:Initial force failed (incorrect Tgraph)\n" $
                             tryForceBoundary $ makeBoundaryState g

{-| forcedBoundaryVCovering g - produces a list of all boundary covers of force g as with
forcedBoundaryECovering g but covering all boundary vertices rather than just boundary edges.                        
-}
forcedBoundaryVCovering:: Tgraph -> [Tgraph]
forcedBoundaryVCovering g = fmap recoverGraph $ boundaryVCovering gforcedBdry where
     gforcedBdry = runTry $ onFail "forcedBoundaryVCovering:Initial force failed (incorrect Tgraph)\n" $
                             tryForceBoundary $ makeBoundaryState g

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
                             (atLeastOne $ tryDartAndKite bs de)

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
        Just de -> covers $ fmap (\b -> (b, es))  (atLeastOne $ tryDartAndKite open de) ++opens
    | otherwise =  covers $ fmap (\b -> (b, commonBdry des b)) (atLeastOne $ tryDartAndKite open de) ++opens  
    where (de,des) = Set.deleteFindMin es
                  
-- | tryDartAndKite b de - returns the list of (2) results after adding a dart (respectively kite)
-- to edge de on boundary state b and forcing. Each result is a Try.
tryDartAndKite:: BoundaryState -> Dedge -> [Try BoundaryState]
tryDartAndKite b de = 
    [ onFail ("tryDartAndKite: Dart on edge: " ++ show de ++ "\n") $ 
        tryAddHalfDartBoundary de b >>= tryForceBoundary
    , onFail ("tryDartAndKite: Kite on edge: " ++ show de ++ "\n") $ 
        tryAddHalfKiteBoundary de b >>= tryForceBoundary
    ]

-- | test function to draw a column of the list of graphs resulting from forcedBoundaryVCovering g
drawFBCovering:: Tgraph -> Diagram B
drawFBCovering g = lw ultraThin $ vsep 1 $ 
     fmap draw $ forcedBoundaryVCovering g

-- | empire1 g - produces a SubTgraph representing the level 1 empire of g.
-- The tgraph of the result is an arbitrarily chosen boundary vertex cover of force g,
-- and the tracked list of the result has the common faces of all the boundary vertex covers (of force g)
-- at the head, followed by the original faces of g.
empire1:: Tgraph -> SubTgraph
empire1 g = makeSubTgraph g0 [fcs,faces g] where
    (g0:others) = forcedBoundaryVCovering g
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
    covers1 = boundaryECovering $ runTry $ onFail "empire2:Initial force failed (incorrect Tgraph)\n" 
              $ tryForceBoundary $ makeBoundaryState g
    covers2 = concatMap boundaryECovering covers1
    (g0:others) = fmap recoverGraph covers2
    fcs = foldl intersect (faces g0) $ fmap g0Intersect others
    de = lowestJoin (faces g)
    g0Intersect g1 = commonFaces (g0,de) (g1,de)


-- | empire2Plus g - produces a SubTgraph representing an extended level 2 empire of g
-- similar to empire2, but using boundaryVCovering insrtead of boundaryECovering.
-- On a kinGraph this currently takes about 4 hours 20 minutes.
empire2Plus:: Tgraph -> SubTgraph
empire2Plus g = makeSubTgraph g0 [fcs, faces g] where
    covers1 = boundaryVCovering $ runTry $ onFail "empire2:Initial force failed (incorrect Tgraph)\n" 
              $ tryForceBoundary $ makeBoundaryState g
    covers2 = concatMap boundaryVCovering covers1
    (g0:others) = fmap recoverGraph covers2
    fcs = foldl intersect (faces g0) $ fmap g0Intersect others
    de = lowestJoin (faces g)
    g0Intersect g1 = commonFaces (g0,de) (g1,de)

-- | drawEmpire1 g - produces a diagram emphasising the common faces of all boundary covers of force g.
-- This is drawn over one of the possible boundary covers and the faces of g are shown in red.
drawEmpire1:: Tgraph -> Diagram B
drawEmpire1 g = drawSubTgraph  [ lw ultraThin . draw
                               , lw thin . drawWith (fillDK lightgrey lightgrey)
                               , lw thin . lc red . draw
                               ]  (empire1 g)

-- | drawEmpire2 g - produces a diagram emphasising the common faces of a doubly-extended boundary cover of force g.
-- This is drawn over one of the possible doubly-extended boundary covers and the faces of g are shown in red.
drawEmpire2:: Tgraph -> Diagram B
drawEmpire2 g = drawSubTgraph  [ lw ultraThin . draw
                               , lw thin . drawWith (fillDK lightgrey lightgrey)
                               , lw thin . lc red . draw
                               ]  (empire2 g)

{-*
Contexts for (forced) Boundary Vertices and Edges
-}

{-
forcedBEContexts:: Dedge -> BoundaryState -> [BoundaryState]
forcedBEContexts bde bd = extend [] (locals [] [bd]) where
--locals:: [BoundaryState] -> [BoundaryState]  -> [BoundaryState]
-- locals produces the cases where 1 edge either side of bde is changed
  locals bds [] = reverse bds
  locals bds (open:opens) | not (Set.member bde (Set.fromList (boundary open))) = locals bds opens
  locals bds (open:opens) | occursIn bds open bde = locals bds opens
  locals bds (open:opens) | otherwise = 
     locals (open:bds) (concatMap (atLeastOne . tryDartAndKite open)
                                  (boundaryEdgeNbs bde open)
                        ++ opens)
--extend:: [BoundaryState] -> [BoundaryState]  -> [BoundaryState]
-- extend adds cases for other boundary edges except those either side of e
  extend done [] = reverse done
  extend done (c:more) | occursIn done c bde = extend done more 
                       | not (nullGraph (compose (recoverGraph c))) = extend (c:done) more
                       | otherwise 
    = extend (c:done) (stillB (concatMap (atLeastOne . tryDartAndKite c) (remoteBes c)) ++ more)
  remoteBes c = boundary c \\ (bde:boundaryEdgeNbs bde c)
  stillB = filter (\bd -> Set.member bde $ Set.fromList $ boundary bd)
-}

{- |forcedBEContexts e bd - 
assumes bd to be a BoundaryState of a forced Tgraph and edge to be a boundary edge of bd.
It calculates all possible face additions either side of the edge,
forcing each case and discarding results where the edge is no longer on the boundary.
It then generates further contexts for those cases by
by making additions round the rest of the boundary.
Repetitions are removed using 'sameGraph'.
The resulting contexts are returned as a list of BoundaryStates.      
-}
forcedBEContexts:: Dedge -> BoundaryState -> [BoundaryState]
forcedBEContexts edge bd = contexts [] $ fmap setup $ locals [] [bd] where
-- after applying locals this setsup cases for processing by contexts
  setup bd = (bd, Set.delete edge $ boundaryEdgeSet bd)
--locals:: [BoundaryState] -> [BoundaryState]  -> [BoundaryState]
-- locals produces the cases where 1 edge either side of bde is changed
  locals bds [] = reverse bds
  locals bds (open:opens) | not (Set.member edge (boundaryEdgeSet open)) = locals bds opens
  locals bds (open:opens) | occursIn bds open edge = locals bds opens
  locals bds (open:opens) | otherwise = 
     locals (open:bds) (concatMap (atLeastOne . tryDartAndKite open)
                                  (boundaryEdgeNbs edge open)
                        ++ opens)
  contexts done [] = reverse done
  contexts done ((bs,es):opens) 
    | occursIn done bs edge = contexts done opens
    | not (Set.member edge (boundaryEdgeSet bs)) = contexts done opens
    | nullGraph $ compose $ recoverGraph bs
          = let newcases = concatMap (makecases (bs,es)) (boundary bs \\ [edge])
            in  contexts (bs:done) $ (newcases++opens)
    | Set.null es = contexts (bs:done) opens
    | otherwise = contexts (bs:done) (newcases ++ opens)
        where newcases = concatMap (makecases (bs,es)) (Set.toList es)
  makecases (bs,es) de = fmap attachEdgeSet (atLeastOne $ tryDartAndKite bs de)
    where attachEdgeSet b = (b, commonBdry (Set.delete de es) b)

-- | occursIn bds b e - asks if (the Tgraph of) b occurs in the list (of Tgraphs of) bds
-- after relabelling to match edge e in each case.
-- Actually e must be a boundary directed edge, so gets reversed before matching.
occursIn:: [BoundaryState] -> BoundaryState -> Dedge -> Bool
occursIn bds b e
  = any (sameGraph (recoverGraph b,edge)) (fmap (\bd -> (recoverGraph bd,edge)) bds)
    where edge = reverseD e


-- |boundaryEdgeNbs bde b - returns the list of 2 directed boundary edges either side
--  of the directed boundary edge bde in BoundaryState b.
-- It raises an error if bde is not a boundary edge of b.
boundaryEdgeNbs:: Dedge -> BoundaryState -> [Dedge]
boundaryEdgeNbs (a,b) bd = boundaryEdgesWith [a,b] bd \\ [(a,b)]

-- |boundaryEdgesAt v bd - returns boundary edges with vertex v in BoundaryState bd.
boundaryEdgesAt:: Vertex -> BoundaryState -> [Dedge]
boundaryEdgesAt v = boundaryEdgesWith [v] 
--boundaryEdgesAt v = filter (\(x,y) -> x==v || y==v) . boundary 

-- |boundaryEdgesWith vs bd - returns all boundary edges with vertices in vs in BoundaryState bd.
boundaryEdgesWith:: [Vertex] -> BoundaryState -> [Dedge]
boundaryEdgesWith vs = filter (\(x,y) -> x `elem` vs || y `elem` vs) . boundary 

{-
-- |boundaryButOne v bd - returns the list of 2 directed boundary edges that are
-- one step away from the boundary directed edges either side of v in BoundaryState bd.
boundaryButOne:: Vertex -> BoundaryState -> [Dedge]
boundaryButOne v bd = affectedBoundary bd es \\ es where
  es = boundaryEdgesAt v bd
-}


{-
-- |forcedBVContexts v e bd - where bd is a boundary state of a forced Tgraph,
-- e is a boundary directed edge of bd, and v is one of the vertices of edge e.
-- This will generate the possible boundary contexts of v.
-- It first generates singleChoiceEdges of bd by adding kite/dart on the boundary either side of v and forcing,
-- and then generates further singleChoiceEdges in each case with the next 2 nearest boundary edges to v.
-- Any case where v is no longer on the boundary is excluded in each case.
-- The edge argument is necessary for doing 'sameGraph' comparisons to remove repetitions. 
forcedBVContexts:: Vertex -> Dedge -> BoundaryState -> [BoundaryState]
forcedBVContexts x (a,b) bd 
  | x/=a && x/=b = error $ ":vertex " ++ show x ++ " must be from edge " ++ show (a,b)
  | otherwise = extend x [] (locals x [] [bd]) where
--    locals:: Vertex -> [BoundaryState] -> [BoundaryState]  -> [BoundaryState]
      locals x bds [] = reverse bds
      locals x bds (open:opens) | not (IntSet.member x $ boundaryVertexSet open) = locals x bds opens
      locals x bds (open:opens) | occursIn bds open (a,b) = locals x bds opens
      locals x bds (open:opens) | otherwise = 
          locals x (open:bds) (concatMap (atLeastOne . tryDartAndKite open)
                                         (boundaryEdgesAt x open)
                              ++ opens)
--    extend:: Vertex -> [BoundaryState] -> [BoundaryState]  -> [BoundaryState]
      extend x done [] = reverse done
      extend x done (c:more) | occursIn done c (a,b) = extend x done more 
                             | otherwise 
         = extend x (c:done) (stillB x (concatMap (atLeastOne . tryDartAndKite c) (drawL x c)) ++ more)
      stillB v = filter (\bd -> v `IntSet.member` boundaryVertexSet bd)
-}

-- |forcedBVContexts v e bd - where bd is a boundary state of a forced Tgraph,
-- e is a boundary directed edge of bd, and v is one of the vertices of edge e.
-- This will generate the possible boundary contexts of v in a forced Tgraph.
-- It first generates local cases for bd by repeatedly adding kite/dart on the boundary either side of v and forcing,
-- It then generates further contexts in each case by adding all possibilities to the rest of the boundary.
-- Any case where v is no longer on the boundary is excluded in each case.
-- The edge argument is necessary for doing 'sameGraph' comparisons to remove repetitions. 
forcedBVContexts:: Vertex -> Dedge -> BoundaryState -> [BoundaryState]
forcedBVContexts x edge bStart 
  | x/= fst edge && x/= snd edge = error $ ":vertex " ++ show x ++ " must be from edge " ++ show edge
  | otherwise =  contexts [] $ fmap setup $ (locals [] [bStart]) where
-- after applying locals this sets up cases for processing by contexts
      setup bs = (bs, boundaryEdgeSet bs)
--    locals:: Vertex -> [BoundaryState] -> [BoundaryState]  -> [BoundaryState]
      locals bds [] = reverse bds
      locals bds (open:opens) | not (IntSet.member x $ boundaryVertexSet open) = locals bds opens
      locals bds (open:opens) | occursIn bds open edge = locals bds opens
      locals bds (open:opens) | otherwise = 
          locals (open:bds) (concatMap (atLeastOne . tryDartAndKite open)
                                         (boundaryEdgesAt x open)
                              ++ opens)
      contexts done [] = reverse done
      contexts done ((bs,es):opens) 
        | occursIn done bs edge = contexts done opens
        | not (x `IntSet.member` boundaryVertexSet bs) = contexts done opens
        | nullGraph $ compose $ recoverGraph bs
          = let newcases = concatMap (makecases (bs,es)) (boundary bs)
            in  contexts (bs:done) $ (newcases++opens)
        | Set.null es = contexts (bs:done) opens
        | otherwise = contexts (bs:done) (newcases ++ opens)
            where newcases = concatMap (makecases (bs,es)) (Set.toList es)
      makecases (bs,es) de = fmap attachEdgeSet (atLeastOne $ tryDartAndKite bs de)
        where attachEdgeSet b = (b, commonBdry (Set.delete de es) b)
    
-- | returns the set of boundary vertices of a BoundaryState
boundaryVertexSet :: BoundaryState -> VertexSet
boundaryVertexSet bd = IntSet.fromList $ fmap fst (boundary bd)

-- | returns the set of internal vertices of a BoundaryState
internalVertexSet :: BoundaryState -> VertexSet
internalVertexSet bd = vertexSet (recoverGraph bd) IntSet.\\ boundaryVertexSet bd


{-*
Super Force with boundary edge covers
-}

-- |superForce g -after forcing g this looks for single choice boundary edges.
-- That is a boundary edge for which only a dart or only a kite addition occurs in all boundary edge covers.
-- If there is at least one such edge, it makes the choice for the first such edge and recurses,
-- otherwise it returns the forced Tgraph (force g).
-- This will raise an error if force encounters a stuck (incorrect) Tgraph or if
-- both forced extensions fail for some boundary edge.
-- Otherwise, the resulting Tgraph has exactly two correct possible extensions for each boundary edge.
superForce :: Tgraph -> Tgraph
superForce g = runTry $ trySuperForce g

-- |superForceBdry - same as superForce but for boundary states
superForceBdry :: BoundaryState -> BoundaryState
superForceBdry = runTry . trySuperForceBdry

-- |trySuperForce g - this looks for single choice edges after trying to force g.
-- If there is at least one, it makes that choice and recurses.
-- It returns a Left s if force fails or if both choices fail for some edge (where s is a failure report).
-- Otherwise Right g' is returned where g' is the super forced g.
trySuperForce :: Tgraph -> Try Tgraph
trySuperForce g = do bd <- trySuperForceBdry (makeBoundaryState g)
                     return (recoverGraph bd)

-- |trySuperForceBdry - same as trySuperForce but for boundary states
trySuperForceBdry :: BoundaryState -> Try BoundaryState
trySuperForceBdry bd = 
    do forcebd <- onFail "trySuperForceBdry: force failed (incorrect Tgraph)\n" $
                  tryForceBoundary bd
       case singleChoiceEdges forcebd of
          [] -> return forcebd
          (pr:_) -> do extended <-  addHT pr forcebd
                       trySuperForceBdry extended
  where
    addHT (e,l) fbd = if isDart l then tryAddHalfDartBoundary e fbd else tryAddHalfKiteBoundary e fbd

-- |singleChoiceEdges bd - if bd is a boundary state of a forced Tgraph this finds those boundary edges of bd
-- which have a single correct choice, by inspecting boundary edge covers of bd.
-- The result a list of pairs of (edge,label) where edge is a boundary edge with a single choice
-- and label indicates the choice as the common face label.
singleChoiceEdges :: BoundaryState -> [(Dedge,HalfTileLabel)]
singleChoiceEdges bd = commonToCovering (boundaryECovering bd) (boundary bd)  

-- |commonToCovering bds edges - when bds are all the boundary edge covers of some forced Tgraph
-- whose boundary edges are edgelist, this looks for edges in edgelist that have the same tile label added in all covers.
-- This indicates there is a single correct choice for such an edge.
-- The result is a list of pairs: edge and a common tile label.
commonToCovering :: [BoundaryState] -> [Dedge] -> [(Dedge,HalfTileLabel)]
commonToCovering bds edges = common edges (transpose labellists) where
    labellists = fmap (\bd -> reportCover bd edges) bds
    common [] lls = []
    common (e:more) (l:ls) = if matching l 
                             then (e,head l):common more ls
                             else common more ls
    matching [] = error "commonToCovering: empty list of labels" 
    matching (l:ls) = all (==l) ls

-- |reportCover bd edgelist - when bd is a boundary edge cover of some forced Tgraph whose boundary edges are edgelist,
-- this returns the tile label for the face covering each edge in edgelist (in corresponding order).
reportCover :: BoundaryState -> [Dedge] -> [HalfTileLabel]
reportCover bd edgelist = fmap (\e -> tileLabel (getf e)) edgelist where
    efmap = edgeFaceMap (recoverGraph bd)
    getf e = maybe (error $ "reportCover: no face found for edge " ++ show e)
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
 A SubTgraph has a main Tgraph (tgraph) and a list of subsets of faces (tracked).
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
pushFaces sub = sub{ tracked = faces (tgraph sub):tracked sub }

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
    To draw a SubTgraph, we use a list of functions each turning a VPatch into a diagram.
    The first function is applied to a VPatch for untracked faces
    Subsequent functions are applied to VPatches for the respective tracked subsets.
    Each diagram is atop earlier ones, so the diagram for the untracked VPatch is at the bottom.
    The VPatches will all have been made from the same VPatch vertex location map so will be aligned/scaled
    appropriately.
-}
drawSubTgraph:: [VPatch -> Diagram B] -> SubTgraph -> Diagram B
drawSubTgraph drawList sub = mconcat $ reverse $ zipWith ($) drawList vpList where
    vp = makeVP (tgraph sub)
    untracked = vpFaces vp \\ concat (tracked sub)
    vpList = fmap (subVP vp) (untracked:tracked sub)

{-
-- |drawing non tracked faces only
drawWithoutTracked:: SubTgraph -> Diagram B
drawWithoutTracked sub = drawSubTgraph [draw] sub
-}


{-|
    To draw a SubTgraph rotated.
    Same as drawSubTgraph but with additional angle argument for the rotation.
    The angle argument is used to rotate the common vertex location map before drawing
    (to ensure labels are not rotated).
-}
drawSubTgraphRotated:: [VPatch -> Diagram B] -> Angle Double -> SubTgraph -> Diagram B
drawSubTgraphRotated drawList a sub = mconcat $ reverse $ zipWith ($) drawList vpList where
    vp = rotate a $ makeVP (tgraph sub)
    untracked = vpFaces vp \\ concat (tracked sub)
    vpList = fmap (subVP vp) (untracked:tracked sub)




 