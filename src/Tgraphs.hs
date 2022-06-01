{-|
Module      : Tgraphs
Description : Collects and exports the various Tgraph modules 
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This is the main module for Tgraphs which collects and exports the various other Tgraph modules 
and includes a definition of emplace and other experimental combinations.
-}
module Tgraphs ( module Tgraphs
               , module Tgraph.Prelude
               , module Tgraph.Decompose
               , module Tgraph.Compose
               , module Tgraph.Force
               , module Tgraph.Convert
               ) where

-- WAS  JUST import Data.List (intersect)
import Data.List (intersect, (\\), union,find,nub,partition)
import qualified Data.Map as Map (Map, lookup, insert, empty, fromList,union)

import Tgraph.Prelude
import Tgraph.Decompose
import Tgraph.Compose
import Tgraph.Force
import Tgraph.Convert

{----------------------------
********************************************
EXPERIMENTAL BITS
********************************************
------------------------------}

{----------------------------
EMPLACEMENTS
------------------------------}

-- |emplace does maximal composing with force and composeG, 
-- then applies decomposeG and force repeatedly back to the starting level.
-- It produces the emplacement of influence of the argument graph.   
emplace:: Tgraph -> Tgraph
emplace g = if nullGraph g'
            then fg 
            else (force . decomposeG . emplace) g'
    where fg = force g
          g' = composeG fg 
            
-- |emplacements is best supplied with a maximally composed or near maximally composed graph
-- It produces an infinite list of emplacements of the starting graph and its decompositions.
emplacements :: Tgraph -> [Tgraph]
emplacements = iterate (force . decomposeG) . emplace -- was .force

{-------------------------------------------------------------------------
 makeChoices, emplaceChoices
------------------------------------------------------------------------------}

-- |a version of emplace using makeChoices at the top level.
-- after makeChoices we use emplace to attempt further compositions but with no further choices
emplaceChoices:: Tgraph -> [Tgraph]
emplaceChoices g = 
       let fg = force g
           g' = composeG fg 
       in
           if nullGraph g'
           then emplace <$> makeChoices g
           else force . decomposeG <$> emplaceChoices g'
                                 
{-| makeChoices is a temporary tool which does not attempt to analyse choices for correctness.
It can thus create some choices which will be incorrect.
The unknowns returned from classifyDartWings can become largeKiteCentres or largeDartBases.
This produces 2^n choices where n is the number of unknowns (excluding lone dart wing tips with valencyD 2).
-}
makeChoices :: Tgraph -> [Tgraph]
makeChoices g = choices unks [g] where
    unks = filter ((>2).valencyD g) (unknowns (classifyDartWings g))
    choices [] gs = gs
    choices (v:more) gs = choices more (fmap (forceLKC v) gs ++ fmap (forceLDB v) gs)              



{-------------------------------------------------------------------------
 compForce, allCompFs, allComps, maxCompose, maxFCompose
------------------------------------------------------------------------------}

-- |compForce does a force then composeG but it
-- by-passes the check on the composed graph because the argument was forced
compForce:: Tgraph -> Tgraph
compForce = snd . partCompose . force
    
    
-- |allCompFs g produces a list of all forced compositions starting from g up to but excluding the empty graph
allCompFs:: Tgraph -> [Tgraph]
allCompFs g = takeWhile (not . nullGraph) $ iterate compForce g

-- |allComps g produces a list of all compositions starting from g up to but excluding the empty graph.
-- This is not safe in general as it can fail by producing
-- a non-connected graph or graph with crossing boundaries.
allComps:: Tgraph -> [Tgraph]
allComps g = takeWhile (not . nullGraph) $ iterate composeG g


-- |maxCompose and maxFCompose produce a maximal graph.
maxCompose, maxFCompose:: Tgraph -> Tgraph
maxCompose g = last $ allComps g
maxFCompose g = force $ last $ allCompFs g

-- |remove halftile faces that do not have their matching half tile
removeIncompleteTiles:: Tgraph -> Tgraph
removeIncompleteTiles g = removeFaces halfTiles g
       where bdry = makeBoundary g
             halfTiles = fmap snd $ incompleteHalves bdry $ bDedges bdry
 

{-
******************** TESTING OUT RELABELLING *****************************
-}
         
-- |Relabelling is a special case of mappings from vertices to vertices.
-- We use the identity map for vertices not found in the mapping keys
-- (see relabelV)       
type Relabelling = Map.Map Vertex Vertex
-- |Uses a vertex to vertex mapping to change vertices in a Tgraph
-- relabelGraph vmap g will produce a valid Tgraph provided:
-- g is a valid Tgraph, and
-- the mapping vmap is 1-1 with no output vertices in common with the vertices of g.
-- Vertices of g that are not in the domain of the mapping are left unchanged.
relabelGraph:: Relabelling -> Tgraph -> Tgraph
relabelGraph vmap g = checkedTgraph newFaces where
   newFaces = fmap (relabelFace vmap) (faces g) 

-- |Uses a vertex to vertex mapping to relabelxf the three vertices of a face
-- Any vertex not in the domain of the mapping is left unchanged.
-- The mapping should be 1-1 to avoid creating a self loop edge.
relabelFace:: Relabelling -> TileFace -> TileFace
relabelFace vmap = fmap (all3 (relabelV vmap))  -- fmap of HalfTile Functor

-- |relabelV vmap v. Use mapping vmap to find a replacement for v (leave as v if none found)
-- I.e relabelV turns a Mapping into a total function using identity for undefined cases. 
relabelV:: Relabelling -> Vertex -> Vertex
relabelV vmap v = maybe v id (Map.lookup v vmap)

-- |Applies a function to all three elements of a triple
all3:: (a -> b) -> (a,a,a) -> (b,b,b)
all3 f (a,b,c) = (f a,f b,f c)

-- |relabelNewExcept fixed avoid g produces a new Tgraph from g by relabelling.
-- Any vertex in g that is in the list fixed will remain unchanged.
-- All Other vertices in g that are in the list avoid will be changed to new vertices that are
-- neither in g nor in the list avoid nor in the list fixed.
-- (If a vertex in g appears in both fixed and avoid, it will remain unchanged) 
relabelNewExcept:: [Vertex] -> [Vertex] -> Tgraph -> Tgraph
relabelNewExcept fixed avoid g = relabelGraph vmap g where
  avoidWithoutFixed = avoid \\ fixed
  avoidAndFixed = avoid `union` fixed
  vertsToChange = vertices g `intersect` avoidWithoutFixed
  vmap = newVMapAvoid avoidAndFixed vertsToChange

-- |newVMapAvoid avoid vs - produces a 1-1 mapping from vertices in vs to new vertices
-- such that no new vertex is in the list avoid or in vs
newVMapAvoid:: [Vertex] -> [Vertex] -> Relabelling
newVMapAvoid avoid vs = Map.fromList $ zip vs newvs
  where avoid' = vs `union` avoid
        newvs = makeNewVs (length vs) avoid'

{-|relabelByCommonEdge g1 (x,y) g2  produces a relabeled version of g2 that is
consistent with g1 on their overlap.
The overlaping region must contain the common directed edge (x,y) without relabelling.

CAVEAT: The overlap must be a SINGLE tile-connected region in g1.
(If the overlap contains more than one tile-connected region the result may not be
a correct relabelling of g2)    
-}
relabelByCommonEdge:: Tgraph -> DEdge -> Tgraph -> Tgraph
relabelByCommonEdge g1 (x,y) g2 = relabelByEdges (g1,(x,y)) (g2,(x,y))

{-|rrelabelByEdges (g1,e1) (g2,e2)  produces a relabeled version of g2 that is
consistent with g1 on their overlap.
The overlaping region must contain the directed edge e1 in g1 and e2 in g2,
which will be identified by the relabelling of g2.

CAVEAT: The overlap must be a SINGLE tile-connected region in g1.
(If the overlap contains more than one tile-connected region the result may not be
a correct relabelling of g2)    
-}
relabelByEdges:: (Tgraph,DEdge) -> (Tgraph,DEdge) -> Tgraph
relabelByEdges (g1,(x1,y1)) (g2,(x2,y2)) = relabelGraph vmap g2prepared where
     g2' = relabelNewExcept [x2,y2] (vertices g1) g2
     g2prepared = relabelGraph (Map.fromList [(x2,x1),(y2,y1)]) g2'
     vmap = case pairing (x1,y1) g1 g2prepared of
             Just (fc1,fc2) -> createVMapUsing (g1,fc1) (g2prepared,fc2)
             _  -> error $ "relabelByEdges: Could not find matching faces for edges "++show [(x1,y1),(x2,y2)]

-- |createVMapUsing (g1,fc1) (g2,fc2) creates a relabelling map to make g2 consistent with g1
-- in a region of overlap if this is possible.
-- fc1 and fc2 should be matching face types and have at least 2 vertices the same (without relabelling),
-- with fc1 in g1 and fc2 in g2.
-- g2 must have no vertices in common with g1 at the outset except for the 2 common vertices in fc1 and fc2
-- The resulting map when applied to fc2 will be identical to fc1.
-- The common overlap needs to be a SINGLE region of tile-connected faces in g1.
-- The common region must contain fc1 in g1 and fc2 in g2
-- The resulting map gives the required relabelling for g2
-- This will not produce correct results if there are other overlaps not tile-connected in g1
-- to the region containing fc1
createVMapUsing:: (Tgraph,TileFace) -> (Tgraph,TileFace) -> Relabelling
createVMapUsing (g1,fc1) (g2,fc2) = addToVmap g1 [fc2] [] (faces g2 \\ [fc2]) (initVMap fc1 fc2)







{-| addToVmap is used by createVMapUsing to build a relabelling map.
(addToVmap g processing tried awaiting vmap) produces a new vmap
to be used for relabelling faces being matched against g.
A match occurs if a relabeled face matches the face type and at least 2
corresponding vertices of a face in g.
Argument g is the Tgraph being matched against.
Argument processing is a list of faces to be matched next and
each has an edge in common with at least one previously matched face.
Argument tried is a list of faces that have been checked for a matching face in g but none found.
They may still be on the boundary of g.
Argument awaiting is a list of faces that have not yet been tried for a match and are not
tile-connected to any faces already matched.
Argument vmap is a vertex to vertex mapping used to store relabelling information during the process.

The idea is that from a single matched starting face we process faces that share an edge with a
previously matched face. We prioritorize processing faces that have a match in g (with 2 matching vertices).
If a face is tried but has no such match, it is stacked up in the tried list to await
a check (with addToVmapBdCheck) for boundary cases after all faces in the overlap have been processed
(when processing == []).
If a processed face has an edge in common with a face in g but the faces do not match, this raises an error
(indicating a mismatch on the overlap).
-}
addToVmap:: Tgraph -> [TileFace] -> [TileFace] -> [TileFace] -> Relabelling -> Relabelling
addToVmap g [] [] [] vMap = vMap
addToVmap g [] [] awaiting vMap = error ("addToVmap: Faces not tile-connected " ++ show awaiting)
addToVmap g (fc:fcs) tried awaiting vMap = 
    case thirdVertexIn g (relabelFace vMap fc) of
       Just prs -> addToVmap g (fcs++fcs') tried awaiting' vMap'
                              where (fcs', awaiting') = partition (edgeNb fc) awaiting
                                    vMap' = Map.union (Map.fromList prs) vMap 
       Nothing  -> addToVmap g fcs (fc:tried) awaiting vMap
addToVmap g [] tried awaiting vMap = addToVmapBdCheck (makeBoundary g) tried vMap


{- addToVmapBdCheck is an auxiliary function for addToVmap to check boundary cases.
When it is called, all faces in the overlap have been processed.
The awaiting list is then dropped because:
faces in awaiting cannot have an edge in common with the processed overlap
and therefore cannot have a  boundary edge.
(If they have a single boundary vertex, this already has a relabelling in vMap)
It remains to process the tried faces to see if they are on the boundary.
If not, they can be ignored, but if they have a boundary edge, we need to find if the third vertex 
coincides with a neighbouring boundary vertex, in case this vertex also needs relabelling.
All this assumes the overlap was a single region (satisfying Tgraph proerties) 
-}
addToVmapBdCheck:: Boundary -> [TileFace] -> Relabelling -> Relabelling
addToVmapBdCheck bdry [] vMap = vMap
addToVmapBdCheck bdry (fc:tried) vMap =
    if hasEdgeIn (bDedges bdry) fc'
    then case checkForBoundaryThirdV bdry fc' of 
          Just (a,b) -> addToVmapBdCheck bdry tried (Map.insert a b vMap)
          Nothing -> addToVmapBdCheck bdry tried vMap
    else addToVmapBdCheck bdry tried vMap
  where fc' = relabelFace vMap fc


-- | checkForBoundaryThirdV bdry fc - fc must be a new face sharing a boundary edge in bdry.
-- It checks if the third vertex of fc coincides with a third vertex on the neighbouring boundary
-- (with possibly different label).  If this is the case, it returns Just(v,v') to indicate the relabelling
-- required for third vertex v (of fc to match with the found vertex v' on the boundary.
-- Returns Nothing if such a vertex is not found.
-- CAVEAT: No location checking is done to check for a non-local touching vertex.
checkForBoundaryThirdV:: Boundary -> TileFace -> Maybe (Vertex,Vertex)
checkForBoundaryThirdV bdry fc =  
  case findThirdV bdry e n m of
      Just v' -> let fc' = relabelFace (Map.insert v v' Map.empty) fc in
                  if noConflictCheck fc' bdry 
                  then Just (v,v')
                  else error $ "checkForBoundaryThirdV: Conflicting face found: " ++ show fc' 
      Nothing -> Nothing
  where (e:_) = faceDedges fc `intersect` (bDedges bdry)
        (v,(n,m)) = thirdVAngles e fc

-- | noConflictCheck fc bdry - f must be a new face sharing a boundary directed edge in bdry.
-- It returns true if f has no conflicts with its neighbouring faces on the boundary.
noConflictCheck :: TileFace -> Boundary -> Bool
noConflictCheck fc bdry = noConflict fc nbrFaces where
    nbrFaces = nub $ concatMap (facesAtBV bdry) (faceVList fc)

-- | thirdVAngles e f - the directed edge e must be one of the directed edges of face f.
-- The result is the third vertex of f paired with the two internal angles of the face on the directed edge
-- (in order).
-- The internal angles are expressed as a pair of integer multiples of ttangle (so 1,2,or 3).
-- These numbers are used by findThirdV.
thirdVAngles::DEdge -> TileFace -> (Vertex,(Int,Int))
thirdVAngles (x,y) fc = (z,(n,m)) where
  intangles = faceAngles fc
  IntAngle n = intangles !! indexV x fc
  IntAngle m = intangles !! indexV y fc
  z = nextV y fc

-- | thirdVertexIn g f tries to find a face in g corresponding to face f,
-- where at least 2 vertices have to match. If successful, 
-- it returns Just [(v,v')] where v needs to be relabeled to v' for a 3 verrtex match, OR
-- it may return Just [] if there is already a 3 vertex match. 
-- A return of Nothing indicates a failure to find a match.                 
thirdVertexIn:: Tgraph -> TileFace -> Maybe [(Vertex, Vertex)]  
thirdVertexIn g face = 
  case matchFace face (faces g) of
      Just matched ->  Just $ filter different $ zip (faceVList face) (faceVList matched)
      _            ->  Nothing
  where different (a,b) = a /= b

-- |pairing e g1 g2 - finds a a face in g1 with directed edge and a face in g2 with directed edge e
-- It returns an empty list if the faces do not match,
pairing :: DEdge -> Tgraph -> Tgraph -> Maybe (TileFace, TileFace)
pairing e g1 g2 = case [(fc1,fc2) | fc1 <- filter (hasEdge e) (faces g1)
                                  , fc2 <- filter (hasEdge e) (faces g2)
                                  , twoVMatch fc1 fc2 ] of
                  [] -> Nothing
                  (pr: _ ) -> Just pr

-- |hasEdge e f returns True if directed edge e is one of the directed edges of face f
hasEdge :: DEdge -> TileFace -> Bool
hasEdge e f = e `elem` (faceDedges f)

-- |initVMap f1 f2 - creates a relabelling (vertex to vertex mapping) so that
-- if applied to face f2, the vertices will match with face f1 exactly.
initVMap :: TileFace -> TileFace -> Relabelling
initVMap f1 f2 -- f2 relabels to f1
  = Map.insert (firstV  f2) (firstV  f1) $
    Map.insert (secondV f2) (secondV f1) $
    Map.insert (thirdV  f2) (thirdV  f1) Map.empty

-- |matchFace f1 fcs - looks for a face in fcs that has a directed edge in common with f1.
-- If such a face is found it checks that it matches (except possibly for one vertex label)
-- Returns Just f if f is the matched face, but an error f does not match.
-- Retruns Nothing if no face in fcs has a directed edge in common with f1
matchFace :: TileFace -> [TileFace] -> Maybe TileFace
matchFace f1 fcs = 
  case filter (hasEdgeIn (faceDedges f1)) fcs of
     f:_ -> if twoVMatch f1 f then Just f else error ("matchFace: Cannot match faces "++ show(f,f1))
     _   -> Nothing

-- |hasEdgeIn es fc - is True if fc has a directed edge in the list of edges es.
hasEdgeIn :: [DEdge] -> TileFace -> Bool
hasEdgeIn es fc = not $ null (es `intersect` faceDedges fc)

-- |twoVMatch f1 f2 is True if the two tilefaces are the same except
-- for a single vertex label possibly not matching.
twoVMatch:: TileFace -> TileFace -> Bool
twoVMatch f1 f2 = sameFaceType f1 f2 &&
                  if firstV f1 == firstV f2
                  then secondV f1 == secondV f2 || thirdV f1 == thirdV f2
                  else secondV f1 == secondV f2 && thirdV f1 == thirdV f2

-- Belongs in HalfTile?
-- | sameFaceType f1 f2 is True if f1 and f2  are of the same form
-- (i.e. both LD or both RD or both LK or both RK)
sameFaceType :: HalfTile rep1 -> HalfTile rep2 -> Bool
sameFaceType (LD _) f = isLD f
sameFaceType (RD _) f = isRD f
sameFaceType (LK _) f = isLK f
sameFaceType (RK _) f = isRK f

