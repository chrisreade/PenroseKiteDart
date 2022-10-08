{-|
Module      : Tgraph.Relabelling
Description : Collects and exports the various Tgraph modules 
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module includes relabelling functions for Tgraphs whose main purpose is
to implement a guided union of Tgraphs (fullUnion and tryFullUnion)
-}
module Tgraph.Relabelling  where


import Data.List (intersect, (\\), union,find,partition)
--import qualified Data.Map.Strict as Map (Map, findWithDefault, lookup, insert, empty, fromList, union)
import qualified Data.IntMap.Strict as VMap (IntMap, findWithDefault, lookup, insert, empty, fromList, union)
import qualified Data.IntSet as IntSet (IntSet,fromList,union,intersection,findMax,toList, (\\),size,null)
import Tgraph.Prelude
import Tgraph.Convert (createVPoints, touching, vertexLocs) -- used for fullUnion


-- |relabelAvoid vs g - produces a new Tgraph from g by relabelling.
-- Any vertex in g that is in the set vs will be changed to a new vertex that is
-- neither in g nor in the set vs. Vertices in g that are not in vs will remain the same.
relabelAvoid :: VertexSet -> Tgraph -> Tgraph
relabelAvoid avoid g = relabelGraph rlab g where
  gverts = vertices g
  avoidMax = if IntSet.null avoid then 0 else IntSet.findMax avoid
  vertsToChange = gverts `IntSet.intersection` avoid
  newvs = (IntSet.size vertsToChange) `newVsAfter` (max (maxV g) avoidMax)
  rlab = VMap.fromList $ zip (IntSet.toList vertsToChange) newvs

{-|prepareFixAvoid fix avoid g - produces a new Tgraph from g by relabelling.
 Any vertex in g that is in the set avoid but not in the list fix will be changed to a new vertex that is
 neither in g nor in the set (avoid with fix removed).
 All other vertices of g (including those in fix) will remain the same.
 Usage: This is used to prepare a graph by avoiding accidental label clashes with the avoid set
 (usually vertices of another graph).
  However we fix a list of vertices which we intend to control in a subsequent relabelling.
  (this is usually a pair of vertices from a directed edge that will get a specific subsequent relabelling).
Note: If any element of the list fix is not a vertex in g, it could end up in the relabelled Tgraph.
-}
prepareFixAvoid :: [Vertex] -> VertexSet -> Tgraph -> Tgraph
prepareFixAvoid fix avoid = relabelAvoid (avoid IntSet.\\ IntSet.fromList fix)

-- |Relabel all vertices in a Tgraph using new labels 1..n (where n is the number of vertices).
relabelContig :: Tgraph -> Tgraph
relabelContig g = relabelGraph rlab g where
   vs = vertices g
   rlab = VMap.fromList $ zip (IntSet.toList vs) [1.. IntSet.size vs]



{- *
Union (and matching) operations
-}

{-
-- |unsafeUnion (g1,e1) (g2,e2) - where edge e1 is in g1 and e2 is in g2,
-- checks if g2 can be relabelled to produce a common single region of overlap with g1
-- (with e2 relabelled to e1). If so then the result is a (checked) Tgraph g
-- where g has the union of the faces. Otherwise an error is raised.
-- 
-- CAVEAT: This is an unsafe operation:
-- The overlap must be a SINGLE tile-connected region in g1.
-- Any overlapping boundary edges which are not part of a shared face can produce touching vertices
-- which may not be discovered. This is checked for only in fullUnion/tryFullUnion.
unsafeUnion :: (Tgraph,DEdge) -> (Tgraph,DEdge) -> Tgraph
unsafeUnion (g1,e1) (g2,e2) = getResult $ tryUnsafeUnion (g1,e1) (g2,e2)

-- |tryUnsafeUnion (g1,e1) (g2,e2) - where edge e1 is in g1 and e2 is in g2,
-- checks if g2 can be relabelled to produce a common single region of overlap with g1
-- (with e2 relabelled to e1). If so then the result is Right g where g is the union of the faces.
-- Otherwise the result is Left lines where lines explains the problem.
-- 
-- CAVEAT: This is an unsafe operation:
-- The overlap must be a SINGLE tile-connected region in g1.
-- Any overlapping boundary edges which are not part of a shared face can produce touching vertices
-- which may not be discovered. This is checked for only in fullUnion/tryFullUnion.
tryUnsafeUnion ::(Tgraph,DEdge) -> (Tgraph,DEdge) -> ReportFail Tgraph
tryUnsafeUnion (g1,e1) (g2,e2) = fmap unify (tryMatchByEdges (g1,e1) (g2,e2)) where
        unify g = checkedTgraph $ faces g1 `union` faces g
-}

{-| fullUnion (g1,e1) (g2,e2) will try to create the full union of g1 and g2
    by matching the respective edges e1 and e2 and relabelling g2 to match g1.
    It will raise an error if there is a mismatch.
    This version uses geometry of tiles (vertex locations) to correct for multiple overlapping regions
    of tiles in g1 and g2             
-}
fullUnion:: (Tgraph,DEdge) -> (Tgraph,DEdge) -> Tgraph
fullUnion (g1,e1) (g2,e2) = getResult $ tryFullUnion (g1,e1) (g2,e2)

{-| tryFullUnion (g1,e1) (g2,e2) will try to create the full union of g1 and g2
    by matching the respective edges e1 and e2 and relabelling g2 to match g1.
    It returns Left lines  if there is a mismatch (where lines explains the problem).
    If succesfull it returns Right g where g is the resulting union (checked for Tgraph properties)
    This version uses geometry of tiles (vertex locations) to correct for multiple overlapping regions
    of tiles in g1 and relabelled g2             
-}
tryFullUnion:: (Tgraph,DEdge) -> (Tgraph,DEdge) -> ReportFail Tgraph
tryFullUnion (g1,e1) (g2,e2) =
  do g3 <- tryMatchByEdges (g1,e1) (g2,e2)
     let g4 = relabelTouching g1 g3  
     checkTgraphProps $ faces g1 `union` faces g4

{-|relabelTouching is used by tryFullUnion (and fullUnion).
relabelTouching g1 g2 assumes that there are vertex labels in g2 that match with vertex labels in g1
in at least a matching tile-connected overlap region. It then identifies other vertices in g2 that need to be
relabelled to match vertices in g1 based on a vertex location calculation.
It returns the result of applying this further relabelling to g2.
-}
relabelTouching :: Tgraph -> Tgraph -> Tgraph
relabelTouching g1 g2 = relabelGraph (VMap.fromList $ overlaps) g2 where
      vsg1 = vertices g1
      vsg2 = vertices g2
      overlaps = [ (v2,v1) 
                 | v2 <- IntSet.toList $ vsg2 IntSet.\\ vsg1
                 , v1 <- IntSet.toList $ vsg1
                 , v1/=v2
                 , let Just p1 = VMap.lookup v1 vlocs
                 , let Just p2 = VMap.lookup v2 vlocs
                 , touching p1 p2
                 ]
      vlocs = createVPoints $ faces g1 `union` faces g2 

{-|matchByEdges (g1,e1) (g2,e2)  produces a relabelled version of g2 that is
consistent with g1 on their overlap.
The overlapping region must contain the directed edge e1 in g1. The edge e2 in g2
will be identified with e1 by the relabelling of g2.
This produces an error if a mismatcch is found in the overlap.

CAVEAT: The overlap must be a SINGLE tile-connected region in g1.
(If the overlap contains more than one tile-connected region the result may not be
a correct relabelling of g2)    
-}
matchByEdges:: (Tgraph,DEdge) -> (Tgraph,DEdge) -> Tgraph
matchByEdges ge1 ge2 = getResult $ tryMatchByEdges ge1 ge2
 
{-|tryMatchByEdges (g1,e1) (g2,e2) produces either Right g where g is a relabelled version of g2 that is
consistent with g1 on their overlap or Left lines if there is a mismatch (lines explaining the problem).
The overlapping region must contain the directed edge e1 in g1. The edge e2 in g2
will be identified with e1 by the relabelling of g2.

CAVEAT: The overlap must be a SINGLE tile-connected region in g1.
(If the overlap contains more than one tile-connected region the result may not be
a correct relabelling of g2)    
-}
tryMatchByEdges :: (Tgraph,DEdge) -> (Tgraph,DEdge) -> ReportFail Tgraph
tryMatchByEdges (g1,(x1,y1)) (g2,(x2,y2)) = onFail "tryMatchByEdges:\n" $ 
  do let g2prepared = prepareFixAvoid [x2,y2] (vertices g1) g2
     fc2 <- find (hasDEdge (x2,y2)) (faces g2prepared)
            `nothingFail` ("No face found for edge " ++ show (x2,y2))                      
     maybef <- matchFaceIn g1 $ relabelFace (VMap.fromList [(x2,x1),(y2,y1)]) fc2
     fc1 <- maybef `nothingFail` 
                   ("No matching face found at edge "++show (x1,y1)++
                    "\nfor relabelled face " ++ show fc2)  
     rlab <- findRelabelling (g1,fc1) (g2prepared,fc2)
     return $ relabelGraph rlab g2prepared

 
{-
{-|matchByCommonEdge g1 e g2  produces a relabelled version of g2 that is
consistent with g1 on their overlap.
The overlaping region must contain the common directed edge e without relabelling.
This produces an error if a mismatcch is found in the overlap.

CAVEAT: The overlap must be a SINGLE tile-connected region in g1.
(If the overlap contains more than one tile-connected region the result may not be
a correct relabelling of g2)    
-}
matchByCommonEdge:: Tgraph -> DEdge -> Tgraph -> Tgraph
matchByCommonEdge g1 e g2 = getResult $ tryMatchByCommonEdge g1 e g2

{-|tryMatchByCommonEdge g1 e g2  produces either Right g where g is a relabelled version of g2 that is
consistent with g1 on their overlap or Left lines if there is a mismatch (lines explaining the problem).
The overlaping region must contain the common directed edge e without relabelling.

CAVEAT: The overlap must be a SINGLE tile-connected region in g1.
(If the overlap contains more than one tile-connected region the result may not be
a correct relabelling of g2)    
-}
tryMatchByCommonEdge:: Tgraph -> DEdge -> Tgraph -> ReportFail Tgraph
tryMatchByCommonEdge g1 e g2 = tryMatchByEdges (g1,e) (g2,e)
-}

{- *
Operations using relabelling maps
-}

-- |Relabelling is a special case of mappings from vertices to vertices.
-- We use the identity map for vertices not found in the mapping domain
-- (see relabelV).  Relabellings are expected to be 1-1 on their domain.       
type Relabelling = VMap.IntMap Vertex

-- |relabelGraph uses a relabelling map to change vertices in a Tgraph.
-- relabelGraph rlab g will produce a valid Tgraph provided:
-- g is a valid Tgraph, and
-- the mapping rlab extended with the identity is 1-1 on vertices in g.
-- (Vertices of g that are not in the domain of the mapping are left unchanged.)
relabelGraph:: Relabelling -> Tgraph -> Tgraph
relabelGraph rlab g = checkedTgraph newFaces where
   newFaces = fmap (relabelFace rlab) (faces g) 

-- |Uses a relabelling to relabel the three vertices of a face.
-- Any vertex not in the domain of the mapping is left unchanged.
-- The mapping should be 1-1 on the 3 vertices to avoid creating a self loop edge.
relabelFace:: Relabelling -> TileFace -> TileFace
relabelFace rlab = fmap (all3 (relabelV rlab))  -- fmap of HalfTile Functor
   where all3 f (a,b,c) = (f a,f b,f c)
   
-- |relabelV rlab v. Uses relabelling rlab to find a replacement for v (leaves as v if none found).
-- I.e relabelV turns a relabelling map into a total function using identity for undefined cases. 
relabelV:: Relabelling -> Vertex -> Vertex
relabelV rlab v = VMap.findWithDefault v v rlab


{- *
Creating relabelling maps
-}

 
{-|findRelabelling is an auxiliary function for tryMatchByEdges.
findRelabelling (g1,fc1) (g2,fc2) - fc1 and fc2 should be matching face types,
with fc1 in g1 and fc2 in g2.
g2 must have no vertices in common with g1 except for (possibly) vertices in fc2.
The result is either Right rel where
rel is a relabelling map to make g2 consistent with g1 in a region of overlap if this is possible, or
Left lines if there is a mismatch (lines explaining the problem).
In the successful case rel when applied to fc2 will be identical to fc1.

CAVEAT: The common overlap needs to be a SINGLE region of tile-connected faces in g1.
The common region must contain fc1 in g1 and fc2 in g2.
This may not produce correct results if there are other overlaps not tile-connected in g1
to the region containing fc1
-}
findRelabelling:: (Tgraph,TileFace) -> (Tgraph,TileFace) -> ReportFail Relabelling
findRelabelling (g1,fc1) (g2,fc2) = onFail "findRelabelling:\n" $ 
   addRelabel g1 [fc2] (faces g2 \\ [fc2]) (initRelabelling fc1 fc2)

-- |initRelabelling f1 f2 - creates a relabelling so that
-- if applied to face f2, the vertices will match with face f1 exactly.
-- It does not check that the tile faces have the same form (LK,RK,LD,RD).
initRelabelling :: TileFace -> TileFace -> Relabelling
initRelabelling f1 f2 -- f2 relabels to f1
  = VMap.fromList $ differing $ zip (faceVList f2) (faceVList f1)

{-|addRelabel is used by findRelabelling to build a relabelling map which can fail, producing Left lines.
In the successful case (addRelabel g processing awaiting rlab) produces a Right rel
where rel is the required relabelling. The arguments are:
g - the Tgraph being matched against.
processing - a list of faces to be matched next where
each has an edge in common with at least one previously matched face (or it is the starting face).
awaiting - a list of faces that have not yet been tried for a match and are not
tile-connected to any faces already matched.
rlab - a vertex to vertex mapping used to store relabelling information during the process.

The idea is that from a single matched starting face we process faces that share an edge with a
previously matched face. We process faces that have a match in g (with 2 matching vertices).
If a face is tried but has no such match, it is ignored (it may share some boundary with g, but
for the overlap to be a single tile-connected region, only boundaries with matched tiles are possible
and therefore relabelling will already be done for the boundary).
If a processed face has an edge in common with a face in g it has to match exactly
apart from (possibly) the third vertex label,
otherwise the faces do not match and this
indicates a mismatch on the overlap and Left ... is returned.
-}
addRelabel:: Tgraph -> [TileFace] -> [TileFace] -> Relabelling -> ReportFail Relabelling
addRelabel g [] awaiting rlab = Right rlab -- awaiting are not tile-connected to overlap region
addRelabel g (fc:fcs) awaiting rlab = 
  do maybef <- matchFaceIn g (relabelFace rlab fc)
     case maybef of
       Nothing   -> addRelabel g fcs awaiting rlab
       Just orig -> addRelabel g (fcs++fcs') awaiting' rlab'
                    where (fcs', awaiting') = partition (edgeNb fc) awaiting
                          rlab' = VMap.union (initRelabelling orig fc) rlab


{- *
Other Auxiliary functions
-}


-- |selects only non-matching pairs from a list
differing :: [(Vertex,Vertex)] -> [(Vertex,Vertex)]
differing = filter (\(a,b) -> a/=b)
          
                      
{-|
matchFaceIn g f - looks for a face in g that corresponds to f (sharing a directed edge),
If the corresponding face does not match properly (with twoVMatch) this stops the
matching process returning Left ... to indicate a failed match.
Otherwise it returns either Right (Just f) where f is the matched face or
Right Nothing if there is no corresponding face.
-}
matchFaceIn:: Tgraph -> TileFace -> ReportFail (Maybe TileFace)  
matchFaceIn g face = onFail "matchFaceIn:\n" $
  case find (hasDEdgeIn (faceDedges face)) (faces g) of
    Nothing      -> Right Nothing
    Just corresp -> if twoVMatch corresp face
                    then Right $ Just corresp
                    else Left $ "Found non matching faces " ++ show (corresp, face) ++ "\n"

-- |twoVMatch f1 f2 is True if the two tilefaces are the same except
-- for a single vertex label possibly not matching.
twoVMatch:: TileFace -> TileFace -> Bool
twoVMatch f1 f2 = matchingHalfTile f1 f2 &&
                  if firstV f1 == firstV f2
                  then secondV f1 == secondV f2 || thirdV f1 == thirdV f2
                  else secondV f1 == secondV f2 && thirdV f1 == thirdV f2

