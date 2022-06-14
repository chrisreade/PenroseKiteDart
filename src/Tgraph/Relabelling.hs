{-|
Module      : Tgraph.Relabelling
Description : Collects and exports the various Tgraph modules 
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module includes relabelling functions for Tgraphs and a guided union of Tgraphs
-}
module Tgraph.Relabelling  where


import Data.List (intersect, (\\), union,find,nub,partition,intercalate)
import qualified Data.Map.Strict as Map (Map, lookup, insert, empty, fromList,union)

import Tgraph.Prelude
import Tgraph.Force (Boundary(..), makeBoundary, facesAtBV, findThirdV, noConflict, faceAngles, IntAngle(IntAngle))
         
-- |Relabelling is a special case of mappings from vertices to vertices.
-- We use the identity map for vertices not found in the mapping domain
-- (see relabelV).  Relabellings are expected to be 1-1 on their domain.       
type Relabelling = Map.Map Vertex Vertex

-- |Uses a relabelling to change vertices in a Tgraph.
-- relabelGraph vmap g will produce a valid Tgraph provided:
-- g is a valid Tgraph, and
-- the mapping vmap extended with the identity is 1-1 on vertices in g.
-- (Vertices of g that are not in the domain of the mapping are left unchanged.)
relabelGraph:: Relabelling -> Tgraph -> Tgraph
relabelGraph vmap g = checkedTgraph newFaces where
   newFaces = fmap (relabelFace vmap) (faces g) 

-- |Uses a relabelling to relabel the three vertices of a face.
-- Any vertex not in the domain of the mapping is left unchanged.
-- The mapping should be 1-1 on the 3 vertices to avoid creating a self loop edge.
relabelFace:: Relabelling -> TileFace -> TileFace
relabelFace vmap = fmap (all3 (relabelV vmap))  -- fmap of HalfTile Functor

-- |relabelV vmap v. Uses relabelling vmap to find a replacement for v (leaves as v if none found).
-- I.e relabelV turns a relabelling into a total function using identity for undefined cases. 
relabelV:: Relabelling -> Vertex -> Vertex
relabelV vmap v = maybe v id (Map.lookup v vmap)

-- |Applies a function to all three elements of a triple.
all3:: (a -> b) -> (a,a,a) -> (b,b,b)
all3 f (a,b,c) = (f a,f b,f c)

-- |partRelabelFixChange fix change g - produces a new Tgraph from g by relabelling.
-- Any vertex in g that is in the list fix will remain unchanged.
-- All Other vertices in g that are in the list change will be changed to new vertices that are
-- neither in g nor in the list change nor in the list fix.
-- (If a vertex in g appears in both fix and change, it will remain unchanged). 
partRelabelFixChange:: [Vertex] -> [Vertex] -> Tgraph -> Tgraph
partRelabelFixChange fix change g = relabelGraph vmap g where
  gverts = vertices g
  changeWithoutFixed = change \\ fix
  avoidTarget = fix `union` change `union` gverts -- new targets must not clash with existing labels
  vertsToChange = gverts `intersect` changeWithoutFixed
  vmap = newVMapAvoid avoidTarget vertsToChange

-- |newVMapAvoid avoid vs - produces a 1-1 mapping from vertices in vs to new vertices
-- such that no new vertex is in the list avoid or in vs.
newVMapAvoid:: [Vertex] -> [Vertex] -> Relabelling
newVMapAvoid avoid vs = Map.fromList $ zip vs newvs
  where avoid' = vs `union` avoid
        newvs = makeNewVs (length vs) avoid'

-- |Relabel all vertices in a Tgraph using new labels 1..n (where n is the number of vertices).
relabelAny :: Tgraph -> Tgraph
relabelAny g = relabelGraph vmap g where
   vs = vertices g
   vmap = Map.fromList $ zip vs [1..length vs]



-- |tryUnionGraphs (g1,e1) (g2,e2) - where edge e1 is in g1 and e2 is in g2,
-- checks if g2 can be relabelled to produce a common single region of overlap with g1
-- (with e2 relabelled to e1). If so then the result is Right g where g is the union of the faces.
-- Otherwise the result is Left lines where lines explains the problem.
tryUnionGraphs ::(Tgraph,DEdge) -> (Tgraph,DEdge) -> ReportFail Tgraph
tryUnionGraphs (g1,e1) (g2,e2) = either Left (Right .unify) (tryMatchByEdges (g1,e1) (g2,e2)) where
        unify g = checkedTgraph $ faces g1 `union` faces g

-- |unionGraphs (g1,e1) (g2,e2) - where edge e1 is in g1 and e2 is in g2,
-- checks if g2 can be relabelled to produce a common single region of overlap with g1
-- (with e2 relabelled to e1). If so then the result is Right g where g is the union of the faces.
-- Otherwise an error is raised.
unionGraphs :: (Tgraph,DEdge) -> (Tgraph,DEdge) -> Tgraph
unionGraphs (g1,e1) (g2,e2) = getResult (tryUnionGraphs (g1,e1) (g2,e2))

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

{-|matchByEdges (g1,e1) (g2,e2)  produces a relabelled version of g2 that is
consistent with g1 on their overlap.
The overlapping region must contain the directed edge e1 in g1 and e2 in g2,
which will be identified by the relabelling of g2.
This produces an error if a mismatcch is found in the overlap.

CAVEAT: The overlap must be a SINGLE tile-connected region in g1.
(If the overlap contains more than one tile-connected region the result may not be
a correct relabelling of g2)    
-}
matchByEdges:: (Tgraph,DEdge) -> (Tgraph,DEdge) -> Tgraph
matchByEdges ge1 ge2 = getResult $ tryMatchByEdges ge1 ge2
 
{-|tryMatchByEdges (g1,e1) (g2,e2) produces either Right g where g is a relabelled version of g2 that is
consistent with g1 on their overlap or Left lines if there is a mismatch (lines explaining the problem).
The overlapping region must contain the directed edge e1 in g1 and e2 in g2,
which will be identified by the relabelling of g2.

CAVEAT: The overlap must be a SINGLE tile-connected region in g1.
(If the overlap contains more than one tile-connected region the result may not be
a correct relabelling of g2)    
-}
tryMatchByEdges :: (Tgraph,DEdge) -> (Tgraph,DEdge) -> ReportFail Tgraph
tryMatchByEdges (g1,(x1,y1)) (g2,(x2,y2)) = onFail "tryMatchByEdges:\n" $ 
  do let g2prepared = partRelabelFixChange [x2,y2] (vertices g1) g2
     fc2 <- find (hasDEdge (x2,y2)) (faces g2prepared)
            `nothingFail` ("No face found for edge " ++ show (x2,y2))                      
     maybef <- matchFaceIn g1 $ relabelFace (Map.fromList [(x2,x1),(y2,y1)]) fc2
     fc1 <- maybef `nothingFail` 
                   ("No matching face found at edge "++show (x1,y1)++
                    "\nfor relabelled face " ++ show fc2)  
     vmap <- findRelabelling (g1,fc1) (g2prepared,fc2)
     return $ relabelGraph vmap g2prepared
 
{-|findRelabelling is an auxiliary function for tryMatchByEdges and tryMatchByCommonEdge.
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
   addRelabel g1 [fc2] [] (faces g2 \\ [fc2]) (initRelabelling fc1 fc2)

-- |initRelabelling f1 f2 - creates a relabelling so that
-- if applied to face f2, the vertices will match with face f1 exactly.
-- It does not check that the tile faces have the same form (LK,RK,LD,RD).
initRelabelling :: TileFace -> TileFace -> Relabelling
initRelabelling f1 f2 -- f2 relabels to f1
  = Map.fromList $ differences $ zip (faceVList f2) (faceVList f1)

-- |selects only non-matching pairs from a list
differences :: [(Vertex,Vertex)] -> [(Vertex,Vertex)]
differences = filter different where
              different (a,b) = a /= b


{-|addRelabel is used by findRelabelling to build a relabelling map which can fail, producing Left lines.
In the successful case (addRelabel g processing tried awaiting vmap) produces a Right rel
where rel is the required relabelling. The arguments are:
g - the Tgraph being matched against.
processing - a list of faces to be matched next where
each has an edge in common with at least one previously matched face (or it is the starting face).
tried - a list of faces that have been checked for a matching face in g but none found.
They may still be on the boundary of g.
awaiting - a list of faces that have not yet been tried for a match and are not
tile-connected to any faces already matched.
vmap - a vertex to vertex mapping used to store relabelling information during the process.

The idea is that from a single matched starting face we process faces that share an edge with a
previously matched face. We prioritorize processing faces that have a match in g (with 2 matching vertices).
If a face is tried but has no such match, it is stacked up in the tried list to await
a check (with addRelabelBdCheck) for boundary cases after all faces in the overlap have been processed
(when processing == []).
If a processed face has an edge in common with a face in g it has to match exactly
apart from (possibly) the third vertex label,
otherwise the faces do not match and this
indicates a mismatch on the overlap and Left ... is returned.
-}
addRelabel:: Tgraph -> [TileFace] -> [TileFace] -> [TileFace] -> Relabelling -> ReportFail Relabelling
addRelabel g [] tried awaiting vMap = onFail "addRelabelBdCheck:\n" $
   addRelabelBdCheck (makeBoundary g) tried vMap -- awaiting do not need to be checked
addRelabel g (fc:fcs) tried awaiting vMap = 
  do maybef <- matchFaceIn g (relabelFace vMap fc)
     case maybef of
       Nothing   -> addRelabel g fcs (fc:tried) awaiting vMap
       Just orig -> addRelabel g (fcs++fcs') tried awaiting' vMap'
                    where (fcs', awaiting') = partition (edgeNb fc) awaiting
                          vMap' = Map.union (initRelabelling orig fc) vMap
    

{-|addRelabelBdCheck is an auxiliary function for addRelabel to check boundary cases.
When it is called, all faces in the overlap have been processed.
The awaiting list is then dropped because:
faces in awaiting cannot have an edge in common with the processed overlap
and therefore cannot have a  boundary edge.
(If they have a single boundary vertex, this already has a relabelling in vMap)
It remains to process the tried faces to see if they are on the boundary.
If not, they can be ignored, but if they have a boundary edge, we need to find if the third vertex 
coincides with a neighbouring boundary vertex, in case this vertex also needs relabelling.

CAVEAT: All this assumes the overlap is a single region. 
-}
addRelabelBdCheck:: Boundary -> [TileFace] -> Relabelling -> ReportFail Relabelling
addRelabelBdCheck bdry [] vMap = Right vMap
addRelabelBdCheck bdry (fc:tried) vMap =
    if hasDEdgeIn (bDedges bdry) fc'
    then do prs <- checkForBoundaryThirdV bdry fc'
            addRelabelBdCheck bdry tried (Map.union (Map.fromList prs) vMap)
    else addRelabelBdCheck bdry tried vMap
  where fc' = relabelFace vMap fc

{-| checkForBoundaryThirdV bdry fc - fc must be a new face sharing a boundary edge in bdry.
It checks if the third vertex of fc coincides with a third vertex on the neighbouring boundary
(with possibly different label).  If this is NOT the case  Right [] is returned.
If this is the case, a check is made that the face has no conflicts with faces on the boundary.
If there are no conflicts, Right [(v,v')] is returned to indicate the relabelling
required for third vertex v (of fc) to match with the found vertex v' on the boundary.
If there is a conflict, Left lines is returned (where lines explains the conflict)
  
CAVEAT: No location checking is done to check for a non-local touching vertex.
-}
checkForBoundaryThirdV:: Boundary -> TileFace -> ReportFail [(Vertex,Vertex)]
checkForBoundaryThirdV bdry fc = onFail "checkForBoundaryThirdV:\n" $
  case findThirdV bdry e (n,m) of
      Nothing -> Right []
      Just v' -> let fc' = relabelFace (Map.insert v v' Map.empty) fc in
                 if noConflictCheck fc' bdry 
                 then Right [(v,v')]
                 else Left $ "Conflicting face found: " ++ show fc' ++
                              "\nRelabelled from: " ++ show fc ++ "\n"                           
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

