{-|
Module      : Tgraph.Relabelling
Description : Guided union and commonFaces using relabelling operations 
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module includes relabelling functions for Tgraphs whose main purpose is
to implement a guided union of Tgraphs (fullUnion and tryFullUnion)
and also a commonFaces operation (a kind of intersection which need not be a Tgraph)
-}
module Tgraph.Relabelling  where


import Data.List (intersect, (\\), union,find,partition,nub)
import qualified Data.IntMap.Strict as VMap (IntMap, findWithDefault, fromList, fromAscList, union)
import qualified Data.IntSet as IntSet (IntSet,fromList,intersection,findMax,elems,(\\),null,member)

import Tgraph.Prelude
import Tgraph.Convert (touchingVertices, touchingVerticesGen) -- used for fullUnion and commonFaces


-- |relabelAvoid avoid g - produces a new Tgraph from g by relabelling.
-- Any vertex in g that is in the set avoid will be changed to a new vertex that is
-- neither in g nor in the set avoid. Vertices in g that are not in avoid will remain the same.
relabelAvoid :: VertexSet -> Tgraph -> Tgraph
relabelAvoid avoid g = relabelGraph rlab g where
  gverts = vertexSet g
  avoidMax = if IntSet.null avoid then 0 else IntSet.findMax avoid
  vertsToChange = gverts `IntSet.intersection` avoid
  rlab = relabellingFrom (1+ max (maxV g) avoidMax) vertsToChange
  -- assert: rlab is 1-1 on the vertices of g
  -- assert: the relabelled Tgraph satisfies Tgraph properties (if g does)
  -- assert: the relabelled Tgraph does not have vertices in the set avoid

  
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
  -- assert: the relabelled Tgraph satisfies Tgraph properties (if the argument Tgraph does)
  -- assert: the relabelled Tgraph does not have vertices in the set (avoid\\fix)

-- |Relabel all vertices in a Tgraph using new labels 1..n (where n is the number of vertices).
relabelContig :: Tgraph -> Tgraph
relabelContig g = relabelGraph rlab g where
   rlab = relabellingFrom 1 (vertexSet g)
  -- assert: rlab is 1-1 on the vertices of g
  -- assert: the relabelled Tgraph satisfies Tgraph properties (if g does)



{- *
Assisted Union (and matching) operations
-}

{-| fullUnion (g1,e1) (g2,e2) will try to create the union of g1 and g2
    by matching the respective edges e1 and e2 and relabelling g2 to match g1 on that overlap.
    It will raise an error if there is a mismatch.
    It then uses geometry of tiles (vertex locations) to correct for multiple overlapping regions
    of tiles in g1 and relabelled g2 by a further relabelling of touching vertices.
    The resulting union of faces requires an expensive checkTgraphProps, so this is not efficient for large Tgraphs.
    However the checks are not needed when there are no touching vertices.          
-}
fullUnion:: (Tgraph,Dedge) -> (Tgraph,Dedge) -> Tgraph
fullUnion (g1,e1) (g2,e2) = runTry $ tryFullUnion (g1,e1) (g2,e2)

{-| tryFullUnion (g1,e1) (g2,e2) will try to create the union of g1 and g2
    by matching the respective edges e1 and e2 and relabelling g2 to match g1 on a tile-connected region containing e1.
    It returns Left lines  if there is a mismatch (where lines explains the problem).
    If succesfull it then uses geometry of tiles (vertex locations) to correct for multiple overlapping regions
    of tiles in g1 and relabelled g2 by a further relabelling of touching vertices. 
    If there were touching vertices (i.e when the overlap is not a single tile connected region)
    it then checks the union of faces for Tgraph properties
    and returns Right g where g is the resulting union if this succeeds (Left report otherwise).
    However no checks are needed on the union of faces when there are no touching vertices.          
    Since checkTgraphProps is expensive for large Tgraphs, 
    the union is not efficient when there are multiple overlapping regions.
-}
tryFullUnion:: (Tgraph,Dedge) -> (Tgraph,Dedge) -> Try Tgraph
tryFullUnion (g1,e1) (g2,e2) = onFail "tryFullUnion:\n" $
  do g3 <- tryMatchByEdges (g1,e1) (g2,e2)
     let fcs = faces g1 `union` faces g3
         touchVs = touchingVertices fcs
     if null touchVs
     then return $ Tgraph { faces = fcs, maxV = facesMaxV fcs } -- no properties check needed!
     else let vertg1 = vertexSet g1
              correct e@(a,b) = if a `IntSet.member` vertg1 then (b,a) else e
              newrel = newRelabelling $ fmap correct touchVs
          in checkTgraphProps $ nub $ fmap (relabelFace newrel) fcs

{-|matchByEdges (g1,e1) (g2,e2)  produces a relabelled version of g2 that is
consistent with g1 on a single tile-connected overlap.
The overlapping region must contain the directed edge e1 in g1. The edge e2 in g2
will be identified with e1 by the relabelling of g2.
This produces an error if a mismatch is found in the overlap.

CAVEAT: The relabelling may not be complete if the overlap is not just a SINGLE tile-connected region in g1.
If the overlap is more than a single tile-connected region, then the union of the relabelled faces with faces in g1
will be tile-connected but may have touching vertices.
This limitation is addressed by fullUnion. 
-}
matchByEdges:: (Tgraph,Dedge) -> (Tgraph,Dedge) -> Tgraph
matchByEdges ge1 ge2 = runTry $ tryMatchByEdges ge1 ge2
 
{-|tryMatchByEdges (g1,e1) (g2,e2) produces either Right g where g is a relabelled version of g2 that is
consistent with g1 on an overlapping tile-connected region or Left lines if there is a mismatch (lines explaining the problem).
The overlapping region must contain the directed edge e1 in g1. The edge e2 in g2
will be identified with e1 by the relabelling of g2.

CAVEAT: The relabelling may not be complete if the overlap is not just a SINGLE tile-connected region in g1.
If the overlap is more than a single tile-connected region, then the union of the relabelled faces with faces in g1
will be tile-connected but may have touching vertices.    
This limitation is addressed by tryFullUnion. 
-}
tryMatchByEdges :: (Tgraph,Dedge) -> (Tgraph,Dedge) -> Try Tgraph
tryMatchByEdges (g1,(x1,y1)) (g2,(x2,y2)) = onFail "tryMatchByEdges:\n" $ 
  do let g2prepared = prepareFixAvoid [x2,y2] (vertexSet g1) g2
     fc2 <- find (`hasDedge` (x2,y2)) (faces g2prepared)
            `nothingFail` ("No face found for edge " ++ show (x2,y2))                      
     maybef <- tryMatchFace (relabelFace (newRelabelling [(x2,x1),(y2,y1)]) fc2) g1
     fc1 <- maybef `nothingFail` 
                   ("No matching face found at edge "++show (x1,y1)++
                    "\nfor relabelled face " ++ show fc2)  
     rlab <- findRelabelling (g1,fc1) (g2prepared,fc2)
     return $ relabelGraph rlab g2prepared
 
{- *
Creating and Using Relabellings
-}

-- |Relabelling is a special case of mappings from vertices to vertices that are not the 
-- identity on a finite number of cases.
-- They are represented by keeping the non identity cases in a finite map.
-- When applied, we assume the identity map for vertices not found in the representation domain
-- (see relabelV).  Relabellings must be 1-1 on their representation domain,
-- and redundant identity mappings are removed in the representation.
-- Vertices in the range of a relabelling must be >0.
newtype Relabelling = Relabelling (VMap.IntMap Vertex)

-- | newRelabelling prs - make a relabelling from a finite list of vertex pairs.
-- The first item in each pair relabels to the second in the pair.
-- The resulting relabelling excludes any identity mappings of vertices.
-- An error is raised if second items of the pairs contain duplicated numbers or a number<1
newRelabelling :: [(Vertex,Vertex)] -> Relabelling
newRelabelling prs 
    | wrong (map snd prs) = error $ "newRelabelling: Not 1-1 or Non-positive label in range " ++ show prs
    | otherwise = Relabelling $ VMap.fromList $ differing prs
  where wrong vs = any (<1) vs || not (null (duplicates vs))

-- | relabellingFrom n vs - make a relabelling from finite set of vertices vs.
-- Elements of vs are ordered and relabelled from n upwards (an error is raised if n<1).
-- The resulting relabelling excludes any identity mappings of vertices.
relabellingFrom :: Int -> VertexSet -> Relabelling
relabellingFrom n vs 
    | n<1 = error $ "relabellingFrom: Label not positive " ++ show n
    | otherwise = Relabelling $ VMap.fromAscList $ differing $ zip (IntSet.elems vs) [n..] 

-- | f1 `relabellingTo` f2  - creates a relabelling so that
-- if applied to face f1, the vertices will match with face f2 exactly.
-- It does not check that the tile faces have the same form (LK,RK,LD,RD).
relabellingTo :: TileFace -> TileFace -> Relabelling
f1 `relabellingTo` f2 = newRelabelling $ zip (faceVList f1) (faceVList f2) -- f1 relabels to f2

-- | Combine relabellings (assumes disjoint representation domains and disjoint representation ranges but
-- no check is made for these).
relabelUnion:: Relabelling -> Relabelling -> Relabelling
relabelUnion (Relabelling r1) (Relabelling r2) = Relabelling $ VMap.union r1 r2 

-- |relabelGraph rlab g - uses a Relabelling rlab to change vertices in a Tgraph g.
-- Caveat: This should only be used when it is known that:
-- rlab is 1-1 on its (representation) domain, and
-- the vertices of g are disjoint from those vertices in the representation range
-- which are not in the representation domain of rlab.
-- This ensures rlab (extended with the identity) remains 1-1 on vertices in g,
-- so that the resulting Tgraph does not need an expensive check for Tgraph properties.
-- (See also checkRelabelGraph)
relabelGraph:: Relabelling -> Tgraph -> Tgraph
relabelGraph rlab g = Tgraph {faces = newFaces, maxV = facesMaxV newFaces } where
   newFaces = fmap (relabelFace rlab) (faces g) 

-- |checkRelabelGraph uses a relabelling map to change vertices in a Tgraph,
-- then checks that the result is a valid Tgraph. (see also relabelGraph)
checkRelabelGraph:: Relabelling -> Tgraph -> Tgraph
checkRelabelGraph rlab g = checkedTgraph newFaces where
   newFaces = fmap (relabelFace rlab) (faces g) 

-- |Uses a relabelling to relabel the three vertices of a face.
-- Any vertex not in the domain of the mapping is left unchanged.
-- The mapping should be 1-1 on the 3 vertices to avoid creating a self loop edge.
relabelFace:: Relabelling -> TileFace -> TileFace
relabelFace rlab = fmap (all3 (relabelV rlab)) where -- fmap of HalfTile Functor
  all3 f (a,b,c) = (f a,f b,f c)

-- |relabelV rlab v - uses relabelling rlab to find a replacement for v (leaves as v if none found).
-- I.e relabelV turns a Relabelling into a total function using identity
-- for undefined cases in the Relabelling representation. 
relabelV:: Relabelling -> Vertex -> Vertex
relabelV (Relabelling r) v = VMap.findWithDefault v v r

 

-- |renumberFaces allows for a non 1-1 relabelling represented by a list of pairs.
-- It is used only for tryCorrectTouchingVs in Tgraphs which then checks the result 
renumberFaces :: [(Vertex,Vertex)] -> [TileFace] -> [TileFace]
renumberFaces prs = fmap renumberFace where
    mapping = VMap.fromList $ differing prs
    renumberFace = fmap (all3 renumber)
    all3 f (a,b,c) = (f a,f b,f c)
    renumber v = VMap.findWithDefault v v mapping
 
{- *
Creating Relabellings by matching
-}
 
{-|findRelabelling is an auxiliary function for tryMatchByEdges.
findRelabelling (g1,fc1) (g2,fc2) - fc1 and fc2 should have the same form (RK,LK,RD,LD),
with fc1 a face in g1 and fc2 a face in g2.
g2 must have no vertices in common with g1 except for (possibly) vertices in fc2.
The result is either Right rel where
rel is a relabelling map to make g2 consistent with g1 in a single region of overlap if this is possible, or
Left lines if there is a mismatch (lines explaining the problem).
In the successful case rel when applied to fc2 will be identical to fc1.

CAVEAT: Only the single tile-connected region of common overlap (containing fc2) of g2 gets relabelled
to match with g1.
-}
findRelabelling:: (Tgraph,TileFace) -> (Tgraph,TileFace) -> Try Relabelling
findRelabelling (g1,fc1) (g2,fc2) = onFail "findRelabelling:\n" $ 
   tryGrowRelabel g1 [fc2] (faces g2 \\ [fc2]) (fc2 `relabellingTo` fc1)


{-|tryGrowRelabel is used by findRelabelling to build a relabelling map which can fail, producing Left lines.
In the successful case (tryGrowRelabel g processing awaiting rlab) produces a Right rel
where rel is the required relabelling. The arguments are:
g - the Tgraph being matched against.
processing - a list of faces to be matched next where
each has an edge in common with at least one previously matched face (or it is the starting face).
awaiting - a list of faces that have not yet been tried for a match and are not
tile-connected to any faces already matched.
rlab - the relabelling so far.

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
tryGrowRelabel:: Tgraph -> [TileFace] -> [TileFace] -> Relabelling -> Try Relabelling
tryGrowRelabel g [] awaiting rlab = Right rlab -- awaiting are not tile-connected to overlap region
tryGrowRelabel g (fc:fcs) awaiting rlab = 
  do maybef <- tryMatchFace (relabelFace rlab fc) g
     case maybef of
       Nothing   -> tryGrowRelabel g fcs awaiting rlab
       Just orig -> tryGrowRelabel g (fcs++fcs') awaiting' rlab'
                    where (fcs', awaiting') = partition (edgeNb fc) awaiting
                          rlab' = relabelUnion (fc `relabellingTo` orig) rlab


{- *
commonFaces (Assisted Intersection)
-}

-- | commonFaces (g1,e1) (g2,e2) relabels g2 to match with g1 (where they match)
-- and returns the common faces as a subset of faces of g1.
-- i.e. with g1 vertex labelling.
-- It requires a face in g1 with directed edge e1 to match a face in g2 with directed edge e2,
-- (apart from the third vertex label) otherwise an error is raised.
-- This uses vertex locations to correct touching vertices in multiply overlapping regions.
-- >>>> touching vertices being 1-1 is sensitive to nearness check of touchingVerticesGen <<<<<<<<<
commonFaces:: (Tgraph,Dedge) -> (Tgraph,Dedge) -> [TileFace]
commonFaces (g1,e1) (g2,e2) = faces g1 `intersect` relFaces where
  g3 = matchByEdgesIgnore (g1,e1) (g2,e2)
  fcs = faces g1 `union` faces g3
  touchVs = touchingVerticesGen fcs -- requires generalised version of touchingVertices
  relFaces = fmap (relabelFace $ newRelabelling $ fmap correct touchVs) (faces g3)
  vertg1 = vertexSet g1
  correct e@(a,b) = if a `IntSet.member` vertg1 then (b,a) else e

-- |same as matchByEdges but ignores non-matching faces (except for the initial 2)
-- The initial 2 faces are those on the given edges, and an error is raised if they do not match.
matchByEdgesIgnore :: (Tgraph,Dedge) -> (Tgraph,Dedge) -> Tgraph
matchByEdgesIgnore (g1,(x1,y1)) (g2,(x2,y2)) = relabelGraph rlab g2prepared where
  g2prepared = prepareFixAvoid [x2,y2] (vertexSet g1) g2
  fc2 = case find (`hasDedge` (x2,y2)) (faces g2prepared) of
           Nothing -> error $ "No face found for edge " ++ show (x2,y2)
           Just f -> f                      
  fc1 = case matchFaceIgnore (relabelFace (newRelabelling [(x2,x1),(y2,y1)]) fc2) g1 of
           Nothing -> error $ "No matching face found at edge "++show (x1,y1)++
                              "\nfor relabelled face " ++ show fc2
           Just f -> f
  rlab = findRelabellingIgnore (g1,fc1) (g2prepared,fc2)
   
-- |findRelabellingIgnore is the same as findRelabelling except that it uses matchFaceIgnore
-- which ignores non-matching faces rather than failing. It thus returns a definite Relabelling.
findRelabellingIgnore:: (Tgraph,TileFace) -> (Tgraph,TileFace) -> Relabelling
findRelabellingIgnore (g1,fc1) (g2,fc2) =  
   tryGrowRelabelIgnore g1 [fc2] (faces g2 \\ [fc2]) (fc2 `relabellingTo` fc1)

-- |tryGrowRelabelIgnore is the same as tryGrowRelabel except that it uses matchFaceIgnore
-- which ignores non-matching faces rather than failing. It thus returns a definite Relabelling.
tryGrowRelabelIgnore:: Tgraph -> [TileFace] -> [TileFace] -> Relabelling -> Relabelling
tryGrowRelabelIgnore g [] awaiting rlab = rlab -- awaiting are not tile-connected to overlap region
tryGrowRelabelIgnore g (fc:fcs) awaiting rlab = 
     case matchFaceIgnore (relabelFace rlab fc) g of
       Nothing   -> tryGrowRelabelIgnore g fcs awaiting rlab
       Just orig -> tryGrowRelabelIgnore g (fcs++fcs') awaiting' rlab'
                    where (fcs', awaiting') = partition (edgeNb fc) awaiting
                          rlab' = relabelUnion (fc `relabellingTo` orig) rlab


{- *
Directed equality
-}
                      
-- | sameGraph (g1,e1) (g2,e2) checks to see if g1 and g2 are the same Tgraph after relabelling g2.
-- The relabelling is based on directed edge e2 in g2 matching e1 in g1 (where the direction is clockwise round a face).
sameGraph :: (Tgraph,Dedge) -> (Tgraph,Dedge) -> Bool
sameGraph (g1,e1) (g2,e2) =  length (faces g1) == length (faces g2) &&
                             ifFail False tryResult where
 tryResult = do g <- tryMatchByEdges (g1,e1) (g2,e2)
                return (vertexSet g == vertexSet g1)


{- *
Other Auxiliary functions
-}

-- |selects only non-matching pairs from a list
differing :: Eq a => [(a,a)] -> [(a,a)]
differing = filter (\(a,b) -> a/=b)
                     
{-|
tryMatchFace f g - looks for a face in g that corresponds to f (sharing a directed edge),
If the corresponding face does not match properly (with twoVMatch) this stops the
matching process returning Left ... to indicate a failed match.
Otherwise it returns either Right (Just f) where f is the matched face or
Right Nothing if there is no corresponding face.
-}
tryMatchFace:: TileFace -> Tgraph -> Try (Maybe TileFace)  
tryMatchFace face g = onFail "tryMatchFace:\n" $
  case find (`hasDedgeIn` faceDedges face) (faces g) of
    Nothing      -> Right Nothing
    Just corresp -> if twoVMatch corresp face
                    then Right $ Just corresp
                    else Left $ "Found non matching faces " ++ show (corresp, face) ++ "\n"

-- |twoVMatch f1 f2 is True if the two tilefaces are the same except
-- for a single vertex label possibly not matching.
twoVMatch:: TileFace -> TileFace -> Bool
twoVMatch f1 f2 = isMatched f1 f2 &&
                  if firstV f1 == firstV f2
                  then secondV f1 == secondV f2 || thirdV f1 == thirdV f2
                  else secondV f1 == secondV f2 && thirdV f1 == thirdV f2

{-|A version of tryMatchFace that just ignores mismatches.
matchFaceIgnore f g - looks for a face in g that corresponds to f (sharing a directed edge),
If there is a corresponding face f' which matches label and corresponding directed edge then Just f' is returned
Otherwise Nothing is returned. (Thus ignoring a clash)
-}
matchFaceIgnore:: TileFace -> Tgraph -> Maybe TileFace  
matchFaceIgnore face g = case tryMatchFace face g of
   Right mf -> mf
   Left _   -> Nothing
