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
and a guided equality check (sameGraph).
-}

{-# LANGUAGE Strict            #-} 

module Tgraph.Relabelling
  ( -- * Guided operations
    fullUnion
  , tryFullUnion
  , commonFaces
  , sameGraph
    -- * Tgraph Matching
  , relabelToMatch
  , tryRelabelToMatch
--  , tryRelabelFromFaces
--  , tryGrowRelabel
  , relabelToMatchIgnore
--  , relabelFromFacesIgnore
--  , growRelabelIgnore
    -- * Relabellings
  , Relabelling()
  , newRelabelling
  , unsafeDom
--  , relabellingFrom
--  , relabellingTo
--  , extendRelabelling
  , relabelGraph
  , checkRelabelGraph
  , relabelContig
  -- * Auxiliary Functions
  , relabelFace
  , relabelV
--  , relabelAvoid
  , prepareFixAvoid
    --  * Renumbering (not necessarily 1-1)
--  , tryMatchFace
--  , twoVMatch
--  , matchFaceIgnore
-- , differing
  ) where


import Data.List (intersect, (\\), union,find,partition,nub)
import qualified Data.IntMap.Strict as VMap (IntMap, findWithDefault, fromList, fromAscList, elems, keysSet, union)
import qualified Data.IntSet as IntSet (fromList,intersection,findMax,elems,(\\),null,member)

import Tgraph.Prelude


{-| fullUnion (g1,e1) (g2,e2) will try to create the union of g1 and g2.  That is, it will try to combine the faces of g1
    and (possibly relabelled) faces of g2 as a Tgraph.  It does this
    by first matching the respective edges e1 and e2 and relabelling g2 to match g1 on a tile-connected region containing e1.
    It will raise an error if there is a mismatch.
    If succesfull it then uses geometry of tiles (vertex locations) to correct for multiple overlapping regions
    of tiles in g1 and relabelled g2 by a further relabelling of any touching vertices.
    The resulting union of faces requires an expensive tryTgraphProps if touching vertices were found.
    However the check is not needed when there are no touching vertices (i.e. a single tile-connected overlap).          
-}
fullUnion:: (Tgraph,Dedge) -> (Tgraph,Dedge) -> Tgraph
fullUnion (g1,e1) (g2,e2) = runTry $ tryFullUnion (g1,e1) (g2,e2)

{-| tryFullUnion (g1,e1) (g2,e2) will try to create the union of g1 and g2.  That is, it will try to combine the faces of g1
    and (possibly relabelled) faces of g2 as a Tgraph.  It does this
    by first matching the respective edges e1 and e2 and relabelling g2 to match g1 on a tile-connected region containing e1.
    It returns Left lines  if there is a mismatch (where lines explains the problem).
    If succesfull it then uses geometry of tiles (vertex locations) to correct for multiple overlapping regions
    of tiles in g1 and relabelled g2 by a further relabelling of any touching vertices. 
    The resulting union of faces requires an expensive tryTgraphProps if any touching vertices were found,
    and will return Left ... if this fails and Right t otherwise, where t is a Tgraph
    containing the union of faces.
    The check is not used when there are no touching vertices (i.e. a single tile-connected overlap).          
-}
tryFullUnion:: (Tgraph,Dedge) -> (Tgraph,Dedge) -> Try Tgraph
tryFullUnion (g1,e1) (g2,e2) = onFail "tryFullUnion:\n" $
  do g3 <- tryRelabelToMatch (g1,e1) (g2,e2)
     let fcs = faces g1 `union` faces g3
         touchVs = touchingVertices fcs
     if null touchVs
     then return $ makeUncheckedTgraph fcs -- no properties check needed!
     else let vertg1 = vertexSet g1
              correct e@(a,b) = if a `IntSet.member` vertg1 then (b,a) else e
              newrel = newRelabelling $ map correct touchVs
          in tryTgraphProps $ nub $ map (relabelFace newrel) fcs

-- | commonFaces (g1,e1) (g2,e2) relabels g2 to match with g1 (where they match)
-- and returns the common faces as a subset of faces of g1.
-- i.e. with g1 vertex labelling.
-- It requires a face in g1 with directed edge e1 to match a face in g2 with directed edge e2,
-- (apart from the third vertex label) otherwise an error is raised.
-- This uses vertex locations to correct touching vertices in multiply overlapping regions.
commonFaces:: (Tgraph,Dedge) -> (Tgraph,Dedge) -> [TileFace]
commonFaces (g1,e1) (g2,e2) = faces g1 `intersect` relFaces where
  g3 = relabelToMatchIgnore (g1,e1) (g2,e2)
  fcs = faces g1 `union` faces g3
  touchVs = touchingVerticesGen fcs -- requires generalised version of touchingVertices
  relFaces = map (relabelFace $ newRelabelling $ map correct touchVs) (faces g3)
  vertg1 = vertexSet g1
  correct e@(a,b) = if a `IntSet.member` vertg1 then (b,a) else e

                      
-- | sameGraph (g1,e1) (g2,e2) checks to see if g1 and g2 are the same Tgraph after relabelling g2.
-- The relabelling is based on directed edge e2 in g2 matching e1 in g1 (where the direction is clockwise round a face)
-- and uses tryRelabelToMatch.
sameGraph :: (Tgraph,Dedge) -> (Tgraph,Dedge) -> Bool
sameGraph (g1,e1) (g2,e2) =  length (faces g1) == length (faces g2) &&
                             ifFail False tryResult where
 tryResult = do g <- tryRelabelToMatch (g1,e1) (g2,e2)
                return (vertexSet g == vertexSet g1)


{-|Relabelling is a special case of mappings from vertices to vertices that are not the 
identity on a finite number of vertices.
They are represented by keeping the non identity cases in a finite map.
When applied, we assume the identity map for vertices not found in the keys of the relabelling.
(see relabelV).  Relabellings must be 1-1 on their keys,
and redundant identity mappings are removed in the representation.
Vertices in the range of a relabelling must be positive integers.

Call the set of elements (range) of a relabelling that are not keys of the relabelling
the /unsafe domain/ of the relabelling.

A relabelling is 1-1 on any set (of positive integers) that is disjoint from its unsafe domain.
-}
newtype Relabelling = Relabelling (VMap.IntMap Vertex)

-- | newRelabelling prs - make a relabelling from a finite list of vertex pairs.
-- The first item in each pair relabels to the second in the pair.
-- The resulting relabelling excludes any identity mappings of vertices.
-- An error is raised if the list of second items of the pairs contains duplicates
-- or a non-positive integer.
newRelabelling :: [(Vertex,Vertex)] -> Relabelling
newRelabelling prs 
    | wrong (map snd prs) = error $ "newRelabelling: Not 1-1 or Non-positive label in range " ++ show prs
    | otherwise = Relabelling $ VMap.fromList $ differing prs
  where wrong vs = any (<1) vs || not (null (duplicates vs))

-- | relabellingFrom n vs - make a relabelling from finite set of vertices vs.
-- Elements of vs are ordered and relabelled from n upwards (an error is raised if n<1).
-- The resulting relabelling excludes any identity mappings of vertices.
-- The resulting relabelling (for n>0) is clearly 1-1 on vs
relabellingFrom :: Int -> VertexSet -> Relabelling
relabellingFrom n vs 
    | n<1 = error $ "relabellingFrom: Label not positive " ++ show n
    | otherwise = Relabelling $ VMap.fromAscList $ differing $ zip (IntSet.elems vs) [n..] 

-- | Returns the /unsafe domain/ of a relabelling.
-- The unsafe domain is the set of elements (range) of a relabelling that are not keys of the relabelling.
-- I.e. those vertex numbers to be avoided when applying a relabelling to guarantee the relabelling is 1-1.
unsafeDom :: Relabelling -> VertexSet
unsafeDom (Relabelling vmap) = 
    IntSet.fromList [ e | e <- VMap.elems vmap
                       , not (e `IntSet.member` VMap.keysSet vmap )
                       ]

-- | f1 \`relabellingTo\` f2  - creates a relabelling so that
-- if applied to face f1, the vertices will match with face f2 exactly.
-- It does not check that the tile faces have the same constructor (LK,RK,LD,RD).
relabellingTo :: TileFace -> TileFace -> Relabelling
f1 `relabellingTo` f2 = newRelabelling $ zip (faceVList f1) (faceVList f2) -- f1 relabels to f2

-- | (not exported) extendRelabelling fc1 fc2 r - Extend r to also relabel face fc1 to fc2
extendRelabelling :: TileFace -> TileFace -> Relabelling -> Relabelling
extendRelabelling fc1 fc2 (Relabelling r) = Relabelling $ VMap.union extra r
  where Relabelling extra = (fc1 `relabellingTo` fc2)

{- No longer used
-- | Combine relabellings (assumes disjoint representation domains and disjoint representation ranges but
-- no check is made for these).
relabelUnion:: Relabelling -> Relabelling -> Relabelling
relabelUnion (Relabelling r1) (Relabelling r2) = Relabelling $ VMap.union r1 r2 
 -}

{-|relabelToMatch (g1,e1) (g2,e2)  produces a relabelled version of g2 that is
consistent with g1 on a single tile-connected region of overlap.
The overlapping region must contain the directed edge e1 in g1. The edge e2 in g2
will be identified with e1 by the relabelling of g2.
This produces an error if a mismatch is found anywhere in the overlap.

CAVEAT: The relabelling may not be complete if the overlap is not just a SINGLE tile-connected region in g1.
If the overlap is more than a single tile-connected region, then the union of the relabelled faces with faces in g1
will be tile-connected but may have touching vertices.
This limitation is addressed by fullUnion. 
-}
relabelToMatch:: (Tgraph,Dedge) -> (Tgraph,Dedge) -> Tgraph
relabelToMatch ge1 ge2 = runTry $ tryRelabelToMatch ge1 ge2
 
{-|tryRelabelToMatch (g1,e1) (g2,e2) produces either Right g where g is a relabelled version of g2 that is
consistent with g1 on an overlapping tile-connected region or Left lines if there is a mismatch (lines explaining the problem).
The overlapping region must contain the directed edge e1 in g1. The edge e2 in g2
will be identified with e1 by the relabelling of g2.

CAVEAT: The relabelling may not be complete if the overlap is not just a SINGLE tile-connected region in g1.
If the overlap is more than a single tile-connected region, then the union of the relabelled faces with faces in g1
will be tile-connected but may have touching vertices.    
This limitation is addressed by tryFullUnion. 
-}
tryRelabelToMatch :: (Tgraph,Dedge) -> (Tgraph,Dedge) -> Try Tgraph
tryRelabelToMatch (g1,(x1,y1)) (g2,(x2,y2)) = onFail "tryRelabelToMatch:\n" $ 
  do let g2prepared = prepareFixAvoid [x2,y2] (vertexSet g1) g2
     fc2 <- find (`hasDedge` (x2,y2)) (faces g2prepared)
            `nothingFail` ("No face found for edge " ++ show (x2,y2))                      
     maybef <- tryMatchFace (relabelFace (newRelabelling [(x2,x1),(y2,y1)]) fc2) g1
     fc1 <- maybef `nothingFail` 
                   ("No matching face found at edge "++show (x1,y1)++
                    "\nfor relabelled face " ++ show fc2)  
  -- assert g2prepared has no labels in common with g1 except possibly those in fc2
     tryRelabelFromFaces (g1,fc1) (g2prepared,fc2)

{-|tryRelabelFromFaces is an auxiliary function for tryRelabelToMatch.
tryRelabelFromFaces (g1,fc1) (g2,fc2) - fc1 and fc2 should have the same form (RK,LK,RD,LD),
with fc1 a face in g1 and fc2 a face in g2.
g2 must have no vertices in common with g1 except for (possibly) vertices in fc2.
The result is either Right g3 where
g3 is a relabelling of g2 which is consistent with g1 in a single region of overlap containing fc1 if this is possible, or
Left lines if there is a mismatch (lines explaining the problem).
In the successful case fc2 will be relabelled to fc1.

CAVEAT: Only the single tile-connected region of common overlap (containing fc2) of g2 gets relabelled
to match with g1.
-}
tryRelabelFromFaces :: (Tgraph,TileFace) -> (Tgraph,TileFace) -> Try Tgraph
tryRelabelFromFaces (g1,fc1) (g2,fc2) = onFail "tryRelabelFromFaces:\n" $ 
   do rlab <- tryGrowRelabel g1 [fc2] (faces g2 \\ [fc2]) (fc2 `relabellingTo` fc1)
      return $ relabelGraph rlab g2
      
{-|tryGrowRelabel is used by tryRelabelFromFaces to build a relabelling map which can fail, producing Left lines.
In the successful case it produces a Right rlab
where rlab is the required relabelling. 
The arguments are:  g processing awaiting rlab where
g is the Tgraph being matched against;
processing is a list of faces to be matched next
(each has an edge in common with at least one previously matched face or it is the starting face);
awaiting is a list of faces that have not yet been tried for a match and are not
tile-connected to any faces already matched.
rlab is the relabelling so far.

The idea is that from a single matched starting face we process faces that share an edge with a
previously matched face. Each face processed should have a match in g (with 2 matching vertices).
If a face is tried but has no such match, it is ignored (it may share some boundary with g, but
for the overlap to be a single tile-connected region, only boundaries with matched tiles are possible
and therefore relabelling will already be done for the boundary).
If a processed face has an edge in common with a face in g it has to match exactly
apart from (possibly) the third vertex label,
otherwise the faces do not match and this
indicates a mismatch on the overlap and Left ... is returned.
-}
tryGrowRelabel:: Tgraph -> [TileFace] -> [TileFace] -> Relabelling -> Try Relabelling
tryGrowRelabel _ [] _ rlab = Right rlab -- awaiting are not tile-connected to overlap region
tryGrowRelabel g (fc:fcs) awaiting rlab = 
  do maybef <- tryMatchFace (relabelFace rlab fc) g
     case maybef of
       Nothing   -> tryGrowRelabel g fcs awaiting rlab
       Just orig -> tryGrowRelabel g (fcs++fcs') awaiting' rlab'
                    where (fcs', awaiting') = partition (edgeNb fc) awaiting
                          rlab' = extendRelabelling fc orig rlab 
                          -- relabelUnion (fc `relabellingTo` orig) rlab



-- |same as relabelToMatch but ignores non-matching faces (except for the initial 2)
-- The initial 2 faces are those on the given edges, and an error is raised if they do not match.
-- This is used by commonFaces
relabelToMatchIgnore :: (Tgraph,Dedge) -> (Tgraph,Dedge) -> Tgraph
relabelToMatchIgnore (g1,(x1,y1)) (g2,(x2,y2)) = relabelFromFacesIgnore (g1,fc1) (g2prepared,fc2) where
  g2prepared = prepareFixAvoid [x2,y2] (vertexSet g1) g2
  fc2 = case find (`hasDedge` (x2,y2)) (faces g2prepared) of
           Nothing -> error $ "No face found for edge " ++ show (x2,y2)
           Just f -> f                      
  fc1 = case matchFaceIgnore (relabelFace (newRelabelling [(x2,x1),(y2,y1)]) fc2) g1 of
           Nothing -> error $ "No matching face found at edge "++show (x1,y1)++
                              "\nfor relabelled face " ++ show fc2
           Just f -> f
   

{-| relabelFromFacesIgnore is an auxiliary function for relabelToMatchIgnore.
It is similar to tryRelabelFromFaces except that it uses growRelabelIgnore and matchFaceIgnore
which ignores non-matching faces rather than failing. It thus returns a definite relabelled Tgraph.
tryRelabelFromFaces (g1,fc1) (g2,fc2) - fc1 and fc2 should have the same form (RK,LK,RD,LD),
with fc1 a face in g1 and fc2 a face in g2.
g2 must have no vertices in common with g1 except for (possibly) vertices in fc2.
The result is g3 where
g3 is a relabelling of g2 which is consistent with g1 in a common single region of overlap containing fc1.

CAVEAT: Only the single tile-connected region of common overlap (containing fc2) of g2 gets relabelled
to match with g1.
-}
relabelFromFacesIgnore :: (Tgraph,TileFace) -> (Tgraph,TileFace) -> Tgraph
relabelFromFacesIgnore (g1,fc1) (g2,fc2) = relabelGraph rlab g2 where
    rlab = growRelabelIgnore g1 [fc2] (faces g2 \\ [fc2]) (fc2 `relabellingTo` fc1)

-- |growRelabelIgnore is similar to tryGrowRelabel except that it uses matchFaceIgnore (instead of tryMatchFace)
-- which ignores non-matching faces rather than failing. It thus returns a definite Relabelling.
growRelabelIgnore:: Tgraph -> [TileFace] -> [TileFace] -> Relabelling -> Relabelling
growRelabelIgnore _ [] _ rlab = rlab -- awaiting are not tile-connected to overlap region
growRelabelIgnore g (fc:fcs) awaiting rlab = 
     case matchFaceIgnore (relabelFace rlab fc) g of
       Nothing   -> growRelabelIgnore g fcs awaiting rlab
       Just orig -> growRelabelIgnore g (fcs++fcs') awaiting' rlab'
                    where (fcs', awaiting') = partition (edgeNb fc) awaiting
                          rlab' = extendRelabelling fc orig rlab 
                          -- relabelUnion (fc `relabellingTo` orig) rlab

-- |relabelGraph rlab g - uses a relabelling rlab to change vertices in a Tgraph g.
-- Caveat: This should only be used when it is known that:
-- rlab (extended with the identity) remains 1-1 on vertices in g.
-- That is, the vertices of g are disjoint from the unsafe domain of rlab.
-- This ensures the resulting Tgraph does not need an expensive check for Tgraph properties.
-- (See also checkRelabelGraph)
relabelGraph:: Relabelling -> Tgraph -> Tgraph
relabelGraph rlab g = makeUncheckedTgraph newFaces where
   newFaces = map (relabelFace rlab) (faces g) 

-- |checkRelabelGraph uses a relabelling map to change vertices in a Tgraph,
-- then checks that the result is a valid Tgraph. (see also relabelGraph)
checkRelabelGraph:: Relabelling -> Tgraph -> Tgraph
checkRelabelGraph rlab g = checkedTgraph newFaces where
   newFaces = map (relabelFace rlab) (faces g) 

-- |Uses a relabelling to relabel the three vertices of a face.
-- Any vertex not in the key set of the relabelling is left unchanged.
-- The mapping should be 1-1 on the 3 vertices to avoid creating a self loop edge.
-- This will be the case if the 3 vertices are not in the unsafe domain of the relabelling.
relabelFace:: Relabelling -> TileFace -> TileFace
relabelFace rlab = fmap (all3 (relabelV rlab)) where -- fmap of HalfTile Functor
  all3 f (a,b,c) = (f a,f b,f c)

-- |relabelV rlab v - uses relabelling rlab to find a replacement for v (returns v if none found).
-- I.e relabelV turns a Relabelling into a total function using identity
-- for vertices not in the key set of the relabelling. 
relabelV:: Relabelling -> Vertex -> Vertex
relabelV (Relabelling r) v = VMap.findWithDefault v v r

-- |relabelAvoid avoid g - produces a new Tgraph from g by relabelling.
-- Any vertex in g that is in the set avoid will be changed to a new vertex that is
-- neither in g nor in the set avoid. Vertices in g that are not in avoid will remain the same.
relabelAvoid :: VertexSet -> Tgraph -> Tgraph
relabelAvoid avoid g = 
  case nullFaces g of
      True -> g
      _ -> relabelGraph rlab g
  where
    gverts = vertexSet g
    gMax = IntSet.findMax gverts
    avoidMax = if IntSet.null avoid then 0 else IntSet.findMax avoid
    vertsToChange = gverts `IntSet.intersection` avoid
    rlab = relabellingFrom (1+ max gMax avoidMax) vertsToChange
  -- assert: rlab is 1-1 on the vertices of g
  --    because the unsafe domain of rlab excludes all vertices of g
  -- assert: the relabelling preserves Tgraph properties
  -- assert: the relabelled Tgraph does not have vertices in the set avoid

{- 
relabelAvoid :: VertexSet -> Tgraph -> Tgraph
relabelAvoid avoid g = relabelGraph rlab g where
  gverts = vertexSet g
  avoidMax = if IntSet.null avoid then 0 else IntSet.findMax avoid
  vertsToChange = gverts `IntSet.intersection` avoid
  rlab = relabellingFrom (1+ max (maxV g) avoidMax) vertsToChange

  -- assert: rlab is 1-1 on the vertices of g
  -- assert: the relabelled Tgraph satisfies Tgraph properties (if g does)
  -- assert: the relabelled Tgraph does not have vertices in the set avoid
 -}
  
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
  -- assert: the relabelling preserves Tgraph properties
  -- assert: the relabelled Tgraph does not have vertices in the set (avoid\\fix)

-- |Relabel all vertices in a Tgraph using new labels 1..n (where n is the number of vertices).
relabelContig :: Tgraph -> Tgraph
relabelContig g = relabelGraph rlab g where
   rlab = relabellingFrom 1 (vertexSet g)
  -- assert: rlab is 1-1 on the vertices of g
  --  (the unsafe domain of rlab is disjoint from the vertices of g)
  -- assert: the relabelled Tgraph preserves the Tgraph properties
                     
{-|
tryMatchFace f g - looks for a face in g that corresponds to f (sharing a directed edge),
If the corresponding face does not match constructor (LK,RK,LD,RD) this stops the
matching process returning Left ... to indicate a failed match.
Otherwise it returns either Right (Just f) where f is the matched face or
Right Nothing if there is no corresponding face.
Note, a matched face must have either two or three of the correponding vertices the same.
-}
tryMatchFace:: TileFace -> Tgraph -> Try (Maybe TileFace)  
tryMatchFace face g = onFail "tryMatchFace:\n" $
  case find (`hasDedgeIn` faceDedges face) (faces g) of
    Nothing      -> Right Nothing
 --   Just corresp -> if twoVMatch corresp face (redundant test of 2 vertex match)
    Just corresp -> if isMatched corresp face
                    then Right $ Just corresp
                    else failReports 
                            ["Found non matching faces "
                            ,show (corresp, face)
                            ,"\n"
                            ]
{-  No longer used
-- |twoVMatch f1 f2 is True if the two tilefaces are the same except
-- for a single vertex label possibly not matching.
twoVMatch:: TileFace -> TileFace -> Bool
twoVMatch f1 f2 = isMatched f1 f2 &&
                  if firstV f1 == firstV f2
                  then secondV f1 == secondV f2 || thirdV f1 == thirdV f2
                  else secondV f1 == secondV f2 && thirdV f1 == thirdV f2
 -}

{-|A version of tryMatchFace that just ignores mismatches.
matchFaceIgnore f g - looks for a face in g that corresponds to f (sharing a directed edge),
If there is a corresponding face f' which matches constructor (LK,RK,LD,RD)
and corresponding directed edge then Just f' is returned
Otherwise Nothing is returned. (Thus ignoring a clash).
Note, a matched face must have either two or three of the correponding vertices the same.
-}
matchFaceIgnore:: TileFace -> Tgraph -> Maybe TileFace  
matchFaceIgnore face g = case tryMatchFace face g of
   Right mf -> mf
   Left _   -> Nothing

-- |selects the non-equal pairs from a list
differing :: Eq a => [(a,a)] -> [(a,a)]
differing = filter $ uncurry (/=) -- (\(a,b) -> a/=b)



