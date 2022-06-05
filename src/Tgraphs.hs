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
import Data.List (intersect, (\\), union,find,nub,partition,intercalate)
import qualified Data.Map.Strict as Map (Map, lookup, insert, empty, fromList,union)

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
-- We use the identity map for vertices not found in the mapping domain
-- (see relabelV).  Relabellings are expected to be 1-1.       
type Relabelling = Map.Map Vertex Vertex

-- |Uses a relabelling to change vertices in a Tgraph
-- relabelGraph vmap g will produce a valid Tgraph provided:
-- g is a valid Tgraph, and
-- the mapping vmap extended with the identity is 1-1 on vertices in g.
-- (Vertices of g that are not in the domain of the mapping are left unchanged.)
relabelGraph:: Relabelling -> Tgraph -> Tgraph
relabelGraph vmap g = checkedTgraph newFaces where
   newFaces = fmap (relabelFace vmap) (faces g) 

-- |Uses a relabelling to relabel the three vertices of a face
-- Any vertex not in the domain of the mapping is left unchanged.
-- The mapping should be 1-1 on the 3 vertices to avoid creating a self loop edge.
relabelFace:: Relabelling -> TileFace -> TileFace
relabelFace vmap = fmap (all3 (relabelV vmap))  -- fmap of HalfTile Functor

-- |relabelV vmap v. Uses relabelling vmap to find a replacement for v (leaves as v if none found)
-- I.e relabelV turns a relabelling into a total function using identity for undefined cases. 
relabelV:: Relabelling -> Vertex -> Vertex
relabelV vmap v = maybe v id (Map.lookup v vmap)

-- |Applies a function to all three elements of a triple
all3:: (a -> b) -> (a,a,a) -> (b,b,b)
all3 f (a,b,c) = (f a,f b,f c)

-- |partRelabelFixChange fix change g - produces a new Tgraph from g by relabelling.
-- Any vertex in g that is in the list fix will remain unchanged.
-- All Other vertices in g that are in the list change will be changed to new vertices that are
-- neither in g nor in the list change nor in the list fix.
-- (If a vertex in g appears in both fix and change, it will remain unchanged) 
partRelabelFixChange:: [Vertex] -> [Vertex] -> Tgraph -> Tgraph
partRelabelFixChange fix change g = relabelGraph vmap g where
  gverts = vertices g
  changeWithoutFixed = change \\ fix
  avoidTarget = fix `union` change `union` gverts -- new targets must not clash with existing labels
  vertsToChange = gverts `intersect` changeWithoutFixed
  vmap = newVMapAvoid avoidTarget vertsToChange

-- |newVMapAvoid avoid vs - produces a 1-1 mapping from vertices in vs to new vertices
-- such that no new vertex is in the list avoid or in vs
newVMapAvoid:: [Vertex] -> [Vertex] -> Relabelling
newVMapAvoid avoid vs = Map.fromList $ zip vs newvs
  where avoid' = vs `union` avoid
        newvs = makeNewVs (length vs) avoid'

-- |Relabel all vertices in Tgraph using new labels 1..n (where n is the number of vertices)
relabelAny :: Tgraph -> Tgraph
relabelAny g = relabelGraph vmap g where
   vs = vertices g
   vmap = Map.fromList $ zip vs [1..length vs]


{-|matchByCommonEdge g1 e g2  produces a relabelled version of g2 that is
consistent with g1 on their overlap.
The overlaping region must contain the common directed edge e without relabelling.
This produces an error if a mismatcch is found in the overlap.

CAVEAT: The overlap must be a SINGLE tile-connected region in g1.
(If the overlap contains more than one tile-connected region the result may not be
a correct relabelling of g2)    
-}
matchByCommonEdge:: Tgraph -> DEdge -> Tgraph -> Tgraph
matchByCommonEdge g1 e g2 = either showError id $ tryMatchByCommonEdge g1 e g2 where
  showError lines = error $ intercalate "\n" lines
 
{-|tryMatchByCommonEdge g1 e g2  produces either Right g where g is a relabelled version of g2 that is
consistent with g1 on their overlap or Left lines if there is a mismatch (lines explaining the problem).
The overlaping region must contain the common directed edge e without relabelling.

CAVEAT: The overlap must be a SINGLE tile-connected region in g1.
(If the overlap contains more than one tile-connected region the result may not be
a correct relabelling of g2)    
-}
tryMatchByCommonEdge:: Tgraph -> DEdge -> Tgraph -> Either [String] Tgraph
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
matchByEdges ge1 ge2 = either showError id $ tryMatchByEdges ge1 ge2 where
  showError lines = error $ intercalate "\n" lines
 
{-|tryMatchByEdges (g1,e1) (g2,e2) produces either Right g where g is a relabelled version of g2 that is
consistent with g1 on their overlap or Left lines if there is a mismatch (lines explaining the problem).
The overlapping region must contain the directed edge e1 in g1 and e2 in g2,
which will be identified by the relabelling of g2.

CAVEAT: The overlap must be a SINGLE tile-connected region in g1.
(If the overlap contains more than one tile-connected region the result may not be
a correct relabelling of g2)    
-}
tryMatchByEdges :: (Tgraph,DEdge) -> (Tgraph,DEdge) -> Either [String] Tgraph
tryMatchByEdges (g1,(x1,y1)) (g2,(x2,y2)) = 
   case find  (hasDEdge (x1,y1)) (faces g2prepared) of
     Just fc2 -> case matchFaceIn g1 fc2 of
                   Right (Just fc1) -> case findRelabelling (g1,fc1) (g2,fc2) of
                                          Right vmap  -> Right $ relabelGraph vmap g2prepared
                                          Left  lines -> Left lines
                   Right Nothing ->   Left ["tryMatchByEdges:"
                                           ,"no matching face found at edge "++show (x1,y1)
                                           ,"for relabelled face " ++ show fc2]
                   Left lines -> Left lines
     _ -> Left ["tryMatchByEdges:", "No face found for edge " ++ show (x2,y2)]
   where
     g2' = partRelabelFixChange [x2,y2] (vertices g1) g2
     g2prepared | null prs = g2'
                | otherwise = relabelGraph (Map.fromList prs) g2'
     prs = differences [(x2,x1),(y2,y1)]


{-|findRelabelling is an auxiliary function for tryMatchByEdges and tryMatchByCommonEdge.
fc1 and fc2 should be matching face types and have at least 2 vertices the same (without relabelling),
with fc1 in g1 and fc2 in g2.
g2 must have no vertices in common with g1 except for the common vertices in fc1 and fc2.
findRelabelling (g1,fc1) (g2,fc2) - produces either Right rel where
rel is a relabelling map to make g2 consistent with g1 in a region of overlap if this is possible, or
Left lines if there is a mismatch (lines explaining the problem).
In the successful case rel when applied to fc2 will be identical to fc1.

CAVEAT: The common overlap needs to be a SINGLE region of tile-connected faces in g1.
The common region must contain fc1 in g1 and fc2 in g2.
This may not produce correct results if there are other overlaps not tile-connected in g1
to the region containing fc1
-}
findRelabelling:: (Tgraph,TileFace) -> (Tgraph,TileFace) -> Either [String] Relabelling
findRelabelling (g1,fc1) (g2,fc2) = addRelabel g1 [fc2] [] (faces g2 \\ [fc2]) (initVMap fc1 fc2)

-- |initVMap f1 f2 - creates a relabelling so that
-- if applied to face f2, the vertices will match with face f1 exactly.
initVMap :: TileFace -> TileFace -> Relabelling
initVMap f1 f2 -- f2 relabels to f1
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
If a processed face has an edge in common with a face in g but the faces do not match, this
indicates a mismatch on the overlap and Left ... is returned.
-}
addRelabel:: Tgraph -> [TileFace] -> [TileFace] -> [TileFace] -> Relabelling -> Either [String] Relabelling
addRelabel g [] tried awaiting vMap = 
  addRelabelBdCheck (makeBoundary g) tried vMap -- awaiting do not need to be checked
addRelabel g (fc:fcs) tried awaiting vMap = 
  case matchVsIn g (relabelFace vMap fc) of
       Right prs -> addRelabel g (fcs++fcs') tried awaiting' vMap'
                      where (fcs', awaiting') = partition (edgeNb fc) awaiting
                            vMap' = Map.union (Map.fromList prs) vMap 
       Left lines -> Left lines


{-|addRelabelBdCheck is an auxiliary function for addRelabel to check boundary cases.
When it is called, all faces in the overlap have been processed.
The awaiting list is then dropped because:
faces in awaiting cannot have an edge in common with the processed overlap
and therefore cannot have a  boundary edge.
(If they have a single boundary vertex, this already has a relabelling in vMap)
It remains to process the tried faces to see if they are on the boundary.
If not, they can be ignored, but if they have a boundary edge, we need to find if the third vertex 
coincides with a neighbouring boundary vertex, in case this vertex also needs relabelling.

CAVEAT: All this assumes the overlap was a single region (satisfying Tgraph proerties) 
-}
addRelabelBdCheck:: Boundary -> [TileFace] -> Relabelling -> Either [String] Relabelling
addRelabelBdCheck bdry [] vMap = Right vMap
addRelabelBdCheck bdry (fc:tried) vMap =
    if hasDEdgeIn (bDedges bdry) fc'
    then case checkForBoundaryThirdV bdry fc' of 
          Right prs -> addRelabelBdCheck bdry tried (Map.union (Map.fromList prs) vMap)
          Left lines -> Left lines
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
checkForBoundaryThirdV:: Boundary -> TileFace -> Either [String] [(Vertex,Vertex)]
checkForBoundaryThirdV bdry fc =  
  case findThirdV bdry e n m of
      Just v' -> let fc' = relabelFace (Map.insert v v' Map.empty) fc in
                  if noConflictCheck fc' bdry 
                  then Right [(v,v')]
                  else Left ["checkForBoundaryThirdV:"
                            ,"Conflicting face found: " ++ show fc'
                            ,"Relabelled from: " ++ show fc
                            ]
      Nothing -> Right []
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
matchVsIn g f - looks for a face in g that corresponds to f (sharing a directed edge),
If the corresponding face does not match properly (with twoVMatch) this stops the
matching process returning Left ... to indicate a failed match.
Otherwise it returns Right [(v,v')] when the third vertices are different (a relabel for a full match)
It returns Right [] if either there is no corresponding face or if there is a corresponding
face which matches all 3 vertices.
-}
matchVsIn:: Tgraph -> TileFace -> Either [String] [(Vertex, Vertex)] 
matchVsIn g face = 
  case correspondingIn g face of
    Just corresp -> if twoVMatch corresp face
                    then Right $  differences $ zip (faceVList face) (faceVList corresp)
                    else Left ["matchVsIn: Found non matching faces ", show (corresp, face)]
    _            -> Right []

-- |correspondingIn g fc - returns  Just f if f is a face in g with a directed edge matching an edge in fc.
-- It does NOT check if the faces are the same with same ordering of edges.
-- It returns Nothing if no such face is found.
correspondingIn:: Tgraph -> TileFace -> Maybe TileFace
correspondingIn g fc = find (hasDEdgeIn (faceDedges fc)) (faces g)

{-|
matchFaceIn g f - looks for a face in g that corresponds to f (sharing a directed edge),
If the corresponding face does not match properly (with twoVMatch) this stops the
matching process returning Left ... to indicate a failed match.
Otherwise it returns either Right (Just f) where f is the matched face or
Right Nothing if there is no corresponding face.
-}
matchFaceIn:: Tgraph -> TileFace -> Either [String] (Maybe TileFace)  
matchFaceIn g face = 
  case correspondingIn g face of
    Just corresp -> if twoVMatch corresp face
                    then Right $ Just corresp
                    else Left ["matchFaceIn: Found non matching faces ", show (corresp, face)]
    _            -> Right Nothing


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

