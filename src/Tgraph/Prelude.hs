{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Tgraph.Prelude
Description : Introducing type Tgraph and basic operations for Tgraphs
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

Introduces Tgraphs and includes operations on vertices, edges and faces as well as Tgraphs.
Also includes functions to calculate (relative) locations of vertices (createVPoints)
and re-exports module HalfTile
-}
module Tgraph.Prelude (module Tgraph.Prelude, module HalfTile) where

import Data.List ((\\), intersect, nub, elemIndex, partition, intercalate) -- partition used in addVPoints
import qualified Data.Map.Strict as Map (Map, lookup, insert, empty)

import Diagrams.Prelude  -- necessary for createVPoints

import HalfTile
import TileLib (ttangle,phi) -- necessary for New createVPoints

-- | Mapping abbreviates Data.Map.Map
type Mapping = Map.Map

{---------------------
*********************
Tgraphs
*********************
-----------------------}


{- *
Tgraphs
-}
-- |Tgraph vertices
type Vertex = Int
-- | directed edge
type DEdge = (Vertex,Vertex)


-- |Tgraph faces  (vertices clockwise starting with tile origin vertex)
-- a specialisation of HalfTile
type TileFace = HalfTile (Vertex,Vertex,Vertex)

-- |A Tgraph contains vertices, and faces (each are lists treated as sets with no repetitions).
-- Every vertex must be a face vertex and vice versa
data Tgraph = Tgraph { vertices :: [Vertex]
                     , faces    :: [TileFace]
                     } deriving (Show)

{-------------------------------------------
********************************************
Basic Tgraph, vertex, edge, face operations
********************************************
--------------------------------------------}


-- |Creates a (possibly invalid) Tgraph from a list of faces by calculating vertices.
-- It does not perform checks on the faces. Use checkedTgraph to perform checks.
makeUncheckedTgraph:: [TileFace] -> Tgraph
makeUncheckedTgraph fcs =
    Tgraph { vertices = nub $ concatMap faceVList fcs
           , faces = fcs
           }

-- |Creates a Tgraph from a list of faces AND checks for edge conflicts and
-- crossing boundaries and connectedness with checkTgraphProps.
-- (No crossing boundaries and connected implies tile-connected).
-- Produces an error if a check fails.
checkedTgraph:: [TileFace] -> Tgraph
checkedTgraph fcs = getResult $ onFail report (checkTgraphProps fcs)
 where report = "checkedTgraph:\nFailed for faces: \n" ++ show fcs ++ "\n"

-- |Checks a list of faces for edge loops, edge conflicts (illegal tilings) and
-- crossing boundaries and connectedness.
-- (No crossing boundaries and connected implies tile-connected)
-- Returns Right g where g is a Tgraph on passing checks.
-- Returns Left lines if a test fails, where lines describes the problem found.
checkTgraphProps:: [TileFace] -> ReportFail Tgraph
checkTgraphProps fcs
      | hasEdgeLoops fcs  =    Left $ "Non-valid tile-face(s)\n" ++
                                      "Edge Loops at: " ++ show (findEdgeLoops fcs) ++ "\n"
      | illegalTiling g   =    Left $ "Non-legal tiling\n" ++
                                      "Conflicting face edges (non-planar tiling): "
                                      ++ show (conflictingDedges g) ++
                                      "\nIllegal tile juxtapositions: "
                                      ++ show (illegals g) ++ "\n"
      | otherwise            = checkConnectedNoCross g 
  where g = makeUncheckedTgraph fcs

-- |Checks a Tgraph for crossing boundaries and connectedness.
-- (No crossing boundaries and connected implies tile-connected)
-- Returns Right g where g is a Tgraph on passing checks.
-- Returns Left lines if a test fails, where lines describes the problem found.
checkConnectedNoCross:: Tgraph -> ReportFail Tgraph
checkConnectedNoCross g
  | not (connected g) =    Left "Non-valid Tgraph (Not connected)\n" 
  | crossingBoundaries g = Left $ "Non-valid Tgraph\n" ++
                                  "Crossing boundaries found at " ++ show (crossingBVs g) ++ "\n"
  | otherwise            = Right g 
  
-- |selects faces from a Tgraph (removing any not in the list),
-- but checks resulting Tgraph for required properties
-- e.g. connectedness and no crossing boundaries.
selectFaces :: [TileFace] -> Tgraph -> Tgraph
selectFaces fcs g = checkedTgraph (faces g `intersect` fcs)

-- |removes faces from a Tgraph,
-- but checks resulting Tgraph for required properties
-- e.g. connectedness and no crossing boundaries.
removeFaces :: [TileFace] -> Tgraph -> Tgraph
removeFaces fcs g = checkedTgraph (faces g \\ fcs)

-- |removeVertices vs g - removes any vertex in the list vs from g
-- by removing all faces at those vertices. Resulting Tgrpah is checked
-- for required properties  e.g. connectedness and no crossing boundaries.
removeVertices :: [Vertex] -> Tgraph -> Tgraph
removeVertices vs g = removeFaces (filter (hasVIn vs) (faces g)) g

-- |selectVertices vs g - removes any face that does not have a vertex in the list vs from g.
-- Resulting Tgrpah is checked
-- for required properties  e.g. connectedness and no crossing boundaries.
selectVertices :: [Vertex] -> Tgraph -> Tgraph
selectVertices vs g = selectFaces (filter (hasVIn vs) (faces g)) g

-- |is the graph empty?
nullGraph:: Tgraph -> Bool
nullGraph g = null (faces g)


{- *
Tests and Tgraph properties
-}

-- |Returns any repeated vertices in a single tileface for a list of tilefaces.
findEdgeLoops:: [TileFace] -> [Vertex]
findEdgeLoops = concatMap (duplicates . faceVList)

-- |Checks if there are repeated vertices within a tileface for a list of tilefaces.
-- Returns True if there are any.
hasEdgeLoops:: [TileFace] -> Bool
hasEdgeLoops = not . null . findEdgeLoops


-- |duplicates finds duplicated items in a list
duplicates :: Eq a => [a] -> [a]
duplicates [] = []
duplicates (e:es) | e `elem` es = e:duplicates es
                | otherwise = duplicates es
--    duplicates es = es \\ nub es

-- |conflictingDedges g returns a list of conflicting directed edges of the faces in g
-- (which should be null)
conflictingDedges :: Tgraph -> [DEdge]
conflictingDedges g = duplicates $  graphDedges g

-- |Returns the list of all directed edges (clockwise round) a list of tile faces
facesDedges :: [TileFace] -> [(Vertex, Vertex)]
facesDedges = concatMap faceDedges

-- | type used to classify edges of faces 
data EdgeType = Short | Long | Join deriving (Show,Eq)

-- | edgeType d f - classifies the directed edge d
-- which must be one of the three directed edges of face f.
-- An error is raised if it is not a directed edge of the face
edgeType:: DEdge -> TileFace -> EdgeType
edgeType d f | d == longE f  = Long
             | d == shortE f = Short
             | d == joinE f  = Join 
             | otherwise = error $ "edgeType: edge " ++ show d ++ 
                                   " not found in face " ++ show f

-- |For a Tgraph g this produces a list of tuples of the form (f1,f2,etpe1,etype2)
-- where f1 and f2 share a common edge and etype1 is the type of the shared edge in f1 and
-- etype2 is the type of the shared edge in f2.
-- This list can then be checked for inconsistencies / illegal pairings (using legal).
sharedEdges:: Tgraph -> [(TileFace,TileFace,EdgeType,EdgeType)]
sharedEdges g = [(f1,f2,edgeType d1 f1,edgeType d2 f2) 
                 | f1 <- faces g
                 , d1 <- faceDedges f1
                 , let d2 = reverseD d1
                 , f2 <- filter (hasDEdge d2) (faces g)
                ]

-- | legal (f1,f2,etype1,etype2) is True if and only if it is legal for f1 and f2 to share an edge
-- with edge type etype1 and etype2 is equal to etype1.                   
legal:: (TileFace,TileFace,EdgeType,EdgeType) -> Bool                
legal (LK _, RK _, e1 , e2    ) = e1 ==e2 
legal (RK _, LK _, e1 , e2    ) = e1 ==e2 
legal (LK _, RD _, Short,Short) = True
legal (RD _, LK _, Short,Short) = True
legal (LK _, RD _, Long, Long ) = True
legal (RD _, LK _, Long, Long ) = True
legal (LD _, RD _, Join, Join ) = True
legal (RD _, LD _, Join, Join ) = True
legal (LD _, RD _, Long, Long ) = True
legal (RD _, LD _, Long, Long ) = True
legal (LD _, RK _, Short,Short) = True
legal (RK _, LD _, Short,Short) = True
legal (LD _, RK _, Long, Long ) = True
legal (RK _, LD _, Long, Long ) = True
legal _ = False               

-- | Returns a list of illegal face parings of the form (f1,f2,e1,e2) where f1 and f2 share an edge
-- and e1 is the type of this edge in f1, and e2 is the type of this edge in f2.
-- The list should be null for a legal Tgraph.
illegals:: Tgraph -> [(TileFace,TileFace,EdgeType,EdgeType)]
illegals g = filter (not . legal) $ sharedEdges g

-- | Returns True if there are conflicting directed edges in g or if there are illegal shared edges in g
illegalTiling:: Tgraph -> Bool
illegalTiling g = not (null (illegals g)) || not (null (conflictingDedges g))


{- OLDER

-- |conflictingLengthEdges g returns a list of conflicting lengthed edges of the faces in g
-- (which should be null)     
conflictingLengthEdges :: Tgraph -> [DEdge]
conflictingLengthEdges g = phiEdges g `intersect` nonPhiEdges g -- using undirected edges

-- |predicate - true if there are edge conflicts in a Tgraph
edgeConflicts :: Tgraph -> Bool
edgeConflicts g = not $ null $ conflictingDedges g ++ conflictingLengthEdges g

-}

-- |crossingBVs g returns a list of vertices with crossing boundaries
-- (which should be null).               
crossingBVs :: Tgraph -> [Vertex]
crossingBVs g = duplicates $ fst <$> boundaryDedges g

-- |There are crossing boundaries if vertices occur more than once
-- at the start of all boundary directed edges
-- (or more than once at the end of all boundary directed edges).
crossingBoundaries :: Tgraph -> Bool
crossingBoundaries g = not $ null $ crossingBVs g

-- |Predicate to check a Tgraph is a connected graph. 
connected :: Tgraph -> Bool
connected g =   nullGraph g || null (vs \\ connectedBy (graphEdges g) (head vs) vs)
                   where vs = vertices g

-- |Auxiliary function for calculating connectedness by depth first search.
-- connectedBy edges v verts returns the sublist of verts connected to v
-- by a chain of edges
connectedBy :: Eq a => [(a, a)] -> a -> [a] -> [a]
connectedBy edges v verts = dfs [] [v] (verts \\[v]) where 
-- depth first search arguments:  done (=processed), visited, unvisited
  dfs done visited [] = visited++done
  dfs done [] unvisited = done -- any unvisited are not connected
  dfs done (x:visited) unvisited 
     = dfs (x:done) (newVs ++ visited) (unvisited \\ newVs)
       where nextVs = map snd $ filter ((== x) . fst) edges
             newVs = nextVs \\ (done++visited) -- assumes no self-loops






       
{- *
Face and Vertex Operations
-}
-- | selecting left darts, right darts, left kite, right kites from a Tgraph
ldarts,rdarts,lkites,rkites :: Tgraph -> [TileFace]
ldarts g = filter isLD (faces g)
rdarts g = filter isRD (faces g)
lkites g = filter isLK (faces g)
rkites g = filter isRK (faces g) 

-- |directed edge valency of a vertex
valencyD :: Tgraph -> Vertex -> Int                  
valencyD g v = length $ filter (\(a,b) -> a==v || b==v) (graphDedges g) -- assumes no self-loops

-- |triple of face vertices in order clockwise - tileRep specialised to TileFace
faceVs::TileFace -> (Vertex,Vertex,Vertex)
faceVs = tileRep

-- |list of (three) face vertices in order clockwise
faceVList::TileFace -> [Vertex]
faceVList = (\(x,y,z) -> [x,y,z]) . faceVs
--faceVList fc = [x,y,z] where (x,y,z) =  faceVs fc

-- |Whilst first, second and third vertex of a face are obvious (clockwise), 
-- it is often more convenient to refer to the originV (=firstV),
-- oppV (the vertex at the other end of the join edge), and
-- wingV (the remaining vertex not on the join edge)
firstV,secondV,thirdV,originV,wingV,oppV:: TileFace -> Vertex
firstV  fc = a where (a,_,_) = faceVs fc
secondV fc = b where (_,b,_) = faceVs fc
thirdV  fc = c where (_,_,c) = faceVs fc

originV = firstV

wingV (LD(_,_,c)) = c
wingV (RD(_,b,_)) = b
wingV (LK(_,b,_)) = b
wingV (RK(_,_,c)) = c

oppV (LD(_,b,_)) = b
oppV (RD(_,_,c)) = c
oppV (LK(_,_,c)) = c
oppV (RK(_,b,_)) = b

-- |indexV finds the index of a vertex in a face (firstV -> 0, secondV -> 1, thirdV -> 2)
indexV :: Int -> TileFace -> Int
indexV v fc = case elemIndex v (faceVList fc) of
                  Just i -> i
                  _      -> error ("indexV: " ++ show v ++ " not found in " ++ show fc)                

-- |nextV returns the next vertex in a face going clockwise from v
-- where v must be a vertex of the face
nextV :: Vertex -> TileFace -> Vertex
nextV v fc = case indexV v fc of
                    0 -> secondV fc
                    1 -> thirdV fc
                    2 -> firstV fc
-- |prevV returns the previous vertex in a face (i.e. next going anti-clockwise) from v
-- where v must be a vertex of the face
prevV :: Vertex -> TileFace -> Vertex
prevV v fc = case indexV v fc of
                    0 -> thirdV fc
                    1 -> firstV fc
                    2 -> secondV fc

-- |isAtV v fc asks if a face fc has v as a vertex
isAtV:: Vertex -> TileFace -> Bool           
isAtV v face  =  v `elem` faceVList face

-- |hasVIn vs fc - asks if face fc has an element of vs as a vertex
hasVIn:: [Vertex] -> TileFace -> Bool           
hasVIn vs fc = not $ null $ faceVList fc `intersect` vs

-- |given existing vertices vs, create n new vertices
makeNewVs :: Int -> [Vertex] -> [Vertex]
makeNewVs n vs = [k+1..k+n] where k = maximum vs
-- |return one new vertex
makeNewV :: [Vertex] -> Vertex
makeNewV vs = 1+maximum vs


{- * Edge Operations -}
{-
(a,b) is regarded as a directed edge from a to b.
A list of such pairs will usually be regarded as a list of directed edges.
In the special case that the list is symmetrically closed [(b,a) is in the list whenever (a,b) is in the list]
we will refer to this as an edge list rather than a directed edge list.                  
-}

-- |directed edges (clockwise) round a face
faceDedges::TileFace -> [DEdge]
faceDedges face = [(a,b),(b,c),(c,a)] where (a,b,c) = faceVs face

-- |opposite directed edge
reverseD:: DEdge -> DEdge
reverseD (a,b) = (b,a)

-- |Whilst first, second and third edges are obvious (always clockwise), 
-- it is often more convenient to refer to the joinE (join edge),
-- shortE (the short edge which is not a join edge), and
-- longE (the long edge which is not a join edge).
-- These are also directed clockwise.
-- joinOfTile also returns the join edge but in the direction away from the origin
firstE,secondE,thirdE, joinE, shortE, longE, joinOfTile:: TileFace -> DEdge
firstE = head . faceDedges
secondE = head . tail . faceDedges
thirdE = head . tail . tail . faceDedges

-- |the join edge of a face in the clockwise direction going round the face (see also joinOfTile).
joinE (LD(a,b,_)) = (a,b)
joinE (RD(a,_,c)) = (c,a)
joinE (LK(a,_,c)) = (c,a)
joinE (RK(a,b,_)) = (a,b)
-- |The short edge of a face in the clockwise direction going round the face.
-- This is the non-join short edge for darts.
shortE = secondE
-- |The long edge of a face in the clockwise direction going round the face.
-- This is the non-join long edge for kites.
longE (LD(a,_,c)) = (c,a)
longE (RD(a,b,_)) = (a,b)
longE (LK(a,b,_)) = (a,b) 
longE (RK(a,_,c)) = (c,a)

-- |The join edge of a face but directed from the origin (not clockwise for RD and LK)
joinOfTile fc = (originV fc, oppV fc)

facePhiEdges, faceNonPhiEdges::  TileFace -> [DEdge]
-- |The phi edges of a face (both directions)
-- which is long edges for darts, and join and long edges for kites
facePhiEdges fc@(RD _) = [e, reverseD e] where e = longE fc
facePhiEdges fc@(LD _) = [e, reverseD e] where e = longE fc
facePhiEdges fc        = [e, reverseD e, j, reverseD j] 
                         where e = longE fc
                               j = joinE fc

-- |The non-phi edges of a face (both directions)
-- which is short edges for kites, and join and short edges for darts
faceNonPhiEdges fc = bothDir' (faceDedges fc) \\ facePhiEdges fc


-- |matchingE etype fc is a predicate on tile faces 
-- where etype finds a particular edge type of a face
-- (etype could be joinE or longE or shortE for example).
-- This is True for fc' if fc' has an etype edge matching the (reversed) etype edge of fc
matchingE :: (TileFace -> DEdge) -> TileFace -> TileFace -> Bool
matchingE etype fc = (== reverseD (etype fc)) . etype

-- |special cases of matchingE etype 
-- where etype is longE, shortE, and joinE
matchingLongE,matchingShortE,matchingJoinE ::  TileFace -> TileFace -> Bool
matchingLongE  = matchingE longE
matchingShortE = matchingE shortE
matchingJoinE  = matchingE joinE

-- |hasDEdge e f returns True if directed edge e is one of the directed edges of face f
hasDEdge :: DEdge -> TileFace -> Bool
hasDEdge e f = e `elem` (faceDedges f)

-- |hasDEdgeIn es fc - is True if fc has a directed edge in the list of edges es.
hasDEdgeIn :: [DEdge] -> TileFace -> Bool
hasDEdgeIn es fc = not $ null (es `intersect` faceDedges fc)

-- |A list of all the directed edges of a graph (going clockwise round faces)
graphDedges :: Tgraph -> [(Vertex, Vertex)]
graphDedges = facesDedges . faces

-- |phiEdges returns a list of the phi-edges of a Tgraph (= long non-join edges of the faces).
-- This includes both directions of each edge.
phiEdges :: Tgraph -> [(Vertex, Vertex)]
phiEdges g = bothDir $ fmap longE (faces g)
                       ++ fmap joinE (lkites g ++ rkites g) 

-- |nonPhiEdges returns a list of the shorter non-join edges of a Tgraph.
-- This includes both directions of each edge.
nonPhiEdges :: Tgraph -> [(Vertex, Vertex)]
nonPhiEdges g = bothDir $ fmap shortE (faces g)
                          ++ fmap joinE (ldarts g ++ rdarts g)

-- |graphEdges returns a list of all the edges of a Tgraph (both directions of each edge).
graphEdges :: Tgraph -> [(Vertex, Vertex)]
graphEdges = bothDir . graphDedges

-- |bothDir adds missing reverse directed edges to a list of directed edges and then removes duplicates
bothDir:: [DEdge] -> [DEdge]
bothDir = nub . bothDir'

-- |bothDir' adds the reverse directed edges to a list of directed edges without checking for duplicates 
bothDir':: [DEdge] -> [DEdge]
bothDir' = concatMap (\e -> [e,reverseD e])


-- |boundaryDedges g are missing reverse directed edges in graphDedges g (these are single directions only)
-- Direction is such that a face is on LHS and exterior is on RHS of each boundary directed edge.
boundaryDedges :: Tgraph -> [(Vertex, Vertex)]
boundaryDedges g = bothDir des \\ des where 
    des = graphDedges g


-- |boundary edges are face edges not shared by 2 faces (but both directions).
boundaryEdges :: Tgraph -> [(Vertex, Vertex)]
boundaryEdges  = bothDir' . boundaryDedges

-- |internal edges are shared by two faces = all edges except boundary edges
internalEdges :: Tgraph -> [(Vertex, Vertex)]
internalEdges g = des \\ fmap reverseD bdes where
    des = graphDedges g
    bdes = bothDir des \\ des


-- |two tile faces are edge neighbours
edgeNb::TileFace -> TileFace -> Bool
edgeNb fc = any (`elem` edges) . faceDedges where
      edges = fmap reverseD (faceDedges fc)




{- * Vertex Location Calculation -}

{- createVPoints
Uses addVPoints and thirdVertexLoc
and points from Diagram.Prelude
             
Used for Boundary Information and also in 
GraphConvert.makeVPatch  to make VPatches and Patches
-}

{-| createVPoints: processes a list of faces to associate points for each vertex.
     Faces must be tile-connected. It aligns the join of the first face on the x-axis.
      Returns a vertex-to-point Map.
-}
createVPoints:: [TileFace] -> Mapping Vertex (Point V2 Double)
createVPoints [] = Map.empty
createVPoints (face:more) = addVPoints [face] more (initJoin face)

{-| addVPoints readyfaces fcOther vpMap.
The first argument list of faces (readyfaces) contains the ones being processed next in order where
each will have at least two known vertex locations.
The second argument list of faces (fcOther) are faces that have not yet been added
and may not yet have known vertex locations.
The third argument is the mapping of vertices to points.
This is used in tryUpdate as well as createVPoints.
-}
addVPoints:: [TileFace] -> [TileFace] -> Mapping Vertex (Point V2 Double) -> Mapping Vertex (Point V2 Double)
addVPoints [] [] vpMap = vpMap 
addVPoints [] fcOther vpMap = error ("addVPoints: Faces not tile-connected " ++ show fcOther)
addVPoints (fc:fcs) fcOther vpMap = addVPoints (fcs++fcs') fcOther' vpMap' where
  vpMap' = case thirdVertexLoc fc vpMap of
             Just (v,p) -> Map.insert v p vpMap
             Nothing -> vpMap
  (fcs', fcOther')   = partition (edgeNb fc) fcOther

-- |initJoin fc 
-- initialises a vertex to point mapping with locations for the join edge vertices of fc
-- with originV fc at the origin and aligned along the x axis. (Used to initialise createVPoints)
initJoin::TileFace -> Mapping Vertex (Point V2 Double)                
initJoin (LD(a,b,_)) = Map.insert a origin $ Map.insert b (p2(1,0)) Map.empty -- [(a,origin), (b, p2(1,0))]
initJoin (RD(a,_,c)) = Map.insert a origin $ Map.insert c (p2(1,0)) Map.empty --[(a,origin), (c, p2(1,0))]
initJoin (LK(a,_,c)) = Map.insert a origin $ Map.insert c (p2(phi,0)) Map.empty --[(a,origin), (c, p2(phi,0))]
initJoin (RK(a,b,_)) = Map.insert a origin $ Map.insert b (p2(phi,0)) Map.empty -- [(a,origin), (b, p2(phi,0))]

-- |lookup 3 vertex locations in a vertex to point map.
find3Locs::(Vertex,Vertex,Vertex) -> Mapping Vertex (Point V2 Double)
             -> (Maybe (Point V2 Double),Maybe (Point V2 Double),Maybe (Point V2 Double))              
find3Locs (v1,v2,v3) vpMap = (Map.lookup v1 vpMap, Map.lookup v2 vpMap, Map.lookup v3 vpMap)

{-| New Version - Assumes all edge lengths are 1 or phi.
It now uses signorm to produce vectors of length 1 rather than rely on relative lengths.
(Requires ttangle and phi from TileLib).

     thirdVertexLoc fc vpMap
     where fc is a tileface and
     vpMap associates points with vertices (positions)
     It looks up all 3 vertices of fc in vpMap hoping to find at least 2 of them, it then returns Just pr
     where pr is an association pair for the third vertex.
     If all 3 are found, returns Nothing.
     If none or one found this is an error (a non tile-connected face)
-}
thirdVertexLoc:: TileFace -> Mapping Vertex (Point V2 Double) -> Maybe (Vertex, Point V2 Double)        
thirdVertexLoc fc@(LD _) vpMap = case find3Locs (faceVs fc) vpMap of
  (Just loc1, Just loc2, Nothing) -> Just (wingV fc, loc1 .+^ v)   where v = phi*^signorm (rotate (ttangle 9) (loc2 .-. loc1))
  (Nothing, Just loc2, Just loc3) -> Just (originV fc, loc2 .+^ v) where v = signorm (rotate (ttangle 7) (loc3 .-. loc2))
  (Just loc1, Nothing, Just loc3) -> Just (oppV fc, loc1 .+^ v)    where v = signorm (rotate (ttangle 1) (loc3 .-. loc1))
  (Just _ , Just _ , Just _)      -> Nothing
  _ -> error ("thirdVertexLoc: face not tile-connected?: " ++ show fc)

thirdVertexLoc fc@(RD _) vpMap = case find3Locs (faceVs fc) vpMap of
  (Just loc1, Just loc2, Nothing) -> Just (oppV fc, loc1 .+^ v)    where v = signorm (rotate (ttangle 9) (loc2 .-. loc1))
  (Nothing, Just loc2, Just loc3) -> Just (originV fc, loc3 .+^ v) where v = signorm (rotate (ttangle 3) (loc2 .-. loc3))
  (Just loc1, Nothing, Just loc3) -> Just (wingV fc, loc1 .+^ v)   where v = phi*^signorm (rotate (ttangle 1) (loc3 .-. loc1))
  (Just _ , Just _ , Just _)      -> Nothing
  _ -> error ("thirdVertexLoc: face not tile-connected?: " ++ show fc)
 
thirdVertexLoc fc@(LK _) vpMap = case find3Locs (faceVs fc) vpMap of
  (Just loc1, Just loc2, Nothing) -> Just (oppV fc, loc1 .+^ v)    where v = phi*^signorm (rotate (ttangle 9) (loc2 .-. loc1))
  (Nothing, Just loc2, Just loc3) -> Just (originV fc, loc2 .+^ v) where v = phi*^signorm (rotate (ttangle 8) (loc3 .-. loc2))
  (Just loc1, Nothing, Just loc3) -> Just (wingV fc, loc1 .+^ v)   where v = phi*^signorm (rotate (ttangle 1) (loc3 .-. loc1))
  (Just _ , Just _ , Just _)      -> Nothing
  _ -> error ("thirdVertexLoc: face not tile-connected?: " ++ show fc)
 
thirdVertexLoc fc@(RK _) vpMap = case find3Locs (faceVs fc) vpMap of
  (Just loc1, Just loc2, Nothing) -> Just (wingV fc, loc1 .+^ v)   where v = phi*^signorm (rotate (ttangle 9) (loc2 .-. loc1))
  (Nothing, Just loc2, Just loc3) -> Just (originV fc, loc2 .+^ v) where v = phi*^signorm (rotate (ttangle 8) (loc3 .-. loc2))
  (Just loc1, Nothing, Just loc3) -> Just (oppV fc, loc1 .+^ v)    where v = phi*^signorm (rotate (ttangle 1) (loc3 .-. loc1))
  (Just _ , Just _ , Just _)      -> Nothing
  _ -> error ("thirdVertexLoc: face not tile-connected?: " ++ show fc)



{- * SubTgraphs -}
{-|
 SubTgraph - introduced to allow tracking of subsets of faces
 in both force and decompose oerations.
 A SubTgraph has a main Tgraph (fullgraph) and a list of subsets of faces (trackedSubsets).
 The list allows for tracking different subsets of faces at the same time
-}
data SubTgraph = SubTgraph{ fullGraph:: Tgraph, trackedSubsets::[[TileFace]]}

-- |makeSubTgraph g trackedlist creates a SubTgraph from a Tgraph g
-- from trackedlist where each list in trackedlist is a subset of the faces of g.
-- Any faces not in g are ignored.
makeSubTgraph :: Tgraph -> [[TileFace]] -> SubTgraph
makeSubTgraph g trackedlist = SubTgraph{ fullGraph = g, trackedSubsets = fmap (`intersect` faces g) trackedlist}



{- * Error reporting (for partial operations) -}

-- | Abbreviation for use of Either String.  Used for results of partial functions
-- which return either Right something when defined or Laft string when there is a problem
-- where string is a failure report.
type ReportFail a = Either String a

-- | onFail s exp - inserts s at the front of failure report if exp fails with Left report
onFail:: String -> ReportFail a -> ReportFail a
onFail s = either (Left . (s++)) Right

-- | Converts a Maybe Result into a ReportFail result by treating Nothing as a failure
-- (the string s is prepended to the failure report on failure).
-- Usually used as infix (exp `nothingFail` s)
nothingFail :: Maybe b -> String -> ReportFail b
nothingFail a s = maybe (Left s) Right a

-- |Extract the (Right) result from a ReportFail, producing an error if the ReportFail is Left s.
-- the failure report is passed to error for an error report.
getResult:: ReportFail a -> a
getResult = either error id


