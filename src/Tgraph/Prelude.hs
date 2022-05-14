{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Tgraph.Prelude
Description : Introducing type Tgraph and basic operations for Tgraphs
Copyright   : (c) Chris Reade, 2021
License     : MIT
Maintainer  : chrisreade@mac.com
Stability   : experimental

Introduces Tgraphs and includes operations on vertices, edges and faces as well as Tgraphs.
Also includes functions to calculate (relative) locations of vertices (createVPoints)
and re-exports module HalfTile
-}
module Tgraph.Prelude (module Tgraph.Prelude, module HalfTile) where

import Data.List ((\\), intersect, nub, elemIndex, partition) -- partition used in addVPoints
import qualified Data.Map as Map (Map, lookup, insert, empty)

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


-- |provided a list of faces makes sense and are face-edge-connected
-- makeTgraph will create a Tgraph from the faces by calculating vertices
makeTgraph:: [TileFace] -> Tgraph
makeTgraph fcs =
    Tgraph { vertices = nub $ concatMap faceVList fcs
           , faces = fcs
           }

-- |checkTgraph creates a graph from faces but checks for edge conflicts and
-- crossing boundaries and connectedness
-- No crossing boundaries and connected => face-connected
checkTgraph:: [TileFace] -> Tgraph
checkTgraph fcs = 
    let g = makeTgraph fcs in
    if not (connected g)    then error ("checkTgraph: \nTgraph not connected\n" ++ show g) 
    else if edgeConflicts g then error ("checkTgraph: \nConflicting face edges: " ++ show (conflictingDedges g) ++
                                        "\nConflicting length edges: " ++ show (conflictingLengthEdges g) ++
                                        "\nin\n" ++ show g
                                       )
    else if crossingBoundaries g 
         then error ("checkTgraph: crossing boundaries found at " ++ show (crossingBVs g) ++
                     "\nin\n" ++ show g
                    )
         else g

-- |select or remove faces from a Tgraph,
-- but check resulting graph for connectedness and no crossing boundaries
selectFaces, removeFaces  :: [TileFace] -> Tgraph -> Tgraph
selectFaces fcs g = checkTgraph (faces g `intersect` fcs)
removeFaces fcs g = checkTgraph (faces g \\ fcs)

-- |is the graph empty?
nullGraph:: Tgraph -> Bool
nullGraph g = null (faces g)


{- *
Tests and Tgraph properties
-}


-- |conflictingDedges g returns a list of conflicting directed edges of the faces in g
-- (which should be null)
conflictingDedges :: Tgraph -> [DEdge]
conflictingDedges g = duplicates $  graphDedges g where
     duplicates es = es \\ nub es

-- |conflictingLengthEdges g returns a list of conflicting lengthed edges of the faces in g
-- (which should be null)     
conflictingLengthEdges :: Tgraph -> [DEdge]
conflictingLengthEdges g = phiEdges g `intersect` nonPhiEdges g -- using undirected edges

-- |predicate - true if there are edge conflicts in a Tgraph
edgeConflicts :: Tgraph -> Bool
edgeConflicts g = not $ null $ conflictingDedges g ++ conflictingLengthEdges g

-- |crossingBVs g returns a list of vertices with crossing boundaries
-- (which should be null).               
crossingBVs :: Tgraph -> [Vertex]
crossingBVs g = bVerts \\ nub bVerts  -- leaves any duplicates
     where bVerts = fst <$> boundaryDedges g -- snd could replace fst here

-- |There are crossing boundaries if vertices occur more than once
-- at the start of all boundary directed edges
-- (or more than once at the end of all boundary directed edges).
crossingBoundaries :: Tgraph -> Bool
crossingBoundaries g = not $ null $ crossingBVs g

-- |predicate to check Tgraph is a connected graph 
connected :: Tgraph -> Bool
connected g =   nullGraph g || null (vs \\ connectedTo (head vs) vs (graphEdges g))
                   where vs = vertices g

-- |auxiliary function for calculating connectedness by depth first search
-- connectedTo v unvisited edges returns list of vertices connected to v
-- from the list of vertices (unvisited) using given list of edges
connectedTo :: Eq a => a -> [a] -> [(a, a)] -> [a]
connectedTo v unvisited edges = dfs [] [v] (unvisited \\[v]) where 
-- depth first search arguments:  processed, visited, unvisited
  dfs done vs [] = vs++done
  dfs done [] unvisited = done -- any unvisited not connected
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
reverseE:: DEdge -> DEdge
reverseE (a,b) = (b,a)

-- |Whilst first, second and third edges are obvious (always clockwise), 
-- it is often more convenient to refer to the joinE (join edge),
-- shortE (the short edge which is not a join edge), and
-- longE (the long edge which is not a join edge)
-- these are also directed clockwise.
-- joinOfTile also returns the join edge but in the direction away from the origin
firstE,secondE,thirdE, joinE, shortE, longE, joinOfTile:: TileFace -> DEdge
firstE = head . faceDedges
secondE = head . tail . faceDedges
thirdE = head . tail . tail . faceDedges

-- |joinE preserves the clockwise direction unlike joinOfTile
joinE (LD(a,b,_)) = (a,b)
joinE (RD(a,_,c)) = (c,a)
joinE (LK(a,_,c)) = (c,a)
joinE (RK(a,b,_)) = (a,b)
-- |shortE not the join edge in the dart cases
shortE = secondE
-- |longE not the join edge in the kite cases
longE (LD(a,_,c)) = (c,a)
longE (RD(a,b,_)) = (a,b)
longE (LK(a,b,_)) = (a,b) 
longE (RK(a,_,c)) = (c,a)

-- |The directed join edge of a face but origin first (not clockwise for RD and LK)
joinOfTile fc = (originV fc, oppV fc)

facePhiEdges, faceNonPhiEdges::  TileFace -> [DEdge]
-- |The phi edges of a face (both directions)
-- which is long edges of darts, and join and long edges of kites
facePhiEdges fc@(RD _) = [e, reverseE e] where e = longE fc
facePhiEdges fc@(LD _) = [e, reverseE e] where e = longE fc
facePhiEdges fc        = [e, reverseE e, j, reverseE j] 
                         where e = longE fc
                               j = joinE fc

-- |The non-phi edges of a face (both directions)
-- which is short edges of kites, and join and short edges of darts
faceNonPhiEdges fc = bothDir' (faceDedges fc) \\ facePhiEdges fc


-- |matchingE etype fc is a predicate on tile faces 
-- where etype finds a particular type of edge of a face
-- etype could be joinE or longE or shortE   for example
-- it maps fc' to True if fc' has an etype edge matching the (reversed) etype edge of fc
matchingE :: (TileFace -> DEdge) -> TileFace -> TileFace -> Bool
matchingE etype fc = (== reverseE (etype fc)) . etype

-- |special cases of matchingE etype 
-- where etype is longE, shortE and joinE
matchingLongE,matchingShortE,matchingJoinE ::  TileFace -> TileFace -> Bool
matchingLongE  = matchingE longE
matchingShortE = matchingE shortE
matchingJoinE  = matchingE joinE

-- |all the directed edges of a graph
graphDedges :: Tgraph -> [(Vertex, Vertex)]
graphDedges g = concatMap faceDedges (faces g)

-- |phiEdges returns a list of the longer edges of a Tgraph (both directions of each edge)
phiEdges :: Tgraph -> [(Vertex, Vertex)]
phiEdges g = bothDir $ fmap longE (faces g)
                       ++ fmap joinE (lkites g ++ rkites g) 

-- |nonPhiEdges returns a list of the shorter edges of a Tgraph (both directions of each edge)
nonPhiEdges :: Tgraph -> [(Vertex, Vertex)]
nonPhiEdges g = bothDir $ fmap shortE (faces g)
                          ++ fmap joinE (ldarts g ++ rdarts g)

-- |graphEdges returns a list of all the edges of a Tgraph (both directions of each edge)
graphEdges :: Tgraph -> [(Vertex, Vertex)]
graphEdges = bothDir . graphDedges

-- |bothDir adds missing reverse directed edges to a list of directed edges and then removes duplicates
bothDir:: [DEdge] -> [DEdge]
bothDir = nub . bothDir'

-- |bothDir' adds the reverse directed edges to a list of directed edges without checking for duplicates 
bothDir':: [DEdge] -> [DEdge]
bothDir' [] = []
bothDir' (e:more) = e:reverseE e:bothDir' more

-- |boundaryDedges g are missing reverse directed edges in graphDedges g (these are single directions only)
-- Direction is such that a face is on LHS and exterior is on RHS of each boundary directed edge
boundaryDedges :: Tgraph -> [(Vertex, Vertex)]
boundaryDedges g = bothDir des \\ des where 
    des = graphDedges g


-- |boundary edges are face edges not shared by 2 faces (both directions)
boundaryEdges :: Tgraph -> [(Vertex, Vertex)]
boundaryEdges  = bothDir' . boundaryDedges

-- |internal edges are shared by two faces = all edges except boundary edges
internalEdges :: Tgraph -> [(Vertex, Vertex)]
internalEdges g = des \\ fmap reverseE bdes where
    des = graphDedges g
    bdes = bothDir des \\ des


-- |two tile faces are edge neighbours
edgeNb::TileFace -> TileFace -> Bool
edgeNb fc = any (`elem` edges) . faceDedges where
      edges = fmap reverseE (faceDedges fc)




{- * Vertex Location Calculation -}

{- createVPoints
Uses addVPoints and thirdVertexLoc
and points from Diagram.Prelude
             
Used for Boundary Information and also in 
GraphConvert.makeVPatch  to make VPatches and Patches
-}

{-| createVPoints: process list of faces to associate points for each vertex.
     Faces must be tile-connected.
-}
createVPoints:: [TileFace] -> Mapping Vertex (Point V2 Double)
createVPoints [] = Map.empty
createVPoints (face:more) = addVPoints [face] more (initJoin face)

{-| addVPoints readyfaces fcOther vpMap
The first argument list of faces (readyfaces) contains the ones being processed next in order where
each will have at least two known vertex points.
The second argument list of faces (fcOther) have not yet been added and may not yet have known vertex points.
The third argument is the mapping of vertices to points.
This is used in tryUpdate as well as createVPoints
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
-- initialises a vpMap with locations for join edge vertices of fc along x axis - used to initialise createVPoints
initJoin::TileFace -> Mapping Vertex (Point V2 Double)                
initJoin (LD(a,b,_)) = Map.insert a origin $ Map.insert b (p2(1,0)) Map.empty -- [(a,origin), (b, p2(1,0))]
initJoin (RD(a,_,c)) = Map.insert a origin $ Map.insert c (p2(1,0)) Map.empty --[(a,origin), (c, p2(1,0))]
initJoin (LK(a,_,c)) = Map.insert a origin $ Map.insert c (p2(phi,0)) Map.empty --[(a,origin), (c, p2(phi,0))]
initJoin (RK(a,b,_)) = Map.insert a origin $ Map.insert b (p2(phi,0)) Map.empty -- [(a,origin), (b, p2(phi,0))]

-- |lookup 3 vertex locations
find3Locs::(Vertex,Vertex,Vertex) -> Mapping Vertex (Point V2 Double)
             -> (Maybe (Point V2 Double),Maybe (Point V2 Double),Maybe (Point V2 Double))              
find3Locs (v1,v2,v3) vpMap = (Map.lookup v1 vpMap, Map.lookup v2 vpMap, Map.lookup v3 vpMap)

{-| thirdVertexLoc fc vpMap

New Version - Assumes all edge lengths are 1 or phi
It now uses signorm to produce vectors of length 1 rather than rely on relative lengths.

Requires ttangle and phi from TileLib

     thirdVertexLoc fc vpMap
     where fc is a tileface and
     vpMap associates points with vertices (positions)
     It looks up all 3 vertices in vpMap hoping to find 2 of them, it then returns Just pr
     where pr is an association pair for the third vertex.
     If all 3 are found, returns Nothing
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
 in both force and decompose oerations
 A SubTgraph has a main Tgraph (fullgraph) and a list of subsets of faces.
 The list allows for tracking different subsets of faces at the same time
-}
data SubTgraph = SubTgraph{ fullGraph:: Tgraph, trackedSubsets::[[TileFace]]}

-- |makeSubTgraph g trackedlist creates a SubTgraph from a Tgraph g
-- from trackedlist where each list in trackedlist is a subsets of the faces of g
-- (any faces not in g are ignored)
makeSubTgraph :: Tgraph -> [[TileFace]] -> SubTgraph
makeSubTgraph g trackedlist = SubTgraph{ fullGraph = g, trackedSubsets = fmap (`intersect` faces g) trackedlist}


