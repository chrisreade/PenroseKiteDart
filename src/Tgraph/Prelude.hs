{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Tgraph.Prelude
Description : Introducing type Tgraph and basic operations for Tgraphs
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

Introduces Tgraphs and includes operations on vertices, edges and faces as well as Tgraphs.
Includes experimental SubTgraphs and also
type ReportFail for use as result of partial operations.
This module re-exports module HalfTile.
-}
module Tgraph.Prelude (module Tgraph.Prelude, module HalfTile) where

import Data.List ((\\), intersect, nub, elemIndex,foldl',group,sort)
import qualified Data.IntMap.Strict as VMap (IntMap, elems, filterWithKey, insert, empty, alter, lookup, fromList, fromListWith, (!),fromAscList)
import qualified Data.IntSet as IntSet (empty,singleton,insert,delete,fromList,toList,null,(\\),union,notMember,deleteMin,findMin)
import HalfTile

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
-- Every vertex must be a face vertex and vice versa.
-- Valid Tgraphs should be constructed with checkedTgraph to ensure required properties are checked.
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
-- This is intended for use only in testing and in checkTgraphProps
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

-- |is the graph empty?
nullGraph:: Tgraph -> Bool
nullGraph g = null (faces g)

{- *
Face removal/selection
-}

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



{- *
Required Tgraph properties
-}

-- |Checks a list of faces for edge loops, edge conflicts (illegal tilings) and
-- crossing boundaries and connectedness.
-- (No crossing boundaries and connected implies tile-connected)
-- Returns Right g where g is a Tgraph on passing checks.
-- Returns Left lines if a test fails, where lines describes the problem found.
checkTgraphProps:: [TileFace] -> ReportFail Tgraph
checkTgraphProps fcs
      | hasEdgeLoops fcs  =    Left $ "Non-valid tile-face(s)\n" ++
                                      "Edge Loops at: " ++ show (findEdgeLoops fcs) ++ "\n"
      | illegalTiling fcs   =  Left $ "Non-legal tiling\n" ++
                                      "Conflicting face edges (non-planar tiling): "
                                      ++ show (conflictingDedges fcs) ++
                                      "\nIllegal tile juxtapositions: "
                                      ++ show (illegals fcs) ++ "\n"
      | otherwise            = checkConnectedNoCross $ makeUncheckedTgraph fcs 

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


-- |Returns any repeated vertices in a single tileface for a list of tilefaces.
findEdgeLoops:: [TileFace] -> [Vertex]
findEdgeLoops = concatMap (duplicates . faceVList)

-- |Checks if there are repeated vertices within a tileface for a list of tilefaces.
-- Returns True if there are any.
hasEdgeLoops:: [TileFace] -> Bool
hasEdgeLoops = not . null . findEdgeLoops


-- |duplicates finds duplicated items in a list (reverses order but unique results)
duplicates :: Eq a => [a] -> [a]
duplicates = fst . foldl' check ([],[]) where
 check (dups,seen) x | x `elem` dups = (dups,seen)
                     | x `elem` seen = (x:dups,seen)
                     | otherwise = (dups,x:seen)
--    duplicates es = nub $ es \\ nub es

-- |conflictingDedges fcs returns a list of conflicting directed edges in fcs
-- (which should be null for a Tgraph)
conflictingDedges :: [TileFace] -> [DEdge]
conflictingDedges = duplicates . facesDedges

-- |Returns the list of all directed edges (clockwise round) a list of tile faces
facesDedges :: [TileFace] -> [(Vertex, Vertex)]
facesDedges = concatMap faceDedges
---[(a,b),(b,c),(c,a)] where (a,b,c) = tileRep face
-- | type used to classify edges of faces 
data EdgeType = Short | Long | Join deriving (Show,Eq)

-- | edgeType d f - classifies the directed edge d
-- which must be one of the three directed edges of face f.
-- An error is raised if it is not a directed edge of the face
edgeType:: DEdge -> TileFace -> EdgeType
edgeType d f | d == longE f  = Long
             | d == shortE f = Short
             | d == joinE f  = Join 
             | otherwise = error $ "edgeType: directed edge " ++ show d ++ 
                                   " not found in face " ++ show f

-- |For a list of tile faces fcs this produces a list of tuples of the form (f1,f2,etpe1,etype2)
-- where f1 and f2 share a common edge and etype1 is the type of the shared edge in f1 and
-- etype2 is the type of the shared edge in f2.
-- This list can then be checked for inconsistencies / illegal pairings (using legal).
sharedEdges:: [TileFace] -> [(TileFace,TileFace,EdgeType,EdgeType)]
sharedEdges fcs = [(f1,f2,edgeType d1 f1,edgeType d2 f2) 
                   | f1 <- fcs
                   , d1 <- faceDedges f1
                   , let d2 = reverseD d1
                   , f2 <- filter (hasDEdge d2) fcs
                  ]

-- | legal (f1,f2,etype1,etype2) is True if and only if it is legal for f1 and f2 to share an edge
-- with edge type etype1 and etype2 is equal to etype1.                   
legal:: (TileFace,TileFace,EdgeType,EdgeType) -> Bool                
legal (LK _, RK _, e1 , e2    ) = e1 == e2 
legal (RK _, LK _, e1 , e2    ) = e1 == e2 
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
illegals:: [TileFace] -> [(TileFace,TileFace,EdgeType,EdgeType)]
illegals = filter (not . legal) .  sharedEdges

-- | Returns True if there are conflicting directed edges or if there are illegal shared edges
-- in the list of tile faces
illegalTiling:: [TileFace] -> Bool
illegalTiling fcs = not (null (illegals fcs)) || not (null (conflictingDedges fcs))

-- |crossingBVs g returns a list of vertices with crossing boundaries
-- (which should be null).               
crossingBVs :: Tgraph -> [Vertex]
crossingBVs g = crossingVertices $ boundaryDedges g

-- |crossingVertices returns a list of vertices occurring more than one at the start
-- or more than once at the end of the directed edges in a list
crossingVertices:: [DEdge] -> [Vertex]
crossingVertices bdes = duplicates (fmap fst bdes) ++ duplicates (fmap snd bdes)

-- |There are crossing boundaries if vertices occur more than once
-- at the start of all boundary directed edges
-- (or more than once at the end of all boundary directed edges).
crossingBoundaries :: Tgraph -> Bool
crossingBoundaries g = not $ null $ crossingBVs g


-- |Predicate to check a Tgraph is a connected graph.
connected:: Tgraph -> Bool
connected g =   nullGraph g || (null $ snd $ connectedBy (graphEdges g) (head vs) vs)
                   where vs = vertices g

-- |Auxiliary function for calculating connectedness.
-- connectedBy edges v verts returns the sublist of verts connected to v 
-- by a chain of edges, paired with a list of vertices that are not connected to v.
-- This version uses an IntMap to represent edges (Vertex to [Vertex])
-- and uses Sets for the search algorithm arguments.
connectedBy :: [DEdge] -> Vertex -> [Vertex] -> ([Vertex],[Vertex])
connectedBy edges v verts = search IntSet.empty (IntSet.singleton v) (IntSet.delete v $ IntSet.fromList verts) where 
  nextMap = VMap.fromListWith (++) $ map (\(a,b)->(a,[b])) edges
-- search arguments (sets):  done (=processed), visited, unvisited.
  search done visited unvisited 
    | IntSet.null unvisited = (IntSet.toList visited ++ IntSet.toList done,[])
    | IntSet.null visited = (IntSet.toList done, IntSet.toList unvisited)  -- any unvisited are not connected
    | otherwise =
        search (IntSet.insert x done) (IntSet.union newVs visited') (unvisited IntSet.\\ newVs)
        where x = IntSet.findMin visited
              visited' = IntSet.deleteMin visited
              newVs = IntSet.fromList $ filter (`IntSet.notMember` done) $ nextMap VMap.! x 

{- Older Version without sets
-- |Auxiliary function for calculating connectedness by depth first search.
-- connectedBy edges v verts returns the sublist of verts connected to v 
-- by a chain of edges, paired with a list of vertices not connected.
-- This version uses an IntMap to represent edges (Vertex to [Vertex])
-- and uses a depth first search algorithm.
connectedBy :: [DEdge] -> Vertex -> [Vertex] -> ([Vertex],[Vertex])
connectedBy edges v verts = dfs [] [v] (verts \\[v]) where 
  nextMap = VMap.fromListWith (++) $ map singleton edges
  singleton (a,b) = (a,[b])
-- depth first search arguments:  done (=processed), visited, unvisited.
  dfs done visited [] = (visited++done,[])
  dfs done [] unvisited = (done,unvisited) -- any unvisited are not connected
  dfs done (x:visited) unvisited 
     = dfs (x:done) (nextVs ++ visited) (unvisited \\ nextVs)
       where nextVs = case VMap.lookup x nextMap of
                       Just vs -> filter (`notElem` done) $ filter (`notElem` visited) vs
                       Nothing -> error $ "connectedBy: vertex missing from edge map: " ++ show x
-}

       
{- *
Face and Vertex Operations
-}
-- | selecting left darts, right darts, left kite, right kites from a Tgraph
ldarts,rdarts,lkites,rkites :: Tgraph -> [TileFace]
ldarts g = filter isLD (faces g)
rdarts g = filter isRD (faces g)
lkites g = filter isLK (faces g)
rkites g = filter isRK (faces g) 


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
-- isAtV v face  =  v==a || v==b || v==c where (a,b,c) = tileRep face
isAtV v (LD(a,b,c))  =  v==a || v==b || v==c
isAtV v (RD(a,b,c))  =  v==a || v==b || v==c
isAtV v (LK(a,b,c))  =  v==a || v==b || v==c
isAtV v (RK(a,b,c))  =  v==a || v==b || v==c

-- |hasVIn vs fc - asks if face fc has an element of vs as a vertex
hasVIn:: [Vertex] -> TileFace -> Bool           
hasVIn vs fc = not $ null $ faceVList fc `intersect` vs

-- |given existing vertices vs, create n new vertices
makeNewVs :: Int -> [Vertex] -> [Vertex]
makeNewVs n vs = [k+1..k+n] where k = maximum vs
-- |return one new vertex
makeNewV :: [Vertex] -> Vertex
makeNewV vs = 1+maximum vs

-- |graphValency of a vertex in a graph is the number of edges incident with the vertex.
-- (Unmatched directed edges are completed, then the total count for directed edges is divided by 2)
graphValency:: Tgraph -> Vertex -> Int
graphValency g = (valencyMap VMap.!) where
    valencyMap = VMap.fromAscList $ fmap count $ group $ sort (fmap fst edges ++ fmap snd edges)
    edges = graphEdges g
    count as@(a:_) = (a,length as `div` 2)
    count _ = error "valency: count found empty list of verticies - impossible"


{- * Edge Operations -}
{-
(a,b) is regarded as a directed edge from a to b.
A list of such pairs will usually be regarded as a list of directed edges.
In the special case that the list is symmetrically closed [(b,a) is in the list whenever (a,b) is in the list]
we will refer to this as an edge list rather than a directed edge list.                  
-}

-- |directed edges (clockwise) round a face
faceDedges::TileFace -> [DEdge]
--faceDedges face = [(a,b),(b,c),(c,a)] where (a,b,c) = tileRep face
faceDedges (LD(a,b,c)) = [(a,b),(b,c),(c,a)]
faceDedges (RD(a,b,c)) = [(a,b),(b,c),(c,a)]
faceDedges (LK(a,b,c)) = [(a,b),(b,c),(c,a)]
faceDedges (RK(a,b,c)) = [(a,b),(b,c),(c,a)]

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
faceNonPhiEdges fc = bothDirOneWay (faceDedges fc) \\ facePhiEdges fc

-- |matchingE eselect fc is a predicate on tile faces 
-- where eselect selects a particular edge type of a face
-- (eselect could be joinE or longE or shortE for example).
-- This is True for fc' if fc' has an eselect edge matching the (reversed) eselect edge of fc
matchingE :: (TileFace -> DEdge) -> TileFace -> TileFace -> Bool
matchingE eselect fc = (== reverseD (eselect fc)) . eselect

-- |special cases of matchingE eselect 
-- where eselect is longE, shortE, and joinE
-- Used in Compose.processD
matchingLongE,matchingShortE,matchingJoinE ::  TileFace -> TileFace -> Bool
matchingLongE  = matchingE longE
matchingShortE = matchingE shortE
matchingJoinE  = matchingE joinE

-- |hasDEdge e f returns True if directed edge e is one of the directed edges of face f
hasDEdge :: DEdge -> TileFace -> Bool
hasDEdge e f = e `elem` faceDedges f

-- |hasDEdgeIn es fc - is True if fc has a directed edge in the list of edges es.
hasDEdgeIn :: [DEdge] -> TileFace -> Bool
hasDEdgeIn es fc = not $ null $ es `intersect` faceDedges fc

-- |A list of all the directed edges of a graph (going clockwise round faces)
graphDedges :: Tgraph -> [(Vertex, Vertex)]
graphDedges = facesDedges . faces

-- |phiEdges returns a list of the phi-edges of a Tgraph (including kite joins).
-- This includes both directions of each edge.
phiEdges :: Tgraph -> [(Vertex, Vertex)]
phiEdges g = bothDir $ concatMap facePhiEdges $ faces g

-- |nonPhiEdges returns a list of the shorter edges of a Tgraph (including dart joins).
-- This includes both directions of each edge.
nonPhiEdges :: Tgraph -> [(Vertex, Vertex)]
nonPhiEdges g = bothDir $ concatMap faceNonPhiEdges $ faces g

-- |graphEdges returns a list of all the edges of a Tgraph (both directions of each edge).
graphEdges :: Tgraph -> [(Vertex, Vertex)]
graphEdges = bothDir . graphDedges

-- |bothDir adds missing reverse directed edges to a list of directed edges
-- to complete edges (Result is a complete edge list)
-- It assumes no duplicates in argument.
bothDir:: [DEdge] -> [DEdge]
bothDir es = missingRevs es ++ es
-- bothDir = nub . bothDirOneWay

-- |bothDirOneWay adds all the reverse directed edges to a list of directed edges
-- without checking for duplicates.
-- Should be used on lists with single directions only.
-- If the argument may contain reverse directions, use bothDir to avoid duplicates.
bothDirOneWay:: [DEdge] -> [DEdge]
--bothDirOneWay = concatMap (\(a,b) -> [(a,b),(b,a)])
bothDirOneWay [] = []
bothDirOneWay ((e@(a,b)):es)= e:(b,a):bothDirOneWay es

-- |boundaryDedges g are missing reverse directed edges in graphDedges g (these are single directions only)
-- Direction is such that a face is on LHS and exterior is on RHS of each boundary directed edge.
boundaryDedges :: Tgraph -> [(Vertex, Vertex)]
boundaryDedges g = missingRevs (graphDedges g) where
{- original
boundaryDedges g = bothDir des \\ des where 
    des = graphDedges g
-}

-- | efficiently finds missing reverse directions from a list of directed edges (using IntMap)
missingRevs:: [DEdge] -> [DEdge]
missingRevs es = revUnmatched es where
    imap = VMap.fromListWith (++) $ map singleton es
    singleton (a,b) = (a,[b])
    seekR (a,b) = case VMap.lookup b imap of
                   Nothing -> False
                   Just vs -> a `elem` vs
                    
    revUnmatched [] = []
    revUnmatched (e@(a,b):es) | seekR e = revUnmatched es
                              | otherwise = (b,a):revUnmatched es

-- |boundary edges are face edges not shared by 2 faces (but both directions).
boundaryEdges :: Tgraph -> [(Vertex, Vertex)]
boundaryEdges  = bothDirOneWay . boundaryDedges

-- |internal edges are shared by two faces = all edges except boundary edges
internalEdges :: Tgraph -> [(Vertex, Vertex)]
internalEdges g =  des \\ fmap reverseD (missingRevs des) where
    des = graphDedges g
{-
internalEdges g = des \\ fmap reverseD bdes where
    des = graphDedges g
    bdes = bothDir des \\ des
-}

-- |two tile faces are edge neighbours
edgeNb::TileFace -> TileFace -> Bool
edgeNb fc = any (`elem` edges) . faceDedges where
      edges = fmap reverseD (faceDedges fc)


-- |Abbreviation for Mapping with Vertex keys (used for Boundaries)
type VertexMap a = VMap.IntMap a

{- |
Given list of vertices and list of faces
create an IntMap from each vertex to the list of faces (from the list) that are at that vertex
-}
makeVFMapFor:: [Vertex] -> [TileFace] -> VertexMap [TileFace]
makeVFMapFor vs = foldl' insertf start where
    start = VMap.fromList $ fmap (\v -> (v,[])) vs
    insertf vfmap f = foldr (VMap.alter addf) vfmap (faceVList f)
                      where addf Nothing = Nothing
                            addf (Just fs) = Just (f:fs)


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

-- |pushSub f sub - pushes a new tracked subset of tilefaces onto the tracked subsets of sub
-- calculated with f sub
pushSub:: (SubTgraph -> [TileFace]) -> SubTgraph -> SubTgraph
pushSub f sub = SubTgraph{fullGraph = g, trackedSubsets = f sub `intersect` faces g:trackedSubsets sub} where
    g = fullGraph sub




{- * Failure reporting (for partial operations) -}

-- | ReportFail is a synonym for Either String.  Used for results of partial functions
-- which return either Right something when defined or Laft string when there is a problem
-- where string is a failure report.
type ReportFail a = Either String a

-- | onFail s exp - inserts s at the front of failure report if exp fails with Left report
onFail:: String -> ReportFail a -> ReportFail a
onFail s = either (Left . (s++)) Right

-- | Converts a Maybe Result into a ReportFail result by treating Nothing as a failure
-- (the string s is the failure report on failure).
-- Usually used as infix (exp `nothingFail` s)
nothingFail :: Maybe b -> String -> ReportFail b
nothingFail a s = maybe (Left s) Right a

-- |Extract the (Right) result from a ReportFail, producing an error if the ReportFail is Left s.
-- The failure report is passed to error for an error report.
getResult:: ReportFail a -> a
getResult = either error id


