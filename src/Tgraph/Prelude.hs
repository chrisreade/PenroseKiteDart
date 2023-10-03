{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE TupleSections         #-}

{-|
Module      : Tgraph.Prelude
Description : Introducing type Tgraph and basic operations for Tgraphs
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

Introduces Tgraphs and includes operations on vertices, edges and faces as well as Tgraphs.
Includes type Try for use as result of partial operations.
This module re-exports module HalfTile.
-}
module Tgraph.Prelude (module Tgraph.Prelude, module HalfTile) where

import Data.List ((\\), intersect, union, nub, elemIndex,foldl')
import Data.Either(fromRight, lefts, rights)
import qualified Data.IntMap.Strict as VMap (IntMap, alter, lookup, fromList, fromListWith, (!),fromAscList)
import qualified Data.IntSet as IntSet (IntSet,union,empty,singleton,insert,delete,fromList,toList,null,(\\),notMember,deleteMin,findMin,findMax)
import qualified Data.Map.Strict as Map (Map, fromList, lookup)
import Control.Monad(liftM) -- for Try

import HalfTile



{---------------------
*********************
Tgraphs
*********************
-----------------------}


{-*
Tgraphs
-}
-- |Tgraph vertices
type Vertex = Int
-- | directed edge
type Dedge = (Vertex,Vertex)
-- | Vertex Sets
type VertexSet = IntSet.IntSet

-- |Tgraph faces  (vertices clockwise starting with tile origin vertex)
-- a specialisation of HalfTile
type TileFace = HalfTile (Vertex,Vertex,Vertex)

-- |A Tgraph is a list of faces along with the maximum value used for a vertex in the faces (0 for an empty list).
-- (All vertex labels should be positive, so 0 is not used as a vertex label throughout)
-- Tgraphs should be constructed with makeTgraph or checkedTgraph to check required properties.
data Tgraph = Tgraph { maxV :: !Vertex  -- 0 for empty graph
                     , faces    :: [TileFace]
                     } deriving (Show)

-- |The empty Tgraph
emptyTgraph :: Tgraph
emptyTgraph = Tgraph { maxV = 0, faces = []} -- 0 never used as a vertex number

-- |the set of vertices of a graph
vertexSet:: Tgraph -> VertexSet
vertexSet = facesVSet . faces

{-------------------------------------------
********************************************
Basic Tgraph, vertex, edge, face operations
********************************************
--------------------------------------------}



-- |Creates a (possibly invalid) Tgraph from a list of faces by calculating maxV.
-- It does not perform checks on the faces. Use makeTgraph or checkedTgraph to perform checks.
-- This is intended for use only in testing
makeUncheckedTgraph:: [TileFace] -> Tgraph
makeUncheckedTgraph fcs =
    Tgraph { maxV = facesMaxV fcs
           , faces = fcs
           }

{-| Creates a Tgraph from a list of faces AND checks for edge conflicts and
crossing boundaries and connectedness with checkTgraphProps.
(No crossing boundaries and connected implies tile-connected).
Produces an error if a check fails.

Note: This does not check for touching vertices (distinct labels for the same vertex).
To perform this additional check use makeTgraph (which also calls checkTgraphProps) 
-}
checkedTgraph:: [TileFace] -> Tgraph
checkedTgraph fcs = runTry $ onFail report (checkTgraphProps fcs)
 where report = "checkedTgraph:\nFailed for faces: \n" ++ show fcs ++ "\n"


-- |is the Tgraph empty?
nullGraph:: Tgraph -> Bool
nullGraph g = null (faces g)

{-*
Basic Tgraph face operations
-}

-- | selecting left darts, right darts, left kite, right kites from a Tgraph
ldarts,rdarts,lkites,rkites :: Tgraph -> [TileFace]
ldarts g = filter isLD (faces g)
rdarts g = filter isRD (faces g)
lkites g = filter isLK (faces g)
rkites g = filter isRK (faces g) 

-- |selects faces from a Tgraph (removing any not in the list),
-- but checks resulting Tgraph for connectedness and no crossing boundaries.
selectFaces :: [TileFace] -> Tgraph -> Tgraph
selectFaces fcs g = runTry $ checkConnectedNoCross $ 
                    Tgraph {faces = newfaces, maxV = facesMaxV newfaces}
                    where newfaces = faces g `intersect` fcs
--selectFaces fcs g = runTry $ checkConnectedNoCross $ makeUncheckedTgraph $ faces g `intersect` fcs
--selectFaces fcs g = checkedTgraph (faces g `intersect` fcs)

-- |removes faces from a Tgraph,
-- but checks resulting Tgraph for connectedness and no crossing boundaries.
removeFaces :: [TileFace] -> Tgraph -> Tgraph
removeFaces fcs g = runTry $ checkConnectedNoCross $ 
                    Tgraph {faces = newfaces, maxV = facesMaxV newfaces}
                    where newfaces = faces g \\ fcs
--removeFaces fcs g = runTry $ checkConnectedNoCross $ makeUncheckedTgraph $ faces g \\ fcs
--removeFaces fcs g = checkedTgraph (faces g \\ fcs)

-- |removeVertices vs g - removes any vertex in the list vs from g
-- by removing all faces at those vertices. Resulting Tgraph is checked
-- for required properties  e.g. connectedness and no crossing boundaries.
removeVertices :: [Vertex] -> Tgraph -> Tgraph
removeVertices vs g = removeFaces (filter (hasVIn vs) (faces g)) g

-- |selectVertices vs g - removes any face that does not have a vertex in the list vs from g.
-- Resulting Tgraph is checked
-- for required properties  e.g. connectedness and no crossing boundaries.
selectVertices :: [Vertex] -> Tgraph -> Tgraph
selectVertices vs g = selectFaces (filter (hasVIn vs) (faces g)) g



{-*
Required Tgraph properties
-}

{- | Checks a list of faces for 
    edge loops,
    edge conflicts (same directed edge on two or more faces),
    illegal tilings (breaking legal rules for tiling),
    vertices not >0 ,
    no crossing boundaries, and 
    connectedness.

(No crossing boundaries and connectedness implies tile-connected)

Returns Right g where g is a Tgraph on passing checks.
Returns Left lines if a test fails, where lines describes the problem found.
-}
checkTgraphProps:: [TileFace] -> Try Tgraph
checkTgraphProps []       =  Right emptyTgraph 
checkTgraphProps fcs
      | hasEdgeLoops fcs  =  Left $ "Non-valid tile-face(s)\n" ++
                                      "Edge Loops at: " ++ show (findEdgeLoops fcs) ++ "\n"
      | illegalTiling fcs =  Left $ "Non-legal tiling\n" ++
                                      "Conflicting face edges (non-planar tiling): "
                                      ++ show (conflictingDedges fcs) ++
                                      "\nIllegal tile juxtapositions: "
                                      ++ show (illegals fcs) ++ "\n"
      | otherwise         = let vs = facesVSet fcs
                            in if IntSet.findMin vs <1 -- any (<1) $ IntSet.toList vs
                               then Left $ "Vertex numbers not all >0: " ++ show (IntSet.toList vs)
                               else checkConnectedNoCross $ Tgraph{faces = fcs, maxV = IntSet.findMax vs} 

-- |Checks a potential Tgraph for crossing boundaries and connectedness.
-- (No crossing boundaries and connected implies tile-connected)
-- Returns Right g where g is a Tgraph on passing checks.
-- Returns Left lines if a test fails, where lines describes the problem found.
-- This is used by checkTgraphProps after other checks have been made,
-- but can be used alone when other properties are known to hold (e.g. in tryPartCompose)
checkConnectedNoCross:: Tgraph -> Try Tgraph
checkConnectedNoCross g
  | not (connected g) =    Left $ "Non-valid Tgraph (Not connected)\n" ++ show (faces g) ++ "\n"
  | crossingBoundaries g = Left $ "Non-valid Tgraph\n" ++
                                  "Crossing boundaries found at " ++ show (crossingBVs g) 
                                  ++ "\nwith faces\n" ++ show (faces g)
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

-- |conflictingDedges fcs returns a list of conflicting directed edges in fcs
-- (which should be null for a Tgraph)
conflictingDedges :: [TileFace] -> [Dedge]
conflictingDedges = duplicates . facesDedges

-- |Returns the list of all directed edges (clockwise round each) of a list of tile faces
facesDedges :: [TileFace] -> [Dedge]
facesDedges = concatMap faceDedges

-- | type used to classify edges of faces 
data EdgeType = Short | Long | Join deriving (Show,Eq)

-- | edgeType d f - classifies the directed edge d
-- which must be one of the three directed edges of face f.
-- An error is raised if it is not a directed edge of the face
edgeType:: Dedge -> TileFace -> EdgeType
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
                   , f2 <- filter (`hasDedge` d2) fcs
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
crossingBVs = crossingVertices . graphBoundary 

-- |Given a list of directed boundary edges, crossingVertices returns a list of vertices occurring
-- more than once at the start of the directed edges in the list.
-- Used for finding crossing boundary vertices when the boundary is already calculated.
crossingVertices:: [Dedge] -> [Vertex]
crossingVertices des = duplicates (fmap fst des) -- OR duplicates (fmap snd des)

-- |There are crossing boundaries if vertices occur more than once
-- at the start of all boundary directed edges
-- (or more than once at the end of all boundary directed edges).
crossingBoundaries :: Tgraph -> Bool
crossingBoundaries g = not $ null $ crossingBVs g

-- |Predicate to check a Tgraph is a connected graph.
connected:: Tgraph -> Bool
connected g =   nullGraph g || null (snd $ connectedBy (graphEdges g) (IntSet.findMin vs) vs)
                   where vs = vertexSet g

-- |Auxiliary function for calculating connectedness.
-- connectedBy edges v verts returns a pair of lists of vertices (conn,unconn)
-- where conn is a list of vertices from the set verts that are connected to v by a chain of edges,
-- and unconn is a list of vertices from set verts that are not connected to v.
-- This version creates an IntMap to represent edges (Vertex to [Vertex])
-- and uses IntSets for the search algorithm arguments.
connectedBy :: [Dedge] -> Vertex -> VertexSet -> ([Vertex],[Vertex])
connectedBy edges v verts = search IntSet.empty (IntSet.singleton v) (IntSet.delete v verts) where 
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

       
{-*
Other Face and Vertex Operations
-}

-- |triple of face vertices in order clockwise starting with origin - tileRep specialised to TileFace
faceVs::TileFace -> (Vertex,Vertex,Vertex)
faceVs = tileRep

-- |list of (three) face vertices in order clockwise starting with origin
faceVList::TileFace -> [Vertex]
faceVList = (\(x,y,z) -> [x,y,z]) . faceVs

-- |the set of vertices of a face
faceVSet :: TileFace -> VertexSet
faceVSet = IntSet.fromList . faceVList

-- |the set of vertices of a list of faces
facesVSet:: [TileFace] -> VertexSet
facesVSet = mconcat . fmap faceVSet

-- |find the maximum vertex for a list of faces (0 for an empty list).
facesMaxV :: [TileFace] -> Vertex
facesMaxV [] = 0
facesMaxV fcs = IntSet.findMax $ facesVSet fcs

-- Whilst first, second and third vertex of a face are obvious (clockwise), 
-- it is often more convenient to refer to the originV (=firstV),
-- oppV (the vertex at the other end of the join edge), and
-- wingV (the remaining vertex not on the join edge)

-- |firstV, secondV and thirdV vertices of a face are counted clockwise starting with the origin
firstV,secondV,thirdV:: TileFace -> Vertex
firstV  fc = a where (a,_,_) = faceVs fc
secondV fc = b where (_,b,_) = faceVs fc
thirdV  fc = c where (_,_,c) = faceVs fc

originV,wingV,oppV:: TileFace -> Vertex
-- |the origin vertex of a face (firstV)
originV = firstV
-- |wingV returns the vertex not on the join edge of a face
wingV (LD(_,_,c)) = c
wingV (RD(_,b,_)) = b
wingV (LK(_,b,_)) = b
wingV (RK(_,_,c)) = c
-- |oppV returns the vertex at the opposite end of the join edge from the origin of a face
oppV (LD(_,b,_)) = b
oppV (RD(_,_,c)) = c
oppV (LK(_,_,c)) = c
oppV (RK(_,b,_)) = b

-- |indexV finds the index of a vertex in a face (firstV -> 0, secondV -> 1, thirdV -> 2)
indexV :: Vertex -> TileFace -> Int
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
isAtV v (LD(a,b,c))  =  v==a || v==b || v==c
isAtV v (RD(a,b,c))  =  v==a || v==b || v==c
isAtV v (LK(a,b,c))  =  v==a || v==b || v==c
isAtV v (RK(a,b,c))  =  v==a || v==b || v==c

-- |hasVIn vs fc - asks if face fc has an element of vs as a vertex
hasVIn:: [Vertex] -> TileFace -> Bool           
hasVIn vs fc = not $ null $ faceVList fc `intersect` vs

-- |n `newVsAfter` v - given existing maxV v, create a list of n new vertices [v+1..v+n]
newVsAfter :: Int -> Vertex -> [Vertex]
n `newVsAfter` v = [v+1..v+n]

{-* Other Edge Operations -}
{-
(a,b) is regarded as a directed edge from a to b.
A list of such pairs will usually be regarded as a list of directed edges.
In the special case that the list is symmetrically closed [(b,a) is in the list whenever (a,b) is in the list]
we will refer to this as an edge list rather than a directed edge list.                  
-}

-- |directed edges (clockwise) round a face
faceDedges::TileFace -> [Dedge]
faceDedges (LD(a,b,c)) = [(a,b),(b,c),(c,a)]
faceDedges (RD(a,b,c)) = [(a,b),(b,c),(c,a)]
faceDedges (LK(a,b,c)) = [(a,b),(b,c),(c,a)]
faceDedges (RK(a,b,c)) = [(a,b),(b,c),(c,a)]

-- |opposite directed edge
reverseD:: Dedge -> Dedge
reverseD (a,b) = (b,a)

-- Whilst first, second and third edges are obvious (always clockwise), 
-- it is often more convenient to refer to the joinE (join edge),
-- shortE (the short edge which is not a join edge), and
-- longE (the long edge which is not a join edge).
-- These are also directed clockwise.
-- joinOfTile also returns the join edge but in the direction away from the origin

-- |firstE, secondE and thirdE are the directed edges of a face counted clockwise from the origin, 
firstE,secondE,thirdE:: TileFace -> Dedge
firstE = head . faceDedges
secondE = head . tail . faceDedges
thirdE = head . tail . tail . faceDedges

joinE, shortE, longE, joinOfTile:: TileFace -> Dedge
-- |the join directed edge of a face in the clockwise direction going round the face (see also joinOfTile).
joinE (LD(a,b,_)) = (a,b)
joinE (RD(a,_,c)) = (c,a)
joinE (LK(a,_,c)) = (c,a)
joinE (RK(a,b,_)) = (a,b)
-- |The short directed edge of a face in the clockwise direction going round the face.
-- This is the non-join short edge for darts.
shortE = secondE
-- |The long directed edge of a face in the clockwise direction going round the face.
-- This is the non-join long edge for kites.
longE (LD(a,_,c)) = (c,a)
longE (RD(a,b,_)) = (a,b)
longE (LK(a,b,_)) = (a,b) 
longE (RK(a,_,c)) = (c,a)

-- |The join edge of a face directed from the origin (not clockwise for RD and LK)
joinOfTile fc = (originV fc, oppV fc)

facePhiEdges, faceNonPhiEdges::  TileFace -> [Dedge]
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
matchingE :: (TileFace -> Dedge) -> TileFace -> TileFace -> Bool
matchingE eselect fc = (== reverseD (eselect fc)) . eselect

-- |special cases of matchingE eselect 
-- where eselect is longE, shortE, and joinE
-- Used in Compose (getDartWingInfo and composedFaceGroups)
matchingLongE,matchingShortE,matchingJoinE ::  TileFace -> TileFace -> Bool
matchingLongE  = matchingE longE
matchingShortE = matchingE shortE
matchingJoinE  = matchingE joinE

-- |hasDedge f e returns True if directed edge e is one of the directed edges of face f
hasDedge :: TileFace -> Dedge -> Bool
hasDedge f e = e `elem` faceDedges f

-- |hasDedgeIn fc es - is True if fc has a directed edge in the list of directed edges es.
hasDedgeIn :: TileFace -> [Dedge] -> Bool
hasDedgeIn fc es = not $ null $ es `intersect` faceDedges fc


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
graphEdges = facesEdges . faces

-- |facesEdges returns a list of all the edges of a list of TileFaces (both directions of each edge).
facesEdges :: [TileFace] -> [(Vertex, Vertex)]
facesEdges = bothDir . facesDedges

-- |bothDir adds missing reverse directed edges to a list of directed edges
-- to complete edges (Result is a complete edge list)
-- It assumes no duplicates in argument.
bothDir:: [Dedge] -> [Dedge]
bothDir es = missingRevs es ++ es
-- bothDir = nub . bothDirOneWay

-- |bothDirOneWay adds all the reverse directed edges to a list of directed edges
-- without checking for duplicates.
-- Should be used on lists with single directions only.
-- If the argument may contain reverse directions, use bothDir to avoid duplicates.
bothDirOneWay:: [Dedge] -> [Dedge]
bothDirOneWay [] = []
bothDirOneWay (e@(a,b):es)= e:(b,a):bothDirOneWay es

-- |graphBoundary g are missing reverse directed edges in graphDedges g (the result contains single directions only)
-- Direction is such that a face is on LHS and exterior is on RHS of each boundary directed edge.
graphBoundary :: Tgraph -> [(Vertex, Vertex)]
graphBoundary = facesBoundary . faces

-- |facesBoundary fcs are missing reverse directed edges in facesDedges fcs (the result contains single directions only)
-- Direction is such that a face is on LHS and exterior is on RHS of each boundary directed edge.
facesBoundary :: [TileFace] -> [(Vertex, Vertex)]
facesBoundary fcs = missingRevs $ facesDedges fcs

-- |A list of all the directed edges of a Tgraph (going clockwise round faces)
graphDedges :: Tgraph -> [(Vertex, Vertex)]
graphDedges = facesDedges . faces

-- | efficiently finds missing reverse directions from a list of directed edges (using IntMap)
missingRevs:: [Dedge] -> [Dedge]
missingRevs es = revUnmatched es where
    imap = VMap.fromListWith (++) $ map singleton es
    singleton (a,b) = (a,[b])
    seekR (a,b) = case VMap.lookup b imap of
                   Nothing -> False
                   Just vs -> a `elem` vs
                    
    revUnmatched [] = []
    revUnmatched (e@(a,b):es) | seekR e = revUnmatched es
                              | otherwise = (b,a):revUnmatched es

{-
-- |boundary edges are face edges not shared by 2 faces (but both directions).
boundaryEdges :: Tgraph -> [(Vertex, Vertex)]
boundaryEdges  = bothDirOneWay . graphBoundary
-}

-- |internal edges are shared by two faces = all edges except boundary edges
internalEdges :: Tgraph -> [(Vertex, Vertex)]
internalEdges g =  des \\ fmap reverseD (missingRevs des) where
    des = graphDedges g

-- |two tile faces are edge neighbours
edgeNb::TileFace -> TileFace -> Bool
edgeNb fc = any (`elem` edges) . faceDedges where
      edges = fmap reverseD (faceDedges fc)

-- | Produces a mapping from the directed edges of a Tgraph to the unique tileface with that directed edge
-- (expensive)
edgeFaceMap :: Tgraph -> Map.Map Dedge TileFace
edgeFaceMap g = Map.fromList $ concatMap assign (faces g) where
  assign f = fmap (,f) (faceDedges f)

-- | look up a face for an edge in an edge face map
faceForEdge :: Dedge -> Map.Map Dedge TileFace ->  Maybe TileFace
faceForEdge = Map.lookup

-- |Abbreviation for Mapping from Vertex keys (also used for Boundaries)
type VertexMap a = VMap.IntMap a

{-|vertexFacesMap vs fcs -
For list of vertices vs and list of faces fcs,
create an IntMap from each vertex in vs to a list of those faces in fcs that are at that vertex
-}
vertexFacesMap:: [Vertex] -> [TileFace] -> VertexMap [TileFace]
vertexFacesMap vs = foldl' insertf start where
    start = VMap.fromList $ fmap (,[]) vs
    insertf vfmap f = foldr (VMap.alter addf) vfmap (faceVList f)
                      where addf Nothing = Nothing
                            addf (Just fs) = Just (f:fs)


{-* Try - result types with failure reporting (for partial operations) -}

-- | Try is a synonym for Either String.  Used for results of partial functions
-- which return either Right something when defined or Left string when there is a problem
-- where string is a failure report.
-- Note: Either String (and hence Try) is a monad, and this is used frequently for combining  partial operations.
type Try a = Either String a

-- | onFail s exp - inserts s at the front of failure report if exp fails with Left report
onFail:: String -> Try a -> Try a
onFail s = either (Left . (s++)) Right

-- | Converts a Maybe Result into a Try result by treating Nothing as a failure
-- (the string s is the failure report on failure).
-- Usually used as infix (exp `nothingFail` s)
nothingFail :: Maybe b -> String -> Try b
nothingFail a s = maybe (Left s) Right a

-- |Extract the (Right) result from a Try, producing an error if the Try is Left s.
-- The failure report is passed to error for an error report.
runTry:: Try a -> a
runTry = either error id

-- |ifFail a tr - extracts the (Right) result from tr, producing a if tr is Left s.
ifFail :: a -> Try a -> a
ifFail = fromRight 
    
-- |Combines a list of Trys into a single Try with failure overriding success.
-- It concatenates all failure reports if there are any and returns a single Left r.
-- Otherwise it produces Right rs where rs is the list of all (successful) results.
-- In particular, concatFails [] = Try []
concatFails:: [Try a] -> Try [a]
concatFails ls = case lefts ls of
                 [] -> Right $ rights ls
                 other -> Left $ mconcat other -- concatenates strings for single report

-- |Combines a list of Trys into a list of the successes, ignoring any failures.
-- In particular, ignoreFails [] = []
ignoreFails:: [Try a] -> [a]
ignoreFails = rights

-- | atLeastOne rs - returns the list of successful results if there are any, but fails with an error otherwise.
-- The error report will include the concatenated reports from the failures. 
atLeastOne:: [Try a] -> [a]
atLeastOne [] = error "atLeastOne: applied to empty list"
atLeastOne results = case rights results of
                 [] -> runTry $ onFail "atLeastOne: no successful results\n" $ concatFails results
                 _ -> ignoreFails results 

-- | noFails rs - returns the list of successes when all cases succeed, but fails with
-- an error and a concatenated failure report of all failures if there is at least one failure
-- noFails [] = []
noFails:: [Try a] -> [a]
noFails = runTry . concatFails
                          
