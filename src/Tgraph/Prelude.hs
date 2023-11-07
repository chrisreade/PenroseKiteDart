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
Includes type constructor Try for use as result of partial operations.
This module re-exports module HalfTile.
-}
module Tgraph.Prelude (module Tgraph.Prelude, module HalfTile) where

import Data.List ((\\), intersect, union, elemIndex,foldl',find)
import Data.Either(fromRight, lefts, rights)
import qualified Data.IntMap.Strict as VMap (IntMap, alter, lookup, fromList, fromListWith, (!))
import qualified Data.IntSet as IntSet (IntSet,union,empty,singleton,insert,delete,fromList,toList,null,(\\),notMember,deleteMin,findMin,findMax)
import qualified Data.Map.Strict as Map (Map, fromList, lookup)
import Data.Maybe (mapMaybe) -- edgeNbrs

import HalfTile



{---------------------
*********************
Tgraphs
*********************
-----------------------}


{-*
Types for Tgraphs, Vertices, Directed Edges, Faces
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

-- |A Tgraph is a list of faces.
-- (All vertex labels should be positive, so 0 is not used as a vertex label throughout)
-- Tgraphs should be constructed with makeTgraph or checkedTgraph to check required properties.
newtype Tgraph = Tgraph [TileFace]
                 deriving (Show)


{-------------------------------------------
********************************************
Basic Tgraph, vertex, edge, face operations
********************************************
--------------------------------------------}

{-*
Tgraphs and Property Checking
-}


-- |Creates a (possibly invalid) Tgraph from a list of faces.
-- It does not perform checks on the faces. Use makeTgraph or checkedTgraph to perform checks.
-- This is intended for use only when checks are known to be redundant (and data constructor Tgraph is hidden).
makeUncheckedTgraph:: [TileFace] -> Tgraph
makeUncheckedTgraph fcs = Tgraph fcs

{-| Creates a Tgraph from a list of faces AND checks for edge conflicts and
crossing boundaries and connectedness with checkTgraphProps.
(No crossing boundaries and connected implies tile-connected).
Produces an error if a check fails.

Note: This does not check for touching vertices (distinct labels for the same vertex).
To perform this additional check use makeTgraph (which also calls checkTgraphProps) 
-}
checkedTgraph:: [TileFace] -> Tgraph
checkedTgraph fcs = runTry $ onFail report (checkTgraphProps fcs)
 where report = "checkedTgraph: Failed\n"  -- ++ " for faces: " ++ show fcs ++ "\n"


{- | Checks a list of faces to exclude: 
    edge loops,
    edge conflicts (same directed edge on two or more faces),
    illegal tilings (breaking legal rules for tiling),
    vertices not all >0 ,
    crossing boundaries, and 
    non-connectedness.

(No crossing boundaries and connectedness implies tile-connected)

Returns Right g where g is a Tgraph on passing checks.
Returns Left lines if a test fails, where lines describes the problem found.
-}
checkTgraphProps:: [TileFace] -> Try Tgraph
checkTgraphProps []       =  Right emptyTgraph 
checkTgraphProps fcs
      | hasEdgeLoops fcs  =  Left $ "checkTgraphProps: Non-valid tile-face(s)\n" ++
                                      "Edge Loops at: " ++ show (findEdgeLoops fcs) ++ "\n"
      | illegalTiling fcs =  Left $ "checkTgraphProps: Non-legal tiling\n" ++
                                      "Conflicting face edges (non-planar tiling): "
                                      ++ show (conflictingDedges fcs) ++
                                      "\nIllegal tile juxtapositions: "
                                      ++ show (illegals fcs) ++ "\n"
      | otherwise         = let vs = facesVSet fcs
                            in if IntSet.findMin vs <1 -- any (<1) $ IntSet.toList vs
                               then Left $ "checkTgraphProps: Vertex numbers not all >0: " ++ show (IntSet.toList vs) ++ "\n"
                               else checkConnectedNoCross fcs 

-- |Checks a list of faces for crossing boundaries and connectedness.
-- (No crossing boundaries and connected implies tile-connected)
-- Returns Right g where g is a Tgraph on passing checks.
-- Returns Left lines if a test fails, where lines describes the problem found.
-- This is used by checkTgraphProps after other checks have been made,
-- but can be used alone when other properties are known to hold (e.g. in tryPartCompose)
checkConnectedNoCross:: [TileFace] -> Try Tgraph
checkConnectedNoCross fcs
  | not (connected fcs) =    Left $ "checkConnectedNoCross: Non-valid Tgraph (Not connected)\n" ++ show fcs ++ "\n"
  | crossingBoundaries fcs = Left $ "checkConnectedNoCross: Non-valid Tgraph\n" ++
                                  "Crossing boundaries found at " ++ show (crossingBVs fcs) 
                                  ++ "\nwith faces\n" ++ show fcs
  | otherwise            = Right (Tgraph fcs)

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
                                   " not found in face " ++ show f ++ "\n"

-- |For a list of tile faces fcs this produces a list of tuples of the form (f1,f2,etpe1,etype2)
-- where f1 and f2 share a common edge and etype1 is the type of the shared edge in f1 and
-- etype2 is the type of the shared edge in f2.
-- This list can then be checked for inconsistencies / illegal pairings (using legal).
sharedEdges:: [TileFace] -> [(TileFace,EdgeType,TileFace,EdgeType)]
sharedEdges fcs = [(f1, edgeType d1 f1, f2, edgeType d2 f2) 
                   | f1 <- fcs
                   , d1 <- faceDedges f1
                   , let d2 = reverseD d1
                   , f2 <- filter (`hasDedge` d2) fcs
                  ]

-- | legal (f1,etype1,f2,etype2) is True if and only if it is legal for f1 and f2 to share an edge
-- with edge type etype1 (and etype2 is equal to etype1).                   
legal:: (TileFace,EdgeType,TileFace,EdgeType) -> Bool                
legal (LK _, e1,    RK _ , e2    ) = e1 == e2 
legal (RK _, e1,    LK _ , e2    ) = e1 == e2 
legal (LK _, Short, RD _ , Short) = True
legal (RD _, Short, LK _ , Short) = True
legal (LK _, Long,  RD _ , Long ) = True
legal (RD _, Long,  LK _ , Long ) = True
legal (LD _, Join,  RD _ , Join ) = True
legal (RD _, Join,  LD _ , Join ) = True
legal (LD _, Long,  RD _ , Long ) = True
legal (RD _, Long,  LD _ , Long ) = True
legal (LD _, Short, RK _ , Short) = True
legal (RK _, Short, LD _ , Short) = True
legal (LD _, Long,  RK _ , Long ) = True
legal (RK _, Long,  LD _ , Long ) = True
legal _ = False               

-- | Returns a list of illegal face parings of the form (f1,e1,f2,e2) where f1 and f2 share an edge
-- and e1 is the type of this edge in f1, and e2 is the type of this edge in f2.
-- The list should be null for a legal Tgraph.
illegals:: [TileFace] -> [(TileFace,EdgeType,TileFace,EdgeType)]
illegals = filter (not . legal) .  sharedEdges

-- | Returns True if there are conflicting directed edges or if there are illegal shared edges
-- in the list of tile faces
illegalTiling:: [TileFace] -> Bool
illegalTiling fcs = not (null (illegals fcs)) || not (null (conflictingDedges fcs))

-- |crossingBVs fcs returns a list of vertices with crossing boundaries
-- (which should be null).               
crossingBVs :: [TileFace] -> [Vertex]
crossingBVs = crossingVertices . facesBoundary 

-- |Given a list of directed boundary edges, crossingVertices returns a list of vertices occurring
-- more than once at the start of the directed edges in the list.
-- Used for finding crossing boundary vertices when the boundary is already calculated.
crossingVertices:: [Dedge] -> [Vertex]
crossingVertices des = duplicates (fmap fst des) -- OR duplicates (fmap snd des)

-- |There are crossing boundaries if vertices occur more than once
-- at the start of all boundary directed edges
-- (or more than once at the end of all boundary directed edges).
crossingBoundaries :: [TileFace] -> Bool
crossingBoundaries = not . null . crossingBVs

-- |Predicate to check a Tgraph is a connected graph.
connected:: [TileFace] -> Bool
connected [] =  True
connected fcs = null (snd $ connectedBy (facesEdges fcs) (IntSet.findMin vs) vs)
                   where vs = facesVSet fcs

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
Basic Tgraph operations
-}
-- |Retrieve the faces of a Tgraph
faces :: Tgraph -> [TileFace]
faces (Tgraph fcs) = fcs

-- |The empty Tgraph
emptyTgraph :: Tgraph
emptyTgraph = Tgraph []

-- |is the Tgraph empty?
nullGraph:: Tgraph -> Bool
nullGraph = null . faces

-- |find the maximum vertex number in a Tgraph
maxV :: Tgraph -> Int
maxV = facesMaxV . faces

-- | selecting left darts, right darts, left kite, right kites from a Tgraph
ldarts,rdarts,lkites,rkites :: Tgraph -> [TileFace]
ldarts g = filter isLD (faces g)
rdarts g = filter isRD (faces g)
lkites g = filter isLK (faces g)
rkites g = filter isRK (faces g) 

-- |selects faces from a Tgraph (removing any not in the list),
-- but checks resulting Tgraph for connectedness and no crossing boundaries.
selectFaces :: [TileFace] -> Tgraph -> Tgraph
selectFaces fcs g = runTry $ checkConnectedNoCross $ faces g `intersect` fcs

-- |removes faces from a Tgraph,
-- but checks resulting Tgraph for connectedness and no crossing boundaries.
removeFaces :: [TileFace] -> Tgraph -> Tgraph
removeFaces fcs g = runTry $ checkConnectedNoCross $ faces g \\ fcs

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

-- |the set of vertices of a Tgraph
vertexSet:: Tgraph -> VertexSet
vertexSet = facesVSet . faces

-- |A list of all the directed edges of a Tgraph (going clockwise round faces)
graphDedges :: Tgraph -> [Dedge]
graphDedges = facesDedges . faces

-- |graphEdges returns a list of all the edges of a Tgraph (both directions of each edge).
graphEdges :: Tgraph -> [Dedge]
graphEdges = facesEdges . faces

-- |internal edges are shared by two faces = all edges except boundary edges
internalEdges :: Tgraph -> [Dedge]
internalEdges g =  des \\ fmap reverseD (missingRevs des) where
    des = graphDedges g

-- |graphBoundary g are missing reverse directed edges in graphDedges g (the result contains single directions only)
-- Direction is such that a face is on LHS and exterior is on RHS of each boundary directed edge.
graphBoundary :: Tgraph -> [Dedge]
graphBoundary = facesBoundary . faces

-- |phiEdges returns a list of the phi-edges of a Tgraph (including kite joins).
-- This includes both directions of each edge.
phiEdges :: Tgraph -> [Dedge]
phiEdges = bothDir . concatMap facePhiEdges . faces

-- |nonPhiEdges returns a list of the shorter edges of a Tgraph (including dart joins).
-- This includes both directions of each edge.
nonPhiEdges :: Tgraph -> [Dedge]
nonPhiEdges = bothDir . concatMap faceNonPhiEdges . faces 

-- | graphEFMap g - is a mapping associating with each directed edge of g, the unique TileFace with that directed edge.
-- This is more efficient than using dedgesFacesMap for the complete mapping.
graphEFMap :: Tgraph -> Map.Map Dedge TileFace
graphEFMap = buildEFMap . faces

-- |the default alignment of a non-empty Tgraph is (v1,v2) where v1 is the lowest numbered face origin,
-- and v2 is the lowest numbered opp vertex of faces with origin at v1. This is the lowest join of g.
-- An error will be raised if the Tgraph is empty.
defaultAlignment :: Tgraph -> (Vertex,Vertex)
defaultAlignment g | nullGraph g = error "defaultAlignment: applied to empty Tgraph\n"
                   | otherwise = lowestJoin $ faces g

      
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

-- |Returns the list of all directed edges (clockwise round each) of a list of tile faces
facesDedges :: [TileFace] -> [Dedge]
facesDedges = concatMap faceDedges

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

-- |facesEdges returns a list of all the edges of a list of TileFaces (both directions of each edge).
facesEdges :: [TileFace] -> [Dedge]
facesEdges = bothDir . facesDedges

-- |bothDir adds missing reverse directed edges to a list of directed edges
-- to complete edges (Result is a complete edge list)
-- It assumes no duplicates in argument.
bothDir:: [Dedge] -> [Dedge]
bothDir es = missingRevs es ++ es

-- |bothDirOneWay adds all the reverse directed edges to a list of directed edges
-- without checking for duplicates.
-- Should be used on lists with single directions only.
-- If the argument may contain reverse directions, use bothDir to avoid duplicates.
bothDirOneWay:: [Dedge] -> [Dedge]
bothDirOneWay [] = []
bothDirOneWay (e@(a,b):es)= e:(b,a):bothDirOneWay es

-- |facesBoundary fcs are missing reverse directed edges in facesDedges fcs (the result contains single directions only)
-- Direction is such that a face is on LHS and exterior is on RHS of each boundary directed edge.
facesBoundary :: [TileFace] -> [Dedge]
facesBoundary fcs = missingRevs $ facesDedges fcs


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




{-* Other Face Operations -}

-- |two tile faces are edge neighbours
edgeNb::TileFace -> TileFace -> Bool
edgeNb fc = any (`elem` edges) . faceDedges where
      edges = fmap reverseD (faceDedges fc)


-- |Abbreviation for Mapping from Vertex keys (also used for Boundaries)
type VertexMap a = VMap.IntMap a

{-|vertexFacesMap vs fcs -
For list of vertices vs and list of faces fcs,
create an IntMap from each vertex in vs to a list of those faces in fcs that are at that vertex.
-}
vertexFacesMap:: [Vertex] -> [TileFace] -> VertexMap [TileFace]
vertexFacesMap vs = foldl' insertf start where
    start = VMap.fromList $ fmap (,[]) vs
    insertf vfmap f = foldr (VMap.alter addf) vfmap (faceVList f)
                      where addf Nothing = Nothing
                            addf (Just fs) = Just (f:fs)

-- | dedgesFacesMap des fcs - Produces an edge-face map. Each directed edge in des is associated with
-- a unique TileFace in fcs that has that directed edge (if there is one).
-- It will report an error if more than one TileFace in fcs has the same directed edge in des. 
-- If the directed edges and faces are all those from a Tgraph, graphEFMap will be more efficient.
-- dedgesFacesMap is intended for a relatively small subset of directed edges in a Tgraph.
dedgesFacesMap:: [Dedge] -> [TileFace] -> Map.Map Dedge TileFace
dedgesFacesMap des fcs =  Map.fromList (assocFaces des) where
   vs = fmap fst des `union` fmap snd des
   vfMap = vertexFacesMap vs fcs
   assocFaces [] = []
   assocFaces (d@(a,b):more) = case (VMap.lookup a vfMap, VMap.lookup b vfMap) of
      (Just fcs1, Just fcs2) -> case filter (`hasDedge` d) $ fcs1 `intersect` fcs2 of 
                                   [fc] -> (d,fc):assocFaces more
                                   []   -> assocFaces more
                                   _   -> error $ "dedgesFacesMap: more than one Tileface has the same directed edge: "
                                                  ++ show d ++ "\n"
      _ -> assocFaces more


-- |Build a Map from directed edges to faces (the unique face containing the directed edge)
buildEFMap:: [TileFace] -> Map.Map Dedge TileFace
buildEFMap = Map.fromList . concatMap assign where
  assign f = fmap (,f) (faceDedges f)
{-
buildEFMap = mconcat . fmap processFace where
  processFace fc = Map.fromList $ (,fc) <$> faceDedges fc
-}

-- | look up a face for an edge in an edge-face map
faceForEdge :: Dedge -> Map.Map Dedge TileFace ->  Maybe TileFace
faceForEdge = Map.lookup

-- |Given a tileface (fc) and a map from each directed edge to the tileface containing it (efMap)
-- return the list of edge neighbours of fc.
edgeNbs:: TileFace -> Map.Map Dedge TileFace -> [TileFace]
edgeNbs fc efMap = mapMaybe getNbr edges where
    getNbr e = Map.lookup e efMap
    edges = fmap reverseD $ faceDedges fc

-- |For a non-empty list of tile faces
-- find the face with lowest originV (and then lowest oppV).
-- Move this face to the front of the returned list of faces.
-- Used by locateVertices to determine the starting point for location calculation
lowestJoinFirst:: [TileFace] -> [TileFace]
lowestJoinFirst fcs
  | null fcs  = error "lowestJoinFirst: applied to empty list of faces"
  | otherwise = face:(fcs\\[face])
    where a = minimum (fmap originV fcs)
          aFaces = filter ((a==) . originV) fcs
          b = minimum (fmap oppV aFaces)
          face = case find (((a,b)==) . joinOfTile) aFaces of
                  Just fc -> fc
                  Nothing -> error $ "lowestJoinFirst: no face fond at "
                                     ++ show a ++ " with opp vertex at " ++ show b ++ "\n"
--          (face: _) = filter (((a,b)==) . joinOfTile) aFaces

-- |Return the join edge with lowest origin vertex (and lowest oppV vertex if there is more than one).
-- The resulting edge is always directed from the origin to the opp vertex, i.e (orig,opp).
lowestJoin:: [TileFace] -> Dedge
lowestJoin fcs | null fcs  = error "lowestJoin: applied to empty list of faces"
lowestJoin fcs = (a,b) where
    a = minimum (fmap originV fcs)
    aFaces = filter ((a==) . originV) fcs
    b = minimum (fmap oppV aFaces)



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

-- |ifFail a tr - extracts the (Right) result from tr but returning a if tr is Left s.
ifFail :: a -> Try a -> a
ifFail = fromRight 
    
-- |Combines a list of Trys into a single Try with failure overriding success.
-- It concatenates all failure reports if there are any and returns a single Left r.
-- Otherwise it produces Right rs where rs is the list of all (successful) results.
-- In particular, concatFails [] = Right []
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
atLeastOne [] = error "atLeastOne: applied to empty list.\n"
atLeastOne results = case ignoreFails results of
                 [] -> runTry $ onFail "atLeastOne: no successful results.\n" $ concatFails results
                 other -> other 

-- | noFails rs - returns the list of successes when all cases succeed, but fails with
-- an error and a concatenated failure report of all failures if there is at least one failure
-- noFails [] = []
noFails:: [Try a] -> [a]
noFails = runTry . concatFails
                          
