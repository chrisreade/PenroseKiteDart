{-|
Module      : Tgraph.Prelude
Description : Introducing types Tgraph, VPatch and drawing operations.
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

Introduces Tgraphs and includes operations on vertices, edges and faces as well as Tgraphs.
Plus VPatch (Vertex Patch) as intermediary between Tgraph and Diagram.
Conversion and drawing operations to produce Diagrams.
The module also includes functions to calculate (relative) locations of vertices (locateVertices, addVPoint),
touching vertex checks (touchingVertices, touchingVerticesGen), and edge drawing functions.

This module re-exports module HalfTile and module Try.
-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE BangPatterns              #-}

{-# LANGUAGE StrictData             #-}

module Tgraph.Prelude
  ( module HalfTile
  , module Try
    -- * Types for Tgraphs, Faces, Vertices, Directed Edges
  , TileFace
  , Vertex
  , VertexSet
  , VertexMap
    -- $Edges
  , Dedge
  , EdgeType(..)
  , Tgraph() -- not Data Constructor Tgraph
  , makeTgraph
  , tryMakeTgraph
  , checkedTgraph
  , makeUncheckedTgraph
  , HasFaces(..) -- faces, boundary, maxV
  , dedges
  , vertexSet
  , vertices
  , boundaryVs
   -- * Property Checking for Tgraphs
--  , renumberFaces
--  , differing
  , tryTgraphProps
  , tryConnectedNoCross
  , tryCorrectTouchingVs
--  , findEdgeLoops
  , hasEdgeLoops
  , duplicates
--  , conflictingDedges
  , edgeType
  --, findEdgeLoop
--  , sharedEdges
--  , newSharedEdges
  , noNewConflict
-- unused  , noNewConflictFull
--  , legal
--  , illegals
  , illegalTiling
  , crossingBVs
 -- , crossingVertices
  , crossingBoundaries
  , connected
--  , connectedBy
    -- * Basic Tgraph and HasFaces operations
--  , faces
  , emptyTgraph
  , nullFaces
  , evalFaces
  , ldarts
  , rdarts
  , lkites
  , rkites
  , kites
  , darts
  , internalEdges
  , phiEdges
  , nonPhiEdges
  , defaultAlignment
  , selectFaces
  , removeFaces
  , removeVertices
  , selectVertices
  , vertexFacesMap
  --, dwFacesMap
    -- * Other Face/Vertex Operations
  , makeRD
  , makeLD
  , makeRK
  , makeLK
  , faceVs
  , faceVList
  , faceVSet
  , firstV
  , secondV
  , thirdV
  , originV
  , wingV
  , oppV
  , indexV
  , nextV
  , prevV
  , isAtV
  , hasVIn
    -- * Other Edge Operations
  , faceDedges
  , reverseD
  , joinE
  , shortE
  , longE
  , joinOfTile
  , facePhiEdges
  , faceNonPhiEdges
--  , matchingE
  , matchingLongE
  , matchingShortE
  , matchingJoinE
  , hasDedge
  , hasDedgeIn
  , completeEdges
  , bothDir
--   , bothDirOneWay
  , missingRevs
    -- * Other Face Operations
  , edgeNb
  , dedgesFacesMap
  , buildEFMap
  , faceForEdge
  , edgeNbs
  , extractLowestJoin
  , lowestJoin
    -- * VPatch and Conversions
  , VPatch(..)
  , VertexLocMap
  , makeVP
  , subVP
  , relevantVP
  , restrictVP
  , graphFromVP
  , removeFacesVP
  , selectFacesVP
  , findLoc
    -- * Drawing Tgraphs and Vpatches with Labels
  , DrawableLabelled(..)
  , labelSize
  , labelled
  , rotateBefore
  , dropLabels
-- * VPatch alignment with vertices
  , centerOn
  , alignXaxis
  , alignments
  , alignAll
  , alignBefore
  , makeAlignedVP
    -- *  Drawing Edges with a VPatch or a VertexLocationMap
  , drawEdgesVP
  , drawEdgeVP
  , drawLocatedEdges
  , drawLocatedEdge
    -- * Vertex Location and Touching Vertices
  , locateVertices
  , addVPoint
--  , axisJoin
--  , find3Locs
--  , thirdVertexLoc
  , touchingVertices
  , touching
  , touchingVerticesGen
  , locateVerticesGen
  --, drawEdges
  --, drawEdge
  ) where

import Data.List ((\\), intersect, union, elemIndex,foldl',find,nub)
-- import Data.Either(fromRight, lefts, rights, isLeft)
import qualified Data.IntMap.Strict as VMap (IntMap, alter, lookup, fromList, fromListWith, (!), map, filterWithKey,insert, empty, toList, assocs, keys, keysSet, findWithDefault)
import qualified Data.IntSet as IntSet (IntSet,union,empty,singleton,insert,delete,fromList,toList,null,(\\),notMember,deleteMin,findMin,findMax,member,difference,elems)
import qualified Data.Map.Strict as Map (Map, fromList, lookup, fromListWith)
import Data.Maybe (mapMaybe) -- edgeNbrs
import qualified Data.Set as Set  (fromList,member,null,delete,)-- used for locateVertices

import Diagrams.Prelude hiding (union,mapping)
-- import Diagrams.TwoD.Text (Text)

import TileLib
import HalfTile
import Try



{---------------------
*********************
Tgraphs
*********************
-----------------------}



-- Types for Tgraphs, Vertices, Directed Edges, Faces

-- |Vertex labels are integers. They must be positive for a Tgraph (Checked by makeTgraph).
type Vertex = Int
-- | directed edge
type Dedge = (Vertex,Vertex)
-- | Vertex label sets
type VertexSet = IntSet.IntSet

-- |A TileFace is a HalfTile with 3 vertex labels (clockwise starting with the origin vertex).
-- Usually referred to simply as a face.
--
-- For example a right dart: RD(1,2,3) or a left kite: LK(1,5,6)
type TileFace = HalfTile (Vertex,Vertex,Vertex)

{- | A Tgraph is a newtype for a list of faces (that is [TileFace]).
   All vertex labels should be positive, so 0 is not used as a vertex label.
   Tgraphs should be constructed with makeTgraph or checkedTgraph to check required properties.
   The data constructor Tgraph is not exported (but see also makeUncheckedTgraph).

   Use faces to retrieve the list of faces of a Tgraph.
-}
newtype Tgraph = Tgraph { -- | getFaces is not exported (use faces)
                         getFaces ::[TileFace] -- ^ Retrieve the faces of a Tgraph
                        }
                 deriving (Show)

-- |A type used to classify edges of faces.
-- Each (halftile) face has a long edge, a short edge and a join edge. 
data EdgeType = Short | Long | Join deriving (Show,Eq)

-- |Abbreviation for Mapping from Vertex keys (also used for Boundaries)
type VertexMap a = VMap.IntMap a


{-
Tgraphs Property Checking
-}

{-|
makeTgraph performs a no touching vertex check as well as using tryTgraphProps for other required properties.
It produces an error if either check fails.
Note that the other Tgraph properties are checked first, to ensure that calculation of 
vertex locations can be done for a touching vertex check.
-}
makeTgraph :: [TileFace] -> Tgraph
makeTgraph fcs = runTry $ onFail "makeTgraph: (failed):\n" $ tryMakeTgraph fcs

{-|
tryMakeTgraph performs the same checks for Tgraph properties as tryTgraphProps but in addition
it also checks that there are no touching vertices (distinct labels for the same vertex)
using touchingVertices (which calculates vertex locations).
It produces Left ... if either check fails and Right g otherwise where g is the Tgraph.
Note that the other Tgraph properties are checked first, to ensure that calculation of 
vertex locations can be done.
-}
tryMakeTgraph :: [TileFace] -> Try Tgraph
tryMakeTgraph fcs =
 do g <- tryTgraphProps fcs -- must be checked first
    let touchVs = touchingVertices (faces g)
    if null touchVs
    then Right g
    else failReport ("Found touching vertices: "
               ++ show touchVs
               ++ "\nwith faces:\n"
               ++ show fcs
               ++ "\n\n(To fix, use: tryCorrectTouchingVs)\n\n"
              )

{-| tryCorrectTouchingVs fcs finds touching vertices by calculating locations for vertices in the faces fcs,
    then renumbers to remove touching vertices (renumbers higher to lower numbers),
    then checks for Tgraph properties of the resulting faces to produce a Tgraph.
    NB fcs needs to be tile-connected before the renumbering and
    the renumbering need not be 1-1 (hence Relabelling is not used)      
-}
tryCorrectTouchingVs ::  [TileFace] -> Try Tgraph
tryCorrectTouchingVs fcs =
    onFail ("tryCorrectTouchingVs:\n" ++ show touchVs) $
    tryTgraphProps $ nub $ renumberFaces touchVs fcs
        -- renumberFaces allows for a many to 1 relabelling represented by a list 
    where touchVs = touchingVertices fcs -- uses non-generalised version of touchingVertices

-- |renumberFaces allows for a many to 1 relabelling represented by a list of pairs.
-- It is used only for tryCorrectTouchingVs in Tgraphs which then checks the result 
renumberFaces :: [(Vertex,Vertex)] -> [TileFace] -> [TileFace]
renumberFaces prs = map renumberFace where
    mapping = VMap.fromList $ differing prs
    renumberFace = fmap (all3 renumber)
    all3 f (a,b,c) = (f a,f b,f c)
    renumber v = VMap.findWithDefault v v mapping
    differing = filter $ uncurry (/=)

-- |Creates a (possibly invalid) Tgraph from a list of faces.
-- It does not perform checks on the faces. 
--
-- WARNING: This is intended for use only when checks are known to be redundant.
-- Consider using makeTgraph, tryMakeTgraph or checkedTgraph instead to perform checks.
makeUncheckedTgraph:: [TileFace] -> Tgraph
makeUncheckedTgraph = Tgraph

-- |force full evaluation of a list of faces.
evalFaces :: HasFaces a => a -> a
evalFaces a = b where
    !b = find (ev . faceVs) (faces a) `seq` a
    ev (x,y,z) = x `seq` y `seq` z `seq` False
{-     !b = find (notpos . faceVs) (faces a) `seq` a
    notpos (x,y,z) = x<1 || y<1 || z<1
 -}{- evalFaces a = eval $ faces a where
    eval fcs = find (has0 . tileRep) fcs `seq` fcs
    has0 (x,y,z) = x==0 || y==0 || z==0
 -}

{-| Creates a Tgraph from a list of faces using tryTgraphProps to check required properties
and producing an error if a check fails.

Note: This does not check for touching vertices (distinct labels for the same vertex).
To perform this additional check use makeTgraph which also uses tryTgraphProps.
-}
checkedTgraph:: [TileFace] -> Tgraph
checkedTgraph = runTry . onFail report . tryTgraphProps
 where report = "checkedTgraph: Failed\n"  -- ++ " for faces: " ++ show fcs ++ "\n"


{- | Checks a list of faces to ensure: 
    no edge loops,
    no edge conflicts (same directed edge on two or more faces),
    legal tiling (obeys rules for legal tiling),
    all vertex labels >0 ,
    no crossing boundaries, and 
    connectedness.

Returns Right g where g is a Tgraph on passing checks.
Returns Left lines if a test fails, where lines describes the problem found.
-}
tryTgraphProps:: [TileFace] -> Try Tgraph
tryTgraphProps []       =  Right emptyTgraph
tryTgraphProps fcs
      | hasEdgeLoops fcs  =
         failReport $ "tryTgraphProps: Non-valid tile-face(s)\n" ++
                      "Edge Loops at: " ++ show (findEdgeLoops fcs) ++ "\n"
      | illegalTiling fcs =  failReports
                               ["tryTgraphProps: Non-legal tiling\n"
                               ,"Conflicting face directed edges (non-planar tiling): "
                               ,show (conflictingDedges fcs)
                               ,"\nIllegal tile juxtapositions: "
                               ,show (illegals fcs)
                               ,"\n"
                               ]
      | otherwise         = let vs = vertexSet fcs
                            in if IntSet.findMin vs <1 -- any (<1) $ IntSet.toList vs
                               then failReports
                                        ["tryTgraphProps: Vertex numbers not all >0: "
                                        ,show (IntSet.toList vs)
                                        ,"\n"
                                        ]
                               else tryConnectedNoCross fcs

-- |Checks a list of faces for no crossing boundaries and connectedness.
-- (No crossing boundaries and connected implies tile-connected).
-- Returns Right g where g is a Tgraph on passing checks.
-- Returns Left lines if a test fails, where lines describes the problem found.
-- This is used by tryTgraphProps after other checks have been made,
-- but can be used alone when other properties are known to hold (e.g. in tryPartCompose)
tryConnectedNoCross:: [TileFace] -> Try Tgraph
tryConnectedNoCross fcs
  | not (connected fcs) = failReports
                              ["tryConnectedNoCross: Non-valid Tgraph (Not connected)\n"
                              ,show fcs
                              ,"\n"
                              ]
  | crossingBoundaries fcs = failReports
                                ["tryConnectedNoCross: Non-valid Tgraph\n"
                                ,"Crossing boundaries found at "
                                ,show (crossingBVs fcs)
                                ,"\nwith faces\n"
                                ,show fcs
                                ,"\n"
                                ]
  | otherwise            = Right (Tgraph fcs)

-- |Returns any repeated vertices within each TileFace for a list of TileFaces.
findEdgeLoops:: HasFaces a => a -> [Vertex]
findEdgeLoops = concatMap findEdgeLoop . faces

-- |Returns a repeated vertex for TileFace
findEdgeLoop :: TileFace -> [Vertex]
findEdgeLoop = duplicates . faceVList

-- |Checks if there are repeated vertices within any TileFace for a list of TileFaces.
-- Returns True if there are any.
hasEdgeLoops:: HasFaces a => a  -> Bool
hasEdgeLoops = not . null . findEdgeLoops

-- |duplicates finds any duplicated items in a list (unique results).
duplicates :: Eq a => [a] -> [a]
duplicates = reverse . fst . foldl' check ([],[]) where
 check (dups,seen) x | x `elem` dups = (dups,seen)
                     | x `elem` seen = (x:dups,seen)
                     | otherwise = (dups,x:seen)

-- |conflictingDedges fcs returns a list of conflicting directed edges in fcs
-- i.e. different faces having the same edge in the same direction.
-- (which should be null for a Tgraph)
conflictingDedges :: HasFaces a => a -> [Dedge]
conflictingDedges = duplicates . dedges



-- | edgeType d f - classifies the directed edge d
-- which must be one of the three directed edges of face f.
-- An error is raised if it is not a directed edge of the face
edgeType:: Dedge -> TileFace -> EdgeType
edgeType d f | d == longE f  = Long
             | d == shortE f = Short
             | d == joinE f  = Join
             | otherwise = error $ "edgeType: directed edge " ++ show d ++
                                   " not found in face " ++ show f ++ "\n"

-- |For a list of tile faces fcs this produces a list of tuples of the form (f1,etpe1,f2,etype2)
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

-- |A version of sharedEdges comparing a single face against a list of faces.
-- This does not look at shared edges within the list, but just the new face against the list.
newSharedEdges:: TileFace -> [TileFace] -> [(TileFace,EdgeType,TileFace,EdgeType)]
newSharedEdges face fcs =
    [(face, edgeType d1 face, fc', edgeType d2 fc')
     | d1 <- faceDedges face
     , let d2 = reverseD d1
     , fc' <- filter (`hasDedge` d2) fcs
    ]

-- | noNewConflict face fcs returns True if face has an illegal shared edge with fcs.
-- It does not check for illegal cases within the fcs.
noNewConflict :: TileFace -> [TileFace] -> Bool
noNewConflict face fcs = all legal shared where
    shared = newSharedEdges face fcs

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

-- |crossingBVs fcs returns a list of vertices where there are crossing boundaries
-- (which should be null for Tgraphs, VPatches, BoundaryStates, Forced, TrackedTgraph).               
crossingBVs :: HasFaces a => a -> [Vertex]
crossingBVs = duplicates . boundaryVs

-- |There are crossing boundaries if vertices occur more than once
-- in the boundary vertices.
crossingBoundaries :: HasFaces a => a  -> Bool
crossingBoundaries = not . null . crossingBVs

-- |Predicate to check if the faces are connected (in graph theory sense).
connected:: HasFaces a => a -> Bool
connected = conn . faces where
    conn [] =  True
    conn fcs = null (snd $ connectedBy (completeEdges fcs) (IntSet.findMin vs) vs)
                    where vs = vertexSet fcs

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




-- |The empty Tgraph
emptyTgraph :: Tgraph
emptyTgraph = Tgraph []

-- |are there no faces?
nullFaces:: HasFaces a => a -> Bool
nullFaces = null . faces

-- |Class HasFaces for operations using (a list of) TileFaces.
-- 
-- Used to define common functions on [TileFace], Tgraph, VPatch, BoundaryState, Forced, TrackedTgraph
class HasFaces a where
    -- |get the tileface list
    faces :: a -> [TileFace]
    -- |get the directed edges of the boundary
    -- (direction with a tileface on the left and exterior on right).
    boundary :: a -> [Dedge]
    -- |get the maximum vertex in all faces (0 if there are no faces)
    maxV :: a -> Int

-- |An ascending list of the vertices occuring in faces (without duplicates)
vertices :: HasFaces a => a -> [Vertex]
vertices = IntSet.elems . vertexSet

-- |List of boundary vertices
-- May have duplicates when applied to an arbitrary list of TileFace.
-- but no duplicates for Tgraph, VPatch, BoundaryState, Forced, TrackedTgraph. 
boundaryVs :: HasFaces a => a -> [Vertex]
boundaryVs = map fst . boundary

-- |get all the directed edges (directed clockwise round each face)
dedges :: HasFaces a => a -> [Dedge]
dedges = concatMap faceDedges . faces

-- |get the set of vertices in the faces
vertexSet :: HasFaces a => a -> VertexSet
vertexSet = mconcat . map faceVSet . faces

-- |A list of tilefaces is in class HasFaces
instance HasFaces [TileFace] where
    faces = id
    boundary = missingRevs . dedges
    maxV [] = 0
    maxV fcs = IntSet.findMax $ vertexSet fcs

-- |Tgraph is in class HasFaces
instance HasFaces Tgraph where
    faces  = getFaces
    boundary = boundary . faces
    maxV = maxV . faces

ldarts,rdarts,lkites,rkites, kites, darts :: HasFaces a => a -> [TileFace]
-- | selecting left darts from 
ldarts = filter isLD . faces
-- | selecting right darts from the faces
rdarts = filter isRD . faces
-- | selecting left kites from the faces
lkites = filter isLK . faces
-- | selecting right kites from the faces
rkites = filter isRK . faces
-- | selecting half kites from the faces
kites = filter isKite . faces
-- | selecting half darts from the faces
darts = filter isDart . faces

-- |selects faces from a Tgraph (removing any not in the list),
-- but checks resulting Tgraph for connectedness and no crossing boundaries.
selectFaces :: [TileFace] -> Tgraph -> Tgraph
selectFaces fcs g = runTry $ tryConnectedNoCross $ faces g `intersect` fcs

-- |removes faces from a Tgraph,
-- but checks resulting Tgraph for connectedness and no crossing boundaries.
removeFaces :: [TileFace] -> Tgraph -> Tgraph
removeFaces fcs g = runTry $ tryConnectedNoCross $ faces g \\ fcs

-- |removeVertices vs g - removes any vertex in the list vs from g
-- by removing all faces at those vertices. The resulting Tgraph is checked
-- for required properties  e.g. connectedness and no crossing boundaries
-- and will raise an error if these fail.
removeVertices :: [Vertex] -> Tgraph -> Tgraph
removeVertices vs g = removeFaces (filter (hasVIn vs) (faces g)) g

-- |selectVertices vs g - removes any face that does not have at least one vertex in the list vs from g.
-- Resulting Tgraph is checked
-- for required properties  e.g. connectedness and no crossing boundaries
-- and will raise an error if these fail.
selectVertices :: [Vertex] -> Tgraph -> Tgraph
selectVertices vs g = selectFaces (filter (hasVIn vs) (faces g)) g

-- |internal edges are shared by two faces. That is, all edges except those at the boundary.
-- Both directions of each internal directed edge will appear in the result.
internalEdges :: HasFaces a => a -> [Dedge]
internalEdges a =  des \\ map reverseD (missingRevs des) where
    des = dedges a

-- |phiEdges returns a list of the longer (phi-length) edges in the faces (including kite joins).
-- The result includes both directions of each edge.
phiEdges :: HasFaces a => a -> [Dedge]
phiEdges = bothDir . concatMap facePhiEdges . faces

-- |nonPhiEdges returns a list of the shorter edges in the faces (including dart joins).
-- The result includes both directions of each edge.
nonPhiEdges :: HasFaces a => a -> [Dedge]
nonPhiEdges = bothDir . concatMap faceNonPhiEdges . faces

-- |the default alignment of non-empty faces is (v1,v2) where v1 is the lowest numbered face origin,
-- and v2 is the lowest numbered opp vertex of faces with origin at v1. This is the lowest join edge.
-- An error will be raised if the Tgraph is empty.
defaultAlignment :: HasFaces a => a  -> (Vertex,Vertex)
defaultAlignment g | nullFaces g = error "defaultAlignment: applied to null list of faces\n"
                   | otherwise = lowestJoin $ faces g

makeRD,makeLD,makeRK,makeLK :: Vertex -> Vertex -> Vertex -> TileFace
-- |make an RD (strict in arguments)
makeRD !x !y !z = RD (x,y,z)
-- |make an LD (strict in arguments)
makeLD !x !y !z = LD (x,y,z)
-- |make an RK (strict in arguments)
makeRK !x !y !z = RK (x,y,z)
-- |make an LK (strict in arguments)
makeLK !x !y !z = LK (x,y,z)

-- |triple of face vertices in order clockwise starting with origin - tileRep specialised to TileFace
{-# Inline faceVs #-}
faceVs::TileFace -> (Vertex,Vertex,Vertex)
faceVs = tileRep
{- faceVs f = let tr = tileRep f
               (x,y,z) = tr
           in x `seq` y `seq` z `seq` tr
 -}
-- |list of (three) face vertices in order clockwise starting with origin
faceVList::TileFace -> [Vertex]
faceVList = (\(x,y,z) -> [x,y,z]) . faceVs

-- |the set of vertices of a face
faceVSet :: TileFace -> VertexSet
faceVSet = IntSet.fromList . faceVList

{- -- |find the maximum vertex for a list of faces (0 for an empty list).
facesMaxV :: [TileFace] -> Vertex
facesMaxV [] = 0
facesMaxV fcs = IntSet.findMax $ vertexSet fcs
 -}
-- Whilst first, second and third vertex of a face are obvious (clockwise), 
-- it is often more convenient to refer to the originV (=firstV),
-- oppV (the vertex at the other end of the join edge), and
-- wingV (the remaining vertex not on the join edge)

-- |firstV, secondV and thirdV vertices of a face are counted clockwise starting with the origin
firstV,secondV,thirdV:: TileFace -> Vertex
firstV  face = a where (a,_,_) = faceVs face
secondV face = b where (_,b,_) = faceVs face
thirdV  face = c where (_,_,c) = faceVs face

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
indexV v face = case elemIndex v (faceVList face) of
                  Just i -> i
                  _      -> error ("indexV: " ++ show v ++ " not found in " ++ show face)

-- |nextV returns the next vertex in a face going clockwise from v
-- where v must be a vertex of the face
nextV :: Vertex -> TileFace -> Vertex
nextV v face = case indexV v face of
                    0 -> secondV face
                    1 -> thirdV face
                    2 -> firstV face
                    _ -> error "nextV: index error"
-- |prevV returns the previous vertex in a face (i.e. next going anti-clockwise) from v
-- where v must be a vertex of the face
prevV :: Vertex -> TileFace -> Vertex
prevV v face = case indexV v face of
                    0 -> thirdV face
                    1 -> firstV face
                    2 -> secondV face
                    _ -> error "prevV: index error"

-- |isAtV v f asks if a face f has v as a vertex
isAtV:: Vertex -> TileFace -> Bool
isAtV v f = v==a || v==b || v==c
     where (a,b,c) = faceVs f
{-     
isAtV v (LD(a,b,c))  =  v==a || v==b || v==c
isAtV v (RD(a,b,c))  =  v==a || v==b || v==c
isAtV v (LK(a,b,c))  =  v==a || v==b || v==c
isAtV v (RK(a,b,c))  =  v==a || v==b || v==c
 -}
-- |hasVIn vs f - asks if face f has an element of vs as a vertex
hasVIn:: [Vertex] -> TileFace -> Bool
hasVIn vs face = not $ null $ faceVList face `intersect` vs


{- $Edges

Representing Edges:

For vertices a and b, (a,b) is regarded as a directed edge from a to b (a Dedge).

A list of such pairs will usually be regarded as a list of directed edges.
In the special case that the list is symmetrically closed [(b,a) is in the list whenever (a,b) is in the list]
we will refer to this as an edge list rather than a directed edge list.                  
-}


-- |directed edges (clockwise) round a face.
faceDedges::TileFace -> [Dedge]
faceDedges (LD(a,b,c)) = [(a,b),(b,c),(c,a)]
faceDedges (RD(a,b,c)) = [(a,b),(b,c),(c,a)]
faceDedges (LK(a,b,c)) = [(a,b),(b,c),(c,a)]
faceDedges (RK(a,b,c)) = [(a,b),(b,c),(c,a)]

{- -- |Returns the list of all directed edges (clockwise round each) of a list of tile faces.
facesDedges :: [TileFace] -> [Dedge]
facesDedges = concatMap faceDedges
 -}
-- |opposite directed edge.
reverseD:: Dedge -> Dedge
reverseD (a,b) = (b,a)

{-
-- |firstE, secondE and thirdE are the directed edges of a face counted clockwise from the origin, 
firstE,secondE,thirdE:: TileFace -> Dedge
firstE = head . faceDedges
secondE = head . tail . faceDedges
thirdE = head . tail . tail . faceDedges
-}

joinE, shortE, longE, joinOfTile:: TileFace -> Dedge
-- |the join directed edge of a face in the clockwise direction going round the face (see also joinOfTile).
joinE (LD(a,b,_)) = (a,b)
joinE (RD(a,_,c)) = (c,a)
joinE (LK(a,_,c)) = (c,a)
joinE (RK(a,b,_)) = (a,b)
-- |The short directed edge of a face in the clockwise direction going round the face.
-- This is the non-join short edge for darts.
shortE (LD(_,b,c)) = (b,c)
shortE (RD(_,b,c)) = (b,c)
shortE (LK(_,b,c)) = (b,c)
shortE (RK(_,b,c)) = (b,c)

-- |The long directed edge of a face in the clockwise direction going round the face.
-- This is the non-join long edge for kites.
longE (LD(a,_,c)) = (c,a)
longE (RD(a,b,_)) = (a,b)
longE (LK(a,b,_)) = (a,b)
longE (RK(a,_,c)) = (c,a)

-- |The join edge of a face directed from the origin (not clockwise for RD and LK)
joinOfTile face = (originV face, oppV face)

facePhiEdges, faceNonPhiEdges::  TileFace -> [Dedge]
-- |The phi edges of a face (both directions)
-- which is long edges for darts, and join and long edges for kites
facePhiEdges face@(RD _) = [e, reverseD e] where e = longE face
facePhiEdges face@(LD _) = [e, reverseD e] where e = longE face
facePhiEdges face        = [e, reverseD e, j, reverseD j]
                         where e = longE face
                               j = joinE face

-- |The non-phi edges of a face (both directions)
-- which is short edges for kites, and join and short edges for darts.
faceNonPhiEdges face = bothDirOneWay (faceDedges face) \\ facePhiEdges face

-- |matchingE eselect face is a predicate on tile faces 
-- where eselect selects a particular edge type of a face
-- (eselect could be joinE or longE or shortE for example).
-- This is True for face' if face' has an eselect edge matching the (reversed) eselect edge of face.
matchingE :: (TileFace -> Dedge) -> TileFace -> TileFace -> Bool
matchingE eselect face = (== reverseD (eselect face)) . eselect

matchingLongE,matchingShortE,matchingJoinE ::  TileFace -> TileFace -> Bool
      -- Used in Compose (getDartWingInfo and composedFaceGroups).
-- |check if two TileFaces have opposite directions for their long edge.
matchingLongE  = matchingE longE
-- |check if two TileFaces have opposite directions for their short edge.
matchingShortE = matchingE shortE
-- |check if two TileFaces have opposite directions for their join edge.
matchingJoinE  = matchingE joinE

-- |hasDedge f e returns True if directed edge e is one of the directed edges of face f
hasDedge :: TileFace -> Dedge -> Bool
hasDedge f e = e `elem` faceDedges f

-- |hasDedgeIn f es - is True if face f has a directed edge in the list of directed edges es.
hasDedgeIn :: TileFace -> [Dedge] -> Bool
hasDedgeIn face es = not $ null $ es `intersect` faceDedges face

-- |completeEdges returns a list of all the edges of the faces (both directions of each edge).
completeEdges :: HasFaces a => a -> [Dedge]
completeEdges = bothDir . dedges

-- |bothDir adds missing reverse directed edges to a list of directed edges
-- to complete edges (Result is a complete edge list)
-- It assumes no duplicates in argument.
bothDir:: [Dedge] -> [Dedge]
bothDir es = missingRevs es ++ es

-- |bothDirOneWay adds all the reverse directed edges to a list of directed edges
-- without checking for duplicates.
-- Should be used on lists with single directions only.
-- If the argument may contain reverse directions, use bothDir to avoid duplicates.
bothDirOneWay :: [Dedge] -> [Dedge]
bothDirOneWay des = revPlus des where
  revPlus ((a,b):es) = (b,a):revPlus es
  revPlus [] = des
{- 
bothDirOneWay [] = []
bothDirOneWay (e@(a,b):es)= e:(b,a):bothDirOneWay es
 -}

-- | efficiently finds missing reverse directions from a list of directed edges (using IntMap)
missingRevs:: [Dedge] -> [Dedge]
missingRevs es = revUnmatched es where
    vmap = VMap.fromListWith (++) $ map singleton es
    singleton (a,b) = (a,[b])
    seekR (a,b) = case VMap.lookup b vmap of
                   Nothing -> False
                   Just vs -> a `elem` vs

    revUnmatched [] = []
    revUnmatched (e@(a,b):more) | seekR e = revUnmatched more
                                | otherwise = (b,a):revUnmatched more


-- |two tile faces are edge neighbours
edgeNb::TileFace -> TileFace -> Bool
edgeNb face = any (`elem` edges) . faceDedges where
      edges = map reverseD (faceDedges face)



{-|vertexFacesMap vs a -
For list of vertices vs and faces from a,
create an IntMap from each vertex in vs to a list of those faces in a that are at that vertex.
-}
vertexFacesMap:: HasFaces a => [Vertex] -> a -> VertexMap [TileFace]
vertexFacesMap vs = foldl' insertf startVF . faces where
    startVF = VMap.fromList $ map (,[]) vs
    insertf vfmap f = foldl' (flip (VMap.alter addf)) vfmap (faceVList f)
                      where addf Nothing = Nothing
                            addf (Just fs) = Just (f:fs)

{-|dwFacesMap - a special case of vertexFacesMap applied to the dart wing vertices.
Create an IntMap from each dart wing vertex to a list of those faces that are at that vertex.
It makes use of the fact that only kite opps and kite origins and dart wings can
occur at a dart wing

dwFacesMap:: HasFaces a => a -> VertexMap [TileFace]
dwFacesMap g = foldl' insertf startVF (faces g) where
    vs = nub (wingV <$> darts g)
    startVF = VMap.fromList $ fmap (,[]) vs
    insertf vfmap f = foldl' (flip (VMap.alter addf)) vfmap (relevantVs f)
                      where addf Nothing = Nothing
                            addf (Just fs) = Just (f:fs)

    relevantVs  :: TileFace -> [Vertex]
    relevantVs (LK (a,_,c)) = [a,c]
    relevantVs (RK (a,b,_)) = [a,b]
    relevantVs (LD (_,_,c)) = [c]
    relevantVs (RD (_,b,_)) = [b]
-}


-- | dedgesFacesMap des a - Produces an edge-face map. Each directed edge in des is associated with
-- a unique face in a that has that directed edge (if there is one).
-- It will report an error if more than one face in a has the same directed edge in des. 
-- If the directed edges are all the ones in a, buildEFMap will be more efficient.
-- dedgesFacesMap is intended for a relatively small subset of directed edges in a Tgraph.
dedgesFacesMap:: HasFaces a => [Dedge] -> a -> Map.Map Dedge TileFace
dedgesFacesMap des fcs =  Map.fromList (assocFaces des) where
   vs = map fst des `union` map snd des
   vfMap = vertexFacesMap vs fcs
   assocFaces [] = []
   assocFaces (d@(a,b):more) =
       case filter (liftA2 (&&) (isAtV a) (`hasDedge` d)) (vfMap VMap.! b) of
           [face] -> (d,face):assocFaces more
           []   -> assocFaces more
           _   -> error $ "dedgesFacesMap: more than one Tileface has the same directed edge: "
                          ++ show d ++ "\n"
{- dedgesFacesMap des fcs =  Map.fromList (assocFaces des) where
   vs = fmap fst des `union` fmap snd des
   vfMap = vertexFacesMap vs fcs
   assocFaces [] = []
   assocFaces (d@(a,b):more) = case (VMap.lookup a vfMap, VMap.lookup b vfMap) of
      (Just fcs1, Just fcs2) -> case filter (`hasDedge` d) $ fcs1 `intersect` fcs2 of
                                   [face] -> (d,face):assocFaces more
                                   []   -> assocFaces more
                                   _   -> error $ "dedgesFacesMap: more than one Tileface has the same directed edge: "
                                                  ++ show d ++ "\n"
      _ -> assocFaces more
 -}

-- |Build a Map from all directed edges to faces (the unique face containing the directed edge)
buildEFMap:: HasFaces a  => a -> Map.Map Dedge TileFace
buildEFMap = Map.fromList . concatMap assignFace . faces where
  assignFace f = map (,f) (faceDedges f)

-- | look up a face for an edge in an edge-face map
faceForEdge :: Dedge -> Map.Map Dedge TileFace ->  Maybe TileFace
faceForEdge = Map.lookup

-- |Given a tileface (face) and a map from each directed edge to the tileface containing it (efMap)
-- return the list of edge neighbours of face.
edgeNbs:: TileFace -> Map.Map Dedge TileFace -> [TileFace]
edgeNbs face efMap = mapMaybe getNbr edges where
    getNbr e = Map.lookup e efMap
    edges = reverseD <$> faceDedges face

-- |For an argument with a non-empty list of faces,
-- find the face with lowest originV (and then lowest oppV).
-- Move this face to the front of the returned list of faces.
-- This will raise an error if there are no faces.
-- Used by locateVertices to determine the starting point for location calculation
extractLowestJoin:: HasFaces a => a -> (TileFace,[TileFace])
extractLowestJoin = getLJ . faces where
  getLJ fcs
    | null fcs  = error "extractLowestJoin: applied to empty list of faces"
    | otherwise = (face, fcs\\[face])
        where a = minimum (map originV fcs)
              aFaces = filter ((a==) . originV) fcs
              b = minimum (map oppV aFaces)
              face = case find (((a,b)==) . joinOfTile) aFaces of
                    Just f -> f
                    Nothing -> error $ "extractLowestJoin: no face found at "
                                        ++ show a ++ " with opp vertex at " ++ show b ++ "\n"


-- |Return the join edge with lowest origin vertex (and lowest oppV vertex if there is more than one).
-- The resulting edge is always directed from the origin to the opp vertex, i.e (orig,opp).
lowestJoin:: HasFaces a => a -> Dedge
lowestJoin = lowest . faces where
    lowest fcs | null fcs  = error "lowestJoin: applied to empty list of faces"
    lowest fcs = (a,b) where
        a = minimum (map originV fcs)
        aFaces = filter ((a==) . originV) fcs
        b = minimum (map oppV aFaces)

{---------------------
*********************
VPatch and Conversions
*********************
-----------------------}

-- |Abbreviation for finite mappings from Vertex to Location (i.e Point)
type VertexLocMap = VMap.IntMap (Point V2 Double)


-- |A VPatch has a map from vertices to points along with a list of tile faces.
-- It is an intermediate form between Tgraphs and Diagrams
data VPatch = VPatch {vLocs :: VertexLocMap,  vpFaces::[TileFace]} deriving Show

-- |needed for making VPatch transformable
type instance V VPatch = V2
-- |needed for making VPatch transformable
type instance N VPatch = Double


-- |Make VPatch Transformable.
instance Transformable VPatch where
    transform t vp = vp {vLocs = VMap.map (transform t) (vLocs vp)}

-- |VPatch is in class HasFace
instance HasFaces VPatch where
    faces = vpFaces
    boundary = boundary . faces
    maxV = maxV . faces


{-|Convert a Tgraph to a VPatch.
This uses locateVertices to form an intermediate VertexLocMap (mapping of vertices to positions).
This makes the join of the face with lowest origin and lowest oppV align on the positive x axis.
-}
makeVP::Tgraph -> VPatch
makeVP g = VPatch {vLocs = locateVertices fcs, vpFaces  = fcs} where fcs = faces g

-- |Creates a VPatch from a list of tile faces, using the vertex location map from the given VPatch.
-- The vertices in the tile faces should have locations assigned in the given VPatch vertex locations.
-- However THIS IS NOT CHECKED so missing locations for vertices will raise an error when drawing.
-- subVP vp fcs can be used for both subsets of tile faces of vp,
-- and also for larger scale faces which use the same vertex to point assignment (e.g in compositions).
-- The vertex location map is not changed (see also relevantVP and restrictVP).
subVP:: VPatch -> [TileFace] -> VPatch
subVP vp fcs = vp {vpFaces  = fcs}

-- | removes locations for vertices not used in the faces of a VPatch.
-- (Useful when restricting which labels get drawn).
-- relevantVP vp will raise an error if any vertex in the faces of vp is not a key in the location map of vp.
relevantVP :: VPatch -> VPatch
relevantVP vp
  | null diffList = vp{vLocs = locVs}
  | otherwise = error $ "relevantVP: missing locations for: " ++
                                    show diffList ++ "\n"
  where
     vs = vertexSet (faces vp)
     source = VMap.keysSet locVs
     diffList = IntSet.toList $ IntSet.difference vs source
     locVs = VMap.filterWithKey (\ v _ -> v `IntSet.member` vs) $ vLocs vp

-- | A combination of subVP and relevantVP. Restricts a vp to a list of faces, removing locations for vertices not in the faces.
-- (Useful when restricting which labels get drawn)
-- restrictVP vp fcs will raise an error if any vertex in fcs is not a key in the location map of vp.
restrictVP:: VPatch -> [TileFace] -> VPatch
restrictVP vp fcs = relevantVP (subVP vp fcs)

-- |Recover a Tgraph from a VPatch by dropping the vertex positions and checking Tgraph properties.
graphFromVP:: VPatch -> Tgraph
graphFromVP = checkedTgraph . faces

-- |remove a list of faces from a VPatch
removeFacesVP :: VPatch -> [TileFace] -> VPatch
removeFacesVP vp fcs = restrictVP vp (faces vp \\ fcs)

-- |make a new VPatch with a list of selected faces from a VPatch.
-- This will ignore any faces that are not in the given VPatch.
selectFacesVP:: VPatch -> [TileFace] -> VPatch
selectFacesVP vp fcs = restrictVP vp (fcs `intersect` faces vp)

-- |find the location of a single vertex in a VPatch
findLoc :: Vertex -> VPatch -> Maybe (Point V2 Double)
findLoc v = VMap.lookup v . vLocs




-- |VPatches are drawable
instance Drawable VPatch where
    drawWith pd vp = drawWith pd (dropLabels vp)

-- |converts a VPatch to a Patch, removing vertex information and converting faces to Located Pieces.
dropLabels :: VPatch -> Patch
dropLabels vp = map convert (faces vp) where
  locations = vLocs vp
  convert face = case (VMap.lookup (originV face) locations , VMap.lookup (oppV face) locations) of
    (Just p, Just p') -> fmap (const (p' .-. p)) face `at` p -- using HalfTile functor fmap
    _ -> error $ "dropLabels: Vertex location not found for some vertices:\n    "
                ++ show (faceVList face \\ VMap.keys locations)  ++ "\n"

-- |Tgraphs are Drawable
instance Drawable Tgraph where
    drawWith pd = drawWith pd . makeVP

-- | A class for things that can be drawn with labels when given a colour and a measure (size) for the label and a 
-- a draw function (for Patches).
-- So labelColourSize c m  modifies a Patch drawing function to add labels (of colour c and size measure m).
-- Measures are defined in Diagrams. In particular: tiny, verySmall, small, normal, large, veryLarge, huge.
class DrawableLabelled a where
-- labelColourSize :: DrawableLabelled a => Colour Double -> Measure Double -> (Patch -> Diagram B) -> a -> Diagram B
  labelColourSize :: OKBackend b =>
                     Colour Double -> Measure Double -> (Patch -> Diagram b) -> a -> Diagram b
-- The argument type of the draw function is Patch rather than VPatch, which prevents labelling twice.


-- | VPatches can be drawn with labels
instance DrawableLabelled VPatch where
  labelColourSize c m d vp = drawLabels <> d (dropLabels vp) where
     drawLabels = position $ drawlabel <$> VMap.toList (vLocs vp)
     drawlabel(v,p) = (p, baselineText (show v) # fontSize m # fc c)

-- | Tgraphs can be drawn with labels
instance DrawableLabelled Tgraph where
  labelColourSize c r d = labelColourSize c r d . makeVP

-- | Default Version of labelColourSize with colour red. Example usage: labelSize tiny draw a , labelSize normal drawj a
labelSize :: (OKBackend b, DrawableLabelled a) =>
             Measure Double -> (Patch -> Diagram b) -> a -> Diagram b
labelSize = labelColourSize red

-- | Default Version of labelColourSize using red and small (rather than normal label size). Example usage: labelled draw a , labelled drawj a
labelled :: (OKBackend b, DrawableLabelled a) =>
            (Patch -> Diagram b) -> a -> Diagram b
labelled = labelColourSize red small --(normalized 0.023)

-- |rotateBefore vfun a g - makes a VPatch from g then rotates by angle a before applying the VPatch function vfun.
-- Tgraphs need to be rotated after a VPatch is calculated but before any labelled drawing.
-- E.g. rotateBefore (labelled draw) angle graph.
rotateBefore :: (VPatch -> a) -> Angle Double -> Tgraph -> a
rotateBefore vfun angle = vfun . rotate angle . makeVP

-- |center a VPatch on a particular vertex. (Raises an error if the vertex is not in the VPatch vertices)
centerOn :: Vertex -> VPatch -> VPatch
centerOn a vp =
    case findLoc a vp of
        Just loca -> translate (origin .-. loca) vp
                     -- same as moveOriginTo loca vp
                     -- and as  moveOriginBy (loca .-. origin) vp
        _ -> error $ "centerOn: vertex not found (Vertex " ++ show a ++ ")\n"

-- |alignXaxis takes a vertex pair (a,b) and a VPatch vp
-- for centering vp on a and rotating the result so that b is on the positive X axis.
-- (Raises an error if either a or b are not in the VPatch vertices)
alignXaxis :: (Vertex, Vertex) -> VPatch -> VPatch
alignXaxis (a,b) vp =  rotate angle newvp
  where newvp = centerOn a vp
        angle = signedAngleBetweenDirs (direction unitX) (direction (locb .-. origin))
        locb = case findLoc b newvp of
                Just l -> l
                Nothing -> error $ "alignXaxis: second alignment vertex not found (Vertex " ++ show b ++ ")\n"

-- |alignments takes a list of vertex pairs for respective alignments of VPatches in the second list.
-- For a pair (a,b) the corresponding VPatch is centered on a then b is aligned along the positive x axis. 
-- The vertex pair list can be shorter than the list of VPatch - the remaining VPatch are left as they are.
-- (Raises an error if either vertex in a pair is not in the corresponding VPatch vertices)
alignments :: [(Vertex, Vertex)] -> [VPatch] -> [VPatch]
alignments [] vps = vps
alignments _  [] = error "alignments: Too many alignment pairs.\n"  -- non-null list of pairs
alignments ((a,b):more) (vp:vps) =  alignXaxis (a,b) vp : alignments more vps

-- |alignAll (a,b) vpList
-- provided both vertices a and b exist in each VPatch in vpList, the VPatch are all aligned
-- centred on a, with b on the positive x axis.
-- An error is raised if any VPatch does not contain both a and b vertices.
alignAll:: (Vertex, Vertex) -> [VPatch] -> [VPatch]
alignAll (a,b) = map (alignXaxis (a,b))

-- |alignBefore vfun (a,b) g - makes a VPatch from g oriented with centre on a and b aligned on the x-axis
-- before applying the VPatch function vfun
-- Will raise an error if either a or b is not a vertex in g.
-- Tgraphs need to be aligned after a VPatch is calculated but before any labelled drawing.
-- E.g. alignBefore (labelled draw) (a,b) g
alignBefore :: (VPatch -> a) -> (Vertex,Vertex) -> Tgraph -> a
alignBefore vfun vs = vfun . alignXaxis vs . makeVP

-- | makeAlignedVP (a,b) g - make a VPatch from g oriented with centre on a and b aligned on the x-axis.
-- Will raise an error if either a or b is not a vertex in g.
makeAlignedVP:: (Vertex,Vertex) ->  Tgraph -> VPatch
makeAlignedVP = alignBefore id


-- |produce a diagram of a list of edges (given a VPatch)
-- Will raise an error if any vertex of the edges is not a key in the vertex to location mapping of the VPatch.
drawEdgesVP :: OKBackend b =>
               VPatch -> [Dedge] -> Diagram b
drawEdgesVP = drawLocatedEdges . vLocs --foldMap (drawEdgeVP vp)

-- |produce a diagram of a single edge (given a VPatch)
-- Will raise an error if either vertex of the edge is not a key in the vertex to location mapping of the VPatch.
drawEdgeVP:: OKBackend b =>
             VPatch -> Dedge -> Diagram b
drawEdgeVP = drawLocatedEdge . vLocs

-- |produce a diagram of a list of edges (given a mapping of vertices to locations)
-- Will raise an error if any vertex of the edges is not a key in the mapping.
drawLocatedEdges :: OKBackend b =>
             VertexLocMap -> [Dedge] -> Diagram b
drawLocatedEdges = foldMap . drawLocatedEdge


-- |produce a diagram of a single edge (given a mapping of vertices to locations).
-- Will raise an error if either vertex of the edge is not a key in the mapping.
drawLocatedEdge :: OKBackend b =>
                   VertexLocMap -> Dedge -> Diagram b
drawLocatedEdge vpMap (a,b) = case (VMap.lookup a vpMap, VMap.lookup b vpMap) of
                         (Just pa, Just pb) -> pa ~~ pb
                         _ -> error $ "drawEdge: location not found for one or both vertices "++ show (a,b) ++ "\n"

{- {-# DEPRECATED drawEdge, drawEdges "Use drawLocatedEdge, drawLocatedEdges instead" #-}
-- |deprecated (use drawLocatedEdges)
drawEdges :: OKBackend b =>
             VertexLocMap -> [Dedge] -> Diagram b
drawEdges = drawLocatedEdges

-- |deprecated (use drawLocatedEdge)
drawEdge :: OKBackend b =>
            VertexLocMap -> Dedge -> Diagram b
drawEdge = drawLocatedEdge
 -}

{-| locateVertices: processes a list of faces to associate points for each vertex using a default scale and orientation.
The default scale is 1 unit for short edges (phi units for long edges).
It aligns the lowest numbered join of the faces on the x-axis, and returns a vertex-to-point Map.
It will raise an error if faces are not connected.
If faces have crossing boundaries (i.e not locally tile-connected), this could raise an error
or a result with touching vertices (i.e. more than one vertex label with the same location).
-}
locateVertices:: HasFaces a => a -> VertexLocMap
--  This version is made more efficient by calculating an edge to face map
--  and also using Sets for 2nd arg of fastAddVPoints.
locateVertices = locVs . faces where
  locVs [] = VMap.empty
  locVs fcs = fastAddVPoints [joinFace] (Set.fromList more) (axisJoin joinFace) where
    (joinFace,more) = extractLowestJoin fcs
    efMap = buildEFMap fcs  -- map from Dedge to TileFace
{- fastAddVPoints readyfaces fcOther vpMap.
The first argument list of faces (readyfaces) contains the ones being processed next in order where
each will have at least two known vertex locations in vpMap.
The second argument Set of faces (fcOther) are faces that have not yet been added
and may not yet have known vertex locations.
The third argument is the mapping of vertices to points.
-}
    fastAddVPoints [] fcOther vpMap | Set.null fcOther = vpMap
    fastAddVPoints [] fcOther _ = error $ "locateVertices (fastAddVPoints): Faces not tile-connected: "
                                          ++ show fcOther ++ "\n"
    fastAddVPoints (face:fs) fcOther vpMap = fastAddVPoints (fs++nbs) fcOther' vpMap' where
        nbs = filter (`Set.member` fcOther) (edgeNbs face efMap)
        fcOther' = foldl' (flip Set.delete) fcOther nbs
--        fcOther' = foldr Set.delete fcOther nbs
        vpMap' = addVPoint face vpMap

-- |Given a tileface and a vertex to location map which gives locations for at least 2 of the tileface vertices
-- this returns a new map by adding a location for the third vertex (when missing) or the same map when not missing.
-- It will raise an error if there are fewer than 2 tileface vertices with a location in the map
-- (indicating a non tile-connected face).
-- It is possible that a newly added location is already in the range of the map (creating a touching vertices),
-- so this needs to be checked for.
addVPoint:: TileFace -> VertexLocMap -> VertexLocMap
addVPoint face vpMap =
  case thirdVertexLoc face vpMap of
    Just (v,p) -> VMap.insert v p vpMap
    Nothing -> vpMap

-- |axisJoin face - 
-- initialises a vertex to point mapping with locations for the join edge vertices of face
-- with originV face at the origin and aligned along the x axis with unit length for a half dart
-- and length phi for a half kite. (Used to initialise locateVertices)
axisJoin::TileFace -> VertexLocMap
axisJoin face =
  VMap.insert (originV face) origin $ VMap.insert (oppV face) (p2 (x,0)) VMap.empty where
    x = if isDart face then 1 else phi

-- |lookup 3 vertex locations in a vertex to point map.
find3Locs::(Vertex,Vertex,Vertex) -> VertexLocMap
             -> (Maybe (Point V2 Double),Maybe (Point V2 Double),Maybe (Point V2 Double))
find3Locs (v1,v2,v3) vpMap = (VMap.lookup v1 vpMap, VMap.lookup v2 vpMap, VMap.lookup v3 vpMap)

{-| thirdVertexLoc face vpMap,  where face is a tileface and vpMap associates points with vertices (positions).
It looks up all 3 vertices of face in vpMap hoping to find at least 2 of them, it then returns Just pr
where pr associates a new location with the third vertex.
If all 3 are found, returns Nothing.
If none or one found this is an error (a non tile-connected face).

New Version: This assumes all edge lengths are 1 or phi.
It now uses signorm to produce vectors of length 1 rather than rely on relative lengths.
(Requires ttangle and phi from TileLib).
-}
thirdVertexLoc:: TileFace -> VertexLocMap -> Maybe (Vertex, Point V2 Double)
thirdVertexLoc face@(LD _) vpMap = case find3Locs (faceVs face) vpMap of
  (Just loc1, Just loc2, Nothing) -> Just (wingV face, loc1 .+^ v)   where v = phi*^signorm (rotate (ttangle 9) (loc2 .-. loc1))
  (Nothing, Just loc2, Just loc3) -> Just (originV face, loc2 .+^ v) where v = signorm (rotate (ttangle 7) (loc3 .-. loc2))
  (Just loc1, Nothing, Just loc3) -> Just (oppV face, loc1 .+^ v)    where v = signorm (rotate (ttangle 1) (loc3 .-. loc1))
  (Just _ , Just _ , Just _)      -> Nothing
  _ -> error $ "thirdVertexLoc: face not tile-connected?: " ++ show face ++ "\n"

thirdVertexLoc face@(RD _) vpMap = case find3Locs (faceVs face) vpMap of
  (Just loc1, Just loc2, Nothing) -> Just (oppV face, loc1 .+^ v)    where v = signorm (rotate (ttangle 9) (loc2 .-. loc1))
  (Nothing, Just loc2, Just loc3) -> Just (originV face, loc3 .+^ v) where v = signorm (rotate (ttangle 3) (loc2 .-. loc3))
  (Just loc1, Nothing, Just loc3) -> Just (wingV face, loc1 .+^ v)   where v = phi*^signorm (rotate (ttangle 1) (loc3 .-. loc1))
  (Just _ , Just _ , Just _)      -> Nothing
  _ -> error $ "thirdVertexLoc: face not tile-connected?: " ++ show face ++ "\n"

thirdVertexLoc face@(LK _) vpMap = case find3Locs (faceVs face) vpMap of
  (Just loc1, Just loc2, Nothing) -> Just (oppV face, loc1 .+^ v)    where v = phi*^signorm (rotate (ttangle 9) (loc2 .-. loc1))
  (Nothing, Just loc2, Just loc3) -> Just (originV face, loc2 .+^ v) where v = phi*^signorm (rotate (ttangle 8) (loc3 .-. loc2))
  (Just loc1, Nothing, Just loc3) -> Just (wingV face, loc1 .+^ v)   where v = phi*^signorm (rotate (ttangle 1) (loc3 .-. loc1))
  (Just _ , Just _ , Just _)      -> Nothing
  _ -> error $ "thirdVertexLoc: face not tile-connected?: " ++ show face ++ "\n"

thirdVertexLoc face@(RK _) vpMap = case find3Locs (faceVs face) vpMap of
  (Just loc1, Just loc2, Nothing) -> Just (wingV face, loc1 .+^ v)   where v = phi*^signorm (rotate (ttangle 9) (loc2 .-. loc1))
  (Nothing, Just loc2, Just loc3) -> Just (originV face, loc2 .+^ v) where v = phi*^signorm (rotate (ttangle 8) (loc3 .-. loc2))
  (Just loc1, Nothing, Just loc3) -> Just (oppV face, loc1 .+^ v)    where v = phi*^signorm (rotate (ttangle 1) (loc3 .-. loc1))
  (Just _ , Just _ , Just _)      -> Nothing
  _ -> error $ "thirdVertexLoc: face not tile-connected?: " ++ show face ++ "\n"



-- *  Touching Vertices


{-| 
touchingVertices finds if any vertices are too close to each other using locateVertices.
If vertices are too close that indicates we may have different vertex labels at the same location
(the touching vertex problem). 
It returns pairs of vertices that are too close with higher number first in each pair, and no repeated first numbers.
An empty list is returned if there are no touching vertices.
Complexity has order of the square of the number of vertices.
                           
This is used in makeTgraph and fullUnion (via correctTouchingVertices).
-}
touchingVertices:: HasFaces a => a -> [(Vertex,Vertex)]
touchingVertices fcs = check vpAssoc where
  vpAssoc = VMap.assocs $ locateVertices fcs  -- assocs puts in increasing key order so that check returns (higher,lower) pairs
  check [] = []
  check ((v,p):more) = [(v1,v) | v1 <- nearv ] ++ check (filter ((`notElem` nearv).fst) more)
                        where nearv = [v1 | (v1,p1) <- more, touching p p1 ]

{-|touching checks if two points are considered close.
Close means the square of the distance between them is less than a certain number (currently 0.1) so they cannot be
vertex locations for 2 different vertices in a VPatch using unit scale for short edges.
It is used in touchingVertices and touchingVerticesGen and Force.touchCheck).
-}
touching :: Point V2 Double -> Point V2 Double -> Bool
touching p p1 = quadrance (p .-. p1) < 0.1 -- quadrance is square of length of a vector
--  0.1 represents a distance of about 0.316 units (= sqrt 0.1)


{-*  Generalised Touching Vertices
-}

{-| 
touchingVerticesGen  generalises touchingVertices to allow for multiple faces sharing a directed edge.
This can arise when applied to the union of faces from 2 Tgraphs which might clash in places.
It is used in the calculation of commonFaces.  
-}
touchingVerticesGen:: [TileFace] -> [(Vertex,Vertex)]
touchingVerticesGen fcs = check vpAssoc where
  vpAssoc = VMap.assocs $ locateVerticesGen fcs  -- assocs puts in key order so that check returns (higher,lower) pairs  
  check [] = []
  check ((v,p):more) = [(v1,v) | v1 <- nearv ] ++ check (filter ((`notElem` nearv).fst) more)
                        where nearv = [v1 | (v1,p1) <- more, touching p p1 ]

{-| locateVerticesGen generalises locateVertices to allow for multiple faces sharing an edge.
This can arise when applied to the union of faces from 2 Tgraphs (e.g. in commonFaces)    
-}
locateVerticesGen:: HasFaces a => a -> VertexLocMap
locateVerticesGen = locVs . faces where
  locVs [] = VMap.empty
  locVs fcs = fastAddVPointsGen [face] (Set.fromList more) (axisJoin face) where
    (face,more) = extractLowestJoin fcs
    efMapGen = buildEFMapGen fcs  -- map from Dedge to [TileFace]
{- fastAddVPointsGen readyfaces fcOther vpMap.
The first argument list of faces (readyfaces) contains the ones being processed next in order where
each will have at least two known vertex locations in vpMap.
The second argument Set of faces (fcOther) are faces that have not yet been added
and may not yet have known vertex locations.
The third argument is the mapping of vertices to points.
-}
    fastAddVPointsGen [] fcOther vpMap | Set.null fcOther = vpMap
    fastAddVPointsGen [] fcOther _ = error $ "fastAddVPointsGen: Faces not tile-connected " ++ show fcOther ++ "\n"
    fastAddVPointsGen (f:fs) fcOther vpMap = fastAddVPointsGen (fs++nbs) fcOther' vpMap' where
        nbs = filter (`Set.member` fcOther) (edgeNbsGen f)
--        nbs = filter (`Set.member` fcOther) (edgeNbsGen efMapGen fc)
        fcOther' = foldl' (flip Set.delete) fcOther nbs
        vpMap' = addVPoint f vpMap
-- Generalises buildEFMap by allowing for multiple faces on a directed edge.
-- buildEFMapGen:: [TileFace] -> Map.Map Dedge [TileFace]
    buildEFMapGen = Map.fromListWith (++) . concatMap processFace
    processFace f = (,[f]) <$> faceDedges f

-- Generalised edgeNbs allowing for multiple faces on a directed edge.
-- edgeNbsGen:: Map.Map Dedge [TileFace] -> TileFace -> [TileFace]
    edgeNbsGen f = concat $ mapMaybe getNbrs edges where
      getNbrs e = Map.lookup e efMapGen
      edges = map reverseD (faceDedges f)
{-
    edgeNbsGen efMapGen f = concat $ mapMaybe getNbrs edges where
      getNbrs e = Map.lookup e efMapGen
      edges = fmap reverseD (faceDedges f) 
-}





