{-|
Module      : Tgraph.Convert
Description : Conversion of Tgraphs to Patches (and VPinned) for drawing Tgraphs
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

Conversion operations from Tgraphs to Patches and Diagrams as well as the intermediate type VPinned
(Vertex pinned) used when drawing vertex labels.
The module also includes functions to calculate (relative) locations of vertices (locateVertices, addVPoint) and
touching vertex checks (touchingVertices, touchingVerticesGen).
-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Tgraph.Convert where

import Data.List ((\\), find, partition, nub, intersect)
import qualified Data.IntMap.Strict as VMap (IntMap, map, filterWithKey, lookup, insert, empty, toList, assocs, fromList, keys)
import qualified Data.Map.Strict as Map (Map, lookup, fromList, fromListWith) -- used for locateVertices
import qualified Data.Set as Set  (fromList,member,null,delete)-- used for locateVertices
import Data.Maybe (mapMaybe, catMaybes)

import Diagrams.Prelude
import TileLib
import Tgraph.Prelude
import ChosenBackend (B)

-- |Abbreviation for mappings from Vertex to Location (i.e Point)
type VertexLocMap = VMap.IntMap (Point V2 Double)


{-* VPinned
-}

-- |A VPinned has a map from vertices to points along with a list of tile faces.
-- It is an intermediate form between Tgraphs and Patches
data VPinned = VPinned {vLocs :: VertexLocMap,  vpFaces::[TileFace]} deriving Show

-- |needed for making VPinned transformable
type instance V VPinned = V2
-- |needed for making VPinned transformable
type instance N VPinned = Double

-- |Make VPinned Transformable.
instance Transformable VPinned where 
    transform t vp = vp {vLocs = VMap.map (transform t) (vLocs vp)}

{-|Convert a Tgraph to a VPinned.
This uses locateVertices to form an intermediate VertexLocMap (mapping of vertices to positions).
This makes the join of the face with lowest origin and lowest oppV align on the positive x axis.
-}
makeVPinned::Tgraph -> VPinned
makeVPinned g = VPinned {vLocs = locateVertices fcs, vpFaces  = fcs} where fcs = faces g

-- |Creates a VPinned from a list of tile faces, using the vertex locations from the given VPinned.
-- The vertices in the tile faces must have points assigned in the given VPinned.
-- (This is not checked for, but missing locations for vertices will raise an error when drawing.)
-- subVPinned fcs vp can be used for both subsets of tile faces of vp,
-- but also for larger scale faces which use the same vertex to point assignment (e.g in compositions).
subVPinned:: [TileFace] -> VPinned -> VPinned
subVPinned fcs vp = vp {vpFaces  = fcs} 
--subVPinned fcs vp = VPinned {vLocs = vLocs vp, vpFaces  = fcs} 

{-|
makePatch uses makeVPinned first then uses dropLabels to convert faces to located Pieces.
(So dropping the Vertices information.)
-}
makePatch:: Tgraph -> Patch
makePatch = dropLabels . makeVPinned
 
-- |converts a VPinned to a Patch, removing vertex information and converting faces to Located Pieces
dropLabels :: VPinned -> Patch
dropLabels vp = fmap convert (vpFaces vp) where
  locations = vLocs vp
  convert fc = case (VMap.lookup (originV fc) locations , VMap.lookup (oppV fc) locations) of
                (Just p, Just p') -> fmap (\_ -> (p' .-. p)) fc `at` p -- using HalfTile functor fmap
                _ -> error ("dropLabels: Vertex location not found for some vertices:\n" 
                             ++ show (faceVList fc \\ VMap.keys locations))

{-|Auxiliary function For converting a list of TileFaces to a Patch when given a VPinned with a suitable VertexLocMap.
The VertexLocMap in the VPinned must contain locations for all the TileFace vertices.
The alignment is dictated by the VertexLocMap.
This function is intended to save recreating a VertexLocMap for several Patches
with different subsets of the vertices.
(E.g. in displaying subsets of faces of a graph in drawPCompose and in drawForce)
-}
subPatch :: [TileFace] -> VPinned -> Patch
subPatch fcs = dropLabels . subVPinned fcs

-- |Recover a Tgraph from a VPinned by dropping the vertex positions and checking Tgraph properties.
graphFromVP:: VPinned -> Tgraph
graphFromVP = checkedTgraph . vpFaces

-- |remove a list of faces from a VPinned
removeFacesVP :: [TileFace] -> VPinned -> VPinned
removeFacesVP fcs vp = vp {vpFaces = filter (not . (`elem` fcs)) $ vpFaces vp}

-- |make a new VPinned with a list of selected faces from a VPinned.
-- This will ignore any faces that are not in the given VPinned.
selectFacesVP:: [TileFace] -> VPinned -> VPinned
selectFacesVP fcs vp = vp {vpFaces = filter (`elem` fcs) $ vpFaces vp}


-- |selectFacesGtoVP fcs g -  only selected faces (fcs) are kept after converting g to a VPinned
selectFacesGtoVP :: [TileFace] -> Tgraph -> VPinned
selectFacesGtoVP fcs g = selectFacesVP fcs (makeVPinned g)

-- |removeFacesGtoVP fcs g - remove faces (fcs) after converting g to a VPinned
removeFacesGtoVP :: [TileFace] -> Tgraph -> VPinned
removeFacesGtoVP fcs g = removeFacesVP fcs (makeVPinned g)

-- |find the location of a single vertex in a VPinned
findLoc :: Vertex -> VPinned -> Maybe (Point V2 Double)
findLoc v = VMap.lookup v . vLocs



{-* Drawing VPinned and Graphs
-}

-- |simplest drawing without vertex labels
drawGraph:: Tgraph -> Diagram B
drawGraph = drawPatch . makePatch

-- |simplest drawing with dashed join edges but without vertex labels
dashJGraph:: Tgraph -> Diagram B
dashJGraph = dashJPatch . makePatch

-- |simplest drawing including vertex labels
drawVGraph:: Tgraph -> Diagram B
drawVGraph = drawVPinned . makeVPinned

-- |Convert a VPinned to a diagram showing vertex labels
drawVPinned:: VPinned -> Diagram B
drawVPinned = drawVPinnedWith drawPiece

-- |simplest drawing with dashed join edges and vertex labels
dashJVGraph:: Tgraph -> Diagram B
dashJVGraph = dashJVPinned . makeVPinned

-- |convert a VPinned to a diagram with vertex labels and dashed joins
dashJVPinned:: VPinned -> Diagram B
dashJVPinned = drawVPinnedWith dashJPiece

-- |drawVPinnedWith pd vp - converts vp to a diagram with vertex labels using pd to draw pieces
drawVPinnedWith :: (Piece -> Diagram B) -> VPinned -> Diagram B
drawVPinnedWith pd vp = drawVlabels (vLocs vp) <> drawPatchWith pd (dropLabels vp)

-- |draws vertex labels at assigned points.
drawVlabels :: VertexLocMap -> Diagram B
drawVlabels vpMap = position $ fmap (\(v,p) -> (p, label v)) $ VMap.toList vpMap
    where label v = baselineText (show v) # fontSize (global 0.3) # fc red

-- |relevantVPinnedWith pd vp - converts vp to a diagram with vertex labels using pd to draw pieces
-- BUT drops drawing of vertices that are not mentioned in the faces.
relevantVPinnedWith :: (Piece -> Diagram B) -> VPinned -> Diagram B
relevantVPinnedWith pd vp = drawVlabels locVs <> drawPatchWith pd (dropLabels vp) where
     vs = nub $ concatMap faceVList (vpFaces vp)
     locVs = VMap.filterWithKey (\v -> \_ -> (v `elem` vs)) $ vLocs vp

-- | makeAlignedPatch (a,b) g - make a Patch from g oriented with centre on a and b aligned on the x-axis.
-- Will raise an error if either a or b is not a vertex in g.
makeAlignedPatch:: (Vertex,Vertex) ->  Tgraph -> Patch        
makeAlignedPatch vs g = dropLabels $ alignXaxis vs $ makeVPinned g



{-* VPinned Alignment with Vertices
-}

-- |center a VPinned on a particular vertex. (Raises an error if the vertex is not in the VPinned vertices)
centerOn :: Vertex -> VPinned -> VPinned
centerOn a vp = 
    case findLoc a vp of
        Just loca -> translate (origin .-. loca) vp
        _ -> error ("centerOn: vertex not found: "++ show a)

-- |alignXaxis takes a vertex pair (a,b) and a VPinned vp
-- for centering vp on a and rotating the result so that b is on the positive X axis.
-- (Raises an error if either a or b are not in the VPinned vertices)
alignXaxis :: (Vertex, Vertex) -> VPinned -> VPinned    
alignXaxis (a,b) vp =  rotate angle newvp
  where newvp = centerOn a vp
        angle = signedAngleBetweenDirs (direction unitX) (direction (locb .-. origin)) 
        locb = case findLoc b newvp of
                Just l -> l
                Nothing -> error ("alignXaxis: second alignment vertex not found (Vertex " ++ show b ++ ")")

-- |alignments takes a list of vertex pairs for respective rotations of VPinned in the second list.
-- For a pair (a,b) the corresponding VPinned is centered on a then b is aligned along the positive x axis. 
-- The vertex pair list can be shorter than the list of VPinned - the remaining VPinned are left as they are.
alignments :: [(Vertex, Vertex)] -> [VPinned] -> [VPinned]     
alignments [] vps = vps
alignments prs [] = error "alignments: Too many alignment pairs"  -- prs non-null
alignments ((a,b):more) (vp:vps) =  alignXaxis (a,b) vp : alignments more vps

-- |alignAll (a,b) vpList
-- provided both vertices a and b exist in each VPinned in vpList, the VPinned are all aligned
-- centred on a, with b on the positive x axis.
-- An error is raised if any VPinned does not contain both a and b vertices.
alignAll:: (Vertex, Vertex) -> [VPinned] -> [VPinned]     
alignAll (a,b) = fmap (alignXaxis (a,b))


{-* Vertex Location Calculation -}


{-| locateVertices: processes a list of faces to associate points for each vertex.
     Faces must be tile-connected. It aligns the lowest numbered join of the faces on the x-axis.
      Returns a vertex-to-point Map.
  This version is made more efficient by calculating an edge to face map
  and also using Sets for 2nd arg of fastAddVPoints.
-}
locateVertices:: [TileFace] -> VertexLocMap
locateVertices [] = VMap.empty
locateVertices faces = fastAddVPoints [face] (Set.fromList more) (axisJoin face) where
    (face:more) = lowestJoinFirst faces
    efMap = buildEFMap faces  -- map from Dedge to TileFace
{- fastAddVPoints readyfaces fcOther vpMap.
The first argument list of faces (readyfaces) contains the ones being processed next in order where
each will have at least two known vertex locations in vpMap.
The second argument Set of faces (fcOther) are faces that have not yet been added
and may not yet have known vertex locations.
The third argument is the mapping of vertices to points.
-}
    fastAddVPoints [] fcOther vpMap | Set.null fcOther = vpMap 
    fastAddVPoints [] fcOther vpMap | otherwise = error ("fastAddVPoints: Faces not tile-connected " ++ show fcOther)
    fastAddVPoints (fc:fcs) fcOther vpMap = fastAddVPoints (fcs++nbs) fcOther' vpMap' where
        nbs = filter (\f -> Set.member f fcOther) (edgeNbs efMap fc)
        fcOther' = foldr Set.delete fcOther nbs
        vpMap' = addVPoint fc vpMap

-- |For a non-empty list of tile faces
-- find the face with lowest originV (and then lowest oppV).
-- Move this face to the front of the returned list of faces.
-- Used by locateVertices to determine the starting point for location calculation
lowestJoinFirst:: [TileFace] -> [TileFace]
lowestJoinFirst fcs | null fcs  = error "lowestJoinFirst: applied to empty list of faces"
                    | otherwise = face:(fcs\\[face]) where
    a = minimum (fmap originV fcs)
    aFs = filter ((a==) . originV) fcs
    b = minimum (fmap oppV aFs)
    (face: _) = filter (((a,b)==) . joinOfTile) aFs


-- |Return the join edge with lowest origin vertex (and lowest oppV vertex if there is more than one).
lowestJoin:: [TileFace] -> Dedge
lowestJoin fcs | null fcs  = error "lowestJoin: applied to empty list of faces"
lowestJoin fcs | otherwise = (a,b) where
    a = minimum (fmap originV fcs)
    aFs = filter ((a==) . originV) fcs
    b = minimum (fmap oppV aFs)

-- |Given a tileface and a vertex to location map which gives locations for at least 2 of the tileface vertices
-- this returns a new map by adding a location for the third vertex (when missing) or the same map when not missing.
-- It will raise an error if there are fewer than 2 tileface vertices with a location in the map.
addVPoint:: TileFace -> VertexLocMap -> VertexLocMap
addVPoint fc vpMap = 
  case thirdVertexLoc fc vpMap of
    Just (v,p) -> VMap.insert v p vpMap
    Nothing -> vpMap

-- |Build a Map from directed edges to faces (the unique face containing the directed edge)
buildEFMap:: [TileFace] -> Map.Map Dedge TileFace
buildEFMap = mconcat . fmap processFace where
  processFace fc = Map.fromList $ fmap (\e -> (e,fc)) $ faceDedges fc
 
-- |Given a map from each directed edge to the tileface containing it (efMap), a tileface (fc)
-- return the list of edge neighbours of fc.
edgeNbs:: Map.Map Dedge TileFace -> TileFace -> [TileFace]
edgeNbs efMap fc = catMaybes $ fmap getNbr edges where
    getNbr e = Map.lookup e efMap
    edges = fmap reverseD (faceDedges fc) 

-- |axisJoin fc 
-- initialises a vertex to point mapping with locations for the join edge vertices of fc
-- with originV fc at the origin and aligned along the x axis with unit length for a half dart
-- and length phi for a half kite. (Used to initialise locateVertices)
axisJoin::TileFace -> VertexLocMap                
axisJoin fc = 
  VMap.insert (originV fc) origin $ VMap.insert (oppV fc) (p2(x,0)) VMap.empty where
    x = if isDart fc then 1 else phi
{-
axisJoin (LD(a,b,_)) = VMap.insert a origin $ VMap.insert b (p2(1,0)) VMap.empty -- [(a,origin), (b, p2(1,0))]
axisJoin (RD(a,_,c)) = VMap.insert a origin $ VMap.insert c (p2(1,0)) VMap.empty --[(a,origin), (c, p2(1,0))]
axisJoin (LK(a,_,c)) = VMap.insert a origin $ VMap.insert c (p2(phi,0)) VMap.empty --[(a,origin), (c, p2(phi,0))]
axisJoin (RK(a,b,_)) = VMap.insert a origin $ VMap.insert b (p2(phi,0)) VMap.empty -- [(a,origin), (b, p2(phi,0))]
-}

-- |lookup 3 vertex locations in a vertex to point map.
find3Locs::(Vertex,Vertex,Vertex) -> VertexLocMap
             -> (Maybe (Point V2 Double),Maybe (Point V2 Double),Maybe (Point V2 Double))              
find3Locs (v1,v2,v3) vpMap = (VMap.lookup v1 vpMap, VMap.lookup v2 vpMap, VMap.lookup v3 vpMap)

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
thirdVertexLoc:: TileFace -> VertexLocMap -> Maybe (Vertex, Point V2 Double)        
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

{-*  Drawing (located) Edges
-}

-- |produce a diagram of a list of edges (given a mapping of vertices to locations)
drawEdges :: VertexLocMap -> [Dedge] -> Diagram B
drawEdges vpMap = foldMap (drawEdge vpMap)

-- |produce a diagram of a single edge (given a mapping of vertices to locations)
drawEdge :: VertexLocMap -> Dedge -> Diagram B
drawEdge vpMap (a,b) = case (VMap.lookup a vpMap, VMap.lookup b vpMap) of
                         (Just pa, Just pb) -> pa ~~ pb
                         _ -> error ("drawEdge: location not found for one or both vertices "++ show(a,b))
 

{-*  Touching Vertices
-}

{-| 
touchingVertices checks that no vertices are too close to each other using locateVertices.
If vertices are too close that indicates we may have the same point with two different vertex numbers
arising from the touching vertex problem. 
It returns pairs of vertices that are too close (higher number first in each pair)
(i.e less than 0.5 where 1.0 would be the length of short edges)
An empty list is returned if there are no touching vertices.
Complexity has order of the square of the number of vertices.
                           
This is used in makeTgraph and fullUnion (via correctTouchingVertices), but can also be used as a reptrospective check if the touching vertex check 
is switched off in forcing.                          
-}
touchingVertices:: [TileFace] -> [(Vertex,Vertex)]
touchingVertices fcs = check vpAssoc where
  vpAssoc = VMap.assocs $ locateVertices fcs  -- assocs puts in key order so that check returns (higher,lower) pairs
  check [] = []
  check ((v,p):more) = [(v1,v) | v1 <- nearv ] ++ (check $ filter ((`notElem` nearv).fst) more)
                        where nearv = [v1 | (v1,p1) <- more, touching p p1 ]
--  check ((v,p):more) = [(v1,v) | (v1,p1) <- more, touching p p1 ] ++ check more
-- does not correctly deal with 3 or more vertices touching at the same point

{-|touching checks if two points are considered close.
Close means the square of the distance between them is less than 0.25 so they cannot be
vertex locations for 2 different vertices in a VPatch using unit scale for short edges.
It is used in touchingVertices and touchingVerticesGen)
-}
touching :: Point V2 Double -> Point V2 Double -> Bool
touching p p1 = quadrance (p .-. p1) < 0.24--0.0625 -- quadrance is square of length of a vector

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
  check ((v,p):more) = [(v1,v) | v1 <- nearv ] ++ (check $ filter ((`notElem` nearv).fst) more)
                        where nearv = [v1 | (v1,p1) <- more, touching p p1 ]

{-| locateVerticesGen generalises locateVertices to allow for multiple faces sharing an edge.
This can arise when applied to the union of faces from 2 Tgraphs (e.g. in commonFaces)    
-}
locateVerticesGen:: [TileFace] -> VertexLocMap
locateVerticesGen [] = VMap.empty
locateVerticesGen faces = fastAddVPointsGen [face] (Set.fromList more) (axisJoin face) where
    (face:more) = lowestJoinFirst faces
    efMapGen = buildEFMapGen faces  -- map from Dedge to [TileFace]
{- fastAddVPointsGen readyfaces fcOther vpMap.
The first argument list of faces (readyfaces) contains the ones being processed next in order where
each will have at least two known vertex locations in vpMap.
The second argument Set of faces (fcOther) are faces that have not yet been added
and may not yet have known vertex locations.
The third argument is the mapping of vertices to points.
-}
    fastAddVPointsGen [] fcOther vpMap | Set.null fcOther = vpMap 
    fastAddVPointsGen [] fcOther vpMap | otherwise = error ("fastAddVPointsGen: Faces not tile-connected " ++ show fcOther)
    fastAddVPointsGen (fc:fcs) fcOther vpMap = fastAddVPointsGen (fcs++nbs) fcOther' vpMap' where
        nbs = filter (\f -> Set.member f fcOther) (edgeNbsGen efMapGen fc)
        fcOther' = foldr Set.delete fcOther nbs
        vpMap' = addVPoint fc vpMap
-- Generalises buildEFMap by allowing for multiple faces on a directed edge.
-- buildEFMapGen:: [TileFace] -> Map.Map Dedge [TileFace]
    buildEFMapGen = Map.fromListWith (++) . concatMap processFace where
    processFace fc = fmap (\e -> (e,[fc])) $ faceDedges fc

-- Generalised edgeNbs allowing for multiple faces on a directed edge.
-- edgeNbsGen:: Map.Map Dedge [TileFace] -> TileFace -> [TileFace]
    edgeNbsGen efMapGen fc = concat $ catMaybes $ fmap getNbrs edges where
      getNbrs e = Map.lookup e efMapGen
      edges = fmap reverseD (faceDedges fc) 


 

