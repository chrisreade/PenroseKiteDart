{-|
Module      : Tgraph.Convert
Description : Conversion of a Tgraph to a VPatch and drawing operations.
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

Introducing type VPatch (Vertex Patch) as intermediary between Tgraph and Diagram.
Conversion operation from a Tgraph to a VPatch (makeVP) and drawing operations to produce Diagrams.
The module also includes functions to calculate (relative) locations of vertices (locateVertices, addVPoint),
touching vertex checks (touchingVertices, touchingVerticesGen), and edge drawing functions.
-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TupleSections             #-}

module Tgraph.Convert where

import Data.List ((\\), intersect, foldl')
import qualified Data.IntMap.Strict as VMap (IntMap, map, filterWithKey, lookup, insert, empty, toList, assocs, keys)
import qualified Data.Map.Strict as Map (lookup, fromListWith) -- used for locateVertices
import qualified Data.Set as Set  (fromList,member,null,delete)-- used for locateVertices
import Data.Maybe (mapMaybe)
import qualified Data.IntSet as IntSet (member) -- for vertex set in relevantVP

import Diagrams.Prelude
import TileLib
import Tgraph.Prelude
-- import ChosenBackend (B)
import Diagrams.TwoD.Text (Text)

-- |Abbreviation for finite mappings from Vertex to Location (i.e Point)
type VertexLocMap = VMap.IntMap (Point V2 Double)


{-* VPatch
-}

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


{-|Convert a Tgraph to a VPatch.
This uses locateVertices to form an intermediate VertexLocMap (mapping of vertices to positions).
This makes the join of the face with lowest origin and lowest oppV align on the positive x axis.
-}
makeVP::Tgraph -> VPatch
makeVP g = VPatch {vLocs = locateVertices fcs, vpFaces  = fcs} where fcs = faces g

-- |Creates a VPatch from a list of tile faces, using the vertex locations from the given VPatch.
-- The vertices in the tile faces must have points assigned in the given VPatch vertex locations.
-- (This is not checked for, but missing locations for vertices will raise an error when drawing.)
-- subVP vp fcs can be used for both subsets of tile faces of vp,
-- and also for larger scale faces which use the same vertex to point assignment (e.g in compositions).
-- The vertex location map is not changed (see also relevantVP and restrictVP).
subVP:: VPatch -> [TileFace] -> VPatch
subVP vp fcs = vp {vpFaces  = fcs} 

-- | removes locations for vertices not used in the faces of a VPatch.
-- (Useful when restricting which labels get drawn)
relevantVP :: VPatch -> VPatch
relevantVP vp = vp{vLocs = locVs} where
     vs = facesVSet (vpFaces vp)
     locVs = VMap.filterWithKey (\ v _ -> v `IntSet.member` vs) $ vLocs vp

-- | A combination of subVP and relevantVP. Restricts a vp to a list of faces, removing locations for vertices not in the faces.
-- (Useful when restricting which labels get drawn)
restrictVP:: VPatch -> [TileFace] -> VPatch
restrictVP vp fcs = relevantVP (subVP vp fcs)

-- |Recover a Tgraph from a VPatch by dropping the vertex positions and checking Tgraph properties.
graphFromVP:: VPatch -> Tgraph
graphFromVP = checkedTgraph . vpFaces

-- |remove a list of faces from a VPatch
removeFacesVP :: [TileFace] -> VPatch -> VPatch
removeFacesVP fcs vp = restrictVP vp (vpFaces vp \\ fcs)

-- |make a new VPatch with a list of selected faces from a VPatch.
-- This will ignore any faces that are not in the given VPatch.
selectFacesVP:: [TileFace] -> VPatch -> VPatch
selectFacesVP fcs vp = restrictVP vp (fcs `intersect` vpFaces vp)

-- |find the location of a single vertex in a VPatch
findLoc :: Vertex -> VPatch -> Maybe (Point V2 Double)
findLoc v = VMap.lookup v . vLocs



{-* Drawing VPatches and Tgraphs
-}

-- |Make drawing tools applicable to VPatch
instance Drawable VPatch where
    drawWith pd vp = drawWith pd (dropLabels vp)

-- |converts a VPatch to a Patch, removing vertex information and converting faces to Located Pieces.
-- Use can be confined to Drawable VPatch instance and DrawableLabelled VPatch instance.
dropLabels :: VPatch -> Patch
dropLabels vp = fmap convert (vpFaces vp) where
  locations = vLocs vp
  convert face = case (VMap.lookup (originV face) locations , VMap.lookup (oppV face) locations) of
    (Just p, Just p') -> fmap (const (p' .-. p)) face `at` p -- using HalfTile functor fmap
    _ -> error $ "dropLabels: Vertex location not found for some vertices:\n    " 
                ++ show (faceVList face \\ VMap.keys locations)  ++ "\n"

-- |Make drawing tools applicable to Tgraphs.
instance Drawable Tgraph where
-- (Orphaned instance: Placing it in Tgraphs.Prelude or TileLib would make cyclic dependency of modules)
    drawWith pd = drawWith pd . makeVP

-- | A class for things that can be drawn with labels when given a measure for the label size and a 
-- a draw function (for Patches).
-- Thus labelSize m is a modifier of any Patch drawing function to add labels (of size measure m).
-- (Measures are defined in Diagrams - normalized\/output\/local\/global).
-- The argument type of the draw function is Patch rather than VPatch, which prevents labelling twice.
-- (So labelSize m draw typechecks but labelSize m1 (labelSize m2 draw) does not typecheck.)
class DrawableLabelled a where
-- When a specific Backend B is in scope,  labelSize :: Measure Double -> (Patch -> Diagram B) -> a -> Diagram B
  labelSize :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => 
               Measure Double -> (Patch -> Diagram2D b) -> a -> Diagram2D b


-- | VPatches can be drawn with labels
instance DrawableLabelled VPatch where
  labelSize m d vp = drawLabels m (vLocs vp) <> d (dropLabels vp) where
    -- When a specific Backend B is in scope, drawLabels :: Measure Double -> VertexLocMap -> Diagram B
     drawLabels r vpMap = position $ drawlabel <$> VMap.toList vpMap
       where drawlabel(v,p) = (p, baselineText (show v) # fontSize r # fc red)

-- | Tgraphs can be drawn with labels
instance DrawableLabelled Tgraph where
  labelSize r d = labelSize r d . makeVP

labelled,labelSmall,labelLarge :: (Renderable (Path V2 Double) b, Renderable (Text Double) b, DrawableLabelled a) => 
                                  (Patch -> Diagram2D b) -> a -> Diagram2D b
-- | Version of labelSize with a default normal label size. Example usage: labelled draw a , labelled drawj a
-- When a specific Backend B is in scope, labelled :: DrawableLabelled a => (Patch -> Diagram B) -> a -> Diagram B
labelled = labelSize (normalized 0.024)
-- | Version of labelSize with a default large label size. Example usage: labelLarge draw a , labelLarge drawj a 
-- When a specific Backend B is in scope, labelLarge :: DrawableLabelled a => (Patch -> Diagram B) -> a -> Diagram B
labelLarge = labelSize (normalized 0.036) 
-- | Version of labelSize with a default small label size. Example usage: labelSmall draw a , labelSmall drawj a 
-- When a specific Backend B is in scope, labelSmall :: DrawableLabelled a => (Patch -> Diagram B) -> a -> Diagram B
labelSmall = labelSize (normalized 0.006)

-- |rotateBefore vfun a g - makes a VPatch from g then rotates by angle a before applying the VPatch function vfun.
-- Tgraphs need to be rotated after a VPatch is calculated but before any labelled drawing.
-- E.g. rotateBefore (labelled draw) a g.
rotateBefore :: (VPatch -> a) -> Angle Double -> Tgraph -> a
rotateBefore vfun angle = vfun . rotate angle . makeVP


{-* Alignment with Vertices
-}

-- |center a VPatch on a particular vertex. (Raises an error if the vertex is not in the VPatch vertices)
centerOn :: Vertex -> VPatch -> VPatch
centerOn a vp = 
    case findLoc a vp of
        Just loca -> translate (origin .-. loca) vp
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

-- |alignments takes a list of vertex pairs for respective alignmants of VPatches in the second list.
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
alignAll (a,b) = fmap (alignXaxis (a,b))

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



{-* Vertex Location Calculation -}


{-| locateVertices: processes a list of faces to associate points for each vertex.
It aligns the lowest numbered join of the faces on the x-axis, and returns a vertex-to-point Map.
It will raise an error if faces are not connected.
If faces have crossing boundaries (i.e not locally tile-connected), this could raise an error
or a result with touching vertices (i.e. more than one vertex with the same location).
-}
locateVertices:: [TileFace] -> VertexLocMap
--  This version is made more efficient by calculating an edge to face map
--  and also using Sets for 2nd arg of fastAddVPoints.
locateVertices [] = VMap.empty
locateVertices fcs = fastAddVPoints [joinFace] (Set.fromList more) (axisJoin joinFace) where
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
                                          ++ show fcOther ++ "/n"
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

-- |axisJoin face 
-- initialises a vertex to point mapping with locations for the join edge vertices of face
-- with originV face at the origin and aligned along the x axis with unit length for a half dart
-- and length phi for a half kite. (Used to initialise locateVertices)
axisJoin::TileFace -> VertexLocMap                
axisJoin face = 
  VMap.insert (originV face) origin $ VMap.insert (oppV face) (p2(x,0)) VMap.empty where
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

{-*  Drawing (located) Edges
-}

-- |produce a diagram of a list of edges (given a VPatch)
-- Will raise an error if any vertex of the edges is not a key in the vertex to location mapping of the VPatch.
-- When a specific Backend B is in scope, drawEdgesIn :: VPatch -> [Dedge] -> Diagram B
drawEdgesIn :: Renderable (Path V2 Double) b =>
               VPatch -> [Dedge] -> Diagram2D b
drawEdgesIn vp = drawEdges (vLocs vp) --foldMap (drawEdgeWith vp)

-- |produce a diagram of a single edge (given a VPatch)
-- Will raise an error if either vertex of the edge is not a key in the vertex to location mapping of the VPatch.
-- When a specific Backend B is in scope, drawEdgeWith :: VPatch -> Dedge -> Diagram B
drawEdgeWith:: Renderable (Path V2 Double) b =>
               VPatch -> Dedge -> Diagram2D b
drawEdgeWith vp = drawEdge (vLocs vp)

-- |produce a diagram of a list of edges (given a mapping of vertices to locations)
-- Will raise an error if any vertex of the edges is not a key in the mapping.
-- When a specific Backend B is in scope, drawEdges :: VertexLocMap -> [Dedge] -> Diagram B
drawEdges :: Renderable (Path V2 Double) b =>
             VertexLocMap -> [Dedge] -> Diagram2D b
drawEdges vpMap = foldMap (drawEdge vpMap)

-- |produce a diagram of a single edge (given a mapping of vertices to locations).
-- Will raise an error if either vertex of the edge is not a key in the mapping.
-- When a specific Backend B is in scope, drawEdge :: VertexLocMap -> Dedge -> Diagram B
drawEdge :: Renderable (Path V2 Double) b =>
            VertexLocMap -> Dedge -> Diagram2D b
drawEdge vpMap (a,b) = case (VMap.lookup a vpMap, VMap.lookup b vpMap) of
                         (Just pa, Just pb) -> pa ~~ pb
                         _ -> error $ "drawEdge: location not found for one or both vertices "++ show(a,b) ++ "\n"



{-*  Touching Vertices
-}

{-| 
touchingVertices checks that no vertices are too close to each other using locateVertices.
If vertices are too close that indicates we may have different vertex numbers at the same location
(the touching vertex problem). 
It returns pairs of vertices that are too close (higher number first in each pair)
An empty list is returned if there are no touching vertices.
Complexity has order of the square of the number of vertices.
                           
This is used in makeTgraph and fullUnion (via correctTouchingVertices).
-}
touchingVertices:: [TileFace] -> [(Vertex,Vertex)]
touchingVertices fcs = check vpAssoc where
  vpAssoc = VMap.assocs $ locateVertices fcs  -- assocs puts in key order so that check returns (higher,lower) pairs
  check [] = []
  check ((v,p):more) = [(v1,v) | v1 <- nearv ] ++ check (filter ((`notElem` nearv).fst) more)
                        where nearv = [v1 | (v1,p1) <- more, touching p p1 ]
-- check ((v,p):more) = [(v1,v) | (v1,p1) <- more, touching p p1 ] ++ check more
-- does not correctly deal with 3 or more vertices touching at the same point

{-|touching checks if two points are considered close.
Close means the square of the distance between them is less than a certain number (currently 0.1) so they cannot be
vertex locations for 2 different vertices in a VPatch using unit scale for short edges.
It is used in touchingVertices and touchingVerticesGen).
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
locateVerticesGen:: [TileFace] -> VertexLocMap
locateVerticesGen [] = VMap.empty
locateVerticesGen fcs = fastAddVPointsGen [face] (Set.fromList more) (axisJoin face) where
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
        fcOther' = foldr Set.delete fcOther nbs
        vpMap' = addVPoint f vpMap
-- Generalises buildEFMap by allowing for multiple faces on a directed edge.
-- buildEFMapGen:: [TileFace] -> Map.Map Dedge [TileFace]
    buildEFMapGen = Map.fromListWith (++) . concatMap processFace
    processFace f = (,[f]) <$> faceDedges f

-- Generalised edgeNbs allowing for multiple faces on a directed edge.
-- edgeNbsGen:: Map.Map Dedge [TileFace] -> TileFace -> [TileFace]
    edgeNbsGen f = concat $ mapMaybe getNbrs edges where
      getNbrs e = Map.lookup e efMapGen
      edges = fmap reverseD (faceDedges f) 
{-
    edgeNbsGen efMapGen f = concat $ mapMaybe getNbrs edges where
      getNbrs e = Map.lookup e efMapGen
      edges = fmap reverseD (faceDedges f) 
-}


 

