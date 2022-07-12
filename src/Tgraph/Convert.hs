{-|
Module      : Tgraph.Convert
Description : Conversion of Tgraphs to Patches (and VPatches) for drawing Tgraphs
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

Includes conversion operations from Tgraphs to diagrams as well as the intermediate type VPatch
to allow vertex labels to be drawn.
Includes functions to calculate (relative) locations of vertices (createVPoints, adddVPoints)
-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Tgraph.Convert where

import Data.List ((\\), find, partition)
import qualified Data.IntMap.Strict as VMap (IntMap, lookup, insert, empty, toList, fromList)
import Data.Maybe (mapMaybe)

import Diagrams.Prelude
import TileLib
import Tgraph.Prelude
import ChosenBackend (B)

-- |Abbreviation for mappings from Vertex to Location (i.e Point)
type VertexLocMap = VMap.IntMap (Point V2 Double)

{- * VPatches
-}
-- |a DualRep contains two representations - a vector and a face(= 3 vertices)
data DualRep = DualRep {vector:: V2 Double, face::(Vertex,Vertex,Vertex)} deriving Show

-- |needed for making DualRep (and hence VPatch) transformable
type instance N DualRep = Double
-- |needed for making DualRep (and hence VPatch) transformable
type instance V DualRep = V2

-- |needed for making VPatch an instance of Show
type instance N Vertex = Double
-- |needed for making VPatch an instance of Show
type instance V Vertex = V2


-- |making DualRep (and hence VPatch) transformable
instance Transformable DualRep where 
    transform t (DualRep {vector = v, face = vs}) = DualRep {vector = transform t v, face = vs}

-- |construct a dualRep from a vector and a vertex triple
dualRep :: V2 Double -> (Vertex, Vertex, Vertex) -> DualRep
dualRep vec vs = DualRep{vector = vec, face = vs}

-- |A hybrid is a HalfTile with the dual representation of the face vertices and the join vector  
type Hybrid = HalfTile DualRep

-- |A VPatch (vertex patch) is a patch with both face and vertex and vector information.
-- It contains a list of Located vertices and a list of Located Hybrids
data VPatch = VPatch {lVertices :: [Located Vertex],  lHybrids::[Located Hybrid]} deriving Show
type instance N VPatch = Double
type instance V VPatch = V2


instance Transformable VPatch where 
    transform t (VPatch {lVertices = lvs,  lHybrids = lhs})
         =  VPatch {lVertices = fmap (\lv -> unLoc lv `at` transform t (loc lv)) lvs,  lHybrids = transform t lhs}

{-| For converting a Tgraph to a VPatch.
This uses Tgraph.Prelude.createVPoints to form a mapping of vertices to positions.
This makes the join of the face with lowest origin and lowest oppV align on the positive x axis
-}
makeVPatch::Tgraph -> VPatch
makeVPatch g = VPatch { lVertices = fmap locateV (VMap.toList vpMap)
                      , lHybrids  = makeLHyb <$> faces g
                      } where
    vpMap = createVPoints $ faces g
    locateV (v,p) = v `at` p
    makeLHyb fc = case (VMap.lookup (originV fc) vpMap , VMap.lookup (oppV fc) vpMap) of
                  (Just p, Just p') -> fmap (dualRep (p' .-. p)) fc `at` p -- using HalfTile functor fmap
                  _ -> error ("makeVPatch: " ++ show fc)
{- |
makePatch uses makeVPatch first then the Hybrids are converted to Pieces
and the Located Vertex information is dropped
-}
makePatch:: Tgraph -> Patch
makePatch = dropVertices . makeVPatch

-- |An inverse to makeVPatch which checks for connected and no crossing boundaries
graphFromVP:: VPatch -> Tgraph
graphFromVP = checkedTgraph . dropVectors

{- * Drawing VPatches and Graphs
-}

-- |simplest drawing without vertex labels
drawGraph:: Tgraph -> Diagram B
drawGraph = drawPatch . makePatch

-- |simplest drawing without vertex labels
dashJGraph:: Tgraph -> Diagram B
dashJGraph = dashJPatch . makePatch

-- |simplest drawing with vertex labels
drawVGraph:: Tgraph -> Diagram B
drawVGraph = drawVPatch . makeVPatch

-- |convert a VPatch to a diagram with vertex labels
drawVPatch:: VPatch -> Diagram B
drawVPatch = drawVPatchWith dashJPiece

-- |drawVPatchWith pd vp - convert VPatch vp to a diagram with vertex labels using pd to draw pieces
drawVPatchWith :: (Piece -> Diagram B) -> VPatch -> Diagram B
drawVPatchWith pd vp = drawVlabels (lVertices vp) <> patchWith pd (dropVertices vp)

-- |make a diagram of vertex labels given located vertices (used by drawVPatch and drawVPatchWith)
drawVlabels :: [Located Vertex] -> Diagram B
drawVlabels locvs = position $ fmap (viewLoc . mapLoc label) locvs
    where label n = baselineText (show n) # fontSize (global 0.3) # fc red
{- Alternative to global is normalized:
   Best results with global 0.3, normalized 0.25, output 10
   local - same as global for unscaled examples
          label n = baselineText (show n) # fontSize (normalized 0.02) # fc red
-}
-- |remove a list of faces from a VPatch
removeFacesVP :: [TileFace] -> VPatch -> VPatch
removeFacesVP fcs vp = foldr removeFace vp fcs where
    removeFace fc = withHybs (filter (not . matchingF fc))
    matchingF fc lhyb = asFace (unLoc lhyb) == fc

-- |make a new VPatch with a list of selected faces from a VPatch
selectFacesVP:: [TileFace] -> VPatch -> VPatch
selectFacesVP fcs = withHybs (findAll fcs) where
    findAll fcs lfaces = mapMaybe (findIn lfaces) fcs 
    findIn lfaces fc = find (matchingF fc) lfaces
    matchingF fc lhyb = asFace (unLoc lhyb) == fc

-- |selectFacesGtoVP fcs g -  only selected faces (fcs) are kept after converting g to a VPatch
selectFacesGtoVP :: [TileFace] -> Tgraph -> VPatch
selectFacesGtoVP fcs g = selectFacesVP fcs (makeVPatch g)

-- |removeFacesGtoVP fcs g - remove faces (fcs) after converting g to a VPatch
removeFacesGtoVP :: [TileFace] -> Tgraph -> VPatch
removeFacesGtoVP fcs g = removeFacesVP fcs (makeVPatch g)


{- * Alignment with Vertices
-}

-- |center a VPatch on a particular vertex
centerOn :: Vertex -> VPatch -> VPatch
centerOn a vp = 
    case findLoc a vp of
        Just loca -> translate (origin .-. loca) vp
        _ -> error ("centerOn: vertex not found "++ show a)

-- |alignXaxis takes a vertex pair (a,b) and a VPatch vp
-- for centering vp on a and rotating the result so that b is on the positive X axis.
alignXaxis :: (Vertex, Vertex) -> VPatch -> VPatch    
alignXaxis (a,b) vp =  rotate angle newvp
  where newvp = centerOn a vp
        angle = signedAngleBetweenDirs (direction unitX) (direction (locb .-. origin)) 
        locb = case findLoc b newvp of
                Just l -> l
                Nothing -> error ("alignXaxis: second alignment vertex not found (Vertex " ++ show b ++ ")")

-- |alignments takes a list of vertex pairs for respective rotations of VPatches in the second list.
-- For a pair (a,b) the Vpatch is centered on a then b is aligned along the positive x axis. 
-- The vertex pair list can be shorter than the list of vpatches - the remaining vpatches are left as they are.
alignments :: [(Vertex, Vertex)] -> [VPatch] -> [VPatch]     
alignments [] vps = vps
alignments prs [] = error "alignments: Too many alignment pairs"  -- prs non-null
alignments ((a,b):more) (vp:vps) =  alignXaxis (a,b) vp : alignments more vps
--alignments = zipWith alignXaxis -- doesn't allow for shorter alignment list


-- |alignAll (a,b) vpList
-- provided both vertices a and b exist in each Vpatch in vpList, the VPatches are all aligned
-- centred on a, with b on the positive x axis
alignAll :: (Vertex, Vertex) -> [VPatch] -> [VPatch]     
alignAll (a,b) = fmap (alignXaxis (a,b))
    -- alignments ablist vps where ablist = take (length vps) (repeat (a,b))



{- *  Auxiliary definitions for VPatch and Hybrid
-}


-- |makes an association list of vertex to location from a VPatch
vertexLocs :: VPatch -> VertexLocMap
vertexLocs = VMap.fromList . fmap ((\(p,v)->(v,p)) . viewLoc) . lVertices

-- |find the location of a specific vertex in a VPatch
findLoc :: Vertex -> VPatch -> Maybe (Point V2 Double)
findLoc v vp = VMap.lookup v (vertexLocs vp)
 
-- |Apply a function to just the list of located hybrids in a VPatch (leaving located vertices untouched)
withHybs:: ([Located Hybrid]->[Located Hybrid]) -> VPatch -> VPatch
withHybs f (VPatch {lVertices = lvs,  lHybrids = lhs}) = VPatch {lVertices = lvs,  lHybrids = f lhs}


-- |convert a Hybrid to a Piece, dropping the Vertex information
asPiece:: Hybrid -> Piece
asPiece = fmap vector  -- fmap of functor HalfTile

-- |convert a Hybrid to a TileFace, dropping the Vector information
asFace:: Hybrid -> TileFace
asFace = fmap face  -- fmap of functor HalfTile

-- |dropVertices removes vertex information from Hybrids and removes located vertex list
dropVertices:: VPatch -> Patch
dropVertices vp = fmap (mapLoc asPiece) (lHybrids vp)

-- |dropVectors removes vector information from Hybrids and removes located vertex list
dropVectors:: VPatch -> [TileFace]
dropVectors vp = fmap (asFace . unLoc) (lHybrids vp)

{- * Vertex Location Calculation -}

{-| createVPoints: processes a list of faces to associate points for each vertex.
     Faces must be tile-connected. It aligns the lowest numbered join of the faces on the x-axis.
      Returns a vertex-to-point Map.
-}
createVPoints:: [TileFace] -> VertexLocMap
createVPoints [] = VMap.empty
createVPoints faces = addVPoints [face] more (axisJoin face) where
    (face:more) = lowestJoinFirst faces

{-| addVPoints readyfaces fcOther vpMap.
The first argument list of faces (readyfaces) contains the ones being processed next in order where
each will have at least two known vertex locations.
The second argument list of faces (fcOther) are faces that have not yet been added
and may not yet have known vertex locations.
The third argument is the mapping of vertices to points.
This is used in tryUpdate as well as createVPoints.
-}
addVPoints:: [TileFace] -> [TileFace] -> VertexLocMap -> VertexLocMap
addVPoints [] [] vpMap = vpMap 
addVPoints [] fcOther vpMap = error ("addVPoints: Faces not tile-connected " ++ show fcOther)
addVPoints (fc:fcs) fcOther vpMap = addVPoints (fcs++fcs') fcOther' vpMap' where
  vpMap' = case thirdVertexLoc fc vpMap of
             Just (v,p) -> VMap.insert v p vpMap
             Nothing -> vpMap
  (fcs', fcOther')   = partition (edgeNb fc) fcOther

-- |For a non-empty list of tile faces
-- find the face with lowest originV (and then lowest oppV).
-- Move this face to the front of the returned list of faces.
-- Used by createVPoints to determine the starting point for location calculation
lowestJoinFirst:: [TileFace] -> [TileFace]
lowestJoinFirst fcs = face:(fcs\\[face]) where
    a = minimum (fmap originV fcs)
    aFs = filter ((a==) . originV) fcs
    b = minimum (fmap oppV aFs)
    face = case filter (((a,b)==) . joinOfTile) aFs of  -- should be find
           (face:_) -> face
           []       -> error "lowestJoinFirst: empty graph?"

-- |axisJoin fc 
-- initialises a vertex to point mapping with locations for the join edge vertices of fc
-- with originV fc at the origin and aligned along the x axis. (Used to initialise createVPoints)
axisJoin::TileFace -> VertexLocMap                
axisJoin (LD(a,b,_)) = VMap.insert a origin $ VMap.insert b (p2(1,0)) VMap.empty -- [(a,origin), (b, p2(1,0))]
axisJoin (RD(a,_,c)) = VMap.insert a origin $ VMap.insert c (p2(1,0)) VMap.empty --[(a,origin), (c, p2(1,0))]
axisJoin (LK(a,_,c)) = VMap.insert a origin $ VMap.insert c (p2(phi,0)) VMap.empty --[(a,origin), (c, p2(phi,0))]
axisJoin (RK(a,b,_)) = VMap.insert a origin $ VMap.insert b (p2(phi,0)) VMap.empty -- [(a,origin), (b, p2(phi,0))]

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

{- *  Drawing (located) Edges
-}

-- |produce a diagram of a list of edges (given a mapping of vertices to locations)
drawEdges :: VertexLocMap -> [(Vertex,Vertex)] -> Diagram B
drawEdges vpMap = foldMap (drawEdge vpMap)

-- |produce a diagram of a single edge (given a mapping of vertices to locations)
drawEdge :: VertexLocMap -> (Vertex,Vertex) -> Diagram B
drawEdge vpMap (a,b) = case (VMap.lookup a vpMap, VMap.lookup b vpMap) of
                         (Just pa, Just pb) -> pa ~~ pb
                         _ -> error ("drawEdge: location not found for one or both vertices "++ show(a,b))
 

{- *  Drawing SubTgraphs
-}
                     
                     
{-|
    To draw a SubTgraph, we need a list of functions turning patches into diagrams
    The first function is applied to a patch for untracked faces
    Subsequent functions are applied to the respective tracked subsets
    (Each patch is atop earlier ones, so the untracked patch is at the bottom)
-}
drawSubTgraph:: [Patch -> Diagram B] -> SubTgraph -> Diagram B
drawSubTgraph drawList sub = drawAll drawList (pUntracked:pTrackedList) where
          vpFull = makeVPatch (fullGraph sub)
          pTrackedList = fmap (dropVertices . (`selectFacesVP` vpFull)) (trackedSubsets sub)
          pUntracked = dropVertices $ removeFacesVP (concat (trackedSubsets sub)) vpFull
          drawAll [] _ = mempty
          drawAll _ [] = mempty
          drawAll (f:fmore)(p:pmore) =  drawAll fmore pmore <> f p

-- |special case of drawSubTgraph using 1 subset (and 2 patchdrawing functions):
-- normal (black), then red
drawSubTgraph1 :: SubTgraph -> Diagram B
drawSubTgraph1 = drawSubTgraph [ drawPatch
                               , lc red . drawPatch
                               ]
-- |special case of drawSubTgraph using 2 subsets (and 3 patchdrawing functions):
-- normal (black), then red, then filled black
drawSubTgraph2 :: SubTgraph -> Diagram B
drawSubTgraph2 = drawSubTgraph [ drawPatch
                               , lc red . drawPatch
                               , patchWith (fillDK black black)
                               ]

{- *  Touching Vertex global check
-}

{-| A TESTING function
for use if the touching vertex check is switched off in forcing.  This is a reptrospective check.
touchingVertices checks that no vertices are too close to each other using createVPoints.
If vertices are too close that indicates we may have the same point with two different vertex numbers
arising from the touching vertex problem. 
It returns pairs of vertices that are too close 
(i.e less than 0.5 where 1.0 would be the length of short edges)
An empty list is returned if there is no touching vertex problem.
Complexity has order of the square of the number of vertices (calculates distance between all pairs)
-}
touchingVertices:: Tgraph -> [(Vertex,Vertex)]
touchingVertices g = check vpAssoc where
  vpAssoc = VMap.toList $ createVPoints (faces g)  
  check [] = []
  check ((v,p):more) = [(v,v1) | (v1,p1) <- more, touching p p1 ] ++ check more

{-|touching checks if two points are considered close.
Close means the square of the distance between them is less than 0.25 so they cannot be
vertex locations for 2 different vertices in a VPatch using unit scale for short edges.
It is used in touchingVertices checks but also exported (used in Tgraph.Relabelling(fullUnion))
-}
touching :: Point V2 Double -> Point V2 Double -> Bool
touching p p1 = quadrance (p .-. p1) < 0.25 -- quadrance is square of length of a vector



 

