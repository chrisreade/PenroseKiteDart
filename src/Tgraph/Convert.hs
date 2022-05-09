{-|
Module      : Tgraph.Convert
Description : Conversion of Tgraphs to Patches (and VPatches) for drawing Tgraphs
Copyright   : (c) Chris Reade, 2021
License     : MIT
Maintainer  : chrisreade@mac.com
Stability   : experimental

Includes conversion operations from Tgraphs to diagrams as well as the intermediate type VPatch
to allow vertex labels to be drawn.
-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Tgraph.Convert where

import TileLib
import Tgraph.Prelude

import Data.List ((\\), find)
import qualified Data.Map as Map (Map, lookup, toList, fromList)
import Data.Maybe (mapMaybe)

import Diagrams.Prelude
import ChosenBackend (B)

-- |a DualRep contains two representations - a vector and a face(= 3 vertices)
data DualRep = DualRep {vector:: V2 Double, face::(Int,Int,Int)} deriving Show

-- |needed for making DualRep (and hence VPatch) transformable
type instance N DualRep = Double
type instance V DualRep = V2

type instance N Vertex = Double
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

{-| makeVPatch - Converting a graph to a vpatch
An empty graph is a special case.
Otherwise
Use chooseLowest to choose a starting face (placed at front of list of faces of g)
Then use createVPoints to form a mapping of vertices to positions vpMap
Then convert original faces to located hybrids using makeLHyb and vpMap 
and convert vpMap to located vertices using locateV and Map.toList
to form a VPatch
-}
makeVPatch::Tgraph -> VPatch
makeVPatch g = if nullGraph g 
               then VPatch { lVertices = [], lHybrids = [] }
               else VPatch { lVertices = fmap locateV (Map.toList vpMap)
                           , lHybrids  = makeLHyb <$> faces g
                           }
    where
    (face:more) = chooseLowest (faces g)
    vpMap = createVPoints $ chooseLowest $ faces g
    locateV (v,p) = v `at` p
    makeLHyb fc = case (Map.lookup (originV fc) vpMap , Map.lookup (oppV fc) vpMap) of
                  (Just p, Just p') -> fmap (dualRep (p' .-. p)) fc `at` p -- using HalfTile functor fmap
                  _ -> error ("makeVPatch: " ++ show fc)

-- |for Non-empty list of tile faces
-- find the face with lowest originV (and then lowest oppV) 
-- Move the face to the front of the returned list
-- Used by makeVPatch (and hence makePatch) for Non-empty graph
chooseLowest:: [TileFace] -> [TileFace]
chooseLowest fcs = face:(fcs\\[face]) where
    a = minimum (fmap originV fcs)
    aFs = filter ((a==) . originV) fcs
    b = minimum (fmap oppV aFs)
    face = case filter (((a,b)==) . joinOfTile) aFs of  -- should be find
           (face:_) -> face
           []       -> error "chooseLowest: empty graph?"


{- |
makePatch uses makeVPatch first then the Hybrids are converted to Pieces
and the Located Vertex information is dropped
-}
makePatch:: Tgraph -> Patch
makePatch = dropVertices . makeVPatch

-- |graphFromVP: an inverse to makeVPatch which checks for connected and no crossing boundaries
graphFromVP:: VPatch -> Tgraph
graphFromVP = checkTgraph . dropVectors

{-
*************************************
Drawing Patches, VPatches, and Graphs
*************************************
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

-- |make a diagram of located vertices (used by drawVPatch and drawVPatchWith)
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

-- |removeFacesGtoVP fcs g -  only selected faces (fcs) are kept after converting g to a VPatch
removeFacesGtoVP :: [TileFace] -> Tgraph -> VPatch
removeFacesGtoVP fcs g = removeFacesVP fcs (makeVPatch g)

{- 
----------------------------------------
 Alignment with vertices
-------------------------------------------
-}


-- |center a VPatch on a particular vertex
centerOn :: Vertex -> VPatch -> VPatch
centerOn a vp = 
    case findLoc a vp of
        Just loca -> translate (origin .-. loca) vp
        _ -> error ("centerOn: vertex not found "++ show a)

-- |alignXaxis takes a vertex pair (a,b) an a VPatch vp
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
-- The vertex pair list can be shorter than the list of vpatches - the remaining vpatches are left unrotated.
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

{- 
----------------------------------------
 Rotating and Scaling lists
-------------------------------------------
-}

-- |rotations takes a list of integers (ttangles) for respective rotations of items in the second list (things to be rotated).
-- This includes Diagrams, Patches, VPatches
-- The integer list can be shorter than the list of items - the remaining items are left unrotated.
rotations :: (Transformable a, V a ~ V2, N a ~ Double) => [Int] -> [a] -> [a]
rotations (n:ns) (d:ds) = rotate (ttangle n) d: rotations ns ds
rotations [] ds = ds
rotations _  [] = error "rotations: too many rotation integers"

-- |scales takes a list of doubles for respective scalings of items in the second list (things to be scaled).
-- This includes Diagrams, Patches, VPatches
-- The list of doubles can be shorter than the list of items - the remaining items are left unscaled.
scales :: (Transformable a, V a ~ V2, N a ~ Double) => [Double] -> [a] -> [a]
scales (s:ss) (d:ds) = scale s d: scales ss ds
scales [] ds = ds
scales _  [] = error "scales: too many scalars"

-- |increasing scales by phi along a list starting with 1
phiScales:: (Transformable a, V a ~ V2, N a ~ Double) => [a] -> [a]
phiScales = phiScaling 1

-- |increasing scales by phi along a list starting with given argument s
phiScaling:: (Transformable a, V a ~ V2, N a ~ Double) => Double -> [a] -> [a]
phiScaling s [] = []
phiScaling s (d:more) = scale s d: phiScaling (phi*s) more

{-
----------------------------------------
 Auxiliary definitions
-------------------------------------------
-}

-- |makes an association list of vertex to location from a VPatch
vertexLocs :: VPatch -> [(Vertex, Point V2 Double)]
vertexLocs = fmap ((\(p,v)->(v,p)) . viewLoc) . lVertices

-- |find the location of a specific vertex in a VPatch
findLoc :: Vertex -> VPatch -> Maybe (Point V2 Double)
findLoc v vp = lookup v (vertexLocs vp)
 
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



-- |displaying the boundary of a Tgraph in lime
drawGBoundary :: Tgraph -> Diagram B
drawGBoundary g =  lc lime (drawEdges vpMap bd) <> drawVPatch vp where
    vp = makeVPatch g
    vpMap = Map.fromList $ vertexLocs vp
    bd = boundaryDedges g

-- |produce a diagram of a list of edges (given a mapping of vertices to locations)
drawEdges :: Mapping Vertex (Point V2 Double) -> [(Vertex,Vertex)] -> Diagram B
drawEdges vpMap = foldMap (drawEdge vpMap)

-- |produce a diagram of a single edge (given a mapping of vertices to locations)
drawEdge :: Mapping Vertex (Point V2 Double) -> (Vertex,Vertex) -> Diagram B
drawEdge vpMap (a,b) = case (Map.lookup a vpMap, Map.lookup b vpMap) of
                         (Just pa, Just pb) -> pa ~~ pb
                         _ -> error ("drawEdge: location not found for one or both vertices "++ show(a,b))
   

{-
    *********************
    Drawing of SubTgraphs
    *********************
-}
{-|
    To draw a SubTgraph, we need a list of functions turning patches into diagrams
    The first function is applied to a patch for untracked faces
    Subsequent functions are applied to the respective tracked subsets
    (The last patch is atop earlier ones, so the untracked patch is at the bottom)
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


{-| TESTING function touchingVertices
For use if Touching vertex chack is switched off in forcing.  This is a reptrospective check.

touchingVertices checks that no vertices are too close to each other by making a VPatch of a Tgraph.
If vertices are too close that indicates we may have the same point with two different vertex numbers
arising from the touching vertex problem. 
It returns pairs of vertices that are too close 
(i.e less than 0.5 where 1.0 would be the length of short edges)
An empty list is returned if there is no touching vertex problem.
Complexity has order of the square of the number of vertices (calculates distance between all pairs)
-}
touchingVertices:: Tgraph -> [(Vertex,Vertex)]
touchingVertices g = check $ vertexLocs $ makeVPatch g where
  check [] = []
  check ((v,p):more) = [(v,v1) | (v1,p1) <- more, tooClose p p1 ] ++ check more
  tooClose p p1 = quadrance (p .-. p1) < 0.25 -- quadrance is square of length of a vector
--  sqLength vec = dot vec vec
--  assocVP = vertexLocs $ makeVPatch g




 

