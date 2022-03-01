{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module GraphConvert where

import HalfTile
import TileLib
import Tgraphs

import Data.List ((\\), find)
import qualified Data.Map as Map (Map, lookup, toList, fromList)
import Data.Maybe (mapMaybe)

import Diagrams.Prelude
import ChosenBackend (B)

-- a DualRep is a pair of representations - a vector and a face(= 3 vertices)
data DualRep = DualRep {vector:: V2 Double, face::(Int,Int,Int)} deriving Show

-- needed for making transformable
type instance N DualRep = Double
type instance V DualRep = V2

type instance N Vertex = Double
type instance V Vertex = V2

instance Transformable DualRep where 
    transform t (DualRep {vector = v, face = vs}) = DualRep {vector = transform t v, face = vs}

-- construct a dualRep from a vector and a vertex triple
dualRep :: V2 Double -> (Vertex, Vertex, Vertex) -> DualRep
dualRep vec vs = DualRep{vector = vec, face = vs}

-- | A hybrid is a HalfTile with the dual representation of the face vertices and the join vector  
type Hybrid = HalfTile DualRep

-- | A VPatch (vertex patch) is a patch with both face and vertex and vector information.
-- It contains a list of Located vertices and a list of Located Hybrids
data VPatch = VPatch {lVertices :: [Located Vertex],  lHybrids::[Located Hybrid]} deriving Show
type instance N VPatch = Double
type instance V VPatch = V2


instance Transformable VPatch where 
    transform t (VPatch {lVertices = lvs,  lHybrids = lhs})
         =  VPatch {lVertices = fmap (\lv -> unLoc lv `at` transform t (loc lv)) lvs,  lHybrids = transform t lhs}

{- | makeVPatch - Converting a graph to a vpatch
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
                           , lHybrids  = fmap makeLHyb $ faces g
                           }
    where
    (face:more) = chooseLowest (faces g)
--    (assocV,_) = buildVEAssocs [face] more [(originV face,origin)] (initJvec face)
    vpMap = createVPoints $ chooseLowest $ faces g
    locateV (v,p) = v `at` p
    makeLHyb fc = case (Map.lookup (originV fc) vpMap , Map.lookup (oppV fc) vpMap) of
                  (Just p, Just p') -> fmap (dualRep (p' .-. p)) fc `at` p -- using HalfTile functor fmap
                  _ -> error ("makeVPatch: " ++ show fc)

-- | graphFromVP: an inverse to makeVPatch which checks for connected and no crossing boundaries
graphFromVP:: VPatch -> Tgraph
graphFromVP = checkTgraph . dropVectors

{-
makePatch uses makeVPatch first then the Hybrids are converted to Pieces
and the Located Vertex information is thrown away
-}
makePatch:: Tgraph -> Patch
makePatch = dropVertices . makeVPatch



-- | touchingVertices checks that no vertices are too close to each other by making a VPatch of a Tgraph.
-- If vertices are too close that indicates we may have the same point with two different vertex numbers
-- arising from the touching vertex problem. 
-- It returns pairs of vertices that are too close 
-- (i.e less than 0.5 where 1.0 would be the length of short edges)
-- An empty list is returned if there is no touching vertex problem.
-- Complexity has order of the square of the number of vertices (calculates distance between all pairs)
touchingVertices:: Tgraph -> [(Vertex,Vertex)]
touchingVertices g = check assocVP where
  check [] = []
  check ((v,p):more) = [(v,v1) | (v1,p1) <- more, tooClose p p1 ] ++ check more
  tooClose p p1 = quadrance (p .-. p1) < 0.25 -- quadrance is square of length of a vector
--  sqLength vec = dot vec vec
  assocVP = fmap viewLoc' $ lVertices $ makeVPatch g
  viewLoc' x = (v,p) where (p,v) = viewLoc x

{-
*************************************
Drawing Patches, VPatches, and Graphs
*************************************
-}

-- | simplest drawing without vertex labels
drawGraph:: Tgraph -> Diagram B
drawGraph = drawPatch . makePatch

-- | simplest drawing without vertex labels
dashJGraph:: Tgraph -> Diagram B
dashJGraph = dashJPatch . makePatch

-- | simplest drawing with vertex labels
drawVGraph:: Tgraph -> Diagram B
drawVGraph = drawVPatch . makeVPatch

drawVPatch:: VPatch -> Diagram B
drawVPatch = drawVPatchWith dashJPiece

drawVPatchWith :: (Piece -> Diagram B) -> VPatch -> Diagram B
drawVPatchWith cd vp = drawVlabels (lVertices vp) <> patchWith cd (dropVertices vp)

drawVlabels :: [Located Vertex] -> Diagram B
drawVlabels locvs = position $ fmap (viewLoc . mapLoc label) locvs
    where label n = baselineText (show n) # fontSize (global 0.3) # fc red
{- Alternative to global is normalized:
   Best results with global 0.3, normalized 0.25, output 10
   local - same as global for unscaled examples
          label n = baselineText (show n) # fontSize (normalized 0.02) # fc red
-}
removeFacesVP :: [TileFace] -> VPatch -> VPatch
removeFacesVP fcs vp = foldr removeFace vp fcs where
    removeFace fc = withHybs (filter (not . matchingF fc))
    matchingF fc lhyb = asFace (unLoc lhyb) == fc

selectFacesVP:: [TileFace] -> VPatch -> VPatch
selectFacesVP fcs vp = withHybs (findAll fcs) vp where
    findAll fcs lfaces = mapMaybe (findIn lfaces) fcs 
    findIn lfaces fc = find (matchingF fc) lfaces
    matchingF fc lhyb = asFace (unLoc lhyb) == fc

-- | selectFacesGtoVP fcs g -  only selected faces (fcs) are kept after converting g to a VPatch
selectFacesGtoVP :: [TileFace] -> Tgraph -> VPatch
selectFacesGtoVP fcs g = selectFacesVP fcs (makeVPatch g)

-- | removeFacesGtoVP fcs g -  only selected faces (fcs) are kept after converting g to a VPatch
removeFacesGtoVP :: [TileFace] -> Tgraph -> VPatch
removeFacesGtoVP fcs g = removeFacesVP fcs (makeVPatch g)

findLoc :: Vertex -> VPatch -> Maybe (Point V2 Double)
findLoc v vp = fmap loc $ find matchingV (lVertices vp) -- fmap for Functor Maybe
               where matchingV lv = unLoc lv==v

-- | center a VPatch on a particular vertex
centerOn :: Vertex -> VPatch -> VPatch
centerOn a vp = 
    case findLoc a vp of
        Just loca -> translate (origin .-. loca) vp
        _ -> error ("centerOn: vertex not found "++ show a)

-- | alignXaxis takes a vertex pair (a,b) an a VPatch vp
-- for centering vp on a and rotating the result so that b is on the positive X axis.
alignXaxis :: (Vertex, Vertex) -> VPatch -> VPatch    
alignXaxis (a,b) vp =  rotate angle newvp
  where newvp = centerOn a vp
        angle = signedAngleBetweenDirs (direction unitX) (direction (locb .-. origin)) 
        locb = case findLoc b newvp of
                Just l -> l
                Nothing -> error ("alignXaxis: second alignment vertex not found (Vertex " ++ show b ++ ")")

-- | alignments takes a list of vertex pairs for respective rotations of VPatches in the second list.
-- For a pair (a,b) the Vpatch is centered on a then b is aligned along the positive x axis. 
-- The vertex pair list can be shorter than the list of vpatches - the remaining vpatches are left unrotated.
alignments :: [(Vertex, Vertex)] -> [VPatch] -> [VPatch]     
alignments [] vps = vps
alignments prs [] = error "alignments: Too many alignment pairs"  -- prs non-null
alignments ((a,b):more) (vp:vps) =  alignXaxis (a,b) vp : alignments more vps

-- | alignAll (a,b) vpList
-- provided both vertices a and b exist in each Vpatch in vpList, the VPatches are all aligned
-- centred on a, with b on the positive x axis
alignAll :: (Vertex, Vertex) -> [VPatch] -> [VPatch]     
alignAll (a,b) = fmap (alignXaxis (a,b))
    -- alignments ablist vps where ablist = take (length vps) (repeat (a,b))

-- rotations takes a list of integers (ttangles) for respective rotations of items in the second list (things to be rotated).
-- This includes Diagrams, Patches, VPatches
-- The integer list can be shorter than the list of items - the remaining items are left unrotated.
rotations :: (Transformable a, V a ~ V2, N a ~ Double) => [Int] -> [a] -> [a]
rotations (n:ns) (d:ds) = rotate (ttangle n) d: rotations ns ds
rotations [] ds = ds
rotations _  [] = error "rotations: too many integers"

-- scales takes a list of doubles for respective scalings of items in the second list (things to be scaled).
-- This includes Diagrams, Patches, VPatches
-- The list of doubles can be shorter than the list of items - the remaining items are left unscaled.
scales :: (Transformable a, V a ~ V2, N a ~ Double) => [Double] -> [a] -> [a]
scales (s:ss) (d:ds) = scale s d: scales ss ds
scales [] ds = ds
scales _  [] = error "scales: too many scalars"

{- ----------------------------------------
 Auxilliary definitions
------------------------------------------- -}

-- | find the face with lowest originV (and then lowest oppV) 
-- return that face first followed by remaining faces
-- Used by makeVPatch (and hence makePatch) 
chooseLowest fcs = face:(fcs\\[face]) where
    a = minimum (fmap originV fcs)
    aFs = filter ((a==) . originV) fcs
    b = minimum (fmap oppV aFs)
    face = case filter (((a,b)==) . joinOfTile) aFs of  -- should be find
           (face:_) -> face
           []       -> error "chooseLowest: empty graph?"

 
-- Apply a function to just the list of located hybrids in a VPatch (leaving located vertices untouched)
withHybs:: ([Located Hybrid]->[Located Hybrid]) -> VPatch -> VPatch
withHybs f (VPatch {lVertices = lvs,  lHybrids = lhs}) = VPatch {lVertices = lvs,  lHybrids = f lhs}


-- convert a Hybrid to a Piece, dropping the Vertex information
asPiece:: Hybrid -> Piece
asPiece = fmap vector  -- fmap of functor HalfTile

-- convert a Hybrid to a TileFace, dropping the Vector information
asFace:: Hybrid -> TileFace
asFace = fmap face  -- fmap of functor HalfTile

-- dropVertices removes vertex information from Hybrids and removes located vertex list
dropVertices:: VPatch -> Patch
dropVertices vp = fmap (mapLoc asPiece) (lHybrids vp)

-- dropVertices removes vertex information from Hybrids and removes located vertex list
dropVectors:: VPatch -> [TileFace]
dropVectors vp = fmap (asFace . unLoc) (lHybrids vp)
   


{------------------- NEEDS WORK FOR GENERAL TESTING ---------------------------}
-- displaying the boundary of a Tgraph in lime
showGBoundary :: Tgraph -> Diagram B
showGBoundary g =  (lc lime $ drawEdges vpMap bd) <> drawVPatch vp where
    vp = makeVPatch g
    vpMap = Map.fromList $ fmap viewLoc' (lVertices vp)
    bd = boundaryDedges g

drawEdges :: Mapping Vertex (Point V2 Double) -> [(Vertex,Vertex)] -> Diagram B
drawEdges vpMap [] = mempty
drawEdges vpMap (e:more) = drawEdge vpMap e <> drawEdges vpMap more
drawEdge vpMap (a,b) = case (Map.lookup a vpMap, Map.lookup b vpMap) of
                         (Just pa, Just pb) -> pa ~~ pb
                         _ -> error ("drawEdge: location not found for both vertices "++ show(a,b))

viewLoc' :: Located Vertex -> (Vertex, Point V2 Double)
viewLoc' lp = (v,p) where (p,v) = viewLoc lp
 
{- | viewBoundary is a testing tool to inspect the boundary vertex locations of some (intermediate) Boundary
-- (used in conjunction with stepForce to get an intermediate Boundary)
-- The boundary edges of a Boundary are shown in lime - using the Boundary positions of vertices
-- The graph is converted to a vp separately (so using a fresh calculation of positions)
-- Thus rotations may be needed to match up.
-- Use an empty list of integer rotations to see what rotations are needed to align the figures.
-}
viewBoundary :: [Int] -> Boundary -> Diagram B
viewBoundary rots bd =  lc lime bdryFig <> graphFig where 
    [bdryFig, graphFig] = fmap center $ rotations rots [drawEdges vpMap bdE, center $ drawVGraph g]
    g = recoverGraph bd
    vpMap = bvLocMap bd
    bdE = bDedges bd
