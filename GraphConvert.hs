{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module GraphConvert where

import HalfTile
import TileLib
import Tgraphs

import Data.List ((\\), lookup, find)
import Data.Maybe (mapMaybe)

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

{-
Important Changes to make VPatches transformable and therefore can use scale rotate translate
-}

-- a DualRep is essential a pair of representations - a vector and a face(= 3 vertices)
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


{- | makeVPatch and makePatch - Converting a graph to a vpatch / patch
First, buildVEAssocs (from faces)
Starting with a chosen initiial face (face) and the rest (more) - give face zero offset and an orientation
(originally just used first face and oriented join on x-axis, but
now using face with lowest numbered origin and orienting (join edge with lowest oppV) along x-axis)

buildVEAssocs:
Create initial [face] as list of faces to be processed, with the head (face) as current face and
       more as list of 'unencountered' faces
       Start record of assocV and assocE (using first edge vector - both directions and offset vector for origin of face)
     assocV is an association list of (vertex, point)
     assocE is an association list of (directed edge, vector)
Step: For current face (processFace face)
    assign vectors (in each direction) for all 3 edges (not already assigned), and points for the 3 vertices (not already assigned)
    For each edge of current face (clockwise) find other faces in 'unencountered' sharing the edge (reversed)
    Transfer these to the end of list of faces to be processed - and removing from unencountered,
    Remove current face from faces to be processed
When no more faces to be processed  (unencountered should also be empty for edge connected graph) - return both the assoc lists

Second: convert original faces
Extract the join vector and originV location for each of the graph faces from the two assoc lists
to build a vpatch with located hybrids and convert assocV to located vertices.
-}
makeVPatch::Tgraph -> VPatch
makeVPatch g = if nullGraph g 
               then VPatch { lVertices = [], lHybrids = [] }
               else VPatch { lVertices = fmap locateV assocV
                           , lHybrids  = fmap makeLHyb $ faces g
                           }
    where
    (face:more) = chooseLowest (faces g)
    (assocV,assocE) = buildVEAssocs [face] more [(originV face,origin)] (initJvec face)
    locateV (v,p) = v `at` p
    makeLHyb fc = case (lookup (originV fc) assocV , lookup (joinOfTile fc) assocE) of
                  (Just p, Just vec) -> fmap (dualRep vec) fc `at` p -- using HalfTile functor fmap
                  _ -> error ("makeVPatch: " ++ show fc)

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
touchingVertices:: Tgraph -> [(Vertex,Vertex)]
touchingVertices g = [(v1,v2) | v1 <- vertices g, v2 <- vertices g \\[v1], tooClose v1 v2] where
    tooClose v1 v2 = dist v1 v2 < 0.5
    dist v1 v2 = lengthVec (locPoint v1 .-. locPoint v2)
    lengthVec vec = sqrt $ dot vec vec
    locPoint v = p where Just p = lookup v assocVP
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
    matchingF fc lhyb = dropVectors (unLoc lhyb) == fc

selectFacesVP:: [TileFace] -> VPatch -> VPatch
selectFacesVP fcs vp = withHybs (findAll fcs) vp where
    findAll fcs lfaces = mapMaybe (findIn lfaces) fcs 
    findIn lfaces fc = find (matchingF fc) lfaces
    matchingF fc lhyb = dropVectors (unLoc lhyb) == fc

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
                Nothing -> error ("alignX: second alignment vertex not found (Vertex " ++ show b ++ ")")

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

-- | find the face with lowest originV (and then lowest oppV) - return that face first along with other faces
-- used be makeVPatch (and hence makePatch) 
chooseLowest fcs = face:(fcs\\[face]) where
    a = minimum (fmap originV fcs)
    aFs = filter ((a==) . originV) fcs
    b = minimum (fmap oppV aFs)
    face = case filter (((a,b)==) . joinOfTile) aFs of
           (face:_) -> face
           []       -> error "chooseLowest: empty graph?"

-- | initial join edge vectors (2) for starting face join on x axis - used to initialise buildVEAssocs
initJvec::TileFace -> [((Vertex,Vertex), V2 Double)]                  
initJvec (LD(a,b,_)) = [((a,b), unitX), ((b,a), unit_X)]
initJvec (RD(a,_,c)) = [((a,c), unitX), ((c,a), unit_X)]
initJvec (LK(a,_,c)) = [((a,c), phi*^unitX), ((c,a), phi*^unit_X)]
initJvec (RK(a,b,_)) = [((a,b), phi*^unitX), ((b,a), phi*^unit_X)]

{- | buildVEAssocs: process faces to associate vectors for each directed edge and points for each vertex.
The first argument list of faces contains the ones being processed next in order where
each will have at least one known edge vector and at least one known vertex point.
The second argument list of faces have not yet been added and may not yet have a known edge.
The third argument is the association list for (vertices,points).
The fourth argument is the association list for (edges,vectors).
-}
buildVEAssocs [] [] assocV assocE = (assocV, assocE) 
buildVEAssocs [] fcOther assocV assocE = error ("buildVEAssocs: Faces not face-edge-connected " ++ show fcOther)
buildVEAssocs (fc:fcs) fcOther assocV assocE = buildVEAssocs (fcs++fcs') fcOther' assocV' assocE' where
  (assocV', assocE') = processFace fc assocV assocE
  (fcs', fcOther')   = edgeNbs fc fcOther

-- | process a face to get updated association lists
processFace face assocV assocE = (assocV',assocE') where
    evecs   = completeE face assocE
    assocE' = add3E (faceDedges face) evecs assocE
    assocV' = completeV (faceVs face) evecs assocV

-- | for a given face, find edge neighbouring faces in the supplied list of faces
-- returns a pair - the list of the ones found followed by the supplied list with these removed
edgeNbs::TileFace -> [TileFace] -> ([TileFace],[TileFace])
edgeNbs fc fcOther = (fcnbs, fcOther') where
      fcnbs = filter sharedEdge fcOther
      fcOther' = fcOther \\ fcnbs
      sharedEdge fc' = any (\e -> e `elem` fmap reverseE (faceDedges fc)) (faceDedges fc')

-- | calculate points for 3 vertices given 3 edge vectors and assocV
-- needs at least 1 already in assocV and adds new ones to assocV
-- returning new assocV
completeV (a,b,c) (ev1,ev2,ev3) assocV = 
    case (lookup a assocV , lookup b assocV ,lookup c assocV) of
        (Just p, _, _) -> addPair (b,p.+^ev1) $ addPair (c,p.-^ev3) assocV
        (Nothing, Just p, _) -> addPair (a,p .-^ ev1) $ addPair (c,p.-^ev2) assocV
        (Nothing, Nothing, Just p) -> addPair (a,p.+^ev3) $ addPair (b,p.-^ev2) assocV
        _ -> error ("completeV: no locations for " ++ show (a,b,c))

-- | find 3 maybe edge vectors in assocE
find3E [e1,e2,e3] assocE = (lookup e1 assocE, lookup e2 assocE, lookup e3 assocE)

-- | construct 3 vectors clockwise round face 
-- needs at least one already in assocE 
completeE:: TileFace -> [((Vertex,Vertex), V2 Double)] -> (V2 Double,V2 Double,V2 Double)        
completeE fc@(LD _) assocE = case find3E (faceDedges fc) assocE of
     (Just v1, _ , _)            -> (v1,        v ^-^ v1, negated v)          where v = phi*^rotate (ttangle 9) v1
     (Nothing, Just v2, _)       -> (v,         v2,       negated (v ^+^ v2)) where v = rotate (ttangle 2) v2
     (Nothing, Nothing, Just v3) -> (negated v, v ^-^ v3, v3)                 where v = (phi-1)*^rotate (ttangle 1) v3
     _ -> error ("completeE: face not face-edge-connected: " ++ show fc)
completeE fc@(RD _) assocE = case find3E (faceDedges fc) assocE of
     (Just v1, _ , _)            -> (v1,        v ^-^ v1, negated v)          where v = (phi-1)*^rotate (ttangle 9) v1
     (Nothing, Just v2, _)       -> (v,         v2,       negated (v ^+^ v2)) where v = phi*^rotate (ttangle 4) v2
     (Nothing, Nothing, Just v3) -> (negated v, v ^-^ v3, v3)                 where v = phi*^rotate (ttangle 1) v3
     _ -> error ("completeE: face not face-edge-connected: " ++ show fc) 
completeE fc@(LK _) assocE = case find3E (faceDedges fc) assocE of
     (Just v1, _ , _)            -> (v1,        v ^-^ v1, negated v)          where v = rotate (ttangle 9) v1
     (Nothing, Just v2, _)       -> (v,         v2,       negated (v ^+^ v2)) where v = phi*^rotate (ttangle 3) v2
     (Nothing, Nothing, Just v3) -> (negated v, v ^-^ v3, v3)                 where v = rotate (ttangle 1) v3
     _ -> error ("completeE: face not face-edge-connected: " ++ show fc) 
completeE fc@(RK _) assocE = case find3E (faceDedges fc) assocE of  -- same as LK
     (Just v1, _ , _)            -> (v1,        v ^-^ v1, negated v)          where v = rotate (ttangle 9) v1
     (Nothing, Just v2, _)       -> (v,         v2,       negated (v ^+^ v2)) where v = phi*^rotate (ttangle 3) v2
     (Nothing, Nothing, Just v3) -> (negated v, v ^-^ v3, v3)                 where v = rotate (ttangle 1) v3
     _ -> error ("completeE: face not face-edge-connected: " ++ show fc)

-- | adds a missing key/value pair, but will not change existing ones in assoc
addPair (k,v) assoc  = case lookup k assoc of 
    Nothing -> (k,v):assoc
    _       -> assoc

-- | add3E - given 3 directed edges and 3 vectors it will add
-- to assocE 6 vectors for the 3 edges using both directions - adds only missing ones
add3E [e1,e2,e3] (v1,v2,v3) assocE = 
    foldr addPair assocE [(e1,v1),(e2,v2),(e3,v3)
                         ,(reverseE e1, negated v1),(reverseE e2, negated v2),(reverseE e3, negated v3)
                         ]

-- Apply a function to just the list of located hybrids in a VPatch (leaving located vertices untouched)
withHybs:: ([Located Hybrid]->[Located Hybrid]) -> VPatch -> VPatch
withHybs f (VPatch {lVertices = lvs,  lHybrids = lhs}) = VPatch {lVertices = lvs,  lHybrids = f lhs}

-- convert a Hybrid to a TileFace, dropping the vector information
dropVectors:: Hybrid -> TileFace
dropVectors = fmap face  -- fmap of functor HalfTile

-- convert a Hybrid to a Piece, dropping the Vertex information
asPiece:: Hybrid -> Piece
asPiece = fmap vector  -- fmap of functor HalfTile

-- dropVertices removes vertex information from Hybrids and removes located vertex list
dropVertices:: VPatch -> Patch
dropVertices vp = fmap (mapLoc asPiece) (lHybrids vp)

   


