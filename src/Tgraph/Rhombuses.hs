{-|
Module      : Rhombuses
Description : Converting between Kites and Darts (P2 tiles) to Rhombuses (P3 tiles) and drawing
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module introduces Penrose\'s P3 tilings (narrow and wide rhombuses).
It includes HalfTile_P3, Piece_P3, Patch_P3, VPatch_P3, Tgraph_P3 to represent and draw
rhombuses plus conversion to and from Darts and Kites (the P2 tiles).
A class Drawable_P3 is introduced with instance Patch_P3, VPatch_P3, Tgraph_P3
as well as Patch, VPatch, Tgraph.
A class DrawableLabelled_P3 is introduced with instance VPatch_P3, Tgraph_P3
as well as VPatch, Tgraph.
and generalised drawing functions for drawing P3 tilings.
-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-} -- needed for Drawable_P3 Patch_P3
{-# LANGUAGE Strict                    #-}
-- {-# LANGUAGE TypeOperators             #-} -- needed for type equality constraints ~

module Tgraph.Rhombuses 
  ( 
  -- * Tgraph_P3, TileFace_P3
    Tgraph_P3()
  , TileFace_P3
  , decomposeToRhombuses
  , decomposeToTgraph
  , faces_P3
  , maxV_P3
  , vertexSet_P3
  , faceVSet_P3
  , faceVs_P3
  , faceVList_P3
  , originV_P3
  , oppV_P3
  , wingV_P3
  , join_P3
  , isWide
  , isNarrow

  -- * VPatch_P3
  , VPatch_P3(..)
  , makeVP_P3
  , dropLabels_P3
  , convertVPatch_P3
  , convertVPatch
  , deformVPatch_P3
  -- * HalfTile_P3
  , HalfTile_P3(..)
 -- , tileRepP3
  , joinVector_P3
  , drawnEdges_P3
  -- * Piece_P3s
  , Piece_P3
  -- * Converting (located) Pieces
  , decompPieceP2toP3
  , decompPieceP3toP2
  -- * Converting Patches
  , Patch_P3
  , decompPatchP2toP3
  , decompPatchP3toP2
  -- * Drawing Piece_P3s
  , drawPiece_P3
  , drawjPiece_P3
  , drawJPiece_P3
  , fillOnlyPiece_P3
  , fillOnlyPieceWN
  , fillPieceWN
   -- * Drawable_P3 Class
  , Drawable_P3(..)
  -- * Drawing functions producing P3 Rhombuses
  , draw_P3
  , drawj_P3
  , drawJ_P3
  , fillWN
  , fillNW
  -- * DrawableLabelled_P3 Class
  , DrawableLabelled_P3(..)
  -- * Adding labels to functions producing P3 Rhombuses
  , labelSize_P3
  , labelled_P3
  ) where

import Diagrams.Prelude
--import Diagrams.TwoD.Text (Text) -- now in CheckBackend

import CheckBackend
import HalfTile
import TileLib
import Tgraph.Prelude
import Tgraph.Decompose ( phiVMap )

import qualified Data.Map.Strict as Map ((!))
import qualified Data.IntMap.Strict as VMap (fromList,toList,lookup,filterWithKey,keys,map,keysSet)
import qualified Data.IntSet as IntSet(empty,insert,findMax,member,null,difference)
import Data.List ( (\\) )
-- |Tgraph_P3 is a P3 (Rhombus) version of Tgraph
newtype Tgraph_P3 = Tgraph_P3 [TileFace_P3] deriving Show

-- |TileFace_P3 is a P3 (half rhombus) analagous to TileFace
type TileFace_P3 = HalfTile_P3 (Vertex,Vertex,Vertex)

-- |Decompose a Tgraph into Tgraph_P3 (rhombuses)
-- The new vertex numbers are generated with phiVMap from Tgraph.Decompose
decomposeToRhombuses :: Tgraph -> Tgraph_P3
decomposeToRhombuses g = Tgraph_P3 rs where
   rs = concatMap convert (faces g)
   newVFor = phiVMap g --makeEVnewMap (maxV g) (map joinE $ filter isKite $ faces g)
 --  newVFor = makeEVnewMap (maxV g) (map joinE $ filter isKite $ faces g)
   convert (LK(a,b,c)) = [ RW (a,b,x), RN (c,x,b) ] where x = newVFor Map.! (a,c)
   convert (RK(a,b,c)) = [ LW (a,x,c), LN (b,c,x) ] where x = newVFor Map.! (a,b)
   convert (LD(a,b,c)) = [ RW(c,a,b) ]
   convert (RD(a,b,c)) = [ LW(b,c,a) ]

-- |decompose a Tgraph_P3 (rhombuses) to a Tgraph
decomposeToTgraph :: Tgraph_P3 -> Tgraph
decomposeToTgraph (Tgraph_P3 rs) = makeTgraph (concatMap split rs) where
  split (RN(a,b,c)) = [LK(c,a,b)]
  split (LN(a,b,c)) = [RK(b,c,a)]
  split (RW(a,b,c)) = [LD(a,x,c), RK(b,c,x)] where x = (Map.!) newVFor (a,b)
  split (LW(a,b,c)) = [RD(a,b,x), LK(c,x,b)] where x = (Map.!) newVFor (a,c)
  newVFor = makeEVnewMap v (map join_P3 (filter isWide rs))
  v = maxV_P3 rs

-- |Get the TileFace_P3 list from a Tgraph_P3
faces_P3 :: Tgraph_P3 -> [TileFace_P3]
faces_P3 (Tgraph_P3 rs) = rs

-- | max vertex in a list of TileFace_P3 (0 for the empty list)
maxV_P3 :: [TileFace_P3] -> Int
maxV_P3 [] = 0
maxV_P3 rs = IntSet.findMax $ vertexSet_P3 rs

-- | set of vertices in a list of TileFace_P3
vertexSet_P3 :: [TileFace_P3] -> VertexSet
vertexSet_P3 = mconcat . map faceVSet_P3

-- | set of vertices in a TileFace_P3
faceVSet_P3 :: TileFace_P3 -> VertexSet
faceVSet_P3 r = IntSet.insert a $ IntSet.insert b $ IntSet.insert c IntSet.empty
             where (a,b,c) = faceVs_P3 r 

-- | triple of vertices in a TileFace_P3 (clockwise from origin)          
faceVs_P3 :: TileFace_P3 -> (Vertex, Vertex, Vertex)
faceVs_P3 (LW tr) =  tr
faceVs_P3 (RW tr) =  tr
faceVs_P3 (LN tr) =  tr
faceVs_P3 (RN tr) =  tr

-- | list of 3 vertices in a TileFace_P3 (clockwise from origin)
faceVList_P3 :: TileFace_P3 -> [Vertex]
faceVList_P3 r = [a,b,c] where (a,b,c) = faceVs_P3 r

-- | origin vertex of a TileFace_P3
originV_P3 :: TileFace_P3 -> Vertex
originV_P3 r = a where (a,_,_) = faceVs_P3 r

-- | opp vertex of a TileFace_P3
oppV_P3 :: TileFace_P3 -> Vertex
oppV_P3 (LW(_,_,c)) = c
oppV_P3 (RW(_,b,_)) = b
oppV_P3 (LN(_,_,c)) = c
oppV_P3 (RN(_,b,_)) = b

-- | wing vertex of a TileFace_P3
wingV_P3 :: TileFace_P3 -> Vertex
wingV_P3 (LW(_,b,_)) = b
wingV_P3 (RW(_,_,c)) = c
wingV_P3 (LN(_,b,_)) = b
wingV_P3 (RN(_,_,c)) = c

-- | join edge of a TileFace_P3 (clockwise = toward origin for LW and LN, away for RW and RN)
join_P3 :: TileFace_P3 -> Dedge
join_P3 (LW(a,_,c)) = (c,a)
join_P3 (RW(a,b,_)) = (a,b)
join_P3 (LN(a,_,c)) = (c,a)
join_P3 (RN(a,b,_)) = (a,b)

-- | Is the half rhombus a wide one (LW,RW)?
isWide :: TileFace_P3 -> Bool
isWide (LW _) = True
isWide (RW _) = True
isWide _      = False

-- | Is the half rhombus a narrow one (LN,RN)?
isNarrow :: TileFace_P3 -> Bool
isNarrow = not . isWide

-- |VPatch_P3 is a version of VPatch for TileFace_P3s (vertex locations are the same - VertexLocMap)
data VPatch_P3 = VPatch_P3 {vLocs_P3 :: VertexLocMap, vpFaces_P3 :: [TileFace_P3]} deriving Show

-- |needed for making VPatch_P3 transformable
type instance V VPatch_P3 = V2
-- |needed for making VPatch_P3 transformable
type instance N VPatch_P3 = Double

-- |A VPatch_P3 can be transformed.
instance Transformable VPatch_P3 where
    transform :: Transformation V2 Double -> VPatch_P3 -> VPatch_P3
    transform t vp3 = vp3 {vLocs_P3 = VMap.map (transform t) (vLocs_P3 vp3)}

-- | A VPatch_P3 can be deformed
instance Deformable VPatch_P3 VPatch_P3 where
   deform :: Deformation V2 V2 Double -> VPatch_P3 -> VPatch_P3
   deform d vp = vp{vLocs_P3 = VMap.map (deform d) (vLocs_P3 vp)}

   deform' :: Double -> Deformation V2 V2 Double -> VPatch_P3 -> VPatch_P3
   deform' r d vp = vp{vLocs_P3 = VMap.map (deform' r d) (vLocs_P3 vp)}

-- |Create a VPatch_P3 from a Tgraph_P3 (uses decomposeToTgraph)
makeVP_P3 :: Tgraph_P3 -> VPatch_P3
makeVP_P3 p@(Tgraph_P3 rs) = VPatch_P3 {vLocs_P3 = vlocsP3 , vpFaces_P3  = rs} where
    vlocs = locateGraphVertices (decomposeToTgraph p)
    vs = vertexSet_P3 rs
    vlocsP3 = VMap.filterWithKey (\ v _ -> v `IntSet.member` vs) vlocs

-- | Convert a VPatch_P3 to a P3_Patch (by dropping labels and calculating vectors)
dropLabels_P3 :: VPatch_P3 -> Patch_P3
dropLabels_P3 vp = map convert (vpFaces_P3 vp) where
   locations = vLocs_P3 vp
   convert face = case (VMap.lookup (originV_P3 face) locations , VMap.lookup (wingV_P3 face) locations, VMap.lookup (oppV_P3 face) locations) of 
    (Just p, Just p', Just p'') -> fmap (const [p' .-. p, p'' .-. p']) face `at` p -- using HalfTile functor fmap
    _ -> error $ "dropLabelsP3': Vertex location not found for some vertices:\n    "
                 ++ show (faceVList_P3 face \\ VMap.keys locations)  ++ "\n"

-- | Convert a VPatch to a VPatch_P3 by substituting P3 faces.
-- A check is made to ensure vertices in the P3 faces are keys in the location map,
-- raising an error if any are missing.
convertVPatch :: [TileFace_P3] -> VPatch -> VPatch_P3
convertVPatch rs vp
      | IntSet.null diffSet = VPatch_P3 {vLocs_P3 = vlocsP3, vpFaces_P3 = rs}
      | otherwise = error $ "convertVPatch: missing locations for: "
                             ++ show diffSet ++ "\n"
  where
    vs = vertexSet_P3 rs
    vlocs = vLocs vp
    source = VMap.keysSet vlocs
    diffSet = IntSet.difference vs source
    vlocsP3 = VMap.filterWithKey (\ v _ -> v `IntSet.member` vs) vlocs

-- | Convert a VPatch_P3 to a VPatch by substituting (dart,kite) faces.
-- A check is made to ensure vertices in the faces are keys in the location map,
-- raising an error if any are missing.
convertVPatch_P3 :: [TileFace] -> VPatch_P3 -> VPatch
convertVPatch_P3 fcs vp3
      | IntSet.null diffSet = VPatch {vLocs = vlocs, vpFaces = fcs}
      | otherwise = error $ "convertVPatch: missing locations for: "
                             ++ show diffSet ++ "\n"
  where
    vs = vertexSet fcs
    vlocsP3 = vLocs_P3 vp3
    source = VMap.keysSet vlocsP3
    diffSet = IntSet.difference vs source
    vlocs = VMap.filterWithKey (\ v _ -> v `IntSet.member` vs) vlocsP3

deformVPatch_P3 :: Deformation V2 V2 Double -> VPatch_P3 -> VPatch_P3
deformVPatch_P3 = deform
-- | Penrose P3 Tiling uses wide and narrow rhombuses
-- These are split into half tiles (triangles) as with kites and darts
data HalfTile_P3 a
   = LW a -- ^ Left Wide Rhombus
   | RW a -- ^ Right Wide Rhombus
   | LN a -- ^ Left Narrow Rhombus
   | RN a -- ^ Right Narrow Rhombus
   deriving (Show,Eq)

{-# INLINE tileRepP3 #-}
-- | tileRepP3 produces the representation without the label (LW,RW,LN,RN)
tileRepP3 :: HalfTile_P3 a -> a
tileRepP3 (LW a) = a
tileRepP3 (RW a) = a
tileRepP3 (LN a) = a
tileRepP3 (RN a) = a

-- |Needed for Transformable instance of HalfTile_P3 - requires TypeFamilies
type instance N (HalfTile_P3 a) = N a
-- |Needed for Transformable instance of HalfTile_P3 - requires TypeFamilies
type instance V (HalfTile_P3 a) = V a
-- |HalfTile_P3 inherits Transformable  - Requires FlexibleInstances
instance Transformable a => Transformable (HalfTile_P3 a) where
    transform t = fmap (transform t)

-- |A Piece_P3 is a HalfTile_P3 with a list of two vectors for the drawn edges.
-- The join vector for a wide rhombus is on the long diagonal,
-- and the join vector for a narrow rhombus is along the short diagonal.
-- The choice of which vertex is the origin is derived from conversions
-- from Darts and Kites (P2 tilings) 
type Piece_P3 = HalfTile_P3 [V2 Double]

-- |The drawn edges of a Piece_P3 excluding the join edge (as a list of vectors)
-- starting from the origin.
drawnEdges_P3 :: Piece_P3 -> [V2 Double]
drawnEdges_P3 = tileRepP3

-- |The join edge vector of a Piece_P3 (from origin)
joinVector_P3 :: Piece_P3 -> V2 Double
joinVector_P3 = sumV . drawnEdges_P3

-- |Make HalfTile_P3 a Functor
instance Functor HalfTile_P3 where
    fmap f (LW rep) = LW (f rep)
    fmap f (RW rep) = RW (f rep)
    fmap f (LN rep) = LN (f rep)
    fmap f (RN rep) = RN (f rep)

-- |Converting from P2 to P3 tilings.
-- Half darts become half wide rhombuses (LD->RW,RD->LW).
-- The rhombus origin is the dart wing, the rhombus opp is the dart origin, so
-- the rhombus join is the dart long edge.
-- Half kites are decomposed to a half wide and a half narrow rhombus
-- (LK -> RW and RN, RK -> LW and LN).
-- For wide rhombuses, the origin is the kite origin and rhombus opp is the kite wing
-- (the join is the kite long edge).
-- For narrow rhombuses, the origin is the kite opp and the rhombus wing is the kite opp
-- (the join is toward the kite origin.)
decompPieceP2toP3 :: Located Piece -> [Located Piece_P3]
decompPieceP2toP3 lp = case viewLoc lp of
    (p, LK [z1,z2]) -> [ RW [s1, s2] `at` p
                       , RN [negate z2, negate s2] `at` p.+^j
                       ]
        where j = sumV [z1,z2]
              s1 = (phi-1) *^ j
              s2 = z1 ^-^ s1
    (p, RK [z1,z2]) -> [ LW [s1, s2] `at` p
                       , LN [negate z2, negate s2] `at` p.+^j
                       ]
        where j = sumV [z1,z2]
              s1 = (phi-1) *^ j
              s2 = z1 ^-^ s1
    (p, LD [z1,z2]) -> [ RW [z2,s1] `at` p.+^ z1]
        where s1 = negate (sumV [z1,z2])
    (p, RD [z1,z2]) -> [ LW [z2,s1] `at` p.+^ z1]
        where s1 = negate (sumV [z1,z2])
    other -> error $ "decompPieceP2toP3: " ++ show other ++ "/n"

-- |Converting located pieces from P3 to P2 tilings.
-- Half narrow rhombuses become half kites (LN->RK,RN->LK).
-- The rhombus wing becomes the kite origin and the rhombus origin becomes the kite wing.
-- Half wide rhombuses are decomposed to a half dart and a half kite
-- (RW -> LD and RK, LW -> RD and LK).
-- The wide rhombus origin becomes the dart origin, and the wide rhombus opp becomes the kite origin.
-- The wide rhombus wing becomes both the kite opp and dart wing.
decompPieceP3toP2 :: Located Piece_P3 -> [Located Piece]
decompPieceP3toP2 lp = case viewLoc lp of
    (p, LW [z1,z2]) -> 
        [ RD [z1,s] `at` p
        , LK [ (1-phi)*^j, negate s] `at` (p.+^j)
        ] where j = sumV [z1,z2]
                s = (2-phi) *^ j ^-^ z1
    (p, RW [z1,z2]) -> 
        [ LD [z1,s] `at` p
        , RK [ (1-phi)*^j, negate s] `at` (p.+^j)
        ] where j  = sumV [z1,z2]
                s = (2-phi) *^ j ^-^ z1
    (p, LN [z1,z2]) -> [ RK [negate z1, sumV [z1,z2]] `at` p .+^ z1]
    (p, RN [z1,z2]) -> [ LK [negate z1, sumV [z1,z2]] `at` p .+^ z1]
    other -> error $ "decompPieceP3toP2: " ++ show other ++ "/n"


-- | a Patch_P3 is analagous to a Patch (but for for Piece_P3s)
type Patch_P3 =  [Located Piece_P3]

-- |Conversion from a Patch to a Patch_P3 (Kites and Darts to Rhombuses)
decompPatchP2toP3 :: Patch -> Patch_P3
decompPatchP2toP3 = concatMap decompPieceP2toP3

-- |Conversion from a Patch_P3 to a Patch (Rhombuses to Kites and Darts)
-- Note this does not reverse decompPatchP2toP3, but the combination
-- decompPatchP3toP2 . decompPatchP2toP3 is equivalent to a decompose operation (decompPatch)
decompPatchP3toP2 :: Patch_P3 -> Patch
decompPatchP3toP2 = concatMap decompPieceP3toP2


-- |Draws the two drawn edges of a Piece_P3
drawPiece_P3 :: OKBackend b => Piece_P3 -> Diagram b
drawPiece_P3 = strokeLine . fromOffsets . drawnEdges_P3


-- |Draw dashed join only of a Piece_P3. 
dashJOnly_P3 :: OKBackend b => Piece_P3 -> Diagram b
dashJOnly_P3 p = joinDashing (strokeLine $ fromOffsets [joinVector_P3 p])

-- |Draws all edges of a Piece_P3 using a faint dashed line for the join edge
-- (J for plain dashed Join, j for faint dashed join)
drawjPiece_P3 :: OKBackend b => Piece_P3 -> Diagram b
drawjPiece_P3 = drawPiece_P3 <> lw ultraThin . dashJOnly_P3

-- |Draws all edges of a Piece_P3 using a dashed line for the join edge
-- (J for plain dashed Join, j for faint dashed join)
drawJPiece_P3 :: OKBackend b => Piece_P3 -> Diagram b
drawJPiece_P3 = drawPiece_P3 <> dashJOnly_P3

-- |Fills a Piece_P3 with a colour (without drawn edges)
fillOnlyPiece_P3 :: (OKBackend b, Color c) =>
                   c -> Piece_P3 -> Diagram b
fillOnlyPiece_P3 c p = 
    lw none $ fillColor c $ 
    strokeLoop $ closeLine $ fromOffsets $ drawnEdges_P3 p

-- |Fills a Piece_P3 with one of 2 colours (but no drawn edges).
-- The first colour is used for wide rhombuses, and the second for narrow rhombuses.
-- (Note the order WN)
fillOnlyPieceWN :: (OKBackend b, Color cw, Color cn) =>
               cw -> cn -> Piece_P3 -> Diagram b
fillOnlyPieceWN cw cn rp = filledpiece where
    filledpiece = case rp of
        (LW _ ) -> fillOnlyPiece_P3 cw rp
        (RW _ ) -> fillOnlyPiece_P3 cw rp
        _       -> fillOnlyPiece_P3 cn rp

-- |Fills and draws a Piece_P3 with one of 2 colours
-- The first colour is used for wide rhombuses, and the second for narrow rhombuses.
-- (Note the order WN)
fillPieceWN :: (OKBackend b, Color cw, Color cn) =>
               cw -> cn -> Piece_P3 -> Diagram b
fillPieceWN cw cn = drawPiece_P3 <> fillOnlyPieceWN cw cn


-- | A class for things that can be turned to diagrams when given a function to draw Piece_P3s.
class Drawable_P3 a where
  drawWith_P3 :: OKBackend b =>
                (Piece_P3 ->  Diagram b) -> a -> Diagram b

-- | Tgraph_P3 can be drawn (as faces_P3)
instance Drawable_P3 Tgraph_P3 where
  -- | turn a Tgraph_P3 into a diagram given a function for drawing P3_Pieces.
  drawWith_P3 :: OKBackend b => (Piece_P3 -> Diagram b) -> Tgraph_P3 -> Diagram b
  drawWith_P3 pd = drawWith_P3 pd . makeVP_P3

-- | VPatch_P3 can be drawn (as rhombuses)
instance Drawable_P3 VPatch_P3 where
  -- | turn a VPatch_P3 into a diagram given a function for drawing P3_Pieces.
  drawWith_P3 :: OKBackend b => (Piece_P3 -> Diagram b) -> VPatch_P3 -> Diagram b
  drawWith_P3 pd = drawWith_P3 pd . dropLabels_P3

-- | A Patch_P3 is Drawable_P3.
instance Drawable_P3 Patch_P3 where
  -- | turn a Patch_P3 into a diagram given a function for drawing Piece_P3s.
  drawWith_P3 :: OKBackend b => (Piece_P3 -> Diagram b) -> Patch_P3 -> Diagram b
  drawWith_P3 pd = position . fmap (viewLoc . mapLoc pd)

-- | A Tgraph is Drawable_P3.
instance Drawable_P3 Tgraph where
    drawWith_P3 :: OKBackend b => (Piece_P3 -> Diagram b) -> Tgraph -> Diagram b
    drawWith_P3 pd = drawWith_P3 pd . makeVP

-- | A VPatch is Drawable_P3.
instance Drawable_P3 VPatch where
    drawWith_P3 :: OKBackend b => (Piece_P3 -> Diagram b) -> VPatch -> Diagram b
    drawWith_P3 pd = drawWith_P3 pd . dropLabels

-- | A Patch is also Drawable_P3 (by conversion to a Patch_P3).
instance Drawable_P3 Patch where
  drawWith_P3 :: OKBackend b => (Piece_P3 -> Diagram b) -> Patch -> Diagram b
  drawWith_P3 pd = drawWith_P3 pd . decompPatchP2toP3

-- |The main drawing function for anything Drawable_P3
draw_P3 :: (OKBackend b, Drawable_P3 a) => 
          a -> Diagram b
draw_P3 = drawWith_P3 drawPiece_P3

-- |An alternative drawing function for anything Drawable_P3 adding faint dashed lines for join edges
-- (J for plain dashed Join, j for faint dashed join)
drawj_P3 :: (OKBackend b, Drawable_P3 a) => 
          a -> Diagram b
drawj_P3 = drawWith_P3 drawjPiece_P3

-- |An alternative drawing function for anything Drawable_P3 adding dashed lines for join edges
-- (J for plain dashed Join, j for faint dashed join)
drawJ_P3 :: (OKBackend b, Drawable_P3 a) => 
          a -> Diagram b
drawJ_P3 = drawWith_P3 drawJPiece_P3

-- |The main draw and fill function for anything Drawable_P3.
-- The first colour is used for wide rhombuses, and the second for narrow rhombuses.
-- (Note the order W N).
fillWN :: (OKBackend b, Drawable_P3 a, Color cw, Color cn) =>
          cw -> cn -> a -> Diagram b
fillWN cw cn = drawWith_P3 (fillPieceWN cw cn)

-- |A variation on fillWN where
-- the first colour is for narrow rhombuses, the second for wide rhombuses.
-- (Note the order N W).
fillNW :: (OKBackend b, Drawable_P3 a, Color cw, Color cn) =>
          cw -> cn -> a -> Diagram b
fillNW = flip fillWN --drawWith_P3 (fillPieceWN cw cn)


-- | A class for things that can be drawn (P3 style) with labels when given a colour and a measure (size) for the labels and a 
-- a draw function (for Patch_P3es).
-- So labelColourSize_P3 c m  modifies a Patch_P3 drawing function to add labels (of colour c and size measure m).
-- Measures are defined in Diagrams. In particular: tiny, verySmall, small, normal, large, veryLarge, huge.
class DrawableLabelled_P3 a where
   labelColourSize_P3 :: OKBackend b => 
                        Colour Double -> Measure Double -> (Patch_P3 -> Diagram b) -> a -> Diagram b

-- | Tgraph_P3 can be drawn with labels (as faces_P3)
instance DrawableLabelled_P3 Tgraph_P3 where
  labelColourSize_P3 c m d = labelColourSize_P3 c m d . makeVP_P3

-- | VPatch_P3 can be drawn with labels (as rhombuses)
instance DrawableLabelled_P3 VPatch_P3 where
  labelColourSize_P3 c m d vp3 = drawLabels <> d (dropLabels_P3 vp3) where
     drawLabels = position $ drawlabel <$> VMap.toList (vLocs_P3 vp3)
     drawlabel(v,p) = (p, baselineText (show v) # fontSize m # fc c)

-- | Tgraphs can be drawn (Rhombus/P3 style) with labels
-- Thus using such a vertex for alignment will raise an error.
instance DrawableLabelled_P3 Tgraph where
     labelColourSize_P3 c m d = labelColourSize_P3 c m d . decomposeToRhombuses --makeVP

-- | VPatches can be drawn (Rhombus/P3 style) with labels
-- NB: the additional vertices for P3 are only added when drawing and are not part of the VPatch.
-- Thus using such a vertex for alignment will raise an error.
instance DrawableLabelled_P3 VPatch where
     labelColourSize_P3 c m d vp = drawLabels <> d p3Patch where
        p3Patch = decompPatchP2toP3 $ dropLabels vp
        drawLabels = position $ drawlabel <$> VMap.toList (extendLocs_P3 vp)
        drawlabel(v,p) = (p, baselineText (show v) # fontSize m # fc c)

-- | (Not exported) Extend the vertex locations of a VPatch with locations for new kite join vertices
-- appearing in (Rhombus/P3 style) drawing of tiles.
-- The new vertex numbers are generated with phiVMap from Tgraph.Decompose
extendLocs_P3 :: VPatch -> VertexLocMap
extendLocs_P3 vp = locmap <> VMap.fromList (map (locateNew . joinOfTile) (kites vp)) where
    locmap = vLocs vp
    newemap = phiVMap vp
    locateNew (a,b) = case (VMap.lookup a locmap, VMap.lookup b locmap) of
        (Just pa, Just pb) -> (newemap Map.! (a,b), pa .+^ (phi-1) *^ (pb .-. pa))
        _ -> error "extendLocs_P3: Missing location for a kite join"


-- | Default Version of labelColourSize_P3 with colour red.
-- Example usage: labelSize_P3 tiny draw_P3 a , labelSize_P3 normal drawj_P3 a
labelSize_P3 :: (OKBackend b, DrawableLabelled_P3 a) =>
               Measure Double -> (Patch_P3 -> Diagram b) -> a -> Diagram b
labelSize_P3 = labelColourSize_P3 red

-- | Default Version of labelColourSize_P3 using red and small (rather than normal label size).
-- Example usage: labelled_P3 draw_P3 a , labelled_P3 drawj_P3 a
labelled_P3 :: (OKBackend b, DrawableLabelled_P3 a) =>
            (Patch_P3 -> Diagram b) -> a -> Diagram b
labelled_P3 = labelColourSize_P3 red small --(normalized 0.023)
