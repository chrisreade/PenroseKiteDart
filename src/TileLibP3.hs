{-|
Module      : TileLibP3
Description : Converting between Kites and Darts (P2 tiles) to Rhombuses (P3 tiles) and drawing
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module introduces Penrose\'s P3 tilings (narrow and wide rhombuses).
It includes P3_HalfTiles, P3_Pieces and P3_Patches to represent and draw
rhombuses plus conversion to and from Darts and Kites (the P2 tiles).
A class P3_Drawable is introduced with instance P3_Patch, Patch, VPatch, Tgraph
and generalised drawing functions for drawing P3 tilings.
-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-} -- needed for P3_Drawable P3_Patch
{-# LANGUAGE Strict                    #-}
-- {-# LANGUAGE TypeOperators             #-} -- needed for type equality constraints ~

module TileLibP3 
  ( 
  -- * P3_HalfTiles
    P3_HalfTile(..)
  , tileRepP3
  -- * P3_Pieces
  , P3_Piece
  -- * Converting (located) Pieces
  , decompPieceP2toP3
  , decompPieceP3toP2
  -- * Converting Patches
  , P3_Patch
  , decompP2toP3
  , decompP3toP2
  -- * Drawing P3_Pieces
  --, drawnEdgesP3
  , drawPieceP3
  , drawjPieceP3
  , fillOnlyPieceP3
  , fillOnlyPieceWN
  , fillPieceWN
   -- * P3_Drawable Class
  , P3_Drawable(..)
  -- * Drawing functions producing P3 Rhombuses
  , drawP3
  , drawjP3
  , dashjP3
  , fillWN
  , fillNW
  -- * P3_DrawableLabelled Class
  , P3_DrawableLabelled(..)
  -- * Adding labels to functions producing P3 Rhombuses
  , labelSizeP3
  , labelledP3
  ) where

import Diagrams.Prelude
--import Diagrams.TwoD.Text (Text) -- now in CheckBackend

import CheckBackend
import HalfTile
import TileLib
import Tgraph.Prelude
import Tgraph.Decompose ( phiVMap )

import qualified Data.Map.Strict as Map ((!))
import qualified Data.IntMap.Strict as VMap (fromList, toList, lookup)

-- | Penrose P3 Tiling uses wide and narrow rhombuses
-- These are split into half tiles (triangles) as with kites and darts
data P3_HalfTile a
   = LW a -- ^ Left Wide Rhombus
   | RW a -- ^ Right Wide Rhombus
   | LN a -- ^ Left Narrow Rhombus
   | RN a -- ^ Right Narrow Rhombus
   deriving (Show,Eq)

{-# INLINE tileRepP3 #-}
-- | tileRepP3 produces the representation without the label (LW,RW,LN,RN)
tileRepP3 :: P3_HalfTile a -> a
tileRepP3 (LW a) = a
tileRepP3 (RW a) = a
tileRepP3 (LN a) = a
tileRepP3 (RN a) = a

-- |Needed for Transformable instance of P3_HalfTile - requires TypeFamilies
type instance N (P3_HalfTile a) = N a
-- |Needed for Transformable instance of P3_HalfTile - requires TypeFamilies
type instance V (P3_HalfTile a) = V a
-- |P3_HalfTile inherits Transformable  - Requires FlexibleInstances
instance Transformable a => Transformable (P3_HalfTile a) where
    transform t = fmap (transform t)

-- |A P3_Piece is a P3_Halftile with a vector along its join edge.
-- The vector for a wide rhombus is on the long diagonal,
-- and the vector for a narrow rhombus is along the short diagonal.
-- The choice of which vertex is the origin is derived from conversions
-- from Darts and Kites (P2 tilings) 
type P3_Piece = P3_HalfTile (V2 Double)

-- |Make P3_Halftile a Functor
instance Functor P3_HalfTile where
    fmap f (LW rep) = LW (f rep)
    fmap f (RW rep) = RW (f rep)
    fmap f (LN rep) = LN (f rep)
    fmap f (RN rep) = RN (f rep)

-- |Converting from P2 to P3 tilings.
-- Half darts become half wide rhombuses (LD->RW,RD->LW).
-- (The new origin is the dart wing, and 
-- the new join is the dart long edge.)
-- Half kites are decomposed to a half wide and a half narrow rhombus.
-- (For wide rhombuses, the new origin is the kite origin and the join is the kite long edge.)
-- (For narrow rhombuses, the new origin is the kite opp and the join is toward the kite origin.)
decompPieceP2toP3 :: Located Piece -> [Located P3_Piece]
decompPieceP2toP3 lp = case viewLoc lp of
    (p, LK v) -> [ RW z `at` p
                 , RN ((2-phi)*^ negate v) `at` p.+^v
                 ] where z = rotate (ttangle 1) v
    (p, RK v) -> [ LW z `at` p
                 , LN ((2-phi)*^ negate v) `at` p.+^v
                 ]  where z = rotate (ttangle 9) v
    (p, LD v) -> [ RW (negate z) `at` p.+^ z]
                 where z = phi *^ rotate (ttangle 9) v
    (p, RD v) -> [ LW (negate z) `at` p.+^ z]
                 where z = phi *^ rotate (ttangle 1) v

-- |Converting from P3 to P2 tilings.
-- Half narrow rhombuses become half kites
-- (but the origin vertex and join edge are changed).
-- Half wide rhombuses are decomposed to a half dart and a half kite.
decompPieceP3toP2 :: Located P3_Piece -> [Located Piece]
decompPieceP3toP2 lp = case viewLoc lp of
    (p, LW v) -> -- decompPiece (RD (z^-^v) `at` p.+^v)
                 -- where z = (phi-1)*^rotate (ttangle 1) v
                 [ RD ((2-phi)*^v) `at` p
                 , LK (z^-^v) `at` (p.+^v)
                 ] where z = (phi-1)*^rotate (ttangle 1) v
    (p, RW v) -> --decompPiece (LD (z^-^v) `at` p.+^v)
                 --where z = (phi-1)*^rotate (ttangle 9) v
                 [ LD ((2-phi)*^v) `at` p
                 , RK (z^-^v) `at` (p.+^v)
                 ] where z = (phi-1)*^rotate (ttangle 9) v

    (p, LN v) -> [ RK (v^-^z) `at` p.+^ z]
                 where z = phi *^ rotate (ttangle 2) v
    (p, RN v) -> [ LK (v^-^z) `at` p.+^ z]
                 where z = phi *^ rotate (ttangle 8) v

-- | a P3_Patch is analagous to a Patch (but for for P3_Pieces)
type P3_Patch =  [Located P3_Piece]

-- |Conversion from a Patch to a P3_Patch (Kites and Darts to Rhombuses)
decompP2toP3 :: Patch -> P3_Patch
decompP2toP3 = concatMap decompPieceP2toP3

-- |Conversion from a P3_Patch to a Patch (Rhombuses to Kites and Darts)
-- Note this does not reverse decompP2toP3, but the combination
-- decompP3toP2 . decompP2toP3 is equivalent to a decompose operation (decompPatch)
decompP3toP2 :: P3_Patch -> Patch
decompP3toP2 = concatMap decompPieceP3toP2

-- |The drawn edges of a P3_Piece excluding the join edge (as a list of vectors)
drawnEdgesP3 :: P3_Piece -> [V2 Double]
drawnEdgesP3 (LW v) = [z,v^-^z] where z = (phi-1)*^rotate (ttangle 1) v
drawnEdgesP3 (RW v) = [z,v^-^z] where z = (phi-1)*^rotate (ttangle 9) v
drawnEdgesP3 (LN v) = [z,v^-^z] where z = phi*^rotate (ttangle 2) v
drawnEdgesP3 (RN v) = [z,v^-^z] where z = phi*^rotate (ttangle 8) v


-- |Draws the two drawn edges of a P3_Piece
drawPieceP3 :: OKBackend b => P3_Piece -> Diagram b
drawPieceP3 = strokeLine . fromOffsets . drawnEdgesP3

-- |Draw dashed join only of a P3_Piece 
dashjOnlyP3 :: OKBackend b => P3_Piece -> Diagram b
dashjOnlyP3 p = joinDashing (strokeLine $ fromOffsets [tileRepP3 p])


-- |Draws all edges of a P3_Piece using a faint dashed line for the join edge
drawjPieceP3 :: OKBackend b => P3_Piece -> Diagram b
drawjPieceP3 = drawPieceP3 <> dashjOnlyP3

-- |Fills a P3_Piece with a colour (without drawn edges)
fillOnlyPieceP3 :: (OKBackend b, Color c) =>
                   c -> P3_Piece -> Diagram b
fillOnlyPieceP3 c p = 
    lw none $ fillColor c $ 
    strokeLoop $ closeLine $ fromOffsets $ drawnEdgesP3 p

-- |Fills a P3_Piece with one of 2 colours (but no drawn edges).
-- The first colour is used for wide rhombuses, and the second for narrow rhombuses.
-- (Note the order WN)
fillOnlyPieceWN :: (OKBackend b, Color cw, Color cn) =>
               cw -> cn -> P3_Piece -> Diagram b
fillOnlyPieceWN cw cn rp = filledpiece where
    filledpiece = case rp of
        (LW _ ) -> fillOnlyPieceP3 cw rp
        (RW _ ) -> fillOnlyPieceP3 cw rp
        _       -> fillOnlyPieceP3 cn rp

-- |Fills and draws a P3_Piece with one of 2 colours
-- The first colour is used for wide rhombuses, and the second for narrow rhombuses.
-- (Note the order WN)
fillPieceWN :: (OKBackend b, Color cw, Color cn) =>
               cw -> cn -> P3_Piece -> Diagram b
fillPieceWN cw cn = drawPieceP3 <> fillOnlyPieceWN cw cn

{- 
fillPieceWN cw cn rp = drawPieceP3 rp <> filledpiece where
    filledpiece = case rp of
        (LW _ ) -> fillOnlyPieceP3 cw rp
        (RW _ ) -> fillOnlyPieceP3 cw rp
        _       -> fillOnlyPieceP3 cn rp
-}

-- | A class for things that can be turned to diagrams when given a function to draw P3_Pieces.
class P3_Drawable a where
  drawP3With :: OKBackend b =>
                (P3_Piece ->  Diagram b) -> a -> Diagram b

-- | A P3_Patch is P3_Drawable.
instance P3_Drawable P3_Patch where
  -- | turn a P3_Patch into a diagram given a function for drawing P3_Pieces.
  drawP3With :: OKBackend b => (P3_Piece -> Diagram b) -> P3_Patch -> Diagram b
  drawP3With pd = position . fmap (viewLoc . mapLoc pd)

-- | A Patch is also P3_Drawable (by conversion to a P3_Patch).
instance P3_Drawable Patch where
  drawP3With :: OKBackend b => (P3_Piece -> Diagram b) -> Patch -> Diagram b
  drawP3With pd = drawP3With pd . decompP2toP3

-- | A VPatch is P3_Drawable.
instance P3_Drawable VPatch where
    drawP3With :: OKBackend b => (P3_Piece -> Diagram b) -> VPatch -> Diagram b
    drawP3With pd = drawP3With pd . dropLabels

-- | A Tgraph is P3_Drawable.
instance P3_Drawable Tgraph where
    drawP3With :: OKBackend b => (P3_Piece -> Diagram b) -> Tgraph -> Diagram b
    drawP3With pd = drawP3With pd . makeVP

-- |The main drawing function for anything P3_Drawable
drawP3 :: (OKBackend b, P3_Drawable a) => 
          a -> Diagram b
drawP3 = drawP3With drawPieceP3

-- |An alternative drawing function for anything P3_Drawable adding dashed lines for join edges
drawjP3 :: (OKBackend b, P3_Drawable a) => 
          a -> Diagram b
drawjP3 = drawP3With drawjPieceP3

{-# DEPRECATED dashjP3 "Replaced by drawjP3" #-}
-- |Deprecated (renamed as drawjP3)
dashjP3 :: (OKBackend b, P3_Drawable a) => 
          a -> Diagram b
dashjP3 = drawjP3

-- |The main draw and fill function for anything P3_Drawable.
-- The first colour is used for wide rhombuses, and the second for narrow rhombuses.
-- (Note the order W N).
fillWN :: (OKBackend b, P3_Drawable a, Color cw, Color cn) =>
          cw -> cn -> a -> Diagram b
fillWN cw cn = drawP3With (fillPieceWN cw cn)

-- |A variation on fillWN where
-- the first colour is for narrow rhombuses, the second for wide rhombuses.
-- (Note the order N W).
fillNW :: (OKBackend b, P3_Drawable a, Color cw, Color cn) =>
          cw -> cn -> a -> Diagram b
fillNW = flip fillWN --drawP3With (fillPieceWN cw cn)


-- | A class for things that can be drawn (P3 style) with labels when given a colour and a measure (size) for the labels and a 
-- a draw function (for P3_Patches).
-- So labelColourSizeP3 c m  modifies a P3_Patch drawing function to add labels (of colour c and size measure m).
-- Measures are defined in Diagrams. In particular: tiny, verySmall, small, normal, large, veryLarge, huge.
class P3_DrawableLabelled a where
   labelColourSizeP3 :: OKBackend b => 
                        Colour Double -> Measure Double -> (P3_Patch -> Diagram b) -> a -> Diagram b

-- | VPatches can be drawn (Rhombus/P3 style) with labels
-- NB: the additional vertices for P3 are only added when drawing and are not part of the VPatch.
-- Thus using such a vertex for alignment will raise an error.
instance P3_DrawableLabelled VPatch where
     labelColourSizeP3 c m d vp = drawLabels <> d p3Patch where
        p3Patch = decompP2toP3 $ dropLabels vp
        drawLabels = position $ drawlabel <$> VMap.toList (extendLocsP3 vp)
        drawlabel(v,p) = (p, baselineText (show v) # fontSize m # fc c)

-- | (Not exported) Extend the vertex locations of a VPatch with locations for new kite join vertices
-- appearing in (Rhombus/P3 style) drawing of tiles.
-- The new vertex numbers are generated with phiVMap from Tgraph.Decompose
extendLocsP3 :: VPatch -> VertexLocMap
extendLocsP3 vp = locmap <> VMap.fromList (map (locateNew . joinOfTile) (kites vp)) where
    locmap = vLocs vp
    newemap = phiVMap vp
    locateNew (a,b) = case (VMap.lookup a locmap, VMap.lookup b locmap) of
        (Just pa, Just pb) -> (newemap Map.! (a,b), pa .+^ (phi-1) *^ (pb .-. pa))
        _ -> error "extendLocsP3: Missing location for a kite join"

-- | Tgraphs can be drawn (Rhombus/P3 style) with labels
-- NB: the additional vertices for P3 are only added when drawing and are not part of the Tgraph
-- or its VPatch.
-- Thus using such a vertex for alignment will raise an error.
instance P3_DrawableLabelled Tgraph where
     labelColourSizeP3 c m d = labelColourSizeP3 c m d . makeVP

-- | Default Version of labelColourSizeP3 with colour red.
-- Example usage: labelSizeP3 tiny drawP3 a , labelSizeP3 normal drawjP3 a
labelSizeP3 :: (OKBackend b, P3_DrawableLabelled a) =>
               Measure Double -> (P3_Patch -> Diagram b) -> a -> Diagram b
labelSizeP3 = labelColourSizeP3 red

-- | Default Version of labelColourSizeP3 using red and small (rather than normal label size).
-- Example usage: labelledP3 drawP3 a , labelledP3 drawjP3 a
labelledP3 :: (OKBackend b, P3_DrawableLabelled a) =>
            (P3_Patch -> Diagram b) -> a -> Diagram b
labelledP3 = labelColourSizeP3 red small --(normalized 0.023)

