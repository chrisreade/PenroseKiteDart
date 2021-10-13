{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

 {- |
 Penrose Darts and Kites - infinite non-periodic tilings
 Main Operations of Inflate and Decompose
 using Half Tiles represented with a single vector 
 (the vector along the internal join where half tiles come together)
 -}
module TileLib where

import HalfTile
    
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

{- | Piece type for tile halves: Left Dart, Right Dart, Left Kite, Right Kite
with a vector from their origin along the join edge where
origin for a dart is the tip, origin for a kite is the acute angle tip.
Using Imported polymorphic HalfTile
-}
type Piece = HalfTile (V2 Double)

getJVec:: Piece -> V2 Double
getJVec = tileRep

{-****************
Important change
*******************

scalePiece, rotatePiece and scalePatch, rotatePatch, and translatePatch are no longer needed.
They are replaced by scale, rotate, translate accordingly
courtesy of HalfTile instances for V N and Transformable

-- | scale a piece
scalePiece:: Double -> Piece -> Piece
scalePiece = fmap . scale 

-- | rotate a piece
rotatePiece :: Angle Double -> Piece -> Piece
rotatePiece = fmap . rotate 

-- | scale a patch by a real number
scalePatch :: Double -> Patch -> Patch
scalePatch r = fmap (\(v,c) -> (scale r v, scale r c))
--scalePatch r = fmap (\(v,c) -> (scale r v, scalePiece r c))

-- | rotate a patch by an angle
rotatePatch :: Angle Double -> Patch -> Patch
rotatePatch a = fmap (\(v,c) -> (rotate a v, rotate a c))
-- rotatePatch a = fmap (\(v,c) -> (rotate a v, rotatePiece a c))

-- | move a patch by a vector
translatePatch:: V2 Double -> Patch -> Patch
translatePatch v0 = fmap (\(v,c) -> (v0 ^+^ v, c))
-}

type instance N (HalfTile a) = N a
type instance V (HalfTile a) = V a
instance Transformable a => Transformable (HalfTile a) where
    transform t ht = fmap (transform t) ht

-- | These are the only 4 pieces (oriented along x axis)
ldart,rdart,lkite,rkite:: Piece
ldart = LD unitX
rdart = RD unitX
lkite = LK (phi*^unitX)
rkite = RK (phi*^unitX)

-- | All edges are powers of the golden section phi
-- We also have the interesting property of the golden section that phi^2 == phi + 1 and so 1/phi = phi-1
-- also phi^3 = 2phi +1 and 1/phi^2 = 2-phi
phi::Double
phi = (1.0 + sqrt 5.0) / 2.0

-- | All angles used are multiples of tt -  tau/10 rad = 1/10 turn = 36 deg
-- angles are from positive x axis anticlockwise
ttangle:: Int -> Angle Double
ttangle n = (fromIntegral (n `mod` 10))*^tt
             where tt = 1/10 @@ turn

{- |  The two outer tile edges of a piece.
Perhaps confusingly we regard left and right of a dart differently from left and right of a kite.
This is in line with common sense view but darts are reversed from origin point of view
Going clockwise round the origin the Right Dart comes before Left Dart, but 
the Left Kite comes before Right Kite
This is manifest in only pieceEdges, tileEdges, compose and decompPiece
-}
pieceEdges:: Piece -> [V2 Double]
pieceEdges (LD v) = [v',v ^-^ v'] where v' = phi*^rotate (ttangle 9) v
pieceEdges (RD v) = [v',v ^-^ v'] where v' = phi*^rotate (ttangle 1) v
pieceEdges (RK v) = [v',v ^-^ v'] where v' = rotate (ttangle 9) v
pieceEdges (LK v) = [v',v ^-^ v'] where v' = rotate (ttangle 1) v

-- |  the 4 tile edges of a completed half-tile piece (needed for colour fill)
-- These are clockwise from the origin of the tile
tileEdges:: Piece -> [V2 Double]
tileEdges (LD v) = pieceEdges (RD v) ++ map negated (reverse $ pieceEdges (LD v))
tileEdges (RD v) = tileEdges (LD v)
tileEdges (LK v) = pieceEdges (LK v) ++ map negated (reverse $ pieceEdges (RK v))
tileEdges (RK v) = tileEdges (LK v)

-- | drawing lines for the 2 outer edges of a piece
drawPiece:: Piece -> Diagram B
drawPiece = strokeLine . fromOffsets . pieceEdges

-- | fill whole tiles, darts with dcol and kites with kcol (colours)
-- Uses only left pieces to identify whole tile, ignoring right pieces
fillDK':: Colour Double -> Colour Double -> Piece -> Diagram B
fillDK' dcol kcol pc =
     case pc of (LD _) -> (strokeLoop $ glueLine $ fromOffsets $ tileEdges pc)  # fc dcol
                (LK _) -> (strokeLoop $ glueLine $ fromOffsets $ tileEdges pc)  # fc kcol
                _      -> mempty

-- | drawPiece with added join edge (also fillable as a loop)
drawJPiece:: Piece -> Diagram B
drawJPiece = strokeLoop . closeLine . fromOffsets . pieceEdges

-- | similar to fillDK' except using drawJPiece
--  so that half tiles are not completed and both left and right are filled
-- The filled loop is coloured with matching edge colour, then overlayed with piece edges
fillDK:: Colour Double -> Colour Double -> Piece -> Diagram B
fillDK dcol kcol piece = drawPiece piece <> (drawJPiece piece # fc col # lc col) where
    col = case piece of (LD _) -> dcol
                        (RD _) -> dcol
                        (LK _) -> kcol
                        (RK _) -> kcol

-- | join edge added as dashed-line
dashJPiece:: Piece -> Diagram B
dashJPiece piece = drawPiece piece <> (drawJ piece # dashingO [1,1] 0 # lw ultraThin)
-- dashJPiece piece = drawPiece piece <> (drawJ piece # dashingN [0.002,0.002] 0 # lw ultraThin)

-- | draw join only 
drawJ:: Piece -> Diagram B
drawJ piece = strokeLine (fromOffsets [getJVec piece]) 

         
-- | experiment uses a different rule for drawing half tiles.
-- This clearly displays the larger kites and darts.
-- Half tiles are first drawn with dashed lines, then certain edges are overlayed to emphasise
-- Half darts have the join edge emphasised in red, while
-- Half kites have the long edge emphasised in black.
experiment:: Piece -> Diagram B
experiment pc = emph pc <> (drawJPiece pc # dashingN [0.002,0.002] 0 # lw ultraThin)
  where emph pc = case pc of
          (LD v) -> (strokeLine . fromOffsets) [v] # lc red   -- emphasise join edge of darts in red
          (RD v) -> (strokeLine . fromOffsets) [v] # lc red 
          (LK v) -> (strokeLine . fromOffsets) [rotate (ttangle 1) v] -- emphasise long edge for kites
          (RK v) -> (strokeLine . fromOffsets) [rotate (ttangle 9) v]

-- | A patch is a list of Located pieces (i.e. a point associated with originV)
type Patch = [Located Piece]

-- | turn a patch into a diagram using cd for drawing pieces
patchWith:: (Piece -> Diagram B) -> Patch -> Diagram B      
patchWith cd patch = position $ fmap (viewLoc . mapLoc cd) patch

    
-- | special case - turn patches to diagrams with drawPiece
drawPatch:: Patch -> Diagram B      
drawPatch = patchWith drawPiece

-- | special case - turn patches to diagrams with dashJPiece
dashJPatch:: Patch -> Diagram B      
dashJPatch = patchWith dashJPiece

{- |
Decomposing splits each located piece in a patch into a list of smaller located pieces to create a refined patch
Decomposition is unique.
-}
decompose :: Patch -> Patch
decompose = concatMap decompPiece

-- | Decomposing a located piece
decompPiece lp = case viewLoc lp of
  (p, RD vd)-> [ LK vd  `at` p
               , RD vd' `at` (p .+^ v')
               ] where v'  = phi*^rotate (ttangle 1) vd
                       vd' = (2-phi) *^ (negated v') -- (2-phi) = 1/phi^2
  (p, LD vd)-> [ RK vd `at` p
               , LD vd' `at` (p .+^ v')
               ]  where v'  = phi*^rotate (ttangle 9) vd
                        vd' = (2-phi) *^ (negated v')  -- (2-phi) = 1/phi^2
  (p, RK vk)-> [ RD vd' `at` p
               , LK vk' `at` (p .+^ v')
               , RK vk' `at` (p .+^ v')
               ] where v'  = rotate (ttangle 9) vk
                       vd' = (2-phi) *^ v' -- v'/phi^2
                       vk' = ((phi-1) *^ vk) ^-^ v' -- (phi-1) = 1/phi
  (p, LK vk)-> [ LD vd' `at` p
               , RK vk' `at` (p .+^ v')
               , LK vk' `at` (p .+^ v')
               ] where v'  = rotate (ttangle 1) vk
                       vd' = (2-phi) *^ v' -- v'/phi^2
                       vk' = ((phi-1) *^ vk) ^-^ v' -- (phi-1) = 1/phi

-- | Create an infinite list of increasing decompositions of a patch
decompositions:: Patch -> [Patch]
decompositions = iterate decompose

{- |
Inflating produces a list of choices NOT a Patch
Inflating a single piece at p - produces a list of alternative located pieces
Each is a larger scale single piece with new location 
(to ensure the larger piece contains the original in its original position in a decomposition)
-}
inflate :: Located Piece -> [Located Piece]
inflate lp = case viewLoc lp of
  (p, RD vd)-> [ RD vd' `at` (p .+^ v')
               , RK vk  `at` p
               ] where v'  = (phi+1) *^ vd                  -- vd*phi^2
                       vd' = rotate (ttangle 9) (vd ^-^ v')
                       vk  = rotate (ttangle 1) v'
  (p, LD vd)-> [ LD vd' `at` (p .+^ v')
               , LK vk `at` p
               ] where v'  = (phi+1) *^ vd                  -- vd*phi^2
                       vd' = rotate (ttangle 1) (vd ^-^ v')
                       vk  = rotate (ttangle 9) v'
  (p, RK vk)-> [ LD vk  `at` p
               , LK lvk' `at` (p .+^ lv') 
               , RK rvk' `at` (p .+^ rv')
               ] where lv'  = phi*^rotate (ttangle 9) vk
                       rv'  = phi*^rotate (ttangle 1) vk
                       rvk' = phi*^rotate (ttangle 7) vk
                       lvk' = phi*^rotate (ttangle 3) vk
  (p, LK vk)-> [ RD vk  `at` p
               , RK rvk' `at` (p .+^ rv')
               , LK lvk' `at` (p .+^ lv')
               ] where v0 = rotate (ttangle 1) vk
                       lv'  = phi*^rotate (ttangle 9) vk
                       rv'  = phi*^rotate (ttangle 1) vk
                       rvk' = phi*^rotate (ttangle 7) vk
                       lvk' = phi*^rotate (ttangle 3) vk

-- | a list of all the alternatives after n inflation choices
inflations :: Int -> Located Piece -> [Located Piece]
inflations 0 lp = [lp]
inflations n lp = do
    lp' <- inflate lp
    inflations (n-1) lp'
                                
-- | combine 5 copies of a patch (rotated by multiples of 2*tt successively) 
-- use with care to avoid nonsense patches
penta:: Patch -> Patch
penta p = concatMap copy [0..4] 
            where copy n = rotate (ttangle (2*n)) p
  
-- | sun and star patches 
sun,star::Patch         
sun =  penta [rkite `at` origin, lkite `at` origin]
star = penta [rdart `at` origin, ldart `at` origin]






