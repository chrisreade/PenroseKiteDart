{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-} -- needed for Drawable Patch
{-# LANGUAGE TypeOperators             #-} -- needed for type equality constraints ~

{-|
Module      : TileLib
Description : Introducing Pieces and Patches and operations on these including a class for drawing operations
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module introduces Pieces and Patches for drawing finite tilings using Penrose's Dart and Kite tiles.
It includes a decompose operation for Patches, class Drawable, and sun and star example Patches.
-}
module TileLib where

import Diagrams.Prelude

import ChosenBackend (B) 
import HalfTile


{-*
Pieces and drawing Pieces
-}

{-| Piece type for tile halves: Left Dart, Right Dart, Left Kite, Right Kite
with a vector from their origin along the join edge where
origin for a dart is the tip, origin for a kite is the vertex with smallest internal angle.
Using Imported polymorphic HalfTile.

Pieces are Transformable
-}
type Piece = HalfTile (V2 Double)

-- | get the vector representing the join edge in the direction away from the origin of a piece
joinVector:: Piece -> V2 Double
joinVector = tileRep


{- ALTERNATIVE (orphaned) instance of Transformable for Pieces (instead of in Module HalfTile)

-- |Needed for Transformable Piece
type instance N Piece = Double
-- |Needed for Transformable Piece
type instance V Piece = V2
-- |Making Pieces and Patches transformable - Requires FlexibleInstances
instance Transformable Piece where
    transform t = fmap (transform t) -- using fmap from HalfTile functor
-}


-- |ldart,rdart,lkite,rkite are the 4 pieces (with join edge oriented along the x axis, unit length for darts, length phi for kites).
ldart,rdart,lkite,rkite:: Piece
ldart = LD unitX
rdart = RD unitX
lkite = LK (phi*^unitX)
rkite = RK (phi*^unitX)

-- |All edge lengths are powers of the golden ratio (phi).
-- We also have the interesting property of the golden ratio that phi^2 == phi + 1 and so 1/phi = phi-1
-- (also phi^3 = 2phi +1 and 1/phi^2 = 2-phi)
phi::Double
phi = (1.0 + sqrt 5.0) / 2.0

-- |All angles used are multiples of tt where tt is a tenth of a turn
-- (so 36 degrees).
-- ttangle n is n multiples of tt.
ttangle:: Int -> Angle Double
ttangle n = fromIntegral (n `mod` 10) *^tt
             where tt = 1/10 @@ turn

{-|  produces a list of the two adjacent non-join tile directed edges of a piece starting from the origin.

Perhaps confusingly we regard left and right of a dart differently from left and right of a kite.
This is in line with common sense view but darts are reversed from origin point of view.

So for right dart and left kite the edges are directed and ordered clockwise from the piece origin, and for left dart and right kite these are
directed and ordered anti-clockwise from the piece origin.
-}
pieceEdges:: Piece -> [V2 Double]
pieceEdges (LD v) = [v',v ^-^ v'] where v' = phi*^rotate (ttangle 9) v
pieceEdges (RD v) = [v',v ^-^ v'] where v' = phi*^rotate (ttangle 1) v
pieceEdges (RK v) = [v',v ^-^ v'] where v' = rotate (ttangle 9) v
pieceEdges (LK v) = [v',v ^-^ v'] where v' = rotate (ttangle 1) v

-- |the 4 tile edges of a completed half-tile piece (used for colour fill).
-- These are directed and ordered clockwise from the origin of the tile.
wholeTileEdges:: Piece -> [V2 Double]
wholeTileEdges (LD v) = wholeTileEdges (RD v)
wholeTileEdges (RD v) = pieceEdges (RD v) ++ map negated (reverse $ pieceEdges (LD v))
wholeTileEdges (LK v) = pieceEdges (LK v) ++ map negated (reverse $ pieceEdges (RK v))
wholeTileEdges (RK v) = wholeTileEdges (LK v)

-- |drawing lines for the 2 non-join edges of a piece
drawPiece:: Piece -> Diagram B
drawPiece = strokeLine . fromOffsets . pieceEdges

-- |same as drawPiece but with join edge added as dashed-line
dashjPiece:: Piece -> Diagram B
dashjPiece piece = drawPiece piece <> dashjOnly piece

-- |draw join edge only 
drawJoin:: Piece -> Diagram B
drawJoin piece = strokeLine $ fromOffsets [joinVector piece]

-- |draw join edge only (as dashed line)
dashjOnly:: Piece -> Diagram B
dashjOnly piece = drawJoin piece # dashingN [0.004,0.004] 0 # lw ultraThin

-- |same as drawPiece but with added join edge (also fillable as a loop)
drawRoundPiece:: Piece -> Diagram B
drawRoundPiece = strokeLoop . closeLine . fromOffsets . pieceEdges


-- |fillPiece col piece - fills piece with colour col without drawing any lines
fillPiece:: Colour Double -> Piece -> Diagram B
fillPiece col piece  = drawRoundPiece piece # fc col # lw none

-- |fillDK dcol kcol piece - draws and fills the half-tile piece
-- with colour dcol for darts and kcol for kites.
-- Note the order D K.
fillDK:: Colour Double -> Colour Double -> Piece -> Diagram B
fillDK dcol kcol piece = drawPiece piece <> fillPiece col piece where
    col = case piece of (LD _) -> dcol
                        (RD _) -> dcol
                        (LK _) -> kcol
                        (RK _) -> kcol

-- |fillMaybeDK d k piece - draws the half-tile piece and possibly fills as well:
-- darts with dcol if d = Just dcol, kites with kcol if k = Just kcol
-- Nothing indicates no fill for either darts or kites or both
fillMaybeDK:: Maybe (Colour Double) -> Maybe (Colour Double) -> Piece -> Diagram B
fillMaybeDK d k piece = drawPiece piece <> filler where
    maybeFill (Just c) = fillPiece c piece
    maybeFill  Nothing = mempty
    filler = case piece of (LD _) -> maybeFill d
                           (RD _) -> maybeFill d
                           (LK _) -> maybeFill k
                           (RK _) -> maybeFill k

-- |fillMaybeDKG (d,k,g) piece - draws the half-tile piece and possibly fills as well:
-- darts with dcol if d = Just dcol, kites with kcol if k = Just kcol
-- Nothing indicates no fill for either darts or kites or both
-- The g argument is for grout - i.e the non-join edges round tiles.
-- Edges are drawn with gcol if g  = Just gcol and not drawn if g = Nothing
fillMaybeDKG:: (Maybe (Colour Double),  Maybe (Colour Double), Maybe (Colour Double)) -> Piece -> Diagram B
fillMaybeDKG (d,k,g) piece = fillMaybeDK d k piece # maybeGrout g where
    maybeGrout (Just c) = lc c
    maybeGrout Nothing = lw none

-- |leftFillDK dcol kcol pc fills the whole tile when pc is a left half-tile,
-- darts are filled with colour dcol and kites with colour kcol.
-- (Right half-tiles produce nothing, so whole tiles are not drawn twice)
leftFillDK:: Colour Double -> Colour Double -> Piece -> Diagram B
leftFillDK dcol kcol pc =
     case pc of (LD _) -> strokeLoop (glueLine $ fromOffsets $ wholeTileEdges pc)  # fc dcol
                (LK _) -> strokeLoop (glueLine $ fromOffsets $ wholeTileEdges pc)  # fc kcol
                _      -> mempty
        
-- |experiment uses a different rule for drawing half tiles.
-- This clearly displays the larger kites and darts.
-- Half tiles are first drawn with dashed lines, then certain edges are overlayed to emphasise them.
-- Half darts have the join edge emphasised in red, while
-- Half kites have the long edge emphasised in black.
experiment:: Piece -> Diagram B
experiment pc = --emph pc <> (drawRoundPiece pc # dashingO [1,2] 0 # lw ultraThin)
    emph pc <> (drawRoundPiece pc # dashingN [0.003,0.003] 0 # lw ultraThin)
  where emph pc = case pc of
          (LD v) -> (strokeLine . fromOffsets) [v] # lc red   -- emphasise join edge of darts in red
          (RD v) -> (strokeLine . fromOffsets) [v] # lc red 
          (LK v) -> (strokeLine . fromOffsets) [rotate (ttangle 1) v] -- emphasise long edge for kites
          (RK v) -> (strokeLine . fromOffsets) [rotate (ttangle 9) v]


{-*
Patches and Drawable Class
-}

-- |A patch is a list of Located pieces (the point associated with each piece locates its originV)
-- Patches are Transformable
type Patch = [Located Piece]

-- | A class for things that can be turned to diagrams when given a function to draw pieces
class Drawable a where
  drawWith :: (Piece -> Diagram B) -> a -> Diagram B

-- | Patches are drawable
instance Drawable Patch where
  drawWith = drawPatchWith where
    -- |turn a patch into a diagram using the first argument for drawing pieces
    -- drawPatchWith:: (Piece -> Diagram B) -> Patch -> Diagram B      
      drawPatchWith pd = position . fmap (viewLoc . mapLoc pd)

-- | the main default case for drawing using drawPiece
draw :: Drawable a => a -> Diagram B
draw = drawWith drawPiece

-- | alternative default case for drawing adding dashed lines for join edges
drawj :: Drawable a => a -> Diagram B
drawj = drawWith dashjPiece
    
-- |colourDKG (c1,c2,c3) p - fill in a drawable with colour c1 for darts, colour c2 for kites and
-- colour c3 for grout (that is, the non-join edges).
-- Note the order D K G.
colourDKG::  Drawable a => (Colour Double,Colour Double,Colour Double) -> a -> Diagram B
colourDKG (c1,c2,c3) p = drawWith (fillDK c1 c2) p # lc c3

{-*
Patch Decomposition and Compose choices
-}

{-|
Decomposing splits each located piece in a patch into a list of smaller located pieces to create a refined patch.
(See also decompose in Tgraph.Decompose.hs for a more abstract version of this operation).
-}
decompPatch :: Patch -> Patch
decompPatch = concatMap decompPiece

-- |Decomposing a located piece to a list of (2 or 3) located pieces at smaller scale.
decompPiece :: Located Piece -> [Located Piece]
decompPiece lp = case viewLoc lp of
  (p, RD vd)-> [ LK vd  `at` p
               , RD vd' `at` (p .+^ v')
               ] where v'  = phi*^rotate (ttangle 1) vd
                       vd' = (2-phi) *^ negated v' -- (2-phi) = 1/phi^2
  (p, LD vd)-> [ RK vd `at` p
               , LD vd' `at` (p .+^ v')
               ]  where v'  = phi*^rotate (ttangle 9) vd
                        vd' = (2-phi) *^ negated v'  -- (2-phi) = 1/phi^2
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

-- |Create an infinite list of increasing decompositions of a patch
decompositionsP:: Patch -> [Patch]
decompositionsP = iterate decompPatch

{-|
compChoices applied to  a single located piece produces a list of alternative located pieces NOT a Patch.
Each of these is a larger scale single piece with a location such that when decomposed
the original piece in its original position is part of the decomposition)
-}
compChoices :: Located Piece -> [Located Piece]
compChoices lp = case viewLoc lp of
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
               ] where lv'  = phi*^rotate (ttangle 9) vk
                       rv'  = phi*^rotate (ttangle 1) vk
                       rvk' = phi*^rotate (ttangle 7) vk
                       lvk' = phi*^rotate (ttangle 3) vk

-- |compNChoices n lp - gives a list of all the alternatives after n compChoices starting with lp
-- Note that the result is not a Patch as the list represents alternatives.
compNChoices :: Int -> Located Piece -> [Located Piece]
compNChoices 0 lp = [lp]
compNChoices n lp = do
    lp' <- compChoices lp
    compNChoices (n-1) lp'

{-*
Example Patches and rotation/scaling operations
-}
                                
-- |combine 5 copies of a patch (each rotated by ttangle 2 successively)
-- (ttAngle 2 is 72 degrees) 
-- Must be used with care to avoid creating a nonsense patch
penta:: Patch -> Patch
penta p = concatMap copy [0..4] 
            where copy n = rotate (ttangle (2*n)) p
  
sun,star::Patch         
-- |sun is a patch with five kites sharing common origin (base of kite)
sun =  penta [rkite `at` origin, lkite `at` origin]
-- |star is a patch with five darts sharing common origin (tip of dart)
star = penta [rdart `at` origin, ldart `at` origin]

-- |rotations takes a list of integers (representing ttangles) for respective rotations of items in the second list (things to be rotated).
-- This includes Diagrams, Patches, VPatches
-- The integer list can be shorter than the list of items - the remaining items are left unrotated.
-- (Rotations by an angle are anti-clockwise)

rotations :: (Transformable a, V a ~ V2, N a ~ Double) => [Int] -> [a] -> [a]
rotations (n:ns) (d:ds) = rotate (ttangle n) d: rotations ns ds
rotations [] ds = ds
rotations _  [] = error "rotations: too many rotation integers"

-- |scales takes a list of doubles for respective scalings of items in the second list (things to be scaled).
-- This includes Diagrams, Pieces, Patches, VPatches
-- The list of doubles can be shorter than the list of items - the remaining items are left unscaled.
scales :: (Transformable a, V a ~ V2, N a ~ Double) => [Double] -> [a] -> [a]
scales (s:ss) (d:ds) = scale s d: scales ss ds
scales [] ds = ds
scales _  [] = error "scales: too many scalars"

-- |increasing scales by phi along a list starting with 1
phiScales:: (Transformable a, V a ~ V2, N a ~ Double) => [a] -> [a]
phiScales = phiScaling 1

-- |increasing scales by phi along a list starting with given first argument
phiScaling:: (Transformable a, V a ~ V2, N a ~ Double) => Double -> [a] -> [a]
phiScaling _ [] = []
phiScaling s (d:more) = scale s d: phiScaling (phi*s) more



