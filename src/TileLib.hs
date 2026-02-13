{-|
Module      : TileLib
Description : Introducing Pieces and Patches and Drawable class
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module introduces Pieces and Patches for drawing finite tilings using Penrose's Dart and Kite tiles.
It includes several primitives for drawing half tiles (Pieces), a class Drawable with instance Patch
and commonly used operations for the Drawable class (draw, drawj, fillDK,..).
It also introduces class OKBackend to summarise constraints on a Backend for drawing.
There is also a decompose operation for Patches (decompPatch) and sun and star example Patches.
-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-} -- needed for Drawable Patch
{-# LANGUAGE TypeOperators             #-} -- needed for type equality constraints ~
{-# LANGUAGE Strict                    #-} 

module TileLib 
  ( OKBackend
  -- * Pieces
  , Piece
  , joinVector
  , ldart
  , rdart
  , lkite
  , rkite
    -- * Drawing Pieces
  , phi
  , ttangle
  , drawnEdges
  , wholeTileEdges
  -- $OKBackend
  , drawPiece
  , drawjPiece
  , drawJPiece
  , joinDashing
  , dashjOnly
  , dashJOnly
  , drawRoundPiece
  , drawJoin
  , fillOnlyPiece
  , fillOnlyPieceDK
  , fillPieceDK
  -- , fillMaybePieceDK
  , leftFillPieceDK
  , experiment
    -- * Patches and Drawable Class
  , Patch
  , Drawable(..)
  , draw
  , drawj
  , drawJ
  , fillDK
  , fillKD
  -- , fillMaybeDK
  , colourDKG
    -- , colourMaybeDKG
    -- * Patch Decomposition and Compose choices
  , decompPatch
  , decompositionsP
  , compChoices
  , compNChoices
    -- * Example Patches
  , penta
  , sun
  , TileLib.star
  , suns
  , sun5
  , sun6
    -- * Example Diagrams of Patches
  , sun6Fig
  , leftFilledSun6
  , filledSun6
    -- * Rotation and Scaling operations
  , rotations
  , scales
  , phiScales
  , phiScaling
  --, realignX
  ) where

import Diagrams.Prelude
--import Diagrams.TwoD.Text (Text) -- now in CheckBackend

import CheckBackend
import HalfTile

{-| Piece is a type for (scaled and oriented) tile halves: Left Dart, Right Dart, Left Kite, Right Kite
represented by a vector from their origin along the join edge where
origin for a dart is the tip, origin for a kite is the vertex opposite the vertex with
largest internal angle.

(This specialises polymorphic HalfTiles with 2D vectors).

Pieces are Transformable (but not translatable until they are located).
-}
type Piece = HalfTile (V2 Double)

-- | get the vector representing the join edge in the direction away from the origin of a piece
joinVector:: Piece -> V2 Double
joinVector = tileRep

ldart,rdart,lkite,rkite:: Piece
-- |ldart is a left dart at the origin with join edge oriented along the x axis, unit length.
ldart = LD unitX
-- |rdart is a right dartat the origin with join edge oriented along the x axis, unit length.
rdart = RD unitX
-- |lkite is a left kite at the origin with join edge oriented along the x axis, length phi.
lkite = LK (phi*^unitX)
-- |rkite  is a right kite at the origin with join edge oriented along the x axis, length phi.
rkite = RK (phi*^unitX)

-- |All edge lengths are powers of the golden ratio (phi).
-- We have the following roperties of the golden ratio 
-- 
-- phi^2 == phi + 1 and  1/phi = phi-1
--
-- phi^3 = 2phi + 1 and  1/phi^2 = 2-phi
phi::Double
phi = (1.0 + sqrt 5.0) / 2.0

-- |All angles used are multiples of tt where tt is a tenth of a turn
-- (so 36 degrees).
-- ttangle n is n multiples of tt.
ttangle:: Int -> Angle Double
ttangle n = fromIntegral (n `mod` 10) *^tt
             where tt = 1/10 @@ turn

{-|This produces a list of vectors representing the two adjacent non-join tile directed edges of a piece starting from the origin.

We consider left and right as viewed from the origin.
This means that darts are reversed with respect to a view from the tail, but kites are
in keeping with a common view (the kite tail being the origin).

So for right dart and left kite the edges are directed and ordered clockwise from the piece origin, and for left dart and right kite these are
directed and ordered anti-clockwise from the piece origin.
-}
drawnEdges:: Piece -> [V2 Double]
drawnEdges (LD v) = [v',v ^-^ v'] where v' = phi*^rotate (ttangle 9) v
drawnEdges (RD v) = [v',v ^-^ v'] where v' = phi*^rotate (ttangle 1) v
drawnEdges (RK v) = [v',v ^-^ v'] where v' = rotate (ttangle 9) v
drawnEdges (LK v) = [v',v ^-^ v'] where v' = rotate (ttangle 1) v

-- |the 4 tile edges of a completed half-tile piece (used for colour fill).
-- These are directed and ordered clockwise from the origin of the tile.
wholeTileEdges:: Piece -> [V2 Double]
wholeTileEdges (LD v) = wholeTileEdges (RD v)
wholeTileEdges (RD v) = drawnEdges (RD v) ++ map negated (reverse $ drawnEdges (LD v))
wholeTileEdges (LK v) = drawnEdges (LK v) ++ map negated (reverse $ drawnEdges (RK v))
wholeTileEdges (RK v) = wholeTileEdges (LK v)

{- $OKBackend 
Note: Most functions for drawing will have constraint OKBackend b and result type Diagram b
-}
   
-- |drawing lines for the 2 non-join edges of a piece.
drawPiece :: OKBackend b =>
             Piece -> Diagram b
drawPiece = strokeLine . fromOffsets . drawnEdges

-- |same as drawPiece but with join edge added as faint dashed line.
-- J for plain dashed Join, j for faint dashed join
drawjPiece :: OKBackend b =>
              Piece -> Diagram b
drawjPiece = drawPiece <> dashjOnly

-- |same as drawPiece but with join edge added as dashed line.
-- J for plain dashed  Join, j for faint dashed join
drawJPiece :: OKBackend b =>
              Piece -> Diagram b
drawJPiece = drawPiece <> dashJOnly

-- |draw join edge only (as faint dashed line).
-- J for plain dashed Join, j for faint dashed join
dashjOnly :: OKBackend b =>
             Piece -> Diagram b
dashjOnly = lw ultraThin . dashJOnly

-- |draw join edge only (as faint dashed line).
-- J for plain dashed  Join, j for faint dashed join
dashJOnly :: OKBackend b =>
             Piece -> Diagram b
dashJOnly = joinDashing . drawJoin

-- |changes line style to ultraThin dashed lines (for drawing join edges)
joinDashing :: (HasStyle c, N c ~ Double) => c -> c
joinDashing = dashing [dashmeasure,dashmeasure] 0
                     where dashmeasure = normalized 0.003  `atLeast` output 0.5

-- |draw join edge only.
drawJoin :: OKBackend b =>
            Piece -> Diagram b
drawJoin piece = strokeLine $ fromOffsets [joinVector piece]

-- |same as drawPiece but with added join edge (also fillable as a loop).
drawRoundPiece :: OKBackend b =>
                  Piece -> Diagram b
drawRoundPiece = strokeLoop . closeLine . fromOffsets . drawnEdges


-- |fillOnlyPiece col piece - fills piece with colour col without drawing any lines.
-- Can be used with both Colour and AlphaColour
fillOnlyPiece :: (OKBackend b, Color c) =>
                 c -> Piece -> Diagram b
fillOnlyPiece col piece  = drawRoundPiece piece # fillColor col # lw none

-- |fillOnlyPieceDK dcol kcol piece - fills the half-tile piece
-- with colour dcol for darts and kcol for kites.
-- Note the order D K.
-- Can be used with both Colour and AlphaColour
fillOnlyPieceDK :: (OKBackend b, Color c1, Color c2) =>
                   c1 -> c2 -> HalfTile (V2 Double) -> Diagram b
fillOnlyPieceDK dcol kcol piece = 
    if isDart piece 
    then fillOnlyPiece dcol piece
    else fillOnlyPiece kcol piece

-- |fillPieceDK dcol kcol piece - draws and fills the half-tile piece
-- with colour dcol for darts and kcol for kites.
-- Note the order D K.
-- Can be used with both Colour and AlphaColour
fillPieceDK :: (OKBackend b, Color c1, Color c2) =>
               c1 -> c2 -> HalfTile (V2 Double) -> Diagram b
fillPieceDK dcol kcol = drawPiece <> fillOnlyPieceDK dcol kcol
{- 
fillPieceDK dcol kcol piece = drawPiece piece <> filledPiece where
  filledPiece = case piece of
     (LD _) -> fillOnlyPiece dcol piece
     (RD _) -> fillOnlyPiece dcol piece
     (LK _) -> fillOnlyPiece kcol piece
     (RK _) -> fillOnlyPiece kcol piece
 -}
-- |leftFillPieceDK dcol kcol pc fills the whole tile when pc is a left half-tile,
-- darts are filled with colour dcol and kites with colour kcol.
-- (Right half-tiles produce nothing, so whole tiles are not drawn twice).
-- Works with AlphaColours as well as Colours.
leftFillPieceDK :: (OKBackend b, Color c1, Color c2) =>
                   c1 -> c2 -> HalfTile (V2 Double) -> Diagram b
leftFillPieceDK dcol kcol pc =
     case pc of (LD _) -> strokeLoop (glueLine $ fromOffsets $ wholeTileEdges pc)  # fillColor dcol
                (LK _) -> strokeLoop (glueLine $ fromOffsets $ wholeTileEdges pc)  # fillColor kcol
                _      -> mempty
        
-- |experiment uses a different rule for drawing half tiles.
-- This clearly displays the larger kites and darts.
-- Half tiles are first drawn with dashed lines, then certain edges are overlayed to emphasise them.
-- Half darts have the join edge emphasised in red, while
-- Half kites have the long edge emphasised in black.
experiment:: OKBackend b =>
             Piece ->  Diagram b
experiment piece = emph piece <> (drawRoundPiece piece # dashingN [0.003,0.003] 0 # lw ultraThin)
    --emph pc <> (drawRoundPiece pc # dashingO [1,2] 0 # lw ultraThin)
  where emph pc = case pc of
          (LD v) -> (strokeLine . fromOffsets) [v] # lc red   -- emphasise join edge of darts in red
          (RD v) -> (strokeLine . fromOffsets) [v] # lc red 
          (LK v) -> (strokeLine . fromOffsets) [rotate (ttangle 1) v] -- emphasise long edge for kites
          (RK v) -> (strokeLine . fromOffsets) [rotate (ttangle 9) v]



-- |A patch is a list of Located pieces (the point associated with each piece locates its originV)
-- Patches are Transformable (including translatable)
type Patch = [Located Piece]

-- | A class for things that can be turned to diagrams when given a function to draw pieces.
class Drawable a where
  drawWith :: OKBackend b =>
              (Piece ->  Diagram b) -> a ->  Diagram b

-- | Patches are drawable
instance Drawable Patch where
  drawWith = drawPatchWith where
    -- turn a patch into a diagram using the first argument for drawing pieces.
    -- drawPatchWith:: (Piece -> Diagram B) -> Patch -> Diagram B      
      drawPatchWith pd = position . fmap (viewLoc . mapLoc pd)

-- | the main default case for drawing using drawPiece.
draw :: (Drawable a, OKBackend b) =>
        a -> Diagram b
draw = drawWith drawPiece

-- | alternative default case for drawing, adding faint dashed lines for join edges.
-- J for plain dashed  Join, j for faint dashed join
drawj :: (Drawable a, OKBackend b) =>
         a -> Diagram b
drawj = drawWith drawjPiece

-- | alternative default case for drawing, adding dashed lines for join edges.
-- J for plain dashed  Join, j for faint dashed join
drawJ :: (Drawable a, OKBackend b) =>
         a -> Diagram b
drawJ = drawWith drawJPiece

fillDK, fillKD :: (Drawable a, OKBackend b, Color c1, Color c2) =>
                  c1 -> c2 -> a -> Diagram b
-- |fillDK dcol kcol a - draws and fills a with colour dcol for darts and kcol for kites.
-- Note the order D K.
-- Works with AlphaColours as well as Colours.
fillDK c1 c2 = drawWith (fillPieceDK c1 c2)

-- |fillKD kcol dcol a - draws and fills a with colour kcol for kites and dcol for darts.
-- Note the order K D.
-- Works with AlphaColours as well as Colours.
fillKD c1 c2 = fillDK c2 c1

-- |colourDKG (c1,c2,c3) p - fill in a drawable with colour c1 for darts, colour c2 for kites and
-- colour c3 for grout (that is, the non-join edges).
-- Note the order D K G.
-- Can be used with both Colour and AlphaColour
colourDKG :: (Drawable a, OKBackend b, Color c1, Color c2, Color c3) =>
             (c1,c2,c3) -> a -> Diagram b
colourDKG (c1,c2,c3) a = fillDK c1 c2 a # lineColor c3

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
                       vd' = (2-phi) *^ v'  -- (2-phi) = 1/phi^2
                       vk' = ((phi-1) *^ vk) ^-^ v' -- (phi-1) = 1/phi
  (p, LK vk)-> [ LD vd' `at` p
               , RK vk' `at` (p .+^ v')
               , LK vk' `at` (p .+^ v')
               ] where v'  = rotate (ttangle 1) vk
                       vd' = (2-phi) *^ v'  -- (2-phi) = 1/phi^2
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


-- |An infinite list of patches of increasingly decomposed sun
suns::[Patch]
suns = decompositionsP sun
sun5,sun6:: Patch
-- |a patch of a 6 times decomposed sun
sun6 = suns!!6
-- |a patch of a 5 times decomposed sun
sun5 = suns!!5 


   -- * Diagrams of Patches

-- |diagram for sun6.
sun6Fig :: OKBackend b => Diagram b
sun6Fig = draw sun6 # lw thin


-- |Colour filled using leftFillPieceDK. 
leftFilledSun6 :: OKBackend b => Diagram b
leftFilledSun6 = drawWith (leftFillPieceDK red blue) sun6 # lw thin

-- |Colour filled using fillDK.
filledSun6 :: OKBackend b => Diagram b
filledSun6 = fillDK darkmagenta indigo sun6 # lw thin # lc gold


-- |rotations takes a list of integers (representing ttangles) for respective rotations of items in the second list (things to be rotated).
-- This includes Diagrams, Patches, VPatches.
-- The integer list can be shorter than the list of items - the remaining items are left unrotated.
-- It will raise an error if the integer list is longer than the list of items to be rotated.
-- (Rotations by an angle are anti-clockwise)
rotations :: (Transformable a, V a ~ V2, N a ~ Double) => [Int] -> [a] -> [a]
rotations (n:ns) (d:ds) = rotate (ttangle n) d: rotations ns ds
rotations [] ds = ds
rotations _  [] = error "rotations: too many rotation integers"

-- |scales takes a list of doubles for respective scalings of items in the second list (things to be scaled).
-- This includes Diagrams, Pieces, Patches, VPatches.
-- The list of doubles can be shorter than the list of items - the remaining items are left unscaled.
-- It will raise an error if the integer list is longer than the list of items to be scaled.
scales :: (Transformable a, V a ~ V2, N a ~ Double) => [Double] -> [a] -> [a]
scales (s:ss) (d:ds) = scale s d: scales ss ds
scales [] ds = ds
scales _  [] = error "scales: too many scalars"

-- |increasing scales by a factor of phi along a list starting with 1.
phiScales:: (Transformable a, V a ~ V2, N a ~ Double) => [a] -> [a]
phiScales = phiScaling 1

-- |increasing scales by a factor of phi along a list starting with given first argument
phiScaling:: (Transformable a, V a ~ V2, N a ~ Double) => Double -> [a] -> [a]
phiScaling _ [] = []
phiScaling s (d:more) = scale s d: phiScaling (phi*s) more




