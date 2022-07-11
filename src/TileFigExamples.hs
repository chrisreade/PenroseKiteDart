{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

{-|
Module      : TileFigExamples
Description : Example patches of kite and dart tilings (not using Tgraphs) with diagrams
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

-}
module TileFigExamples where
    
import Diagrams.Prelude

import ChosenBackend (B)
import TileLib
import HalfTile

-- |Used for adding text at a point
label l p = baselineText l # fontSize (local 0.2) # fc blue # moveTo p

{-*
Figures for pieces
-}

-- |a list of the four Pieces
thePieces :: [Piece]
thePieces =  [ldart, rdart, lkite, rkite]  
-- |drawn edges of 4 pieces in a row         
piecesFig :: Diagram B
piecesFig = hsep 0.5 $ fmap (showOrigin . drawJPiece) thePieces 
-- |filled 4 pieces in a row         
piecesFig2 = hsep 1 $ fmap (fillDK' red blue) thePieces ++ fmap drawPiece thePieces 


-- |figure showing origins and markings on tiles
markedTiles:: Diagram B
markedTiles = hsep 1  
        [ kiteDiag # showOrigin 
        , dartDiag # showOrigin 
        , kiteDiag <> (pL ~~ pR # lc red # lw thick) 
        , dartDiag <> (origin ~~ p2(1,0) # lc red # lw thick)
        ] where kiteDiag = drawPatch [lkite `at` origin, rkite `at` origin]
                dartDiag = drawPatch [ldart `at` origin, rdart `at` origin]
                pL = origin .+^ phi*^rotate (ttangle 1) unitX
                pR = origin .+^ phi*^rotate (ttangle 9) unitX

-- |another figure showing origins and markings on tiles
markedTiles2:: Diagram B
markedTiles2 = hsep 1  
        [ kiteDiag <> (pL ~~ pR # lc lime # lw thick) 
        , dartDiag <> (origin ~~ p2(1,0) # lc lime # lw thick)
        ] where kiteDiag = drawPatch [lkite `at` origin, rkite `at` origin]
                dartDiag = drawPatch [ldart `at` origin, rdart `at` origin]
                pL = origin .+^ phi*^rotate (ttangle 1) unitX
                pR = origin .+^ phi*^rotate (ttangle 9) unitX

-- |figure showing the four labelled pieces each with join edge and origin shown
newPiecesFig:: Diagram B
newPiecesFig = pad 1.2 $ centerXY $
               label "RD" (p2 (negate 0.4,0.7)) <>
               label "LD" (p2 (0.3,0.7)) <>
               label "LK" (p2 (1.65,1.0)) <>
               label "RK" (p2 (2.55,1.0)) <>
               hsep 0.1 (fmap (rotate (90 @@ deg) . showOrigin . dashJPiece) 
                         [rdart,ldart,lkite,rkite]
                        )
-- |diagram combining markedTiles2 and newPiecesFig
tileIntro:: Diagram B
tileIntro = hsep 1 [markedTiles2, newPiecesFig]

{-*
Figures for decompositions and inflations
-}

-- |figure showing 4 decompositions in a column for each of the four pieces
fourDecomps:: Diagram B
fourDecomps = hsep 1 $ fmap decomps thePieces # lw thin where
         decomps pc = vsep 1 $ fmap drawPatch $ take 5 $ decompositions [pc `at` origin] 

-- |example of compChoices in action with 5 chosen compChoices steps from a left dart.
-- This shows inital and final piece together on the left,  
-- and 5 decomposition of the final piece on the right.
fiveCompChoices:: Diagram B
fiveCompChoices = pad 1.1 $ hsep 1 $ fmap dashJPatch [[ld,lk'], decompositions [lk'] !! 5] where
     -- two seperate patches
       ld  = ldart `at` origin
       lk  = compChoices ld  !!1
       rk  = compChoices lk  !!1
       rk' = compChoices rk  !!2
       ld' = compChoices rk' !!0
       lk' = compChoices ld' !!1

-- |diagram showing first five alternatives of 4-fold compNChoices of a right dart
fiveAlternatives:: Diagram B
fiveAlternatives = hsep 1 $ fmap (dashJPatch . (:[lp])) $ take 5 $ compNChoices 4 lp where
                     lp = (rdart `at` origin)

-- |An infinite list of patches of increasingly decomposed sun
suns::[Patch]
suns = decompositions sun
sun5,sun6:: Patch
-- |a patch of a 6 times decomposed sun
sun6 = suns!!6
-- |a patch of a 5 times decomposed sun
sun5 = suns!!5 

-- |diagram for sun6
sun6Fig::Diagram B
sun6Fig = drawPatch sun6 # lw thin

-- |diagram overlaying sun5 in red atop sun6
sun5Over6Fig::Diagram B
sun5Over6Fig = (drawPatch sun5 # lc red # dashingN [0.003,0.003] 0 <> drawPatch sun6) # lw thin
-- |Using experiment (defined in Tilelib) on sun6 clearly illustrates the embedded sun5
experimentFig::Diagram B
experimentFig = patchWith experiment sun6 # lw thin
-- |Using experiment (defined in Tilelib) on sun4 clearly illustrates the embedded sun3
twoLevelsFig::Diagram B
twoLevelsFig = patchWith experiment (suns!!4)

-- |figure showing two types of dart wing vertices (largeKiteCentre, largeDartBase)                         
dartWingFig::Diagram B
dartWingFig = pad 1.2 $ hsep 1 [dkite, ddart] where
  ddart = showOrigin (translate unit_X $ dashJPatch  $ decompose [ldart `at` origin, rdart `at` origin])
  dkite = showOrigin (translate unit_X $ dashJPatch  $ decompose [lkite `at` origin, rkite `at` origin])

{-*
Colour-filled examples
-}

-- |using fillDK'
filledSun6::Diagram B
filledSun6 = patchWith (fillDK' red blue) sun6 # lw ultraThin
-- |using fillDK
newFillSun6::Diagram B
newFillSun6 = patchWith (fillDK darkmagenta indigo) sun6 # lw ultraThin # lc gold



-- |list of 3 diagrams for colour filled star,sun kite respectivly
threeColouredShapes:: [Diagram B]
threeColouredShapes = [star4,sun4,kite5] where
        star4 = colourDKG (goldenrod, darkturquoise, saddlebrown) (decompositions TileLib.star !!4)
        sun4 = colourDKG (darken 0.7 darkmagenta, indigo, gold) (suns!!4)
        kite5 = colourDKG (darkblue,blend 0.9 red magenta, yellow) $
                 scale phi (decompositions [lkite `at` origin, rkite `at` origin] !!5)

-- |diagram of three coloured shapes in a row
exampleTriple1::Diagram B
exampleTriple1 =  hsep 0.1 $ lw thin $ rotations [0,0,2] $ fmap center threeColouredShapes

-- |diagram of three coloured shapes in an arc
exampleTriple2::Diagram B
exampleTriple2 =  position $ zip [ p2(-2.55,-0.75), p2(0.0,1.0), p2(2.0,-1.2)] $
                   lw thin $ rotations [1,0,1] $ fmap center threeColouredShapes

{-*
Swatches and Samples for colour-filled patches
-}

-- |A sample abbreviates triples of colours (used for Dart,Kite,Grout (Edges) respectively
type Sample = (Colour Double,Colour Double,Colour Double)

-- |The sample is used to fill a 6 times decomposed sun
drawSample:: Sample -> Diagram B
drawSample (c1,c2,c3) = colourDKG (c1,c2,c3) sun6 # lw thin

-- |sampleFig0 is a figure drawn with drawSample 
sampleFig0 :: Diagram B
sampleFig0 = drawSample (darkmagenta, indigo, gold)

-- |The sample is used to fill a 7 times decomposed sun
-- (so smaller kites and darts than drawSample).
drawSampleSmall:: Sample -> Diagram B
drawSampleSmall (c1,c2,c3) = colourDKG (c1,c2,c3) (suns!!7) # lw thin

-- |The sample is used to fill an 8 times decomposed sun
-- (so muuch smaller kites and darts than drawSample).
drawSampleTiny:: Sample -> Diagram B
drawSampleTiny (c1,c2,c3) = colourDKG (c1,c2,c3) (suns!!8) # lw ultraThin


-- |crop a diagram to half width and centred with A4 portrait dimensions.
-- Ideal for decomposed suns.
centreCropA4::Diagram B -> Diagram B
centreCropA4 d = clipTo a4 (d # centerXY) where -- clipped a4 ... seems slow
                  w = width d /2
                  h = w * 290/210
                  a4 = rect w h # centerXY

-- |Make an A4 diagram using sample colours with drawSampleSmall
a4Small :: Sample -> Diagram B
a4Small = centreCropA4 . drawSampleSmall

-- |Make an A4 diagram using sample colours with drawSampleTiny
a4Tiny :: Sample -> Diagram B
a4Tiny = centreCropA4 . drawSampleTiny

-- |diagram for black and white example using a4Tiny
blackAndWhite::Diagram B
blackAndWhite = a4Tiny (white,black, blend 0.5 black white)

-- |diagram for darkmagenta, indigo, gold example using a4Tiny
migA4Tiny::Diagram B
migA4Tiny = a4Tiny (darkmagenta, indigo, gold)

-- |diagram for darkblue,red ,yellow example using a4Small
bryA4Small::Diagram B
bryA4Small = a4Small (darkblue,blend 0.9 red magenta, yellow)

-- |A swatch is a list of samples which are used to fill sun5s by drawSwatch
type Swatch = [Sample]

-- |drawSwatch n sw produces a sun5 filled example for each sample in the swatch sw.
-- These are combined into a single diagram in rows of length n
drawSwatch:: Int -> Swatch -> Diagram B
drawSwatch n swatch = vsep 0.25 (hsep 0.25 . fmap sample <$> group n swatch) where
                     group n l = if length l <= n then [l] else take n l: group n (drop n l)
                     sample (c1,c2,c3) = colourDKG (c1,c2,c3) sun5 # lw ultraThin

-- |an example swatch
swatch0::Swatch
swatch0 = [(darken 0.7 darkmagenta, indigo, gold)
          ,(cyan, darkmagenta, gold)
          ,(teal, darkmagenta, gold)
          ,(deepskyblue, lemonchiffon, darkblue)
          ,(powderblue, peachpuff, mediumvioletred)
          ,(darkseagreen, darksalmon, darkviolet)
          ,(indigo, red, gold)  
          ,(darkgoldenrod, blue, blend 0.7 pink red)
          ,(darkgoldenrod, firebrick, wheat)
          ]

-- |figure for example swatch          
swatchFig0:: Diagram B          
swatchFig0 = drawSwatch 3 swatch0
