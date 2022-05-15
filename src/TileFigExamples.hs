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

-- |example of inflate in action with 5 chosen inflation steps from a left dart.
-- This shows inital and final piece together on the left,  
-- and 5 decomposition of the final piece on the right.
fiveInflate:: Diagram B
fiveInflate = hsep 1 $ fmap drawPatch [[ld,lk'], decompositions [lk'] !! 5] where -- two seperate patches
       ld  = ldart `at` origin
       lk  = inflate ld  !!1
       rk  = inflate lk  !!1
       rk' = inflate rk  !!2
       ld' = inflate rk' !!0
       lk' = inflate ld' !!1
-- fiveInflate = hsep 1 $ fmap drawPatch [[ld,lk'], multiDecomp 5 [lk']] where -- two seperate patches

-- |show just the first 5 alternatives of 4-fold inflations of a located piece
inflatefig:: Located Piece -> Diagram B
inflatefig lp = hsep 1 $ fmap (drawPatch . (:[lp])) $ take 5 $ inflations 4 lp

-- |diagram showing first five alternatives of 4-fold inflations of a right dart
fiveAlternatives:: Diagram B
fiveAlternatives = inflatefig (rdart `at` origin)

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


-- |showing three decomposed shapes with colouring for dart,kite and grout (edges)
-- arranged in an arc
threeShapesSample::Diagram B
threeShapesSample = lw thin $
    position 
    [ (p2(0.0,1.0)  ,colourDKG (darken 0.7 darkmagenta, indigo, gold) sun4)
    , (p2(-3.0,0.0) ,colourDKG (goldenrod, darkturquoise, saddlebrown) star4)
    , (p2(1.4,0.0)  ,colourDKG (darkblue,blend 0.9 red magenta, yellow) kite5)
    ] where
        sun4 = suns!!4
        star4 = decompositions TileLib.star !!4
        kite5 = scale phi (decompositions [lkite `at` origin, rkite `at` origin] !!5)

-- |showing three decomposed shapes with colouring for dart,kite and grout (edges)
-- arranged in row
threeColourFilled::Diagram B
threeColourFilled = lw thin $
    position 
    [ (p2(0.0,0.0)  ,colourDKG (darken 0.7 darkmagenta, indigo, gold) sun4)
    , (p2(-3.0,0.0) ,colourDKG (goldenrod, darkturquoise, saddlebrown) star4)
    , (p2(3.2, -1.4)  ,rotate (90 @@ deg) $ colourDKG (darkblue,blend 0.9 red magenta, yellow) kite5)
    ] where
        sun4 = suns!!4
        star4 = decompositions TileLib.star !!4
        kite5 = scale phi (decompositions [lkite `at` origin, rkite `at` origin] !!5)



{-*
Swatches and Samples for colour-filled patches
-}

{-|  
A swatch is a list of triples of colours which are used to fill sun5s 
The order is dart fill colour, kite fill colour, grout (=edge) colour.
-}
type Swatch = [(Colour Double,Colour Double,Colour Double)]

{-|
drawSwatch n sw produces a diagram from a swatch sw where n is used to indicate how many samples in a row
(best displayed at 800).
-}
drawSwatch:: Int -> Swatch -> Diagram B
drawSwatch n swatch = vsep 0.25 (hsep 0.25 . fmap sample <$> group n swatch) where
                     group n l = if length l <= n then [l] else take n l: group n (drop n l)
                     sample (c1,c2,c3) = colourDKG (c1,c2,c3) sun5 # lw ultraThin

-- |a sample is similarly a triple of colours  for a single sun6
-- (best displayed at 800).
drawSample:: (Colour Double,Colour Double,Colour Double) -> Diagram B
drawSample (c1,c2,c3) = colourDKG (c1,c2,c3) sun6 # lw thin


-- |a large sample is similarly a triple of colours  for a single sun7
-- (best displayed at 1000)
drawLargeSample:: (Colour Double,Colour Double,Colour Double) -> Diagram B
drawLargeSample (c1,c2,c3) = colourDKG (c1,c2,c3) (suns!!7) # lw ultraThin

-- |diagram of black and white large sample
blackAndWhite::Diagram B
blackAndWhite = drawLargeSample (white,black, blend 0.5 black white)

-- |sample coloured tile diagrams (using decomposed suns)
sL1,sL2,sL3,news1,s7,s8,s9,s1,s2,s3,s4,s5,s6:: Diagram B                            
sL1 = drawLargeSample (darkmagenta, indigo, gold)
sL2 = drawLargeSample (goldenrod, darkturquoise, saddlebrown)
sL3 = drawLargeSample (darkblue,blend 0.9 red magenta, yellow) 
news1 = drawSample (darken 0.7 darkmagenta, indigo, gold)
s7 = drawSample (indigo, red, gold)  
s8 = drawSample (darkgoldenrod, blue, blend 0.7 pink red)
s9 = drawSample (darkgoldenrod, firebrick, wheat) 

s1 = drawSample (darkmagenta, indigo, gold)
s2 = drawSample (cyan, darkmagenta, gold)
s3 = drawSample (teal, darkmagenta, gold)
s4 = drawSample (deepskyblue, lemonchiffon, darkblue)
s5 = drawSample (powderblue, peachpuff, mediumvioletred)
s6 = drawSample (darkseagreen, darksalmon, darkviolet)

-- |example swatches
coasterSwatch,darkM,bluegreens,blues,pinks,pastel,greens,reds,swatch3,swatch2,swatch1:: Swatch
coasterSwatch = [(deepskyblue, lemonchiffon, darkblue)
                ,(darken 0.7 darkmagenta, indigo, gold)
                ,(darkblue,blend 0.9 red magenta, yellow)
                ,(indigo, red, gold)

                ,(powderblue, peachpuff, mediumvioletred)
                ,(darkgoldenrod, blue, blend 0.7 pink red)
                ,(cyan, darkmagenta, gold)
                ,(fuchsia, aquamarine, blue)
                
                ,(darkseagreen, darksalmon, darkviolet) 
                ,(darkgoldenrod, firebrick, wheat)               
                ,(blend 0.3 green palegreen, violet, white) --darkviolet)
                ,(blue, violet, yellow)            
--                ,(darkblue, teal, wheat)
--                ,(aquamarine, lavender, crimson)
                ]
                
darkM  = [(darkmagenta, indigo, gold)
         ,(darkblue, darkmagenta, orange)
         ,(teal, darkmagenta, orange)
         ,(teal, darkmagenta, gold)
         ,(cyan, darkmagenta, gold)
         ,(goldenrod, darkmagenta, antiquewhite)
         ]
bluegreens
       = [(darkblue, teal, wheat)
         ,(darkcyan, darkblue, wheat)
         ,(blend 0.6 cyan indigo, indigo, yellow)
         ,(darkgreen, slateblue, turquoise)  --cyan)                          
         ,(indigo, darkgreen, orange)  
         ,(darkblue, green, orange) --,(blue, green, violet)--,(blue, green, orange) -- 
         ,(seagreen, slateblue, lime) 
         ,(cornflowerblue, turquoise, darkblue)         
         ]
blues =  [(coral, darkblue, wheat)
         ,(darkgoldenrod, blue, blend 0.7 pink red)
         ,(darkturquoise, blue, mintcream)
         ,(goldenrod, deepskyblue, indigo)
         ,(cornflowerblue, cyan, darkblue)         
         ]
pinks  = [(hotpink, blueviolet, white)
         ,(blue, violet, yellow)           
         ,(fuchsia, blue, yellow)
         ,(fuchsia, aquamarine, blue)
         ]
pastel = [(darkseagreen, darksalmon, darkviolet)         
         ,(aquamarine, lavender, crimson)         
         ,(deepskyblue, lemonchiffon, darkblue) --royalblue)
         ,(indianred, lemonchiffon, indigo) --black)
         ,(powderblue, peachpuff, mediumvioletred)         
         ,(lightyellow, deepskyblue, indigo)
         ]
greens = [(blend 0.7 yellow saddlebrown, blend 0.7 lime blue, saddlebrown)          
         ,(goldenrod, lime, saddlebrown) 
         ,(goldenrod, blend 0.7 lime blue, saddlebrown)
--         ,(lightyellow, turquoise, saddlebrown)
         ]
reds   = [(indigo, red, gold)
         ,(firebrick, darkblue, gold)  --,(firebrick, blueviolet, gold) 
         ,(darkgoldenrod, firebrick, wheat)
         ,(red, gold, indigo) 
         ]


swatch3 = -- dart, kite, grout
         [(darkmagenta, indigo, gold)
         ,(darkblue, darkmagenta, orange)
         ,(teal, darkmagenta, orange)
         ,(teal, darkmagenta, gold)
         ,(darkcyan, indigo, lightsalmon) --peru)
         ,(darkblue, teal, wheat)
         ,(darkblue, teal, sandybrown)
         ,(darkcyan, indigo, peachpuff)
         ,(blend 0.6 cyan indigo, indigo, peachpuff)--,(darken 0.7 cyan, indigo, peachpuff)
         ,(blue, violet, yellow)           
         ,(firebrick, darkblue, gold)  --,(firebrick, blueviolet, gold) 
         ,(darkblue, green, orange) --,(blue, green, violet)--,(blue, green, orange) -- 
         ] 

swatch2 = -- dart, kite, grout
         [(hotpink, blueviolet, white) 
         ,(red, yellow, indigo) 
         ,(cyan, darkmagenta, gold)
         ,(darkmagenta, indigo, gold) 
         ,(goldenrod, darkturquoise, saddlebrown) 
         ,(goldenrod, limegreen, saddlebrown) 
         ,(lime, blue, white)
         ,(blend 0.6 yellow gold, blend 0.5 green turquoise, saddlebrown)          
         ,(fuchsia, aquamarine, blue)
         ,(indigo, red, gold)         
         ,(goldenrod, lime, saddlebrown) 
         ,(cornflowerblue, chartreuse, blue)         
         ]        
swatch1 =  -- dart, kite, grout                 
         [(fuchsia, deepskyblue, blue)
         ,(deepskyblue, lemonchiffon, darkblue) --royalblue)
         ,(indianred, lemonchiffon, indigo) --black)
         ,(powderblue, peachpuff, mediumvioletred)         
         ,(cyan, teal, plum)
         ,(lavenderblush, teal, plum)
         ,(indigo, darkgreen, orange)  
         ,(darkgreen, slateblue, cyan) -- orange)                         
         ,(darkseagreen, darksalmon, darkviolet)         
         ,(aquamarine, lavender, crimson)         
         ,(lightcoral, indigo, lightblue) -- orange) --lightcyan)
         ,(darkgreen, orangered, yellow) --black)                
         ]



