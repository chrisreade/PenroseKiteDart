{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module TileFigExamples where
    
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import TileLib

-- | filled pieces and drawn edges of 4 pieces in a row         
thePieces =  [ldart, rdart, lkite, rkite]  
piecesFig = hsep 0.5 $ fmap (showOrigin . drawJPiece) thePieces 
piecesFig2 = hsep 1 $ fmap (fillDK' red blue) thePieces ++ fmap drawPiece thePieces 
   
-- | 4 decompositions in a column for each piece
fourDecomps = hsep 1 $ fmap decomps thePieces # lw thin where
         decomps pc = vsep 1 $ fmap drawPatch $ take 5 $ decompositions [pc `at` origin] 

-- | example of inflate in action (5 chosen inflate steps)
-- show inital and final piece together on left patch,  
-- and 5 decomposition of final piece in right patch
-- fiveInflate = hsep 1 $ fmap drawPatch [[ld,lk'], multiDecomp 5 [lk']] where -- two seperate patches
fiveInflate = hsep 1 $ fmap drawPatch [[ld,lk'], decompositions [lk'] !! 5] where -- two seperate patches
       ld  = ldart `at` origin
       lk  = inflate ld  !!1
       rk  = inflate lk  !!1
       rk' = inflate rk  !!2
       ld' = inflate rk' !!0
       lk' = inflate ld' !!1

-- | example of first  5 alternatives of 4-fold inflations of lp (located piece)
inflatefig lp = hsep 1 $ fmap drawPatch $ fmap (:[lp]) $ take 5 $ inflations 4 lp
fiveAlternatives = inflatefig (rdart `at` origin)

-- | Decomposed suns
suns = decompositions sun
sun6 = suns!!6
sun5 = suns!!5 

sun6Fig = drawPatch sun6 # lw thin

-- | overlaying sun5 in red atop sun6
sun5Over6Fig = (drawPatch sun5 # lc red # dashingN [0.003,0.003] 0 <> drawPatch sun6) # lw thin
-- | Using experiment (defined in Tilelib) on sun6 clearly illustrates the embedded sun5
experimentFig = patchWith experiment sun6 # lw thin
-- | similarly experiment on sun5
twoLevels = patchWith experiment sun5 # lw thin
-- | figure showing origins and markings on tiles
markedTiles = hsep 1  
        [ kiteDiag # showOrigin 
        , dartDiag # showOrigin 
        , kiteDiag <> (pL ~~ pR # lc red # lw thick) 
        , dartDiag <> (origin ~~ p2(1,0) # lc red # lw thick)
        ] where kiteDiag = drawPatch [lkite `at` origin, rkite `at` origin]
                dartDiag = drawPatch [ldart `at` origin, rdart `at` origin]
                pL = origin .+^ phi*^rotate (ttangle 1) unitX
                pR = origin .+^ phi*^rotate (ttangle 9) unitX

label l p = baselineText l # fontSize (local 0.2) # fc blue # moveTo p

newPiecesFig = pad 1.2 $ centerXY $
               label "RD" (p2 (negate 0.4,0.7)) <>
               label "LD" (p2 (0.5,0.7)) <>
               label "LK" (p2 (2.1,1.0)) <>
               label "RK" (p2 (3.1,1.0)) <>
               hsep 0.3 (fmap (rotate (90 @@ deg) . showOrigin . dashJPiece) 
                         [rdart,ldart,lkite,rkite]
                        )
                          
dartWingFig = pad 1.2 $ hsep 1 [dkite, ddart] where
  ddart = showOrigin (translate unit_X $ dashJPatch  $ decompose [ldart `at` origin, rdart `at` origin])
  dkite = showOrigin (translate unit_X $ dashJPatch  $ decompose [lkite `at` origin, rkite `at` origin])

-- colour-filled examples

-- | using fillDK'
filledSun6 = patchWith (fillDK' red blue) sun6 # lw ultraThin
-- | using fillDK
newFillSun6 = patchWith (fillDK darkmagenta indigo) sun6 # lw ultraThin # lc gold

-- | A swatch is a list of triples of colours which are used to fill sun5s (dart, kite, grout)
-- showSwatch produces a diagram from a swatch where n is used to indicate how many samples in a row
-- display at 800
showSwatch n swatch = vsep 0.25 $ fmap (hsep 0.25 . fmap sample) $ group n swatch where
                     group n l = if length l <= n then [l] else take n l: group n (drop n l)
                     sample (c1,c2,c3) = patchWith (fillDK c1 c2) sun5 # lw ultraThin # lc c3

-- | a sample is similarly a triple of colours  for a single sun6
-- display at 800
showSample (c1,c2,c3) = patchWith (fillDK c1 c2) sun6 # lw thin # lc c3

-- | a large sample is similarly a triple of colours  for a single sun7
-- display at 1000
showLargeSample (c1,c2,c3) = patchWith (fillDK c1 c2) (suns!!7) # lw ultraThin # lc c3

sL1 = showLargeSample (darkmagenta, indigo, gold)
sL2 = showLargeSample (goldenrod, darkturquoise, saddlebrown)
sL3 = showLargeSample (darkblue,blend 0.9 red magenta, yellow) 
news1 = showSample (darken 0.7 darkmagenta, indigo, gold)
s7 = showSample (indigo, red, gold)  
s8 = showSample (darkgoldenrod, blue, blend 0.7 pink red)
s9 = showSample (darkgoldenrod, firebrick, wheat) 

s1 = showSample (darkmagenta, indigo, gold)
s2 = showSample (cyan, darkmagenta, gold)
s3 = showSample (teal, darkmagenta, gold)
s4 = showSample (deepskyblue, lemonchiffon, darkblue)
s5 = showSample (powderblue, peachpuff, mediumvioletred)
s6 = showSample (darkseagreen, darksalmon, darkviolet)

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



