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


{-*
Figures for pieces
-}

-- |a list of the four Pieces
thePieces :: [Piece]
thePieces =  [ldart, rdart, lkite, rkite]  
-- |drawn edges of 4 pieces in a row         
piecesFig :: Diagram B
piecesFig = hsep 0.5 $ fmap (showOrigin . dashjPiece) thePieces 
-- |filled 4 pieces in a row         
piecesFig2 = hsep 1 $ fmap (leftFillDK red blue) thePieces ++ fmap dashjPiece thePieces 


-- |figure showing origins and markings on tiles
markedTiles:: Diagram B
markedTiles = hsep 1  
        [ kiteDiag # showOrigin # centerXY # rotate (90@@deg)
        , dartDiag # showOrigin # centerXY  # rotate (270@@deg)
        , (kiteDiag <> (pL ~~ pR # lc red # lw thick)) # centerXY # rotate (90@@deg)
        , (dartDiag <> (origin ~~ p2(1,0) # lc red # lw thick)) # centerXY # rotate (270@@deg)
        ] where kiteDiag = draw [lkite `at` origin, rkite `at` origin]
                dartDiag = draw [ldart `at` origin, rdart `at` origin]
                pL = origin .+^ phi*^rotate (ttangle 1) unitX
                pR = origin .+^ phi*^rotate (ttangle 9) unitX
{-
        [ kiteDiag # showOrigin 
        , dartDiag # showOrigin 
        , kiteDiag <> (pL ~~ pR # lc red # lw thick) 
        , dartDiag <> (origin ~~ p2(1,0) # lc red # lw thick)
        ] where kiteDiag = draw [lkite `at` origin, rkite `at` origin]
                dartDiag = draw [ldart `at` origin, rdart `at` origin]
                pL = origin .+^ phi*^rotate (ttangle 1) unitX
                pR = origin .+^ phi*^rotate (ttangle 9) unitX
-}

-- |another figure showing origins and markings on tiles
markedTiles2:: Diagram B
markedTiles2 = hsep 1  
        [ kiteDiag <> (pL ~~ pR # lc lime # lw thick) 
        , dartDiag <> (origin ~~ p2(1,0) # lc lime # lw thick)
        ] where kiteDiag = draw [lkite `at` origin, rkite `at` origin]
                dartDiag = draw [ldart `at` origin, rdart `at` origin]
                pL = origin .+^ phi*^rotate (ttangle 1) unitX
                pR = origin .+^ phi*^rotate (ttangle 9) unitX

-- |figure showing the four labelled pieces each with join edge and origin shown
newPiecesFig:: Diagram B
newPiecesFig = pad 1.2 $ centerXY $
               label "RD" (p2 (negate 0.4,0.7)) <>
               label "LD" (p2 (0.3,0.7)) <>
               label "LK" (p2 (1.65,1.0)) <>
               label "RK" (p2 (2.55,1.0)) <>
               hsep 0.1 (fmap (rotate (90 @@ deg) . showOrigin . dashjPiece) 
                         [rdart,ldart,lkite,rkite]
                        )
  where
    -- |Used for adding text at a point
    label l p = baselineText l # fontSize (local 0.2) # fc blue # moveTo p
    
-- |diagram combining markedTiles2 and newPiecesFig
tileIntro:: Diagram B
tileIntro = hsep 1 [markedTiles2, newPiecesFig]

{-*
Figures for decompositions and compChoices
-}

-- |figure showing 4 decompositions in a column for each of the four pieces
fourDecomps:: Diagram B
fourDecomps = hsep 1 $ fmap decomps thePieces # lw thin where
         decomps pc = vsep 1 $ fmap drawj $ take 5 $ decompositionsP [pc `at` origin] 

-- |example of compChoices in action with 5 chosen compChoices steps from a left dart.
-- This shows inital and final piece together on the left,  
-- and 5 decomposition of the final piece on the right.
fiveCompChoices:: Diagram B
fiveCompChoices = pad 1.1 $ hsep 1 $ fmap drawj [[ld,lk'], decompositionsP [lk'] !! 5] where
     -- two seperate patches
       ld  = ldart `at` origin
       lk  = compChoices ld  !!1
       rk  = compChoices lk  !!1
       rk' = compChoices rk  !!2
       ld' = compChoices rk' !!0
       lk' = compChoices ld' !!1

-- |diagram showing first five alternatives of 4-fold compNChoices of a right dart
fiveAlternatives:: Diagram B
fiveAlternatives = hsep 1 $ fmap (drawj . (:[lp])) $ take 5 $ compNChoices 4 lp where
                     lp = (rdart `at` origin)

-- |An infinite list of patches of increasingly decomposed sun
suns::[Patch]
suns = decompositionsP sun
sun5,sun6:: Patch
-- |a patch of a 6 times decomposed sun
sun6 = suns!!6
-- |a patch of a 5 times decomposed sun
sun5 = suns!!5 

-- |diagram for sun6
sun6Fig::Diagram B
sun6Fig = draw sun6 # lw thin

-- |diagram overlaying sun5 in red atop sun6
sun5Over6Fig::Diagram B
sun5Over6Fig = (draw sun5 # lc red # dashingN [0.003,0.003] 0 <> draw sun6) # lw thin
-- |Using experiment (defined in Tilelib) on sun6 clearly illustrates the embedded sun5
experimentFig::Diagram B
experimentFig = pad 1.1 $ lw thin $ drawWith experiment sun6
-- |Using experiment (defined in Tilelib) on sun4 clearly illustrates the embedded sun3
twoLevelsFig::Diagram B
twoLevelsFig = drawWith experiment (suns!!4)

-- |figure showing two types of dart wing vertices (largeKiteCentre, largeDartBase)                         
dartWingFig::Diagram B
dartWingFig = pad 1.2 $ hsep 1 [dkite, ddart] where
  ddart = showOrigin (translate unit_X $ drawj  $ decompPatch [ldart `at` origin, rdart `at` origin])
  dkite = showOrigin (translate unit_X $ drawj  $ decompPatch [lkite `at` origin, rkite `at` origin])

{-*
Colour-filled examples
-}

-- |using leftFillDK
filledSun6::Diagram B
filledSun6 = drawWith (leftFillDK red blue) sun6 # lw ultraThin
-- |using fillDK
newFillSun6::Diagram B
newFillSun6 = drawWith (fillDK darkmagenta indigo) sun6 # lw ultraThin # lc gold



-- |list of 3 diagrams for colour filled star,sun kite respectively
threeColouredShapes:: [Diagram B]
threeColouredShapes = [star4,sun4,kite5] where
        star4 = colourDKG (goldenrod, darkturquoise, saddlebrown) (decompositionsP TileLib.star !!4)
        sun4 = colourDKG (darken 0.7 darkmagenta, indigo, gold) (suns!!4)
        kite5 = colourDKG (darkblue,blend 0.9 red magenta, yellow) $
                 scale phi (decompositionsP [lkite `at` origin, rkite `at` origin] !!5)

-- |diagram of three coloured shapes in a row
exampleTriple1::Diagram B
exampleTriple1 =  hsep 0.1 $ lw thin $ rotations [0,0,2] $ fmap center threeColouredShapes

-- |diagram of three coloured shapes in an arc
exampleTriple2::Diagram B
exampleTriple2 =  position $ zip [ p2(-2.55,-0.75), p2(0.0,1.0), p2(2.0,-1.2)] $
                   lw thin $ rotations [1,0,1] $ fmap center threeColouredShapes

