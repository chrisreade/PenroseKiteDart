{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TupleSections             #-}

{-|
Module      : TestIllustrate
Description : Testing and Illustrative figures using Tgraphs
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

-}
module TestIllustrate where

-- used for testing
-- import qualified Data.IntMap.Strict as VMap (IntMap, lookup, insert, empty, fromList, union)

import Diagrams.TwoD.Vector (e) -- for decompExplainFig
import Diagrams.Prelude
import qualified Data.Set as Set  (null,toList,delete) -- used for contexts

import ChosenBackend (B)
import TileLib
import Tgraphs
import TgraphExamples

{-*
Illustrations not Using Tgraphs
-}

-- |a list of the four Pieces
thePieces :: [Piece]
thePieces =  [ldart, rdart, lkite, rkite]  
-- |drawn edges of 4 pieces in a row         
piecesFig :: Diagram B
piecesFig = hsep 0.5 $ fmap (showOrigin . dashjPiece) thePieces 
-- |filled 4 pieces in a row         
piecesFig2 :: Diagram B
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
                     lp = rdart `at` origin

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


-- |A figure illustrating decomposition of pieces (and composition choices) with vectors. 
-- (not using Tgraphs)        
decompExplainFig::Diagram B
decompExplainFig = pad 1.2 $ centerXY fig0 where
  fig0 =  mconcat[rkiteAt    $ p2 (2.0,3.5)
                 ,name "RK"  $ p2 (1.0,3.8)
                 ,drkiteAt   $ p2 (2.0,2.0)
                 ,name "RD"  $ p2 (1.5,2.1)
                 ,name "LK"  $ p2 (1.1,2.3)
                 ,name "RK"  $ p2 (0.6,2.3)
                 ,rdartAt    $ p2 (3.0,3.5)
                 ,name "RD"  $ p2 (3.7,3.8)
                 ,drdartAt   $ p2 (3.0,2.0)
                 ,name "RD"  $ p2 (4.0,2.5)
                 ,name "LK"  $ p2 (3.7,2.2)
                 ,srdartAt   $ p2 (2.3,0.5)
                 ,name "RD"  $ p2 (2.8,0.7)
                 ] # lw thin
                 <> arrowV (0.5 *^ unit_Y) # moveTo (p2(1.7,3.2))
                 <> arrowV (0.5 *^ unit_Y) # moveTo (p2(3.3,3.2))
                 <> name "DECOMPOSITION" (p2(2.1,3.0))
                 <> arrowV (0.3 *^ (unitY^+^unitX)) # moveTo (p2(2.5,1.0))
                 <> arrowV (0.3 *^ (unitY^+^unit_X)) # moveTo (p2(2.5,1.0))
                 <> name "COMPOSE CHOICES (FOR RD)" (p2(1.8,1.5))

  rkiteAt p = mconcat [dotAt p blue
                      ,labelEdge "(v')" p point1
                      ,labelVect "vk" p point0
                      ,point1 ~~ point0
                      ,labelP "p" p
                      ] where point1 = p .+^ (phi*^ e (ttangle 4))
                              point0 = p .+^ (phi*^ e (ttangle 5))
                            
  drkiteAt p = mconcat [dotAt p blue
                       ,labelVect "vd" p point3
                       ,p ~~ point2
                       ,point1 ~~ point0
                       ,dotAt point1 blue
                       ,labelVect "vk'" point1 point2
                       ,point3 ~~ point2
                       ,point0 ~~ point2
                       ,point1 ~~ point3
                       ,labelP "p" p
                       ,basetext "p .+^ v'" (point1 .+^ (0.1 *^ unitX)) # fc blue
--                       ,labelP "p .+^ v'" (point1 .+^ (0.1 *^ (unitX ^+^ unitY)))
                       ] where point1 = p .+^ (phi*^ e (ttangle 4))
                               point0 = p .+^ (phi*^ e (ttangle 5))
                               point2 = p .+^ ((1/phi) *^ (point0 .-. p))
                               point3 = p .+^ ((1/phi^2) *^ (point1 .-. p))

  rdartAt p = mconcat [dotAt p blue
                      ,labelVect "vd" p point1
                      ,labelEdge "" p point0
                      ,label "(v')"  (p .+^ (0.5 *^ (point0 .-. p)) ^+^ 0.1 *^ unit_X)
--                      ,altlabel "(v')"  (p .+^ (0.5 *^ (point0 .-. p)))
                      ,point0 ~~ point1
                      ,labelP "p" p
                      ] where point1 = p .+^ unitX
                              point0 = p .+^ (phi*^ e (ttangle 1))
                            
  drdartAt p = mconcat [dotAt p blue
                       ,labelVect "vk = vd" p point1
                       ,p ~~ point2
                       ,dotAt point0 blue
                       ,point0 ~~ point1
                       ,point1 ~~ point2
                       ,labelVect "" point0 point2        -- vd'  
                       ,label "vd'"  (p .+^ (1.3 *^ (point2 .-. p) ^+^ 0.1 *^ unit_X))   
--                       ,altlabel "vd'"  (p .+^ (1.3 *^ (point2 .-. p)))   
                       ,labelP "p" p
                       ,basetext "p .+^ v'" (point0 .+^ (0.1 *^ unitX)) # fc blue
                       ] where point1 = p .+^ unitX
                               point0 = p .+^ (phi*^ e (ttangle 1))
                               point2 = p .+^ e (ttangle 1)

  srdartAt p = mconcat [dotAt p blue
                       ,labelVect "vd" p point1
                       ,p ~~ point0
                       ,point0 ~~ point1
                       ,labelP "p" p
                       ] where point1 = p .+^ ((phi-1)*^unitX)
                               point0 = p .+^ e (ttangle 1)

  dotAt p c = circle 0.02 # lw none # fc c # moveTo p

  basetext l p = baselineText l # fontSize (local 0.1) # moveTo p
  label l p = basetext l (p .+^ 0.05 *^ unitY) # fc green
  labelP l p = topLeftText l # fontSize (local 0.1) # fc blue # moveTo p
  name l p  = basetext l p # fc red

  labelEdge l p p' = edgeArrow p p' <> label l p'' where p'' =  p .+^ (0.5 *^ (p' .-. p))
  labelVect l p p' = labelEdge l p p' # dashingG [0.03, 0.03] 0
  edgeArrow = arrowBetween' (with & headLength .~ small )


-- |Diagram showing decomposition of (left hand) half-tiles.
decompHalfTiles :: Diagram B
decompHalfTiles = padBorder $ lw thin $ vsep 1 $ fmap centerX
   [ addArrow  "d" "decd" [ scale phi $ labelLarge drawj d
               , labelLarge drawj $ decompose d
               ]
   , addArrow  "k" "deck" [ scale phi $ labelLarge drawj k
               , labelLarge drawj $ rotate (ttangle 1) $ makeVP $ decompose k
               ]
   ]
    where d = makeTgraph [LD (1,2,3)]
          k = makeTgraph [LK (1,2,3)]
          addArrow s1 s2 [a,b] = decompArrow s1 s2 $ hsep 2
                           [named s1 $ centerXY a, named s2 $ centerXY b]

-- | diagram illustrating compose, force, and decompose with kinGraph
cdfIllustrate:: Diagram B
cdfIllustrate = position (zip [p2 (0,0), p2 (0,7), p2 (10,0), p2 (10, -12)]
                              [k,dk,fk,cfk]) 
                  # decompArrow "k" "dk"
                  # forceArrow "k" "fk"
                  # composeArrow "fk" "cfk"
                  # labelAt (p2 (0.5,4)) "decompose"
                  # labelAt (p2 (2.5, -1)) "force"
                  # labelAt (p2 (7,-7)) "compose"
                  # lw thin
                  # padBorder
      where fKing = force kingGraph
            k = labelled draw kingGraph # named "k"
            fk = labelled draw fKing # named "fk"
            dk = smart (labelled draw) (decompose kingGraph) # scale (phi-1) # named "dk"
            cfk = rotateBefore (labelled draw) (ttangle 9) (compose fKing) # scale phi # named "cfk"


{-*
Illustrations Using Tgraphs
-}


{-|
This example illustrates that an experimental version of composition (composeK)
which defaults to kites when there are choices (unknowns) is erroneous (does not preserve correctness).
The left Tgraph is force queenGraph and is correct.
The middle Tgraph is the result of applying composeK to the left Tgraph and is an incorrect Tgraph (fails on forcing).
On the right is our usual composition with remainder faces shown green.

Note that both compose and composeK applied to the middle Tgraph produce the mistake1 Tgraph (also incorrect).
-}
counterK :: Diagram B
counterK = padBorder $ lw thin $ hsep 1 $
           rotations [0,6] (phiScales $ fmap drawj [g,composeK g]) ++ [drawPCompose g]
        where g = force queenGraph




-- |diagram illustrating touching vertex situation and forced result.
-- The faces shown in lime are removed from a twice decomposed sun.
-- These are reconstructed by force (with other additional faces). The touching vertex restriction blocks
-- the bottom 4 face additions initially. 
touchingIllustration::  Diagram B
touchingIllustration =
  padBorder $ lw thin $ hsep 1
    [ labelled drawj vpLeft <> (drawj vpGone # lc lime)
    , alignBefore (labelled drawj) (8,3) $ force touchGraph
    ] where
      touchGraph = graphFromVP vpLeft
      vpLeft = removeFacesVP deleted vp
      vpGone = selectFacesVP deleted vp
      vp = makeVP sunD2
      sunD2 = sunDs!!2
      deleted = filter ((==1).originV) (faces sunD2) ++
                [LD (29,41,31),RK (31,79,29),LK (10,29,79),RK (10,79,75)]

-- |faces removed from foolD to illustrate crossing boundary and non tile-connected faces.
-- There is a crossing boundary at 4 in the first case (but the faces are still tile-connected),
-- There is a crossing boundary at 11 in the second case and the faces are not tile-connected.
crossingBdryFig :: Diagram B
crossingBdryFig = padBorder $ hsep 1 [d1,d2]
       where d1 = labelled drawj $ removeFacesVP [RK (3,11,13), LK (3,13,15), RK (3,15,4)] $ makeVP foolD
             d2 = labelled drawj $ removeFacesVP [RK (5,11,2), LD (6,13,11), RD (6,15,13), LD (6,17,15)] $ makeVP foolD

-- |figure showing mistake Tgraph and the point at which forcing fails                
pfMistakeFig :: Diagram B
pfMistakeFig  = padBorder $ hsep 1 [labelled drawj mistake, labelled drawj partForcedMistake] where
   partForcedMistake = makeTgraph
                       [RK (9,1,11),LK (9,10,7),RK (9,7,5),LK (9,5,1),RK (1,2,4)
                       ,LK (1,3,2),RD (3,1,5),LD (4,6,1),LD (3,5,7),RD (4,8,6)
                       ]

-- |decompose mistake and the point at which forcing fails  with  RK (6,26,1)              
forcingDmistakeFig :: Diagram B
forcingDmistakeFig = padBorder $ hsep 1 [labelled drawj (decompose mistake), labelled drawj part] where
    part = makeTgraph
             [RK (26,24,1),RK (5,24,25),LK (5,1,24),RK (3,23,2),LK (3,22,23)
             ,RK (3,21,22),LK (3,15,21),LK (4,2,20),RK (4,20,19),LK (4,19,18),RK (4,18,17)
             ,LK (4,17,16),RK (4,16,12),LD (8,12,16),RK (3,14,15),LK (3,11,14),RD (7,14,11)
             ,RK (4,13,2),LK (4,9,13),RD (1,13,9),LK (3,2,13),RK (3,13,10),LD (1,10,13)
             ,LK (3,10,5),RD (1,5,10),RK (4,6,9),LD (1,9,6),RK (3,5,11),LD (7,11,5)
             ,LK (4,12,6),RD (8,6,12)
             ]

{-|  forcingD2mistakeFig
    Figure showing a stuck graph with error at vertex 35 
    This is involves a twice decomposed mistake which fails when forced.
    The figure shows the graph when the error is discovered.
-}
forcingD2mistakeFig :: Diagram B
forcingD2mistakeFig = padBorder $ labelled drawj partF where
  partF = makeTgraph
            [LK (78,46,35),LK (78,47,45),RK (78,45,46),LK (7,77,73),RK (7,76,77),LK (7,75,76),RK (7,74,75)
            ,LK (7,47,74),RK (7,73,43),LD (44,43,73),RK (8,72,67),LK (8,71,72),RK (8,70,71),LK (8,69,70)
            ,RK (8,68,69),LK (8,42,68),RD (49,68,42),RK (62,40,67),LK (8,67,40),RD (66,35,39),LD (66,39,38)
            ,RD (66,38,65),LK (63,65,38),LD (37,64,63),RD (37,62,64),RK (63,38,37),LK (62,41,40),LK (62,37,36)
            ,RK (62,36,41),LK (30,61,59),RK (30,60,61),LK (30,58,60),LK (48,4,59),RK (30,59,4),RD (58,30,34)
            ,LD (58,34,33),RD (58,33,57),LK (55,57,33),LD (32,56,55),RD (32,54,56),RK (55,33,32),LK (54,53,52)
            ,LK (54,32,31),RK (54,31,53),LD (3,53,31),RD (3,52,53),RK (50,52,51),LD (3,51,52),RD (3,29,51)
            ,LK (50,51,29),RK (50,29,44),LD (49,42,12),RK (12,48,49),LK (12,23,48),RD (4,48,23),RK (7,45,47)
            ,LD (5,46,45),RD (5,35,46),LK (7,22,45),RD (5,45,22),LK (11,44,29),RD (44,11,43),LK (7,43,11)
            ,RK (8,12,42),LD (6,41,36),RD (6,40,41),RK (8,40,24),LD (6,24,40),LK (1,39,35),RK (1,38,39)
            ,LK (1,37,38),RK (1,36,37),RD (6,36,20),LK (1,20,36),RK (1,35,19),LD (5,19,35),LK (2,34,30)
            ,RK (2,33,34),LK (2,32,33),RK (2,31,32),RD (3,31,17),LK (2,17,31),RK (2,30,14),LD (4,14,30)
            ,RK (11,29,21),LD (3,21,29),RK (2,25,13),LK (2,14,25),RD (4,25,14),LK (9,13,25),RK (9,25,15)
            ,LD (4,15,25),LK (1,16,9),RD (13,9,16),LK (2,13,26),RK (2,26,17),LD (3,17,26),RK (10,26,13)
            ,LK (10,18,26),RD (3,26,18),RK (1,10,16),LD (13,16,10),LK (10,5,27),RK (10,27,18),LD (3,18,27)
            ,LK (1,19,10),RD (5,10,19),RK (9,28,6),LK (9,15,28),RD (4,28,15),RK (1,9,20),LD (6,20,9)
            ,RK (11,27,5),LK (11,21,27),RD (3,27,21),RK (7,11,22),LD (5,22,11),LK (12,6,28),RK (12,28,23)
            ,LD (4,23,28),LK (8,24,12),RD (6,12,24)
            ]


-- |partially forced mistake1 (at the point of discovery of incorrect graph
partFMistake1Fig:: Diagram B
partFMistake1Fig = padBorder $ labelled drawj partF where
  partF = makeTgraph [RK (8,1,6),LK (7,5,1),RK (1,2,4),LK (1,3,2),RD (3,1,5),LD (4,6,1)]

-- |decomposed mistake1 is no longer incorrect and can be forced and recomposed
cdMistake1Fig :: Diagram B
cdMistake1Fig = padBorder $ hsep 1 $ fmap (labelled drawj) $ scales [phi,1,1,phi] $ alignAll (1,2) $ fmap makeVP
               [ mistake1 , mistake1D, force mistake1D, compose mistake1D]
               where mistake1D = decompose mistake1

-- |Diagram showing where a (4 times) decomposed version of mistake goes wrong.
-- It shows (top) mistake4 = 4 times decomposed mistake Tgraph - in incorrect Tgraph which fails on forcing.
-- (centre) mistake4' = 4 times decomposed [mistake Tgraph with a half dart removed],
-- (bottom) a forced mistake4' with the common faces of mistake4 emphasised.
-- Thus mistake4' (where the half dart is removed) does not go wrong on forcing and
-- the incorrect mistake4 clashes only near the wing tip of the removed half dart.
mistake4Explore :: Diagram B
mistake4Explore = padBorder $ lw ultraThin $ vsep 1
                [ smart (labelSmall draw) mistake4
                , smart (labelSmall draw) mistake4'
                , drawCommonFaces (force mistake4',(1,41)) (mistake4,(1,46))
                ] where
                   mistake' = removeFaces [RD (4,8,6)] mistake
                   mistake4 = decompositions mistake !!4
                   mistake4' = decompositions mistake' !!4

{-| relatedVType0 lays out figures from forceVFigures plus a kite as single diagram with 3 columns
    without arrows (used by relatedVTypeFig which adds arrows).
-}
relatedVType0:: Diagram B
relatedVType0 = lw thin $
 atPoints [p2 (0,20),p2 (0,12),  p2 (8,20),p2 (8,12),p2 (8,1),  p2 (18,20),p2 (18,12),p2 (18,1) ]
          [sunF,    starF,      aceF,    jackF,    kingF,     kite,     deuceF,   queenF]
 where kite   = labelAt (p2 (-4,2)) "force kite (= kite)" $ named "kite" $ center $ lc red $ draw kiteGraph
       sunF   = labelAt (p2 (-4,2)) "force sun (= sun)" $ named "sunF" $ forceVFigures!!0
       starF  = labelAt (p2 (-4,2.1)) "force star" $ named "starF" $ forceVFigures!!1
       jackF  = labelAt (p2 (-4,2.1)) "force jack" $ named "jackF" $ forceVFigures!!2
       queenF = labelAt (p2 (-4,4)) "force queen" $ named "queenF" $ forceVFigures!!3
       kingF  = labelAt (p2 (-4,4)) "force king" $ named "kingF" $ forceVFigures!!4
       aceF   = labelAt (p2 (-4,2)) "force ace (= ace)" $ named "aceF" $ forceVFigures!!5
       deuceF = labelAt (p2 (-4,2.1)) "force deuce" $ named "deuceF" $ forceVFigures!!6

{-| relatedVTypeFig lays out figures from forceVFigures plus a kite as a single diagram with 3 columns
    showing relationships - forceDecomp (blue arrows) and compose (green arrows)
 -}
relatedVTypeFig:: Diagram B
relatedVTypeFig = (key # rotate (90@@deg) # moveTo (p2 (-10,-10))) <>  mainfig where
  mainfig = padBorder relatedVType0
    # forceDecArrow "sunF" "starF"
    # composeArcUp "starF" "sunF"
    # forceDecArrow "aceF" "jackF"
    # composeArcUp "jackF" "aceF"
    # forceDecArrow "kite" "deuceF"
    # composeArcUp "deuceF" "kite"
    # forceDecArrow "jackF" "kingF"
    # composeArcUp "kingF" "jackF"
    # forceDecArrow "deuceF" "queenF"
    # composeArcUp "queenF" "deuceF"
  key = (a|||box|||b)
         # labelAt (p2 (1,-1)) "force . decompose"
         # labelAt (p2 (2,1.3)) "compose"
         # forceDecArrow "B" "A"
         # composeArcRight "A" "B" where
            box = rect 7 3.8
            a = named "A" (rect 0 0.1 # lw none)
            b = named "B" (rect 0 0.1 # lw none)

-- | Diagram illustrating force rules with 13 figures.
-- The yellow half-tile is the one being added in each case.
-- Mirror symmetric versions are omitted.
forceRules:: Diagram B
forceRules = padBorder $ lw thin $ vsep 1 $ fmap (hsep 1) $ chunks 5 $ fmap drawRule rules where
  rules = [ (  [LD (1,2,3)],                               RD (1,4,2)  )
          , (  [RK (1,2,3)],                               LK (1,4,2)  )
          , (  [LD (1,2,3)],                               RK (4,3,2)  )
          , (  [LK (1,2,3), RD (2,1,4)],                    RK (5,3,2)  )
          , (  [LK (1,3,2), RK (1,2,4), RK (5,2,3)],         LD (6,4,2)  )
          , (  [LK (1,2,3), RK (1,4,2), LK (5,2,4)],         RK (6,3,2)  )
          , (  [LK (1,2,3), RK (1,4,2), LK (5,2,4), RK (6,3,2)],          RD (2,5,7)  )
          , (  [LK (1,2,3),RD (2,1,4),LD (2,4,5),RD (2,5,7),LD (2,7,8)],   RD (2,8,9) )
          , (  [LK (1,4,2),RK (1,2,3),RD (5,2,4),LD (6,3,2)],             LK (2,5,7) )
          , (  [LK (1,3,2), RK (1,2,4), RD (5,2,3), LK (2,5,6)],          LD (7,4,2)  )
          , (  [RK (2,1,3),LK (2,4,1),RK (2,5,4),LK (2,6,5),RK (2,7,6),LK (2,8,7)],   RK (2,9,8) )
          , (  [RK (2,1,3),LK (2,4,1),RK (2,5,4),LK (2,6,5),RK (2,7,6),LK (2,8,7),RK (2,9,8),LK (2,10,9)],   RK (2,11,10) )
          , (  [LD (2,1,3),RD (2,10,1),LD (2,8,9),RD (2,7,8),LD (2,6,7),RD (2,5,6),LD (2,4,5),RD (2,3,4)],   LD (2,11,10) )
          ]
  drawRule (fs,f) = drawWith (fillPiece yellow) vpf <> drawj vp where
    vp = makeAlignedVP (1,2) $ makeTgraph (f:fs)
    vpf = subVP vp [f]



-- |coverForceRules shows cases for a proof that 
-- g' is a perfect composition => 𝚏𝚘𝚛𝚌𝚎 g' is included in (compose.force.decompose) g'
-- Each line has the form  g', force g', force (decompose g'), compose(force (decompose g'))
-- (An empty Tgraph is represented as an orange circle with diagonal line through it)
-- We note that in each line the second and fourth are the same iff the first is a perfect composition.
-- (We only need to show the second is included in the fourth, but a later proof establishes they will indeed be the same)
-- The left hand columns are perfect composition cases, the right hand columns are non-cases i.e not perfect compositions.
-- The perfect composition cases cover each force rule with (alternative) additions to make a perfect composition setting. 
coverForceRules:: Diagram B
coverForceRules = pad 1.05 $ centerXY $ lw ultraThin $ hsep 10
                     [ vsep 1 $ fmap expandLine lines1
                     , vsep 1 $ fmap expandLine lines2
                     ] where
 lines1 = [ [RD (1,2,3),LD (1,3,4)]
          , [LK (1,2,3),RK (1,3,4)]
          , [LK (1,2,3),RD (5,3,2)]
          , [LK (1,2,3),RK (1,3,4),RD (5,3,2)]
          , [LK (1,2,3),RK (4,3,2),LK (4,5,3)]
          , [LK (1,2,3),RD (2,1,4),LD (2,4,5),RK (1,3,6)]
          , [LK (1,2,3),RD (2,1,4),LK (5,4,1),RK (1,3,6)]
          , [LK (1,2,3),RK (1,3,6),RD (2,1,4),LK (5,4,1),RK (5,1,7)]
          , [LK (1,2,3),RK (4,3,2),RK (1,3,5),LK (4,6,3)]
          , [LK (1,2,3),RK (1,3,4),RD (5,3,2),LD (6,4,3)]
          , [RK (1,2,8),LK (1,3,2),RK (1,4,3),LK (1,5,4),RK (1,6,5),LK (1,7,6)]
          , [LD (1,2,3),RD (1,11,2),LD (1,8,9),RD (1,7,8),LD (1,6,7),RD (1,5,6),LD (1,4,5),RD (1,3,4)]
          , [LK (1,2,3),RD (2,1,4),LD (2,4,5),RK (1,3,6),RD (2,5,7),LD (2,7,8)]
          , [LK (1,2,3),RK (4,3,2),RK (1,3,5),LK (4,6,3),RK (1,7,2),LK (1,8,7)]
          , [LK (1,2,3),RK (4,3,2),RK (1,3,5),RK (1,7,2),LK (1,8,7)]
          , [LK (1,2,3),RK (4,3,2),LK (4,6,3),RK (1,7,2),LK (1,8,7)]
          , [LK (1,2,3),RK (4,3,2),RK (1,3,5),LK (4,6,3),RK (1,7,2),LK (1,8,7),LK (9,2,7)]
          , [LK (1,2,3),RK (4,3,2),RK (1,3,5),LK (4,6,3),RK (1,7,2),LK (1,8,7),LK (9,2,7),RK (9,7,10)]
          ]
 lines2 = [ [RD (1,2,3)]
          , [LK (1,2,3)]
          , [RD (1,2,3),LD (1,4,2)]
          , [LK (1,2,3),RD (2,1,4)]
          , [LK (1,2,3),RD (2,1,4),LK (5,4,1)]
          , [LK (1,2,3),RD (2,1,4),LD (2,4,5)]
          , [RK (1,2,11),LK (1,3,2),RK (1,4,3),LK (1,9,8),RK (1,10,9),LK (1,11,10)]
          , [LD (1,2,3),RD (1,11,2),LD (1,10,11),RD (1,7,8),LD (1,6,7),RD (1,5,6),LD (1,4,5),RD (1,3,4)]
          , [LK (1,2,3),RK (4,3,2),RK (1,3,5),LK (4,6,3),RK (1,7,2)]
          , [LK (1,2,3),RK (4,3,2),RK (1,3,5),LK (4,6,3),RK (1,7,2),LK (9,2,7)]
          ]
 expandLine fs1 = hsep 3 line where
    line = [d1, d1 # lc red # lw veryThin <> d0 ,d2 # lc red # lw veryThin <> d3,d4]
    g1 = makeTgraph fs1
    g2 = decompose g1
    g3 = force g2
    g4 = compose g3
    g0 = force g1
    vp = makeAlignedVP (1,2) g3
    d1 = restrictSmart g1 draw vp
    d2 = restrictSmart g2 draw vp
    d3 = restrictSmart g3 draw vp
    d4 = if nullGraph g4
         then emptyRep
         else restrictSmart g4 draw vp
    d0 = scale phi $ alignBefore (restrictSmart g0 draw) (1,2) g0

-- | diagram used to indicate an empty Tgraph (lime circle with diagonal line through it).
emptyRep:: Diagram B
emptyRep = lc orange $ (circle phi :: QDiagram B V2 Double Any) <> rotate (45@@deg) (hrule (3*phi))

{- |
For a proof that (compose . force . decompose) gF = gF for forced Tgraphs gF, this
diagram is to check boundary vertices (of a forced Tgraph) after applying (compose . force . decompose).
This shows all boundary vertex contexts for a forced Tgraph using forcedBVContexts.
For each context (with vertex marked with a red dot) the result of (compose . force . decompose)
is shown underneath. This establishes that no boundary vertex changes for a forced Tgraph
when applying (compose . force . decompose).
-}
checkCFDFig :: Diagram B
checkCFDFig = padBorder $ lw ultraThin $ vsep 10 $ fmap (arrangeRows 10)
  [dartOriginDiags, dartWingDiags, kiteOriginDiags, kiteWingDiags, kiteOppDiags] where
  dartOriginDiags = take 11 alldartOriginDiags ++ fmap (alldartOriginDiags!!) [15,18,21,23]
  dartWingDiags = alldartWingDiags
  kiteOriginDiags = take 4 allkiteOriginDiags ++ fmap (allkiteOriginDiags!!) [5,6,7,8,9,10,12,13,19,21,26,29,30,37,59]
  kiteWingDiags = take 7 allkiteWingDiags ++ fmap (allkiteWingDiags!!) [8,9,10,12,13,15,24,25,26,27,29,30,31,32,33,34,35,37,44]
  kiteOppDiags = take 7 allkiteOppDiags ++ fmap (allkiteOppDiags!!) [8,9,10,11,12,13,19]
                 ++ take 13 (drop 19 allkiteOppDiags) ++ [allkiteOppDiags!!44]
  alldartOriginDiags = drawCases 1 $ makeTgraph [LD (1,3,2)]
  alldartWingDiags = drawCases 2 $ makeTgraph [LD (1,3,2)]
  allkiteOriginDiags = drawCases 2 $ makeTgraph [LK (2,1,3)]
  allkiteWingDiags = drawCases 1 $ makeTgraph [LK (2,1,3)]
  allkiteOppDiags = drawCases 1 $ makeTgraph [LK (3,2,1)]
  edge = (1,2)
  drawCases v g = fmap (drawCase v) $ forcedBVContexts v edge $ makeBoundaryState $ force g
  drawCase v bd = vsep 1 [drawv <> drawg, drawcfd] where
     g = recoverGraph bd
     vp = makeAlignedVP edge g
     drawg = draw vp
 --    drawe = drawEdgeWith vp edge # lc red
     drawv = case findLoc v vp of
               Nothing -> error $ "checkCFDFig: vertex not found " ++ show v
               Just p -> circle 0.2 # fc red # lc red # moveTo p
     drawcfd = (alignBefore draw edge . compose . force . decompose) g

-- | For a proof that (compose . force . decompose) gF = gF for froced Tgraphs gF
-- All internal vertices are dealt with by relatedVTypeFig except for the (forced) star case
-- which this figure deals with.
checkCFDStar:: Diagram B
checkCFDStar = padBorder $ hsep 1 [drawForce starGraph, draw $ compose sfDf, draw sfDf]
   where sfDf = (force . decompose . force) starGraph

-- | figure to check that force can complete a hole
forceHoleTest :: Diagram B
forceHoleTest = padBorder $ lw ultraThin $ rotate (ttangle 1) $ drawForce boundaryFDart5

-- | figure to check that force can complete a hole and extend from boundary faces
forceFillTest :: Diagram B
forceFillTest = padBorder $ lw ultraThin $ rotate (ttangle 1) $ drawForce g
    where g = checkedTgraph $ boundaryFaces $ makeBoundaryState $ dartDs!!6

-- |showing intermediate state of filling the inlet and closing the gap of boundaryGapFDart5
-- using stepForce 2000
gapProgress5 :: Diagram B
gapProgress5 = padBorder $ lw ultraThin $ vsep 1 $ center <$> rotations [1,1]
    [ smartdraw g
    , smartdraw $ stepForce g 2000
    ] where g = boundaryGapFDart5


dartPic0,kitePic0,bigPic :: Diagram B
{-| dartPic0 is a diagram of force/compose relationships for decomposed darts
    without arrows. 
-}
dartPic0 = padBorder $ lw ultraThin $ position $ concat
          [ zip pointsR1 $ rotations [0,1,1] partComps
          , zip pointsR2 $ zipWith named ["a4", "a3","a2","a1","a0"] (dots : rotations [1,1] forceDs)
          , zip pointsR3 $ zipWith named ["b4", "b3","b2","b1","b0"] (dots : rotations [1,1] drts)
          ]
          where
              partComps = phiScales $ fmap drawPCompose $ reverse $ take 5 $ allForceDecomps $ force dartGraph
              forceDs = fmap center $ phiScaling phi $ reverse $ take 4 $ fmap drawForce dartDs
              drts  = fmap center $ phiScaling phi $ reverse $ take 4 $ fmap smartdraw dartDs
              dots = center $ hsep 1 $ replicate 4 (circle 0.5 # fc gray # lw none)
              pointsR1 = map p2 [ (0, 70), (57, 70), (107, 70), (155, 70), (190, 70)]
              pointsR2 = map p2 [ (0, 40), (42, 40), (95, 40), (145, 40), (195, 40)]
              pointsR3 = map p2 [ (0, 0),  (42, 0),  (95, 0),  (145, 0),  (195, 0) ]

{-| kitePic0 is a diagram of force/compose relationships for decomposed kites
    without arrows. 
-}
kitePic0 = padBorder $ lw ultraThin $ position $ concat
          [ zip pointsR1 $ rotations [0,0,1,1] partComps
          , zip pointsR2 $ zipWith named ["a4", "a3","a2","a1","a0"] (dots : rotations [0,1,1] forceDs)
          , zip pointsR3 $ zipWith named ["b4", "b3","b2","b1","b0"] (dots : rotations [0,1,1] kts)
          ]
          where
              partComps = phiScales $ fmap drawPCompose $ reverse $ take 5 $ allForceDecomps $ force kiteGraph
              forceDs = fmap center $ phiScaling phi $ reverse $ take 4 $ fmap drawForce kiteDs
              kts  = fmap center $ phiScaling phi $ reverse $ take 4 $ fmap smartdraw kiteDs
              dots = center $ hsep 1 $ replicate 4 (circle 0.5 # fc gray # lw none)
              pointsR1 = map p2 [ (6, 70), (58, 70), (106, 70), (156, 70), (196, 70)]
              pointsR2 = map p2 [ (0, 44), (42, 44), (95, 44), (140, 44), (186, 44)]
              pointsR3 = map p2 [ (0, 0),  (42, 0),  (95, 0),  (140, 0),  (186, 0) ]

{-| bigPic is a diagram illustrating force/compose relationships for decomposed darts
    and decomposed kites. 
-}
bigPic = addArrows dartPic0 === (box <> key) === addArrows kitePic0 where
    box = rect 120 22 # lw thin
    key = frame 1.1 $ centerXY $ hsep 40 [a,b,c,d]
           # labelAt (p2 (2,-6)) "force . decompose"
           # labelAt (p2 (55,8)) "compose"
           # labelAt (p2 (50,-6)) "decompose"
           # labelAt (p2 (90,-6)) "force"
           # forceDecArrow "B" "A"
           # decompArrow "C" "B"
           # forceArrow "D" "C"
           # composeArcRight "B" "C" where
              a = named "A" (rect 0 0.1 # lw none)
              b = named "B" (rect 0 0.1 # lw none)
              c = named "C" (rect 0 0.1 # lw none)
              d = named "D" (rect 0 0.1 # lw none)


-- | addArrows is used to put arrows in both dartPic0 and kitePic0 (and bigPic)
addArrows:: Diagram B -> Diagram B
addArrows d = d  # composeArcRight "a3" "a2"
                 # composeArcRight "a2" "a1"
                 # composeArcRight "a1" "a0"
                 # composeArcRight "a4" "a3"
                 # composeArcRight "b4" "b3"
                 # composeArcRight "b3" "b2"
                 # composeArcRight "b2" "b1"
                 # composeArcRight "b1" "b0"
                 # decompArrow "b3" "b4"
                 # decompArrow "b2" "b3"
                 # decompArrow "b1" "b2"
                 # decompArrow "b0" "b1"
                 # forceDecArrow "a3" "a4"
                 # forceDecArrow "a2" "a3"
                 # forceDecArrow "a1" "a2"
                 # forceDecArrow "a0" "a1"
                 # forceArrow "b0" "a0"
                 # forceArrow "b1" "a1"
                 # forceArrow "b2" "a2"
                 # forceArrow "b3" "a3"


-- |add a force arrow (black) from 2 named parts of a diagram
forceArrow :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B -> Diagram B
forceArrow = connectOutside' arrowStyleF where
  arrowStyleF = with & headLength .~ verySmall
                     & headGap .~ small & tailGap .~ large

-- |add a (force . decompose) arrow (blue) from 2 named parts of a diagram
forceDecArrow :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B -> Diagram B
forceDecArrow = connectOutside' arrowStyleFD where
  arrowStyleFD = with & headLength .~ verySmall & headStyle %~ fc blue
                      & shaftStyle %~ lc blue
                      & headGap .~ small & tailGap .~ small

-- |add a decompose arrow (ddashed blue) from 2 named parts of a diagram
decompArrow :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B -> Diagram B
decompArrow = connectOutside' arrowStyleD where
  arrowStyleD  = with & headLength .~ verySmall & headStyle %~ fc blue
                      & shaftStyle %~ dashingN [0.005, 0.005] 0
                      & shaftStyle %~ lc blue & headGap .~ large & tailGap .~ large

-- |add a compose arrow (green) from 2 named parts of a diagram
composeArrow :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B -> Diagram B
composeArrow = connectOutside' arrowStyleC where
  arrowStyleC = with & headLength .~ verySmall
                      & headStyle %~ fc green & shaftStyle %~ lc green
                      & headGap .~ large & tailGap .~ large

-- |add a compose arrow arc (green) from 2 named parts of a diagram and start/end angles
composeArc :: (IsName n1, IsName n2) => n1 -> n2 -> Angle Double -> Angle Double -> Diagram B -> Diagram B
composeArc = connectPerim' arrowStyleC where
  arrowStyleC = with & arrowShaft .~ arc xDir (-1/10 @@ turn) & headLength .~ verySmall
                     & headStyle %~ fc green & shaftStyle %~ lc green
                     & headGap .~ large & tailGap .~ large

-- |add a compose arrow arc (green) from 2 named parts of a diagram (horizontal left to right)
composeArcRight :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B -> Diagram B
composeArcRight a b = composeArc a b (1/10 @@ turn) (4/10 @@ turn)

-- |add a compose arrow arc (green) from 2 named parts of a diagram (vertical up)
composeArcUp :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B -> Diagram B
composeArcUp a b = composeArc a b (3/10 @@ turn) (6/10 @@ turn)


{-| curioPic0 is diagram curioPic without arrows
-}
curioPic0 :: Diagram B
curioPic0 = padBorder $ lw ultraThin $ position $ concat
  [ zip pointsRa $ zipWith named ["a4","a3","a2","a1","a0"] (dots : fmap center forceDs)
  , zip pointsRb $ zipWith named ["b4", "b3","b2","b1"] (dots : fmap center forceXDs)
  , zip pointsRc $ zipWith named ["c4", "c3","c2","c1","c0"] (dots : fmap center xDs)
  ] where
    forceDs  = rotations [1,1]  $ phiScaling phi $ reverse $ take 4 $ fmap (draw . force) dartDs
    forceXDs = rotations [9,9,8]  $ phiScaling phi $ reverse $ take 3 $ fmap drawForce xDGraphs
    xDGraphs = decompositions sun3Dart
    xDs  = rotations [9,9,8] $  phiScaling phi $ reverse $
           draw dartGraph : (draw sun3Dart # lc red # lw thin):
           take 2  (drop 1 $ fmap smartdraw xDGraphs)
    dots = center $ hsep 1 $ replicate 4 (circle 0.5 # fc gray # lw none)
    pointsRa = map p2 [ (0, 80), (42, 80), (95, 80), (150, 80), (200, 80)]
    pointsRb = map p2 [ (0, 40), (42, 40), (95, 40), (150, 40)]
    pointsRc = map p2 [ (0, 0),  (42, 0),  (95, 0),  (150, 0), (200, 0)]

{-| curioPic is a diagram illustrating where compose loses information not recovered by force
  with sun3Dart third item in bottom row, (curioPic0 is diagram without arrows)
-}
curioPic :: Diagram B
curioPic =
  curioPic0  # composeArcRight "a3" "a2"
             # composeArcRight "a2" "a1"
             # composeArcRight "a1" "a0"
             # composeArcRight "a4" "a3"
             # composeArcRight "b3" "b2"
             # composeArcRight "b2" "b1"
             # composeArcRight "b4" "b3"
             # composeArrow "b1" "a0"
             # forceDecArrow "a3" "a4"
             # forceDecArrow "a2" "a3"
             # forceDecArrow "a1" "a2"
             # forceDecArrow "a0" "a1"
             # forceDecArrow "b3" "b4"
             # forceDecArrow "b2" "b3"
             # forceDecArrow "b1" "b2"
             # decompArrow "c3" "c4"
             # decompArrow "c2" "c3"
             # decompArrow "c1" "c2"
             # forceArrow "c1" "b1"
             # forceArrow "c2" "b2"
             # forceArrow "c3" "b3"
             # forceArrow "c0" "a0"
             # composeArcRight "c4" "c3"
             # composeArcRight "c3" "c2"
             # composeArcRight "c2" "c1"
             # composeArcRight "c1" "c0"


{-*
Testing (functions and figures and experiments)
-}

testForce4, testForce5 :: Diagram B
-- |diagram of forced Tgraph for boundaryGapFDart4
testForce4 = padBorder $ lw ultraThin $ drawForce boundaryGapFDart4
-- |diagram of forced Tgraph for boundaryGapFDart5
testForce5 = padBorder $ lw ultraThin $ drawForce boundaryGapFDart5


{-| testViewBoundary is a testing tool to inspect the boundary vertex locations of some (intermediate) BoundaryState
-- (used in conjunction with stepForce to get an intermediate BoundaryState)
-- The boundary edges of a BoundaryState are shown in lime - using the BoundaryState positions of vertices.
-- This is overlaid on the full graph drawn with vertex labels.
-}
testViewBoundary :: BoundaryState -> Diagram B
testViewBoundary bd =  (drawEdges vpMap bdE # lc lime) <> labelled drawj g where
    g = recoverGraph bd
    vpMap = bvLocMap bd
    bdE = boundary bd

-- |used to discover accuracy problem of older thirdVertexLoc
-- view tha boundary after n steps of forcing (starting with boundaryGapFDart5)
-- e.g. n = 1900 for inspectForce5 or 200 for inspectForce3
inspectForce5,inspectForce3 :: Int -> Diagram B
inspectForce5 n = padBorder $ lw ultraThin $
                  testViewBoundary $ stepForce (makeBoundaryState boundaryGapFDart5) n

inspectForce3 n = padBorder $ lw ultraThin $
                  testViewBoundary $ stepForce (makeBoundaryState $ dartDs!!3) n


-- |figures showing boundary edges of the boundary gap graphs boundaryGapFDart4 and boundaryGapFDart5 
testBoundary4, testBoundary5 :: Diagram B
testBoundary4 =  padBorder $ lw ultraThin $ drawGBoundary boundaryGapFDart4
testBoundary5 =  padBorder $ lw ultraThin $ drawGBoundary boundaryGapFDart5

-- |test wholeTiles (which adds missing second halves of each face)
checkCompleteFig:: Diagram B
checkCompleteFig =  padBorder $ hsep 1 $ fmap drawj [sunD4, wholeTiles sunD4] where sunD4 = sunDs !! 4


-- |figure testing selectFacesVP by removing all kites
dartsOnlyFig :: Diagram B
dartsOnlyFig = padBorder $ lw thin $ draw $ selectFacesVP darts $ makeVP g where
    g = force $ sunDs !! 5
    darts = filter isDart $ faces g

-- |hollowgraph is the difference between a particular Tgraph and its forced version.
-- This may not be a valid Tgraph in general.
-- The particular Tgraph is a 3 times decomposed fd2 where fd2 is a forced, twice decomposed dart.
hollowGraph::Tgraph
hollowGraph = removeFaces (faces x) fx where
  fx = force x
  x = decompositions fd2 !!3
  fd2 = force (dartDs!!2)

-- |figure showing hollowGraph and the result of forcing which fills the hole
forceHollowFig:: Diagram B
forceHollowFig = padBorder $  lw ultraThin $ hsep 1 $ fmap draw [hollowGraph, force hollowGraph]

-- |What happens if you take the first result of twoChoices, then 
-- select only the faces that were added by force, then force just these faces?
-- It does not quite complete the original faces    
forcedNewFaces:: Diagram B
forcedNewFaces = padBorder $ lw thin $ drawForce g2 where
    g1 = addHalfDart (223,255) (force dartD4)
    g2 = removeFaces (faces g1) (force g1)

{-*
Illustrations using TrackedTgraphs
-}
   
{-
N.B.  Changes to forcing (or decomposing) can affect the vertex numbers chosen in twoChoices...
They should be the long edge of the left dart on the left of a group of 3 darts
Middle of top edge of dartDs!!4.
-}

-- |given a boundary directed edge of a forced graph (either direction)
-- construct two TrackedTgraphs with half dart/kite addition on the edge respectively
-- track the resulting faces and also the singleton new face, then force both TrackedTgraphs
trackTwoChoices:: Dedge -> Tgraph -> [TrackedTgraph]
trackTwoChoices de g = [ttg1,ttg2] where
          ttg1 = forceTracked $ trackFaces $ addHalfDartTracked de $ newTrackedTgraph g
          ttg2 = forceTracked $ trackFaces $ addHalfKiteTracked de $ newTrackedTgraph g

-- |forced 4 times decomposed dart (used for identifying particular boundary
-- edges in twoChoices and moreChoices)
forceDartD4Fig:: Diagram B
forceDartD4Fig = padBorder $ lw ultraThin $ labelSmall drawj $ force dartD4
-- |Take a forced, 4 times decomposed dart, then track the two choices
twoChoices:: [TrackedTgraph]
twoChoices = trackTwoChoices (223,255) (force dartD4) --(233,201) 

-- |show the result of (tracked) two choices
-- with tracked faces in red, new face filled black. 
drawChoice:: TrackedTgraph -> Diagram B
drawChoice = drawTrackedTgraph [draw, lc red . draw, drawWith (fillDK black black)]

-- |show the (tracked) twoChoices with (tracked faces in red, new face filled black)  
twoChoicesFig:: Diagram B
twoChoicesFig  = padBorder $ lw ultraThin $ hsep 1 $ fmap drawChoice twoChoices

-- |track two further choices with the first of twoChoices (fullgraph)  
moreChoices0:: [TrackedTgraph]
moreChoices0 = trackTwoChoices (200,241) (tgraph $ twoChoices !! 0) --(178,219)

-- |track two further choices with the second of twoChoices (fullgraph)  
moreChoices1:: [TrackedTgraph]
moreChoices1 = trackTwoChoices (200,241) (tgraph $ twoChoices !! 1) --(178,219)

-- |figures for 4 further choices
moreChoicesFig0,moreChoicesFig1,moreChoicesFig:: Diagram B
moreChoicesFig0 =  padBorder $ lw ultraThin $ hsep 10 $ fmap drawChoice moreChoices0
moreChoicesFig1 =  padBorder $ lw ultraThin $ hsep 1 $ fmap drawChoice moreChoices1
moreChoicesFig  =  vsep 1 [moreChoicesFig0,moreChoicesFig1]

{-*
Choices by additions at the maximal compose level
-}

-- |Trying to find which extensions to the starting dart correspond to the twoChoicesFig.
-- These basic examples are used in halfWholeFig to compare with twoChoicesFig.
dartHalfDart,dartHalfKite,dartPlusDart,dartPlusKite,kitePlusKite :: Tgraph
-- |a dart with another half dart on a long edge
dartHalfDart = addHalfDart (1,2) dartGraph
-- |a dart with a half kite on a long edge
dartHalfKite = addHalfKite (1,2) dartGraph
-- |two darts sharing a long edge
dartPlusDart = addHalfDart (1,5) dartHalfDart
-- |a dart and a kite sharing a long edge
dartPlusKite = addHalfKite (2,5) dartHalfKite
-- |two kites sharing a long edge
kitePlusKite = addHalfKite (1,5) $ addHalfKite (1,3) kiteGraph


-- |halfWholeFig shows that a whole dart/kite needs to be added to get the same result as twoChoicesFig
-- Adding a half tile has no effect on the forced decomposition
halfWholeFig:: Diagram B
halfWholeFig =  padBorder $ lw ultraThin $ vsep 1 $ fmap (hsep 1) [take 2 figs, drop 2 figs]
  where
    figs = zipWith redEmbed scaledCases forcedD4Cases
    cases = [dartPlusDart, dartPlusKite, dartHalfDart, dartHalfKite]
    scaledCases = alignAll (1,3) $ fmap (scale (phi^4) . makeVP) cases
    forcedD4Cases = alignAll (1,3) $ fmap (makeVP . force . decomp4) cases
    decomp4 g = decompositions g !! 4
    redEmbed g1 g2 = lc red (lw medium $ drawj g1) <> lw ultraThin (draw g2)

-- |Two kites and forceDecomp twice.
-- Composition will reverse the forceDecomps (as in relatedVTypesFig)
kkEmpsFig:: Diagram B
kkEmpsFig = padBorder $ lw ultraThin $ vsep 1 $ rotations [9,0,0] $
            fmap draw  [kk, kkD, kkD2] where
              kk = kitePlusKite
              kkD  = forceDecomp kk
              kkD2 = forceDecomp kkD

-- |Figure for 6 rockets.
-- Starting with sun3Dart, the next rocket is the result of adding a dart (cone) to the tip
-- of a forceDecomp of the previous rocket. No cone added to the last rocket.
-- In each case, adding a kite half at the tip instead of a dart would be an incorrect Tgraph.
-- (See superForce examples).
rocketsFig:: Diagram B
rocketsFig = padBorder $ lw ultraThin $ vsep 1 $ rotations [8,9,9,8,8,9] $
             fmap draw [rc0,rc1,rc2,rc3,rc4,rc5] where
  rc0 = sun3Dart
  rc1 = force $ addHalfDart (59,60) (forceDecomp rc0)
  rc2 = force $ addHalfDart (326,327) (forceDecomp rc1)
  rc3 = force $ addHalfDart (1036,1037) (forceDecomp rc2)
  rc4 = force $ addHalfDart (3019,3020) (forceDecomp rc3)
  rc5 = forceDecomp rc4

-- |6 times forced and decomposed kingGraph. Has 53574 faces (now builds more than 60 times faster after profiling)
-- There are 2906 faces for kingD6 before forcing.
kingFD6:: Diagram B
kingFD6 = padBorder $ lw ultraThin $ colourDKG (darkmagenta, indigo, gold) $ makeVP $
          allForceDecomps kingGraph !!6



{-*
Forced Boundary Edge and Vertex Contexts
-}

{- |
Diagram of contexts for an edge on the boundary of a forced Tgraph (using forcedBEContexts).
The edge is shown in red in each case.
There are 3 groups for the 3 edge types (right-hand variants are not shown).
For each group, we consider both dart and kite additions either side of the red edge of the first Tgraph in the group.
There are further additions on other boundary edges when
the composition is empty.
We remove any Tgraphs where the red edge is no longer on the boundary and remove any repeated cases.
The composition of each Tgraph is shown filled yellow (no yellow means empty composition).
-}
forcedBEContextsFig :: Diagram B
forcedBEContextsFig = padBorder $ lw ultraThin $ vsep 5 $ fmap (arrangeRows 7)
                      [ fmap (dartLongDiags!!)  [1,2,3,6,7,8,9,10,12,13,14,15,16]
                      , fmap (kiteLongDiags!!)  [2,3,4] ++ drop 6 kiteLongDiags
                      , fmap (kiteShortDiags!!) [2,3,4,7,8,9,10,12,16,17,18,19,22]
                      ] where
    edge = (1,2)
    dartLongDiags  = fmap (drawBEContext edge) dartLongContexts
    kiteLongDiags  = fmap (drawBEContext edge) kiteLongContexts
    kiteShortDiags = fmap (drawBEContext edge) kiteShortContexts

-- | Local forced contexts for boundary edges: dart long, kite long, kite short.
dartLongContexts,kiteLongContexts,kiteShortContexts:: [Tgraph]
dartLongContexts  = fmap recoverGraph $ forcedBEContexts (1,2) $ makeBoundaryState $ force $ makeTgraph [LD (1,3,2)]
kiteLongContexts  = fmap recoverGraph $ forcedBEContexts (1,2) $ makeBoundaryState $ force $ makeTgraph [LK (2,1,3)]
kiteShortContexts = fmap recoverGraph $ forcedBEContexts (1,2) $ makeBoundaryState $ force $ makeTgraph [LK (3,2,1)]

-- |drawBEContext e g - draws g, aligning e on the x-axis.
-- It emphasises the edge e with red and shows the composition of g filled yellow. 
drawBEContext::Dedge -> Tgraph -> Diagram B
drawBEContext edge g = drawe <> drawg <> drawComp where
    vp = makeAlignedVP edge g
    drawg = draw vp
    drawe = drawEdgeWith vp edge # lc red # lw thin
    drawComp = lw none $ drawWith (fillDK yellow yellow) $ subVP vp $ faces $ compose g

{- |
Diagram showing boundary vertex contexts for a forced Tgraph using forcedBVContexts.
There are 4 groups for (left hand only) dart origin, kite origin, kite wing, kite opp. 
Note that dart wing cases are a subset of kite opp cases.
(A dart opp cannot be on the boundary of a forced Tgraph).
Repetitions have been removed.
-}
forcedBVContextsFig :: Diagram B
forcedBVContextsFig = padBorder $ lw ultraThin $ vsep 5
--  [dartOriginDiags, dartWingDiags, kiteOriginDiags, kiteWingDiags, kiteOppDiags] where
  [dartOriginDiags, kiteOriginDiags, kiteWingDiags, kiteOppDiags] where
  edge = (1,2)
  dartOriginDiags = arrangeRows 10 $ fmap (alldartOriginDiags!!) [3,4,6,7,8,9,10,18,21,23]
  kiteOriginDiags = arrangeRows 12 $ fmap (allkiteOriginDiags!!) [2,3,5,6,7,9,10,11,12,13,30,36]
  kiteWingDiags = arrangeRows 9 $ fmap (allkiteWingDiags!!) [2,3,4,5,6,8,9,10,12,13,14,15,27,29,32,33,37]
  kiteOppDiags = arrangeRows 7 $ fmap (allkiteOppDiags!!) [4,5,6,8,9,10,11,12,23,24,25,27,28,31]

  alldartOriginDiags = fmap (drawVContext 1 edge) dartOriginContexts
  allkiteOriginDiags = fmap (drawVContext 2 edge) kiteOriginContexts
  allkiteWingDiags = fmap (drawVContext 1 edge) kiteWingContexts
  allkiteOppDiags = fmap (drawVContext 1 edge) kiteOppContexts

-- | Local forced contexts for vertex types: dart origin, kite origin, kite wing, kite opp
dartOriginContexts,kiteOriginContexts,kiteWingContexts,kiteOppContexts:: [Tgraph]
dartOriginContexts = fmap recoverGraph $ forcedBVContexts 1 (1,2) $ force $ makeBoundaryState $ makeTgraph [LD (1,3,2)]
kiteOriginContexts = fmap recoverGraph $ forcedBVContexts 2 (1,2) $ force $ makeBoundaryState $ makeTgraph [LK (2,1,3)]
kiteWingContexts   = fmap recoverGraph $ forcedBVContexts 1 (1,2) $ force $ makeBoundaryState $ makeTgraph [LK (2,1,3)]
kiteOppContexts    = fmap recoverGraph $ forcedBVContexts 1 (1,2) $ force $ makeBoundaryState $ makeTgraph [LK (3,2,1)]
     
-- |drawVContext v e g - draws the Tgraph g with vertex v shown red and edge e aligned on the x-axis and
-- the composition of g shown in yellow.
-- It raises an error if the vertex or edge is not found.
drawVContext::Vertex -> Dedge -> Tgraph -> Diagram B
drawVContext v edge g = drawv <> drawg <> drawComp where
    vp = makeAlignedVP edge g
    drawg = draw vp
--    drawe = drawEdgeWith vp edge # lc red
    drawv = case findLoc v vp of
              Nothing -> error $ "drawVContext: vertex not found " ++ show v
              Just p -> circle 0.2 # fc red # lc red # moveTo p
    drawComp = lw none $ drawWith (fillDK yellow yellow) $ subVP vp $ faces $ compose g


-- |Diagram showing all local forced contexts for a sun vertex.
-- The vertex is shown with a red dot and the composition filled yellow.
-- 5 fold symmetry is used to remove rotated duplicates.
sunVContextsFig :: Diagram B
sunVContextsFig = padBorder $ lw ultraThin $ arrangeRows 7 $ fmap (drawVContext 1 (1,3)) sunContexts

-- |Diagram showing only the cases from sunVContextFig where the vertex is on the composition boundary
sunVContextsCompBoundary :: Diagram B
sunVContextsCompBoundary = padBorder $ lw ultraThin $ hsep 1 $
                           fmap (drawVContext 1 (1,3) . (sunContexts!!)) [2,3,4,7,9,11]
   -- only show cases where v is on the composition boundary and remove one indexed as 10 - a mirror symetric version of 7

-- |All local forced contexts for a sun vertex (as BoundaryStates).
-- The vertex is shown with a red dot and the composition filled yellow.
-- 5 fold symmetry is used to remove rotated duplicates.
sunContexts:: [Tgraph]
sunContexts = recoverGraph <$> contexts [] [(bStart, boundaryEdgeSet bStart)] where
  bStart = makeBoundaryState sunGraph
-- occursRotatedIn deals with 5 rotational symmetries of the sunGraph
-- occursRotatedIn done bs is true if bs matches a case in done in any of 5 rotations
  occursRotatedIn done bs = any (same done bs (1,3)) [(1,3),(1,5),(1,7),(1,9),(1,11)]
  same done bs e e' = any (sameGraph (recoverGraph bs,e) . (,e') . recoverGraph) done
--  same done bs e e' = any (sameGraph (recoverGraph bs,e)) . (\bd -> (recoverGraph bd,e'))) done
-- contexts generates the cases keeping a list of done cases for filtering out copies
  contexts done [] = reverse done
  contexts done ((bs,es):opens)
    | occursRotatedIn done bs = contexts done opens
    | Set.null es = contexts (bs:done) opens -- bs is a completed cover
    | otherwise = contexts (bs:done) (newcases ++ opens)
        where newcases = concatMap (makecases (bs,es)) (Set.toList es)
  makecases (bs,es) de = fmap attachEdgeSet (atLeastOne $ tryDartAndKite bs de)
    where attachEdgeSet b = (b, commonBdry (Set.delete de es) b)


-- |Diagram showing local contexts in a forced Tgraph for a fool/ace vertex.
-- The vertex is shown with a red dot and the composition filled yellow.
-- Mirror symmetric versions have been removed.
foolVContextsFig:: Diagram B
foolVContextsFig  = padBorder $ lw ultraThin $ arrangeRows 8 $ 
                    fmap (drawVContext 3 (1,4) . (foolContexts!!)) 
                    [0,1,2,3,4,5,6,7,9,13,14,15,16,17,18,19,20,21,23,26,27,28]
   -- only include 1 of each mirror symmetric case

-- |Diagram showing only the cases from foolVContextsFig where
-- the vertex is on the composition boundary
foolVContextsCompBoundary:: Diagram B
foolVContextsCompBoundary  = padBorder $ lw ultraThin $ arrangeRows 4 $ 
                             fmap (drawVContext 3 (1,4) . (foolContexts!!)) 
                             [1,6,7,15,19,21,26,28]
   -- only show cases where v is on the composition boundary

-- | Generates the cases for fool vertex contexts in a forced Tgraph (some mirror symetric duplicates)
foolContexts:: [Tgraph]
foolContexts = recoverGraph <$> contexts [] [(bStart, boundaryEdgeSet bStart)] where
  edge = (1,4)
  bStart = makeBoundaryState fool
  contexts done [] = reverse done
  contexts done ((bs,es):opens)
    | occursIn done bs edge = contexts done opens
-- check null composition before null es
    | nullGraph $ compose $ recoverGraph bs
          = let newcases = concatMap (makecases (bs,es)) (boundary bs)
            in  contexts (bs:done) (newcases++opens)
    | Set.null es = contexts (bs:done) opens
    | otherwise = contexts (bs:done) (newcases ++ opens)
                  where newcases = concatMap (makecases (bs,es)) (Set.toList es)

  makecases (bs,es) de = fmap attachEdgeSet (atLeastOne $ tryDartAndKite bs de)
    where attachEdgeSet b = (b, commonBdry (Set.delete de es) b)


-- | Diagram illustrating 3 cases for groups of remainder half-tiles (when composing a forced Tgraph)
remainderGroupsFig:: Diagram B
remainderGroupsFig = padBorder $ hsep 1 [hfDiag, kDiag, fDiag] where
    halfFool = makeTgraph [LD (1,2,3),RK (4,3,2),LK (4,5,3)]
    hfVP = makeVP halfFool
    hfDiag = lc yellow (drawEdgesIn hfVP [(1,2),(2,4)])
             <> lc red (drawEdgesIn hfVP [(3,1),(5,3)])
             <> drawj hfVP
    kite = makeTgraph [RK (1,2,3),LK (1,4,2)]
    kVP = makeVP kite
    kDiag = drawv 1 kVP
             <> lc red (drawEdgesIn kVP [(2,3),(4,2)])
             <> drawj kVP
    fool = makeTgraph [LD (1,2,3),RK (4,3,2),LK (4,5,3),RD (1,6,2),LK (4,2,6),RK (4,6,7)]
    fVP = makeVP fool
    fDiag = drawv 4 fVP
             <> lc red (drawEdgesIn fVP [(3,1),(5,3),(1,6),(6,7)])
             <> drawj fVP
    drawv v vp = case findLoc v vp of
              Nothing -> error $ "remainderGroupsFig: vertex not found " ++ show v
              Just p -> circle 0.05 # fc yellow # lc yellow # moveTo p





-- | oneChoiceGraph is a forced Tgraph where boundary edges (76,77) and (77,78) each have one of their 2 legal extensions
-- incorrect.
oneChoiceGraph:: Tgraph
oneChoiceGraph = force $ addHalfDart (37,59) $ force kingGraph

{- |
This figure shows a successfully forced Tgraph (oneChoiceGraph) and below is an extension (added half kite)
on edge (76,77) which fails on forcing showing it is an incorrect Tgraph, and below that a successful extension
(added half dart on the same boundary edge) after forcing.
It establishes that a single legal face addition to a forced Tgraph can be an incorrect Tgraph.
-}
oneChoiceFig:: Diagram B
oneChoiceFig = padBorder $ lw ultraThin $ vsep 1 $
                     fmap (smart (labelSmall draw)) [oneChoiceGraph,incorrectExtension,successful] where
  successful = force $ addHalfDart (76,77) oneChoiceGraph
  incorrectExtension = addHalfKite (76,77) oneChoiceGraph -- fails on forcing

-- | Figure showing boundaryECovering of oneChoiceGraph
coveringOneChoiceFig:: Diagram B
coveringOneChoiceFig = pad 1.02 $ lw ultraThin $ arrangeRows 3 $
  fmap drawCase beCover where
    beCover = boundaryECovering (makeBoundaryState oneChoiceGraph)
    drawCase bd = overlay <> draw (recoverGraph bd)
    overlay = draw oneChoiceGraph # lc red

-- |boundaryLoopFill tests the calculation of boundary loops of a Tgraph and conversion to a (Diagrams) Path, using
-- boundaryLoopsG and pathFromBoundaryLoops. The conversion of the Path to a Diagram allows
-- a fill colour to be used for the entire internal part of the Tgraph - i.e. not by filling the individual tilefaces.
-- It also associates vertex labels with the respective positions in the diagram using a vertexNames attribute.
boundaryLoopFill:: Colour Double -> Tgraph -> Diagram B
boundaryLoopFill c g = dg # lw ultraThin <> d # fc c where
    vp = makeVP g
    dg = draw vp
    vlocs = vLocs vp
    bdLoops = boundaryLoopsG g
    d = strokeP' (with & vertexNames .~ bdLoops) $ pathFromBoundaryLoops vlocs bdLoops

testLoops1,testLoops2:: Diagram B
-- | diagram using boundaryLoopFill with a single boundary loop
testLoops1 = padBorder $ boundaryLoopFill honeydew boundaryGapFDart4

-- | diagram using boundaryLoopFill with two boundary loops (i.e. a single hole)
testLoops2 = padBorder $ lw ultraThin $ boundaryLoopFill honeydew g where
         g = removeFaces (faces $ recoverGraph bs1) (recoverGraph bs2)
         bs2 = head $ boundaryVCovering bs1
         bs1 = head $ boundaryVCovering bs0
         bs0 = force $ makeBoundaryState kingGraph

{-*
Illustrating Relabelling (fullUnion, commonFaces)
-}

{-|A diagram testing matchByEdges with a single tile-connected overlap.
The top 2 graphs g1 and g2 have possible matching overlaps except for labelling.
The next row has: (left) a relabelling of g2 leaving (1,10) 
which is a preparation step to avoid accidental clashes with g1,
(middle) a further relabelling of g2 by matching against g1 using (1,10)
as the edge to match with (1,15),
(right) the union of this relabelled graph with g1.
The bottom row is as for the row above but using (1,18) as the edge to match with (1,15)
resulting in a different union.
-}
testRelabellingFig:: Diagram B
testRelabellingFig = 
  padBorder $ lw ultraThin $ vsep 1
    [ hsep 1 $ center <$> take 2 eight
    , hsep 1 $ center <$> take 3 $ drop 2 eight
    , hsep 1 $ center <$> drop 5 eight
    ] 
  where eight = fmap (labelSmall drawj) 
                    [ g1
                    , g2
                    , g2_A
                    , matchByEdges (g1, (1,15)) (g2,(1,10))
                    , fullUnion (g1, (1,15)) (g2,(1,10))
                    , g2_B
                    , matchByEdges (g1, (1,15)) (g2,(1,18))
                    , fullUnion (g1, (1,15)) (g2,(1,18))
                    ]
        sunD2 = sunDs!!2
        fsunD2 = force sunD2
        g1 = removeFaces [RK (1,31,41)] (removeVertices [74,79,29] sunD2)
        reduced2 = removeVertices [8,7,6] fsunD2
        g2 = relabelContig reduced2
        g2_A = prepareFixAvoid [1,10] (vertexSet g1) g2
        g2_B = prepareFixAvoid [1,18] (vertexSet g1) g2

{-| Example showing a simple match relabelling failing as well as a successful fullUnion of Tgraphs.
The top right graph g2 is matched against the top left graph g1 
with g2 edge (1,10) matching g1 edge (1,15).
The bottom left shows the relabelling of g2 to match, but this is not correct because the overlap of
g2 and g1 is not a simple tile connected region.
(In the bottom left relabelled graph, vertex 41 does not get matched to 22 in g1, for example)
A call to relabelTouching is essential to produce a valid Tgraph.
The correct fullUnion is shown bottom right.
-}
incorrectAndFullUnionFig:: Diagram B
incorrectAndFullUnionFig = padBorder $ lw ultraThin $ vsep 1
                            [ hsep 1 $ center <$> take 2 thelist
                            , hsep 1 $ center <$> drop 2 thelist
                            ] where
     thelist = fmap (labelled drawj) $ rotations [0,7] $ fmap makeVP
                 [ g1
                 , g2
                 , matchByEdges (g1, (1,15)) (g2,(1,10))
                 , fullUnion  (g1, (1,15)) (g2,(1,10))
                 ]
     sunD2 = sunDs!!2
     fsunD2 = force sunD2
     g1 = removeFaces [RK (1,31,41)] (removeVertices [74,79,29] sunD2)
         --removeFaces [RK(1,16,36)] (removeVertices [20,48,49,35,37] sunD2)
     reduced2 = removeVertices [8,7,6,23] fsunD2
     g2 = relabelContig reduced2



{-| Example showing the use of commonFaces.
 This is applied to the pairs from forcedKingChoicesFig
-}
testCommonFacesFig :: Diagram B
testCommonFacesFig = padBorder $ vsep 1 $ fmap edgecase [(57,58),(20,38),(16,23),(49,59)] where
    fk = force kingGraph
    drawTracked = drawTrackedTgraph [draw, lc red . draw, drawWith (fillDK black black)]
    edgecase e = hsep 1 $ fmap (lw ultraThin) [drawTracked ttg1, drawTracked ttg2, drawCommonFaces (g1,(1,2)) (g2,(1,2))]
      where
        [ttg1, ttg2] = trackTwoChoices e fk
        g1 = tgraph ttg1
        g2 = tgraph ttg2


{-*
Inspection tools
-}

{- | A failure inspection tool.
If a Tgraph is found to be incorrect when forced, findMistake applied to the list of incorrect faces
will track back to g - the last successfully forced Tgraph and returns the next face added (the mistake) paired with g.
This relies on forcing order and new faces being added at the front of the list of faces by forcing.
-}
findMistake :: [TileFace] -> (TileFace,Tgraph)
findMistake [] = error "findMistake: ??"
findMistake (fc:fcs) = inspect fc fcs where
  inspect fc fcs = either (\_ -> inspect (head fcs) (tail fcs))
                          (\g -> (fc,g)) (tryForce $ makeUncheckedTgraph fcs)

{- | Another inspection tool. For a forced Tgraph g,
findCore g finds a Tgraph with the shortest tail of the faces of g that still produces g when forced.
It does this by removing faces from the front of the list of faces one at a time.
If g is not a forced Tgraph, the result will just be g or an error if g is found to be incorrect.
-}
findCore  :: Tgraph -> Tgraph
findCore g = if nullGraph g then g else inspect (faces g)
  where
    (top: _) = faces g
    inspect [] = error "findCore: not possible"
    inspect (fc:fcs) 
      = if top == head (faces $ force $ makeUncheckedTgraph fcs)
        then inspect fcs
        else makeUncheckedTgraph (fc:fcs)


checkSmartFig = padBorder $ hsep 1 [labelled (restrictSmart g draw) g, smart (labelled draw) g] where
    g = foolDminus
