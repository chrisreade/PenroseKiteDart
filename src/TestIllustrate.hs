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
import Data.Tree (Tree(..),levels) -- used for boundaryEdgeCaseTrees    
import qualified Data.Set as Set  (null,toList,delete) -- used for contexts

import ChosenBackend (B)
import TileLib
import Tgraphs
import TgraphExamples


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



{-|touchErrorFaces is an addition of 2 faces to those of foolD which contains touching vertices.
These will be caught by makeTgraph which raises an error.
The error is not picked up by checkedTgraph. It can be fixed using tryCorrectTouchingVs.

*** Exception: makeTgraph: touching vertices [(19,7)]

> checkedTgraph touchErrorFaces
Tgraph {maxV = 19, faces = ...}

> tryCorrectTouchingVs touchErrorFaces
Right (Tgraph {maxV = 18, faces = [..., LK (7,17,18)]})

test with:
padBorder $ drawjLabelled $ runTry $ tryCorrectTouchingVs touchErrorFaces
-}
touchErrorFaces::[TileFace]
touchErrorFaces = faces foolD ++ [RD (6,18,17),LK (19,17,18)]

-- |Diagram showing decomposition of (left hand) half-tiles.
decompHalfTiles :: Diagram B
decompHalfTiles = padBorder $ lw thin $ vsep 1 $ fmap centerX
   [ addArrow  "d" "decd" [ scale phi $ drawjLabelLarge d
               , drawjLabelLarge $ decompose d
               ]
   , addArrow  "k" "deck" [ scale phi $ drawjLabelLarge k
               , drawjLabelLarge $ rotate (ttangle 1) $ makeVP $ decompose k
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
            k = drawLabelled kingGraph # named "k"
            fk = drawLabelled fKing # named "fk"
            dk = smart drawLabelled (decompose kingGraph) # scale (phi-1) # named "dk"
            cfk = rotateBefore drawLabelled (ttangle 9) (compose fKing) # scale phi # named "cfk"

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


{-*
EmplaceChoices
-}

{- Now removed: foolChoices, emplace, makeChoices, forceLDB, tryForceLDB, forceLKC, tryForceLKC

-- |four choices for composing fool.
foolChoices :: Diagram B
foolChoices = padBorder $ vsep 1 
              [hsep 1 $ fmap ((redFool <>) . drawj) choices
              ,hsep 1 $ rotations [1,1,9,4] $ scale phi $ fmap (drawj . compose) choices
              ] where choices = makeChoices fool
                      redFool = drawj fool # lc red

{-| makeChoices no longer used
It is better to use forcedChoices.
makeChoices should only be used on a forced Tgraph.
It is a temporary tool which does not attempt to analyse choices for correctness.
It can thus create some choices which will be incorrect.
The unknowns returned from classifyDartWings can become largeKiteCentres or largeDartBases.
This produces 2^n choices where n is the number of unknowns.
-}
makeChoices :: Tgraph -> [Tgraph]
makeChoices g = choices unks [g] where
    unks = unknowns (getDartWingInfo g) -- g not forced may allow solitary wing tips which will fail
    choices [] gs = gs
    choices (v:more) gs = choices more (fmap (forceLKC v) gs ++ fmap (forceLDB v) gs)


-- |emplace does maximal composing with force and compose, 
-- then applies decompose and force repeatedly back to the starting level.
-- It produces the emplacement of influence of the argument graph.   
emplace:: Tgraph -> Tgraph
emplace g | nullGraph g' = fg
          | otherwise = (forceDecomp . emplace) g'
  where fg = force g
        g' = compose fg 

--  OLD VERSION of emplaceChoices
emplaceChoices:: Tgraph -> [Tgraph]
emplaceChoices g | nullGraph g' = emplace <$> makeChoices fg
                 | otherwise = forceDecomp <$> emplaceChoices g'
  where fg = force g
        g' = compose fg 


-- |For an unclassifiable dart wing v in a Tgraph, force it to become a large dart base (largeDartBase) by
-- adding a second half dart face (sharing the kite below the existing half dart face at v).
-- It raises an error if the result is a stuck/incorrect graph.
-- It assumes exactly one dart wing tip is at v, and that half dart has a full kite below it,
-- raising an error otherwise.
forceLDB :: Vertex -> Tgraph -> Tgraph
forceLDB v = runTry . tryForceLDB v

-- |A version of forceLDB which returns a Try Tgraph, with a Left report
-- if the result is a stuck/incorrect graph. 
-- It assumes exactly one dart wing tip is at v, and that half dart has a full kite below it,
-- returning a Left report  otherwise.  
tryForceLDB :: Vertex -> Tgraph -> Try Tgraph
tryForceLDB v g =
  let bd = makeBoundaryState g
      vFaces = facesAtBV bd v
      ks = filter ((==v) . oppV) $ filter isKite vFaces     
  in do d <- case find ((v==) . wingV) (filter isDart vFaces) of
               Just d -> Right d
               Nothing -> Left $ "forceLDB: no dart wing at " ++ show v ++ "/n"
        k <- case find ((/= oppV d) . wingV) ks of
               Just k -> Right k
               Nothing -> Left $ "forceLDB: incomplete kite below dart " ++ show d ++ "/n"
        u <- addDartShortE bd k
        bdC <- tryUpdate bd u
        return $ recoverGraph $ newBoundaryState bdC

-- |For an unclassifiable dart wing v in a Tgraph, force it to become a large kite centre (largeKiteCentres) by adding
-- 3 faces - a second half dart face sharing the long edge of the existing half dart face at v,
-- and then completing the kite on the new half dart short edge.
-- This assumes exactly one dart wing tip is at v.
-- (Note: farK for a half-dart d is that half of a full kite attached to the short edge of d
-- which does not share an edge with d). 
-- It is safe to add the 3 parts because v being unknown ensures the
-- existing dart has a boundary long edge and 
-- the new farK does not already exist (attached to existing dart farK),
-- provided the existing dart half has no kite or a full kite below.
-- If it has only a half kite below, but the new farK exists, then v will already be a crossing boundary.
forceLKC :: Vertex -> Tgraph -> Tgraph
forceLKC v = runTry . tryForceLKC v

-- |A version of forceLKC which returns a Try Tgraph, with a Left report
-- if the result is a stuck/incorrect graph. 
-- It assumes exactly one dart wing tip is at v, and that half dart has a full kite below it,
-- returning a Left report  otherwise.        
tryForceLKC :: Vertex -> Tgraph -> Try Tgraph
tryForceLKC v g = 
  do let bd0 = makeBoundaryState g
         vFaces0 = facesAtBV bd0 v
     d <- case find ((v==) . wingV) (filter isDart vFaces0) of
            Just d -> Right d
            Nothing -> Left $ "forceLKC: no dart wing at " ++ show v ++ "/n"
     u1 <- addDartLongE bd0 d
     bdC1 <- tryUpdate bd0 u1
     let bd1 = newBoundaryState bdC1
         vFaces1 = facesAtBV bd1 v
         newd = head (vFaces1 \\ vFaces0)
     u2 <- addKiteShortE bd1 newd
     bdC2 <- tryUpdate bd1 u2
     let bd2 = newBoundaryState bdC2
         vFaces2 = facesAtBV bd2 v
         newk = head (vFaces2 \\ vFaces1)
     u3 <- completeHalf bd2 newk
     bdC3 <- tryUpdate bd2 u3
     return $ recoverGraph $ newBoundaryState bdC3

-}


-- |diagram illustrating touching vertex situation and forced result.
-- The faces shown in lime are removed from a twice decomposed sun.
-- These are reconstructed by force (with other additional faces). The touching vertex restriction blocks
-- the bottom 4 face additions initially. 
touchingIllustration::  Diagram B
touchingIllustration =
  padBorder $ lw thin $ hsep 1
    [ drawjLabelled vpLeft <> (drawj vpGone # lc lime)
    , alignBefore drawjLabelled (8,3) $ force touchGraph
    ] where
      touchGraph = graphFromVP vpLeft
      vpLeft = removeFacesVP deleted vp
      vpGone = selectFacesVP deleted vp
      vp = makeVP sunD2
      sunD2 = sunDs!!2
      deleted = filter ((==1).originV) (faces sunD2) ++
                [LD (29,41,31),RK (31,79,29),LK (10,29,79),RK (10,79,75)]

-- |faces removed from foolD to illustrate crossing boundary and non tile-connected faces
-- (using VPatch to draw). Crossing boundary at 4 in first case (but still tile-connected),
-- Crossing boundary at 11 in second case and not tile-connected.
crossingBdryFig :: Diagram B
crossingBdryFig = padBorder $ hsep 1 [d1,d2]
       where d1 = drawjLabelled $ removeFacesVP [RK (3,11,13), LK (3,13,15), RK (3,15,4)] $ makeVP foolD
             d2 = drawjLabelled $ removeFacesVP [RK (5,11,2), LD (6,13,11), RD (6,15,13), LD (6,17,15)] $ makeVP foolD

-- |figure showing mistake Tgraph and the point at which forcing fails                
pfMistakeFig :: Diagram B
pfMistakeFig  = padBorder $ hsep 1 [drawjLabelled mistake, drawjLabelled partForcedMistake] where
   partForcedMistake = makeTgraph
                       [RK (9,1,11),LK (9,10,7),RK (9,7,5),LK (9,5,1),RK (1,2,4)
                       ,LK (1,3,2),RD (3,1,5),LD (4,6,1),LD (3,5,7),RD (4,8,6)
                       ]

-- |decompose mistake and the point at which forcing fails  with  RK (6,26,1)              
forcingDmistakeFig :: Diagram B
forcingDmistakeFig = padBorder $ hsep 1 [drawjLabelled (decompose mistake), drawjLabelled part] where
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
forcingD2mistakeFig = padBorder $ drawjLabelled partF where
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
partFMistake1Fig = padBorder $ drawjLabelled partF where
  partF = makeTgraph [RK (8,1,6),LK (7,5,1),RK (1,2,4),LK (1,3,2),RD (3,1,5),LD (4,6,1)]

-- |decomposed mistake1 is no longer incorrect and can be forced and recomposed
cdMistake1Fig :: Diagram B
cdMistake1Fig = padBorder $ hsep 1 $ fmap drawjLabelled $ scales [phi,1,1,phi] $ alignAll (1,2) $ fmap makeVP
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
                [ smart drawLabelSmall mistake4
                , smart drawLabelSmall mistake4'
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
-- (An empty Tgraph is represented as a lime circle with diagonal line through it)
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
    d1 = smartSub draw g1 vp
    d2 = smartSub draw g2 vp
    d3 = smartSub draw g3 vp
    d4 = if nullGraph g4
         then emptyRep
         else smartSub draw g4 vp
    d0 = scale phi $ alignBefore (smartSub draw g0) (1,2) g0

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
               Just loc -> circle 0.2 # fc red # lc red # moveTo loc
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
forceFillTest = padBorder $  lw ultraThin $ rotate (ttangle 1) $ drawForce g
    where g = checkedTgraph $ boundaryFaces $ makeBoundaryState $ dartDs!!6

-- |showing intermediate state of filling the inlet and closing the gap of boundaryGapFDart5
-- using stepForce 2000
gapProgress5 :: Diagram B
gapProgress5 = lw ultraThin $ vsep 1 $ center <$> rotations [1,1]
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

{-| kitePic0 is a diagram of force/emplacement relationships for decomposed kites
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
    xDGraphs = decompositions sunPlus3Dart'
    xDs  = rotations [9,9,8] $  phiScaling phi $ reverse $
           draw dartGraph : (draw sunPlus3Dart' # lc red # lw thin):
           take 2  (drop 1 $ fmap smartdraw xDGraphs)
    dots = center $ hsep 1 $ replicate 4 (circle 0.5 # fc gray # lw none)
    pointsRa = map p2 [ (0, 80), (42, 80), (95, 80), (150, 80), (200, 80)]
    pointsRb = map p2 [ (0, 40), (42, 40), (95, 40), (150, 40)]
    pointsRc = map p2 [ (0, 0),  (42, 0),  (95, 0),  (150, 0), (200, 0)]

{-| curioPic is a diagram illustrating where compose loses information not recovered by force
  with sunPlus3Dart' third item in bottom row, (curioPic0 is diagram without arrows)
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

-- |figure showing ordering of a decomposed kite (bottom), a test graph with an extra LK(3,6,8),
-- and forced figure at the top and composition of all 3 = kite on the right
graphOrder1 :: Diagram B
graphOrder1 = padBorder $ hsep 2 [center $ vsep 1 [ft,t,dcft], cft] where
              [cft,dcft,ft,t] = fmap drawjLabelled $ scales [phi] $ alignAll (1,2) $ fmap makeVP
                                [cftest, dcftest, ftest, test]
              dcftest = decompose cftest
              cftest = compose ftest
              ftest = force test
              test = makeTgraph [RK (4,7,2),LK (4,5,7),RD (1,7,5),LK (3,2,7)
                                ,RK (3,7,6),LD (1,6,7), LK (3,6,8)
                                ]


{-*
Testing (functions and figures and experiments)
-}
-- |diagrams of forced graphs for boundaryGapFDart4 and boundaryGapFDart5
testForce4, testForce5 :: Diagram B
testForce4 = padBorder $ lw ultraThin $ drawjLabelSmall $ force boundaryGapFDart4
testForce5 = padBorder $ lw ultraThin $ drawjLabelSmall $ force boundaryGapFDart5


{-| testViewBoundary is a testing tool to inspect the boundary vertex locations of some (intermediate) BoundaryState
-- (used in conjunction with stepForce to get an intermediate BoundaryState)
-- The boundary edges of a BoundaryState are shown in lime - using the BoundaryState positions of vertices.
-- This is overlaid on the full graph drawn with vertex labels.
-}
testViewBoundary :: BoundaryState -> Diagram B
testViewBoundary bd =  lc lime (drawEdges vpMap bdE) <> drawjLabelled g where
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

-- |Example for testing crossing boundary detection e.g. using 
-- checkedTgraph testCrossingBoundary, or by using
-- force (makeUncheckedTgraph testCrossingBoundary)
-- produces an error for a non-valid Tgraph.
testCrossingBoundary :: [TileFace]
testCrossingBoundary = [LK (1,8,3),RD (2,3,8),RK (1,3,9),LD (4,9,3),LK (5,10,13),RD (6,13,10)
                       ,LK (3,2,13),RK (3,13,11),RK (3,14,4),LK (3,11,14),LK (7,4,14),RK (7,14,12)
                       ]

-- |test wholeTiles (which adds missing second halves of each face)
checkCompleteFig:: Diagram B
checkCompleteFig =  padBorder $ hsep 1 $ fmap drawj [sunD4, wholeTiles sunD4] where sunD4 = sunDs !! 4

-- |test graphFromVP
checkGraphFromVP :: Diagram B
checkGraphFromVP = padBorder $ (draw . graphFromVP . makeVP) dartD4

-- |figure testing selectFacesVP by removing all kites
dartsOnlyFig :: Diagram B
dartsOnlyFig = padBorder $ lw thin $ draw $ selectFacesVP darts $ makeVP g where
    g = force $ sunDs !! 5
    darts = filter isDart $ faces g

{-*
Using TrackedTgraphs
-}
-- |hollowgraph illustrates an essential use of TrackedTgraphs.
-- Starting with fd2 = force (dartDs!!2) we want to make 3 further decompositions,
-- but also make 3 forced decompositions and then subtract the former faces from the latter.
-- The result happens to be a valid Tgraph but this is not generally the case.
-- TrackedTgraphs are essential to ensure numbering of new vertices in decompositions
-- match up in the 2 cases which they would not do if treated separately.
hollowGraph::Tgraph
hollowGraph = removeFaces (head (tracked exampleTracked)) (tgraph exampleTracked) where
  exampleTracked = iterate (forceTracked . decomposeTracked) (trackFaces (newTrackedTgraph fd2)) !!3
  fd2 = force (dartDs!!2)

-- |figure showing hollowGraph and result of forcing
forceHollowFig:: Diagram B
forceHollowFig = padBorder $  lw ultraThin $ hsep 1 $ fmap draw [hollowGraph, force hollowGraph]

{-
N.B.  Changes to forcing (or decomposing) can affect the vertex numbers chosen in twoChoices...
They should be the long edge of the left dart on the left of a group of 3 darts
Middle of top edge of dartDs!!4.  Use e.g
checkChoiceEdge (force $ dartDs !!4)
to view the vertex numbers
-}
-- |for Tgraph g produce diagram of forced version showing vertices (to inspect edges for selection)
checkChoiceEdge :: Tgraph -> Diagram B
checkChoiceEdge g = padBorder $ lw ultraThin $ drawLabelled $ force g

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
forceDartD4Fig = padBorder $ lw ultraThin $ drawjLabelSmall $ force dartD4
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


-- |What happens if you take the first result of twoChoices, then 
-- select only the faces that were added by force, then force just these faces?
-- It does not quite complete the original faces    
forcedNewFaces:: Diagram B
forcedNewFaces = padBorder $ lw thin $ drawForce g2 where
    g1 = addHalfDart (223,255) (force dartD4) --(233,201)
    g2 = removeFaces (faces g1) (force g1)

-- |Trying to find which extensions to the starting dart correspond to the twoChoicesFig
dartHalfDart,dartHalfKite,dartPlusDart,dartPlusKite :: Tgraph
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

sunPlusDart,sunPlus2Dart,sunPlus2Dart',sunPlus3Dart,sunPlus3Dart' :: Tgraph
-- |A sun with a single complete dart on the boundary
sunPlusDart = addHalfDart (3,4) $ addHalfDart (2,3) sunGraph
-- |A sun with 2 darts adjacent on the boundary
sunPlus2Dart = addHalfDart (5,6) $ addHalfDart (4,5) sunPlusDart
-- |A sun with 2 darts NOT adjacent on the boundary
sunPlus2Dart' = addHalfDart (7,8) $ addHalfDart (6,7) sunPlusDart
-- |A sun with 3 darts adjacent on the boundary
sunPlus3Dart = addHalfDart (7,8) $ addHalfDart (6,7) sunPlus2Dart
-- |A sun with 3 darts on the boundary NOT all adjacent
-- This example has an emplacement that does not include the original but is still a correct Tgraph
sunPlus3Dart' = addHalfDart (9,10) $ addHalfDart (8,9) sunPlus2Dart

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

-- | two kites (force decomp twice) figure
kkEmpsFig:: Diagram B
kkEmpsFig = padBorder $ lw ultraThin $ vsep 1 $ rotations [0,9,9] $
            fmap draw  [kk, kkD, kkD2] where
              kk = kitePlusKite
              kkD = force $ decompose kk
              kkD2 = force $ decompose kkD

-- | two kites added to related vertex types figure
maxShapesFig:: Diagram B
maxShapesFig = relatedVTypeFig ||| kkEmpsFig

-- |An example showing emplace is inferior to force.
-- (Emplace is no longer used, but was designed to force then maximally compose then forceDecomp repeatedly
-- to the starting level.)
-- On the left is sunPlus3Dart' - a sun with 3 darts on the boundary NOT all adjacent
-- Next to that is force sunPlus3Dart' which extends sunPlus3Dart'.
-- On the right is what emplace sunPlus3Dart' would produce.
-- The emplacement does not include all the original sunPlus3Dart'
emplaceProblemFig:: Diagram B
emplaceProblemFig = padBorder $ hsep 1 $ rotations [8,8] $ fmap draw
                        [ g
                        , force g
                        , (force . decompose . compose . force) g
                        ]
                where g = sunPlus3Dart'

-- | force after adding half dart (rocket cone) to sunPlus3Dart'.
-- Adding a kite half gives an incorrect graph discovered by forcing.
rocketCone1:: Tgraph
rocketCone1 =  force $ addHalfDart (59,60) $ forceDecomp sunPlus3Dart'

-- | figure for rocketCone
rocketCone1Fig:: Diagram B
rocketCone1Fig = padBorder $ lw thin $ hsep 1 $ fmap drawjLabelled [r1,rc1] where
  r1 = forceDecomp sunPlus3Dart'
  rc1 = force $ addHalfDart (59,60) r1

-- | figure for rocket5 showing its maximal forced composition
rocket5Fig:: Diagram B
rocket5Fig = padBorder $ lw ultraThin  drawWithMax rocket5

-- | rocket5 is the result of a chain of 5 forceDecomps, each after
-- adding a dart (cone) to the tip of the previous rocket starting with sunPlus3Dart'.
-- As a quick check rocket5 was extends with both choices on a randomly chosen boundary edge (8414,8415)
-- draw $ force $ addHalfKite rocket5 (8414,8415)
-- draw $ force $ addHalfDart rocket5 (8414,8415)
rocket5:: Tgraph
rocket5 = forceDecomp rc4 where
  rc0 = sunPlus3Dart'
  rc1 = force $ addHalfDart (59,60) (forceDecomp rc0)
  rc2 = force $ addHalfDart (326,327) (forceDecomp rc1)
  rc3 = force $ addHalfDart (1036,1037) (forceDecomp rc2)
  rc4 = force $ addHalfDart (3019,3020) (forceDecomp rc3)

-- |6 times forced and decomposed kingGraph. Has 53574 faces (now builds more than 60 times faster after profiling)
-- There are 2906 faces for kingD6 before forcing.
kingFD6:: Diagram B
kingFD6 = padBorder $ lw ultraThin $ colourDKG (darkmagenta, indigo, gold) $ makeVP $
          allForceDecomps kingGraph !!6



-- | Figure displaying all cases for boundary edges of forced Tgraphs.
-- These are produced as trees (but only the levels of the trees are displayed).
-- We start with an edge (shown red) of a face on the boundary after forcing the face.
-- There are only 3 (left-hand) starting face edges we need to consider, so there are 3 trees
-- (Right versions will be symmetric, and joins and dart short edges are immediately covered by forcing so not shown).
-- Each tree is grown by adding a kite/dart face at either end of the boundary edge
-- and forcing, terminating as a leaf node when the red edge is no longer on the boundary.
-- In each case, whenever there is a Tgraph where the red edge is still on the boundary,
-- that Tgraph appears extended with both a kite and a dart on the red edge amongst the diagrams below it in the tree.
-- This provides a completeness argument for forcing.
-- [The edge must be on the boundary if both additions are possible.]
boundaryEdgeCaseTrees:: Diagram B
boundaryEdgeCaseTrees = pad 1.02 $ centerXY $ lw ultraThin $ hsep 5  [vsep 10 [kiteShort,dartLong],kiteLong] where
--boundaryEdgeCaseTrees = pad 1.02 $ centerXY $ lw ultraThin $ vsep 13 $ fmap caseRows examples where
    [dartLong,kiteLong,kiteShort] = fmap caseRows examples
    examples = fmap  (makeTgraph . (:[])) [LD (1,3,2),LK (2,1,3),LK (3,2,1)]
    edge = (1,2)
    caseRows g = vsep 1 $ fmap (centerX . hsep 1) $ levels $ treeFor g
--    caseRows g = vsep 1 $ fmap (centerX . hsep 1 # composeAligned alignT) $ levels $ treeFor g
    treeFor g = drawCase <$> growBothEnds fbd where
      fbd = runTry $ tryForce $ makeBoundaryState g
      drawCase bd = fbdes <> drawg where
                    g = recoverGraph bd
                    vp = makeAlignedVP edge g
                    drawg = draw vp
                    fbdes = ((drawEdgeWith vp edge # lc red) <> drawEdgesIn vp (boundary fbd)) # lw thin

    addOnRight bd = -- add dart/kite on boundary edge starting at v then force each case
      case filter ((== snd edge). fst) (boundary bd) of
          [] -> []
          [de] -> atLeastOne $ tryDartAndKite bd de
    addOnLeft bd = -- add dart/kite on boundary edge ending at v then force each case
      case filter ((== fst edge). snd) (boundary bd) of
          [] -> []
          [de] -> atLeastOne $ tryDartAndKite bd de
-- growBothEnds:: BoundaryState -> Tree BoundaryState
    growBothEnds bd = goB bd where
      continue bd = edge `elem` boundary bd
-- to avoid repetitions, goB produces right and left cases but then recurses to the right only,
-- using goL to deal with left cases recursively.
      goB bd = if continue bd
               then Node{ rootLabel=bd, subForest = fmap goL (addOnLeft bd) ++ fmap goB (addOnRight bd)}
               else Node{ rootLabel=bd, subForest = []}
      goL bd = if continue bd
               then Node{ rootLabel=bd, subForest = fmap goL (addOnLeft bd)}
               else Node{ rootLabel=bd, subForest = []}


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
dartOriginContexts = fmap recoverGraph $ forcedBVContexts 1 (1,2) $ makeBoundaryState $ force $ makeTgraph [LD (1,3,2)]
kiteOriginContexts = fmap recoverGraph $ forcedBVContexts 2 (1,2) $ makeBoundaryState $ force $ makeTgraph [LK (2,1,3)]
kiteWingContexts   = fmap recoverGraph $ forcedBVContexts 1 (1,2) $ makeBoundaryState $ force $ makeTgraph [LK (2,1,3)]
kiteOppContexts    = fmap recoverGraph $ forcedBVContexts 1 (1,2) $ makeBoundaryState $ force $ makeTgraph [LK (3,2,1)]

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
              Just loc -> circle 0.2 # fc red # lc red # moveTo loc
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
              Just loc -> circle 0.05 # fc yellow # lc yellow # moveTo loc

-- | oneChoiceGraph is a forced Tgraph where one boundary edge (259,260) has one of its 2 legal extensions
-- an incorrect Tgraph
oneChoiceGraph:: Tgraph
oneChoiceGraph = force $ addHalfDart (37,59) $ force kingGraph

-- |Diagram showing superForce with initial Tgraph g (top), force g (middle), and superForce g (bottom)
superForceFig :: Diagram B
superForceFig = padBorder $ lw ultraThin $ vsep 1 $
  fmap (rotateBefore drawLabelSmall (ttangle 1)) [g, force g, superForce g] where
    g = addHalfDart (220,221) $ force $ decompositions fool !!3

-- |Diagram showing 4 rockets formed by applying superForce to successive decompositions
-- of sunPlus3Dart'. The decompositions are in red with normal force in black and superforce additions in blue.
superForceRocketsFig :: Diagram B
superForceRocketsFig = padBorder $ lw veryThin $ vsep 1 $ rotations [8,9,9,8] $
   fmap drawSuperForce decomps where
      decomps = take 4 $ decompositions sunPlus3Dart'

{-
-- |Diagram showing an incorrect (stuck) tiling (at the point where force discovers the clash with RK(219,140,222))
-- after adding a half kite to the nose of the red rocket (see also superForceRocketsFig).
wrongRocket:: Diagram B
wrongRocket = padBorder $ lw thin $ rotate (ttangle 3 )(gDiag # lc red <> wrongDiag) where
  wrongDiag =  alignBefore smartdraw (59,60) wrong 
  gDiag = draw $ makeAlignedVP (59,60) g where
  g = force $ decompose $ sunPlus3Dart'
  wrong = stuckGraphFrom $ addHalfKite (59,60) g
-}
{-
  wrong = makeUncheckedTgraph 
          [RK (219,140,222),LK (140,178,222),RD (221,222,178),LD (221,178,177),LD (181,220,176),RK (177,176,220)
          ,LK (219,218,140),LK (219,174,173),RK (219,173,218),RD (91,140,218),LD (91,218,173),LK (170,217,213)
          ,RK (170,216,217),LK (170,215,216),RK (170,214,215),RD (168,214,212),LK (170,212,214),RK (170,213,172)
          ,LD (171,172,213),LD (168,212,169),RK (170,169,212),RK (167,210,211),LK (167,208,210),RD (163,210,208)
          ,LD (163,209,210),RD (163,207,209),LD (163,208,166),RK (167,166,208),LD (163,162,207),RK (161,207,162)
          ,LK (161,206,207),RK (161,205,206),LK (161,204,205),RK (161,200,204),LK (161,203,200),RD (156,200,203)
          ,RK (161,160,203),LD (156,203,160),RD (202,199,158),LD (202,158,157),RD (202,157,201),LK (198,201,157)
          ,LK (200,156,199),LK (110,158,199),RK (110,199,156),RK (198,157,155),LD (155,159,198),RK (154,198,159)
          ,LK (154,197,198),RD (197,154,196),LK (195,196,154),RK (195,154,153),LK (195,153,152),RK (195,152,151)
          ,LK (149,194,190),RK (149,193,194),LK (149,192,193),RK (149,191,192),RD (174,191,189),LK (149,189,191)
          ,RK (149,190,150),LD (151,150,190),LD (174,189,90),RK (149,90,189),LD (148,188,183),RD (148,187,188)
          ,RK (164,187,165),LD (148,165,187),LK (146,186,179),RK (146,185,186),LK (146,184,185),RK (146,183,184)
          ,RD (148,183,182),LK (146,182,183),LD (148,182,145),RK (146,145,182),RD (181,176,144),LD (181,144,143)
          ,RD (181,143,180),LK (179,180,143),RK (146,179,147),RK (179,143,142),LD (142,147,179),RK (140,177,178)
          ,LK (140,175,177),RD (141,177,175),LK (177,141,176),LK (95,144,176),RK (95,176,141),LD (141,175,139)
          ,RK (140,139,175),RK (90,173,174),LK (90,138,173),RD (91,173,138),RD (171,129,172),LK (170,172,129)
          ,RD (171,132,131),LD (171,131,128),LD (171,130,129),RD (171,128,130),RK (170,129,127),LK (170,127,169)
          ,RK (77,169,127),LK (77,126,169),RD (168,169,126),LD (168,137,136),LD (168,126,125),RD (168,125,137)
          ,LK (167,123,166),RK (88,166,123),LK (88,122,166),RD (163,166,122),RD (148,120,165),LK (164,165,120)
          ,RK (164,120,119),LD (163,122,118),RD (163,118,117),LD (163,117,116),RD (163,116,162),LK (161,162,116)
          ,RK (161,116,115),LK (161,115,160),RK (51,160,115),LK (51,114,160),RD (156,160,114),RD (155,112,159)
          ,LK (154,159,112),RK (110,157,158),LK (110,155,157),LD (156,111,110),LD (156,114,108),RD (156,108,111)
          ,LD (155,113,112),RD (155,110,109),LD (155,109,107),RD (155,107,113),RK (154,112,106),LD (106,153,154)
          ,RD (106,152,153),LD (106,105,152),RK (103,152,105),LK (103,151,152),RD (151,103,150),LK (149,150,103)
          ,LK (149,104,90),RK (149,103,104),LD (148,121,120),RD (148,145,101),LD (148,101,100),RD (148,100,121)
          ,RD (142,98,147),LK (146,147,98),RK (146,98,97),LK (146,97,145),LK (49,101,145),RK (49,145,97)
          ,RK (95,143,144),LK (95,142,143),LD (142,99,98),RD (142,95,94),LD (142,94,93),RD (142,93,99)
          ,LD (141,96,95),RD (141,139,102),LD (141,102,92),RD (141,92,96),LK (140,91,139),LK (57,102,139)
          ,RK (57,139,91),LD (91,138,59),RK (90,59,138),LK (86,137,125),RK (86,136,137),LK (86,135,136)
          ,RK (86,124,135),LD (119,85,134),RK (80,134,85),LK (80,133,134),RK (80,132,133),LK (80,131,132)
          ,RK (80,128,131),LK (79,130,128),RK (79,129,130),LK (79,127,129),LK (80,73,128),RK (79,128,73)
          ,RD (127,79,78),LD (127,78,77),RK (77,125,126),LK (77,84,125),RK (86,125,84),LK (86,89,124)
          ,RD (123,124,89),LD (123,89,88),RK (88,118,122),LK (82,121,100),RK (82,120,121),LK (82,119,120)
          ,RD (119,82,85),LK (53,117,118),LK (88,69,118),RK (53,118,69),RK (53,116,117),LK (53,115,116)
          ,RD (115,53,52),LD (115,52,51),RK (51,108,114),LK (43,113,107),RK (43,112,113),LK (43,106,112)
          ,LK (7,111,108),RK (7,110,111),LK (7,109,110),RK (7,107,109),LK (51,6,108),RK (7,108,6),LK (7,8,107)
          ,RK (43,107,8),RD (106,43,62),LD (106,62,61),RD (106,61,105),LK (103,105,61),LD (60,104,103)
          ,RD (60,90,104),RK (103,61,60),RK (57,92,102),RK (49,100,101),RK (82,100,67),LK (49,67,100)
          ,LK (41,99,93),RK (41,98,99),LK (41,97,98),LD (97,50,49),RD (97,41,50),LK (11,96,92),RK (11,95,96)
          ,LK (11,94,95),RK (11,93,94),RK (41,93,2),LK (11,2,93),RK (11,92,10),LK (57,10,92),LD (91,58,57)
          ,RD (91,59,58),LK (90,60,59),RK (86,88,89),LK (86,87,88),RD (69,88,87),LD (69,87,83),RK (86,83,87)
          ,LK (86,84,83),LK (80,85,82),RD (84,77,76),LD (84,76,68),RK (68,83,84),LK (68,75,83),RD (69,83,75)
          ,RK (80,82,81),LD (67,81,82),RD (67,74,81),LK (80,81,74),RK (80,74,73),LD (73,72,79),RK (71,79,72)
          ,LK (71,78,79),RK (71,77,78),LK (71,76,77),RK (71,68,76),LD (69,75,55),RK (68,55,75),LD (67,66,74)
          ,RK (64,74,66),LK (64,73,74),RD (73,64,72),LK (71,72,64),LK (71,70,68),RK (71,64,65),LK (71,65,63)
          ,RK (71,63,70),LD (56,70,63),RD (56,68,70),LD (69,54,53),RD (69,55,54),LK (68,56,55),RD (67,49,48)
          ,LD (67,48,47),RD (67,47,66),LK (64,66,47),LD (46,65,64),RD (46,63,65),RK (64,47,46),RK (63,42,56)
          ,LK (63,45,42),LK (63,46,44),RK (63,44,45),LK (14,62,43),RK (14,61,62),LK (14,60,61),RK (14,59,60)
          ,LK (14,58,59),RK (14,57,58),LK (14,40,57),RD (10,57,40),LK (13,56,42),RK (13,55,56),LK (13,54,55)
          ,RK (13,53,54),LK (13,52,53),RK (13,51,52),LK (13,38,51),RD (6,51,38),LK (12,50,41),RK (12,49,50)
          ,LK (12,48,49),RK (12,47,48),LK (12,46,47),RK (12,44,46),LD (4,45,44),RD (4,42,45),LK (12,36,44)
          ,RD (4,44,36),RK (14,43,39),LD (8,39,43),RK (13,42,37),LD (4,37,42),RK (12,41,35),LD (2,35,41)
          ,RK (14,9,40),LD (10,40,9),LK (14,39,9),RD (8,9,39),RK (13,5,38),LD (6,38,5),LK (13,37,5),RD (4,5,37)
          ,RK (12,3,36),LD (4,36,3),LK (12,35,3),RD (2,3,35),RK (11,16,2),LK (11,34,16),RD (1,16,34),LK (3,2,16)
          ,RK (3,16,18),LD (1,18,16),RK (3,20,4),LK (3,18,20),RD (1,20,18),LK (5,4,20),RK (5,20,22),LD (1,22,20)
          ,RK (5,24,6),LK (5,22,24),RD (1,24,22),LK (7,6,24),RK (7,24,26),LD (1,26,24),RK (7,28,8),LK (7,26,28)
          ,RD (1,28,26),LK (9,8,28),RK (9,28,30),LD (1,30,28),RK (9,32,10),LK (9,30,32),RD (1,32,30)
          ,LK (11,10,32),RK (11,32,34),LD (1,34,32)]
              
-}

{- |
This figure shows a successfully forced Tgraph (oneChoiceGraph) and below is an extension (added half kite)
on edge (76,77) which fails on forcing showing it is an incorrect Tgraph, and below that a successful extension
(added half dart on the same boundary edge) after forcing.
It establishes that a single legal face addition to a forced Tgraph can be an incorrect Tgraph.
-}
oneChoiceFig:: Diagram B
oneChoiceFig = padBorder $ lw ultraThin $ vsep 1 $
                     fmap (smart drawLabelSmall) [oneChoiceGraph,incorrectExtension,successful] where
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
--         bs0 = makeBoundaryState $ force kingGraph

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
  where eight = fmap drawjLabelSmall 
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

{-| Example showing match relabelling failing as well as a successful fullUnion of graphs.
The top right graph g2 is matched against the top left graph g1 
with g2 edge (1,10) matching g1 edge (1,15).
The bottom left shows the relabelling to match, but this is not correct because the overlap of
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
     thelist = fmap drawjLabelled $ rotations [0,7] $ fmap makeVP
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
    (top:rest) = faces g
    inspect [] = error "findCore: not possible"
    inspect (fc:fcs) 
      = if top == head (faces $ force $ makeUncheckedTgraph fcs)
        then inspect fcs
        else makeUncheckedTgraph (fc:fcs)





{-*
Testing Try functions
-}

-- | testing Try  - try arguments 0..5  only 4 succeeds
reportFailTest1 :: Int -> (Int,Int,Int,Int,Int)
reportFailTest1 a = runTry $ onFail "reportFailTest1:\n" $ do
  b <- maybeExample a `nothingFail` ("maybeExample: produced Nothing when applied to "++show a ++"\n")
  c <- onFail "first call:\n"  $ eitherExample (b-1)
  d <- onFail "second call:\n" $ eitherExample (c-1)
  e <- onFail "third call:\n"  $ eitherExample (d-1)
  pure (a,b,c,d,e)

-- | testing Try  - try arguments 0..3   only 2 succeeds
reportFailTest2 :: Int -> (Int,Maybe Int,Int,Int)
reportFailTest2 a = runTry $ onFail "reportFailTest2:\n" $ do
  b <- eitherMaybeExample a
  c <- b `nothingFail` "eitherMaybeExample produced Nothing\n"
  d <- onFail "trying eitherExample:\n" $ eitherExample (c-1)
  pure (a,b,c,d)

-- | for testing in reportFailTest1 and reportFailTest2
eitherExample :: Int -> Try Int
eitherExample a = if a==0 then Left "eitherExample: arg is zero\n" else Right a

-- | for testing in reportFailTest1
maybeExample :: Int -> Maybe Int
maybeExample a = if a<5 then Just a else Nothing

-- | for testing in reportFailTest1 and reportFailTest2
eitherMaybeExample :: Int -> Try (Maybe Int)
eitherMaybeExample a | a<1 = Right Nothing
                     | a<3 = Right (Just a)
                     | otherwise = Left "eitherMaybeExample: arg >=3\n"

