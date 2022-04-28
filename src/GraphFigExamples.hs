--{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# LANGUAGE FlexibleContexts          #-}
--{-# LANGUAGE TypeFamilies              #-}

module GraphFigExamples where

import Data.List ((\\), nub)      
import Diagrams.Prelude

import ChosenBackend (B)
import TileLib
import Tgraphs
import Tgraph.Convert


-- | used for most diagrams
padBorder = pad 1.2 . centerXY







{-
    **************************
    Some Tgraphs and Figures
    *************************
-}
fool, foolD, foolDminus:: Tgraph
-- fool: fool's kite - a decomposed left and right kite back-to-back (i.e. not sharing join edge)
fool = checkTgraph [ RD(1,2,3)
                   , LD(1,3,4)
                   , RK(6,2,5)
                   , LK(6,3,2)
                   , RK(6,4,3)
                   , LK(6,7,4)
                   ]

foolD = decomposeG fool -- or foolDs!!1

foolDs :: [Tgraph]
foolDs = decompositionsG fool

-- foolDminus: 3 faces removed from foolD
foolDminus = removeFaces [RD(6,14,11), LD(6,12,14), RK(5,13,2)] foolD

foolFig :: Diagram B
foolFig = padBorder $ drawVGraph fool

foolAndFoolD :: Diagram B
foolAndFoolD = padBorder $ hsep 1 [(drawVPatch . scale phi . makeVPatch) fool, drawVGraph foolD]




-- sun and sunDs
sunGraph :: Tgraph
sunGraph = checkTgraph [ RK(1,2,11), LK(1,3,2)
                       , RK(1,4,3) , LK(1,5,4)
                       , RK(1,6,5) , LK(1,7,6)
                       , RK(1,8,7) , LK(1,9,8)
                       , RK(1,10,9), LK(1,11,10)
                       ]
sunDs :: [Tgraph]
sunDs =  decompositionsG sunGraph

figSunD3D2,figSunD2D:: Diagram B
figSunD3D2 = padBorder $ hsep 1 [drawVGraph $ sunDs !! 3, scale phi $ drawVGraph $ sunDs !! 2]
figSunD2D = padBorder  $ hsep 1 [drawVGraph $ sunDs !! 2, scale phi $ drawVGraph $ sunDs !! 1]

-- kite and kiteDs
kiteGraph :: Tgraph
kiteGraph = checkTgraph [ RK(1,2,4), LK(1,3,2)]
kiteDs :: [Tgraph]
kiteDs = decompositionsG kiteGraph

-- dart and dartDs
dartGraph :: Tgraph
dartGraph =  checkTgraph [ RD(1,2,3), LD(1,3,4)]
dartDs :: [Tgraph]
dartDs =  decompositionsG dartGraph



{-
  *************************************
  Compositions and figures
  *************************************
-}

-- | Shows labelled vertices For checking partCompose to get alignment vertices
checkPCompose :: Tgraph -> Diagram B
checkPCompose g = 
        padBorder $ hsep 1 [drawVPatch $ selectFacesGtoVP fcs g, scale phi $ drawVGraph g'] where
        (fcs,g') = partCompose g

{- | showPCompose g (a,b)  applies partCompose to g, then aligns and draws the composed graph with the remainder faces (in lime)
It can cope with an empty composed graph.
Vertices a and b must be common to composed g and g
(use checkPCompose to choose vertices)                               
-}
showPCompose ::  Tgraph -> (Vertex, Vertex) -> Diagram B
showPCompose g (a,b) = case nullGraph g' of
    True -> lc lime $ dashJPatch $ dropVertices remg
    False -> lw ultraThin (drawPatch $ dropVertices compg)
             <> lc lime (dashJPatch $ dropVertices remainder)
                 where [remainder,compg] = alignAll (a,b) [remg , scale phi $ makeVPatch g']
  where (fcs,g') = partCompose g
        remg = selectFacesGtoVP fcs g
 

pCompFig1,pCompFig2,pCompFig:: Diagram B
pCompFig1 = hsep 5 $ rotations [1] [drawGraph fd3 # lw ultraThin, showPCompose fd3 (1,3)]
            where fd3 = force $ dartDs !! 3
pCompFig2 = hsep 5 $ rotations [] [drawGraph fk3 # lw ultraThin, showPCompose fk3 (1,2)]
            where fk3 = force $ kiteDs !! 3
pCompFig = padBorder $ vsep 3 [center pCompFig1, center pCompFig2]


{-
  ********************
  Forced graph figures
  ********************
-}
              
forceFoolDminus :: Diagram B              
forceFoolDminus = padBorder $ hsep 1 $ fmap drawVGraph [foolDminus, force foolDminus]

-- new version of drawForce using subTgraphs instead of alignments
drawForce:: Tgraph -> Diagram B
drawForce g = drawSubTgraph [lw ultraThin . drawPatch, lc red .lw thin . drawPatch ]
               $ forceSub $ makeSubTgraph g [faces g]

forceDartD3Fig,forceDartD5Fig,forceKiteD3Fig,forceKiteD5Fig,forceSunD5Fig,forceFig:: Diagram B
forceDartD3Fig = padBorder $ rotate (ttangle 1) $ drawForce $ dartDs !! 3
forceDartD5Fig = padBorder $ drawForce $ dartDs !! 5
forceKiteD3Fig = padBorder $ drawForce $ kiteDs !! 3
forceKiteD5Fig = padBorder $ rotate (ttangle 9) $ drawForce $ kiteDs !! 5
forceSunD5Fig =  padBorder $ drawForce $ sunDs  !! 5
forceFig = hsep 1 [forceDartD5Fig,forceKiteD5Fig]


{-
  *************************************
  Emplacements and emplacement figures
  *************************************
-}

-- | drawEmbed (a,b) g1 g2 embeds  g1 (coloured red) onto g2, by aligning with (a,b)
-- vertices a and b must be common to g1 and g2 and scales are the same
drawEmbed ::  Tgraph -> Tgraph -> (Vertex, Vertex) -> Diagram B
drawEmbed g1 g2 = drawEmbedVP (makeVPatch g1) (makeVPatch g2)

-- | drawEmbedVP (a,b) vp1 vp2 embeds  vp1 (coloured red) onto vp2, after aligning with (a,b)
-- vertices a and b must be common to vp1 and vp2
drawEmbedVP ::  VPatch -> VPatch -> (Vertex, Vertex) -> Diagram B
drawEmbedVP vp1 vp2 (a,b) = 
    lc red (lw thin $ drawPatch $ dropVertices $ alignXaxis (a,b) vp1) 
    <>      lw ultraThin (drawPatch $ dropVertices $ alignXaxis (a,b) vp2)

-- use checkEmbed to find a common pair of vertices to align (for use with drawEmbed)
checkEmbed :: Tgraph -> Tgraph -> Diagram B
checkEmbed g1 g2 = padBorder $ lw ultraThin $ vsep 1 $
                   fmap drawVGraph [g1, g2]


 
-- | returns the join edge with lowest origin (and lowest oppV of faces with that origin)
lowestJoin:: Tgraph -> (Vertex,Vertex)
lowestJoin = joinOfTile . head . chooseLowest . faces
{-
drawMaxEmplace g produces the emplacement of g overlaid with the maximal (forced) omposition of g
-}
drawMaxEmplace :: Tgraph -> Diagram B
drawMaxEmplace g =  drawEmbedVP overlay emp (lowestJoin maxg) where
    fcomps = allFComps g
    n = length fcomps -1
    maxg = last fcomps
    overlay = scale (phi^n) $ makeVPatch $ maxg
    emp = makeVPatch $ emplacements maxg !! n
    
maxEmplaceFig :: Diagram B
maxEmplaceFig = padBorder $ drawMaxEmplace $ decompositionsG dartPlusDart !! 4

{-
    EXPERIMENTAL
-- | empForceSubs g shows an infinite list of emplacements of a graph as SubTgraphs with 2 assumptions
-- (1) It assumes force g = emplace g  (which is NOT always true)
--     This assumption allows us to use SubTgraphs to avoid finding alignment vertices
-- (2) it could give incorrect results if g is not wholeTile complete so incomplete tiles are removed first.
empForceSubs:: Tgraph -> [SubTgraph]
empForceSubs g = 
  iterate (forceSub . decomposeSub) $ forceSub $ makeSubTgraph g' [faces g'] where
      g' = removeIncompleteTiles g

newDartPlusFig = padBorder $ vsep 1 $ rotations [2,4] $ fmap (lw ultraThin . drawSubTgraph1)
    [ empForceSubs dartHalfDart !! 5, empForceSubs dartPlusDart !! 5]

-}




{-
  *************************
  Multi emplace and choices
  *************************
-}

-- four choices for composing fool
foolChoices :: Diagram B
foolChoices = padBorder $ vsep 1 
              [hsep 1 $ fmap ((redFool <>) . dashJGraph) choices
              ,hsep 1 $ rotations [1,1,9,4] $ scale phi $ fmap (dashJGraph . composeG) choices
              ] where choices = makeChoices fool
                      redFool = dashJGraph fool # lc red
                         
-- | emplacement choices for foolD 
--  WARNING: relies on numbering which can change with changes to forcing
--  Vertex 1 is not present in the final choice (hense 29)
emplaceFoolDChoices :: Diagram B
emplaceFoolDChoices = padBorder $ hsep 1 $
        fmap (addFoolD . lw ultraThin . drawPatch . dropVertices) vpChoices where
        (vpFoolD:vpChoices) = alignments [(1,6),(1,6),(1,6),(1,6),(29,6)] 
                                         (fmap makeVPatch (foolD:emplaceChoices foolD))
        addFoolD fig = (lc red . dashJPatch . dropVertices) vpFoolD <> fig



{-
  ************************************************
  Forcing and Composing Tgraphs with removed faces
  ************************************************
-}
dartD4 :: Tgraph
dartD4 = dartDs!!4

-- brokenDart gets repaired by forcing but can also be composed to a maximal graph
brokenDart :: Tgraph
brokenDart = removeFaces deleted dartD4 where
  deleted = [RK(2,15,31),LD(20,31,15),RK(15,41,20),LK(15,30,41),LK(5,20,41)] 

{- | badlyBrokenDart gets repaired by forcing but will fail to produce a valid graph
     if composed twice without forcing 
     *** Exception: checkTgraph: crossing boundaries found at [3]
     in
     Tgraph {vertices = [4,6,3,1,5], faces = [LD (4,6,3),LK (1,5,3)]}
-}
badlyBrokenDart :: Tgraph
badlyBrokenDart = removeFaces deleted dartD4 where
  deleted = [RK(2,15,31),LD(20,31,15),RK(15,41,20),LK(15,30,41),LK(5,20,41)] 
            ++[LK(11,21,44),RK(11,44,34),LD(9,34,44),RD(9,44,35),LK(17,35,44),RK(17,44,21),RK(6,17,33)]
 
-- brokenDartFig shows the faces removed from dartD4 to make brokenDart and badlyBrokenDart
brokenDartFig :: Diagram B
brokenDartFig = padBorder $ lw thin $ hsep 1 $ fmap dashJGraph [dartD4, brokenDart, badlyBrokenDart]
-- the force of the worst case (badlyBrokenDart)
checkBrokenDartFig  :: Diagram B
checkBrokenDartFig = drawGraph $ force badlyBrokenDart

-- First 2 adjacent kites are decomposed. 
-- Next brokenKites is the result of removing the top decomposed half kite.
-- Next is the result of Composing (also force then compose)
-- Lastly the emplacement of brokenKites
-- The figure illustrates that brokenKites is included in emplace brokenKites
-- even though the missing part is not repaired.
brokenKitesDFig :: Diagram B
brokenKitesDFig = padBorder $ hsep 1 $ fmap drawVPatch $ alignAll (1,3) $ scales [1,1,phi] $ fmap makeVPatch 
                 [decomposeG twoKites,brokenKites, composeG brokenKites, emplace brokenKites]
brokenKites = removeFaces [LD(1,14,16),LK(5,4,16),RK(5,16,14)] $ decomposeG twoKites
twoKites = checkTgraph [ RK(1,2,11), LK(1,3,2)
                       , RK(1,4,3) , LK(1,5,4)
                       ]

-- diagram of touching vertex situation and forced result
touchingTestFig::  Diagram B
touchingTestFig = 
  padBorder $ hsep 1
    [ drawVPatch vpLeft <> (dashJPatch (dropVertices vpGone) # lc lime)
    , drawVPatch $ alignXaxis (6,32) $ makeVPatch $ force touchGraph
    ] where    
      touchGraph = graphFromVP vpLeft
      vpLeft = removeFacesVP deleted vp
      vpGone = selectFacesVP deleted vp
      vp = makeVPatch sunD2
      sunD2 = sunDs!!2
      deleted = filter ((==1).originV) (faces sunD2) ++
                [LD(20,36,16),RK(16,49,20),LK(8,20,49),RK(8,49,37)]


{- 
    *****************************************
    Incorrect graphs and other problem graphs
    *****************************************
-}
  

--- faces removed from foolD to illustrate crossing boundary and non-face-connected VPatches
crossingBdryFig :: Diagram B
crossingBdryFig = padBorder $ hsep 1 [d1,d2]
       where d1 = drawVPatch $ removeFacesGtoVP [LK(3,11,14), RK(3,14,4), RK(3,13,11)] foolD
             d2 = drawVPatch $ removeFacesGtoVP [RK(5,13,2), LD(6,11,13), RD(6,14,11), LD(6,12,14)] foolD

-- | mistake is an erroneous graph with a kite bordered by 2 darts
mistake:: Tgraph
mistake = checkTgraph [RK(1,2,4), LK(1,3,2), RD(3,1,5), LD(4,6,1), LD(3,5,7), RD(4,8,6)]

-- mistake and the point at which forcing fails                
pfMistakeFig :: Diagram B
pfMistakeFig  = padBorder $ hsep 1 [drawVGraph mistake, drawVGraph partForcedMistake] where
   partForcedMistake = 
       makeTgraph [RK (9,1,11),LK (9,10,7),RK (9,7,5),LK (9,5,1),RK (1,2,4)
                  ,LK (1,3,2),RD (3,1,5),LD (4,6,1),LD (3,5,7),RD (4,8,6)
                  ]
   
-- decomposeG mistake and the point at which forcing fails  with  RK (6,26,1)              
forcingDmistakeFig :: Diagram B
forcingDmistakeFig = padBorder $ hsep 1 [drawVGraph (decomposeG mistake), drawVGraph part] where
    part =  makeTgraph [RK (26,24,1),RK (5,24,25),LK (5,1,24),RK (3,23,2),LK (3,22,23)
                       ,RK (3,21,22),LK (3,15,21),LK (4,2,20),RK (4,20,19),LK (4,19,18),RK (4,18,17)
                       ,LK (4,17,16),RK (4,16,12),LD (8,12,16),RK (3,14,15),LK (3,11,14),RD (7,14,11)
                       ,RK (4,13,2),LK (4,9,13),RD (1,13,9),LK (3,2,13),RK (3,13,10),LD (1,10,13)
                       ,LK (3,10,5),RD (1,5,10),RK (4,6,9),LD (1,9,6),RK (3,5,11),LD (7,11,5)
                       ,LK (4,12,6),RD (8,6,12)
                       ]

{- |  forcingD2mistakeFig
    Figure showing a stuck graph with error at vertex 35 
    This is the result of twice decomposed mistake which fails when forced
    *** Exception: errorcheckClock:  Found incorrect graph
    Conflict at vertex: 35
    forcingD2mistake is the graph when the error is discovered.
-}
forcingD2mistakeFig :: Diagram B
forcingD2mistakeFig = padBorder $ drawVGraph partF where
  partF = makeTgraph [LK (78,46,35),LK (78,47,45),RK (78,45,46),LK (7,77,73),RK (7,76,77),LK (7,75,76),RK (7,74,75)
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


-- | mistake1 is a kite bordered by 2 half kites (subgraph of mistake and still erroneous)
mistake1:: Tgraph
mistake1 = checkTgraph [RK(1,2,4), LK(1,3,2), RD(3,1,5), LD(4,6,1)]

-- | partially forced mistake1 (at the point of discovery of incorrect graph
partFMistake1Fig:: Diagram B
partFMistake1Fig = padBorder $ drawVGraph partF where
  partF = makeTgraph [RK (8,1,6),LK (7,5,1),RK (1,2,4),LK (1,3,2),RD (3,1,5),LD (4,6,1)]

-- decomposed mistake1 is no longer erroneous and can be forced and recomposed
cdMistake1Fig :: Diagram B
cdMistake1Fig = padBorder $ hsep 1 $ fmap drawVPatch $ scales [phi,1,1,phi] $ alignAll (1,2) $ fmap makeVPatch
               [ mistake1 , mistake1D, force mistake1D, composeG mistake1D]
               where mistake1D = decomposeG mistake1

{-
  ****************************
  Figures of 7 types of vertex
  ****************************
-}
   
{- | vertexTypesFig is 7 vertex types single diagram as a row -}
vertexTypesFig = padBorder $ hsep 1 lTypeFigs
 where
 lTypeFigs = zipWith labelD ["sun","star","jack","queen","king","ace","deuce"] vTypeFigs
 vTypeFigs = zipWith drawVertex 
               [sunGraph, starGraph, jackGraph, queenGraph, kingGraph, aceGraph,  deuceGraph]
               [(1,2),    (1,2),     (1,2),     (1,2),      (1,2),     (3,6),     (2,6)] -- alignments
 drawVertex g alm = lw thin $ showOrigin $ dashJPatch $ dropVertices $ alignXaxis alm $ makeVPatch g

labelD :: String -> Diagram B -> Diagram B
labelD l d = baselineText l # fontSize (local 0.5) # fc blue <> d # moveTo (p2(0,2.2))

{- graphs for the 7 vertex types sunGraph, starGraph, jackGraph, queenGraph, kingGraph, aceGraph,  deuceGraph -}
jackGraph = makeTgraph [LK (1,9,11),RK (1,11,2),LK (7,8,1),RD (9,1,8),RK (1,3,4)
                       ,LK (1,2,3),RK (7,1,5),LD (4,5,1),LD (9,8,10),RD (4,6,5)
                       ]
kingGraph = makeTgraph [LD (1,2,3),RD (1,11,2),LD (1,4,5),RD (1,3,4),LD (1,10,11)
                       ,RD (1,9,10),LK (9,1,7),RK (9,7,8),RK (5,7,1),LK (5,6,7)
                       ]
queenGraph = makeTgraph [LK (7,1,5),RK (3,5,1),LD (1,2,3),RK (7,9,1),LK (11,1,9)
                        ,RD (1,11,2),RK (7,5,6),LK (7,8,9),LK (3,4,5),RK (11,9,10)
                        ]

aceGraph = fool -- centre 3
deuceGraph = makeTgraph [LK (7,8,2),RK (7,2,6),RK (5,2,4),LK (5,6,2),LD (1,4,2)
                        ,RD (1,2,8),RD (1,3,4),LD (1,8,9)
                        ] -- centre 2

starGraph = makeTgraph [LD (1,2,3),RD (1,11,2),LD (1,10,11),RD (1,9,10),LD (1,8,9)
                       ,RD (1,7,8),LD (1,6,7),RD (1,5,6),LD (1,4,5),RD (1,3,4)
                       ]


{- |  forceVFigures is a list of 7 diagrams - force of 7 vertex types -}
forceVFigures :: [Diagram B]
forceVFigures = rotations [0,0,9,5,0,0,1] $
                fmap drawForce [sunGraph,starGraph,jackGraph,queenGraph,kingGraph,aceGraph,deuceGraph]


{- | forceVsFig shows emplacements of 7 vertex types in a row as single diagram -}
forceVsFig :: Diagram B
forceVsFig = padBorder $ hsep 1 forceVFigures

{- | relatedVTypeFig lays out figures from forceVFigures plus a kite as single diagram with 3 columns -}
relatedVTypeFig = padBorder $
 atPoints [p2(0,15),p2(0,10),   p2(8,15),p2(9,10),p2(9,1),  p2(18,15),p2(18,10),p2(20,1) ]
          [sunF,    starF,      aceF,    jackF,    kingF,     kite,     deuceF,   queenF]
 where kite = drawGraph kiteGraph # lw thin
       sunF = forceVFigures!!0
       starF = forceVFigures!!1
       jackF = forceVFigures!!2
       queenF = forceVFigures!!3
       kingF = forceVFigures!!4
       aceF = forceVFigures!!5
       deuceF = forceVFigures!!6

{-
  *************************
  Other Figures
  *************************
-}


-- graphs of the boundary faces only of forced graphs (dartDs!!4 and dartDs!!5)
boundaryFDart4, boundaryFDart5 :: Tgraph
boundaryFDart4 =  
    checkTgraph $ boundaryFaces $ makeBoundary $ force (dartDs!!4)
boundaryFDart5 =  
    checkTgraph $ boundaryFaces $ makeBoundary $ force (dartDs!!5)


-- graphs of the boundary faces only of a forced graph - with extra faces removed to make a gap
boundaryGapFDart4, boundaryGapFDart5 :: Tgraph
boundaryGapFDart4 =   
    checkTgraph $ filter ((/=332).originV)  (faces boundaryFDart4)
boundaryGapFDart5 =   
    checkTgraph $ filter ((/=1287).originV) (faces boundaryFDart5)

-- figures for the boundary gap graphs boundaryGapFDart4, boundaryGapFDart5
boundaryGap4Fig, boundaryGap5Fig :: Diagram B
boundaryGap4Fig = lw ultraThin $ drawVGraph boundaryGapFDart4
boundaryGap5Fig = lw ultraThin $ drawVGraph boundaryGapFDart5

-- showing intermediate state of filling the inlet and closing the gap of boundaryGapFDart5
-- using stepForce 2000
gapProgress5 :: Diagram B
gapProgress5 = lw ultraThin $ vsep 1 $ fmap center $ rotations [1,1]
    [ dashJGraph g
    , drawGraph $ recoverGraph $ boundaryState $ stepForce 2000 g
    ] where g = boundaryGapFDart5

-- showing intermediate state of filling the inlet and closing the gap of boundaryGapFDart4
-- using stepForce 600
gapProgress4 :: Diagram B
gapProgress4 = lw ultraThin $ hsep 1 $ fmap center $ rotations [5,5]
    [ dashJGraph g
    , drawGraph $ recoverGraph $ boundaryState $ stepForce 600 g
    ] where g = boundaryGapFDart4


{- | bigPic is a diagram illustrating force/emplacement relationships for decomposed darts
     bigPic0 is main diagram for bigPic without the arrows
-}
bigPic0,bigPic :: Diagram B
bigPic0 = padBorder $ position $ concat
          [ zip pointsR1 partComps
          , zip pointsR2 $ zipWith named ["e4", "e3","e2","e1","e0"] (dots : rotations [1,1] forceDs)
          , zip pointsR3 $ zipWith named ["d4", "d3","d2","d1","d0"] (dots : rotations [1,1] drts)
          ]
          where
              partComps = phiScales $ reverse $ take 5 $ fmap pCompAlign (emplacements dartGraph)
              pCompAlign g = showPCompose g (1,3)
              forceDs = fmap center $ phiScaling phi $ reverse $ take 4 $ fmap drawForce dartDs
              drts  = fmap (center . lw thin) $ phiScaling phi $ reverse $ take 4 $ fmap dashJGraph dartDs
              dots = center $ hsep 1 $ replicate 4 (circle 0.5 # fc gray # lw none)
              pointsR1 = map p2 [ (0, 70), (52, 70), (100, 70), (150, 70), (190, 70)]
              pointsR2 = map p2 [ (0, 40), (42, 40), (95, 40), (140, 40), (186, 40)]
              pointsR3 = map p2 [ (0, 0),  (42, 0),  (95, 0),  (140, 0),  (186, 0) ]
   
bigPic = 
    bigPic0  # connectPerim' arrowStyleG "e3" "e2" (1/10 @@ turn) (4/10 @@ turn)
             # connectPerim' arrowStyleG "e2" "e1" (1/10 @@ turn) (4/10 @@ turn)
             # connectPerim' arrowStyleG "e1" "e0" (1/10 @@ turn) (4/10 @@ turn)
             # connectPerim' arrowStyleG "e4" "e3" (1/10 @@ turn) (4/10 @@ turn)
             # connectOutside' arrowStyleB2 "d3" "d4"
             # connectOutside' arrowStyleB2 "d2" "d3"
             # connectOutside' arrowStyleB2 "d1" "d2"
             # connectOutside' arrowStyleB2 "d0" "d1"
             # connectOutside' arrowStyleB1 "e3" "e4"
             # connectOutside' arrowStyleB1 "e2" "e3"
             # connectOutside' arrowStyleB1 "e1" "e2"
             # connectOutside' arrowStyleB1 "e0" "e1"
             # connectOutside' arrowStyleE "d0" "e0"
             # connectOutside' arrowStyleE "d1" "e1"
             # connectOutside' arrowStyleE "d2" "e2"
             # connectOutside' arrowStyleE "d3" "e3"
             # connectPerim' arrowStyleG "d4" "d3" (1/10 @@ turn) (4/10 @@ turn)
             # connectPerim' arrowStyleG "d3" "d2" (1/10 @@ turn) (4/10 @@ turn)
             # connectPerim' arrowStyleG "d2" "d1" (1/10 @@ turn) (4/10 @@ turn)
             # connectPerim' arrowStyleG "d1" "d0" (1/10 @@ turn) (4/10 @@ turn)
             
        where
              arrowStyleG = with & arrowShaft .~ shaft & headLength .~ verySmall 
                                 & headStyle %~ fc green & shaftStyle %~ lc green 
                                 & headGap .~ large & tailGap .~ large
              arrowStyleB1 = with & headLength .~ verySmall & headStyle %~ fc blue 
                                  & shaftStyle %~ lc blue 
                                  & headGap .~ small & tailGap .~ small
              arrowStyleB2 = with & headLength .~ verySmall & headStyle %~ fc blue  
                                  & shaftStyle %~ dashingG [1.5, 1.5] 0  
                                  & shaftStyle %~ lc blue & headGap .~ large & tailGap .~ large
              arrowStyleE = with & headLength .~ verySmall 
                                 & headGap .~ small & tailGap .~ large
              shaft = arc xDir (-1/10 @@ turn)


-- | figure showing ordering of a decomposed kite (bottom), a test graph with an extra LK(3,6,8),
-- and forced figure at the top and composition of all 3 = kite on the right
graphOrder1 = padBorder $ hsep 2 [center $ vsep 1 [ft,t,dcft], cft] where
              [cft,dcft,ft,t] = fmap drawVPatch $ scales [phi] $ alignAll (1,2) $ fmap makeVPatch 
                                [cftest, dcftest, ftest, test]
              dcftest = decomposeG cftest
              cftest = composeG ftest
              ftest = force test
              test = Tgraph { vertices = [8,4,7,2,5,1,3,6] 
                            , faces = [RK (4,7,2),LK (4,5,7),RD (1,7,5),LK (3,2,7)
                                      ,RK (3,7,6),LD (1,6,7), LK(3,6,8)]
                            }


{-
  *********************************************
  Testing functions and figures and experiments
  *********************************************
-}
          
{-
Testing newest force with 10 rules   
BUG discovered filling in boundary of a forced graph  
testForce4 ok but 
testForce5 failed (introducing touching vertices) with previous version of thirdVertexLoc
because of accumulated discrepancies in vertex position calculations.

Now works with signum introduced in thirdVertexLoc,
dramatically improving accuracy of position calculation
-}
testForce4, testForce5 :: Diagram B
testForce4 = padBorder $ lw ultraThin $ drawVGraph $ force boundaryGapFDart4
testForce5 = padBorder $ lw ultraThin $ drawVGraph $ force boundaryGapFDart5        


{- | testViewBoundary is a testing tool to inspect the boundary vertex locations of some (intermediate) Boundary
-- (used in conjunction with stepForce to get an intermediate Boundary)
-- The boundary edges of a Boundary are shown in lime - using the Boundary positions of vertices
-- The graph is converted to a vp separately (so using a fresh calculation of positions)
-- Thus rotations may be needed to match up.
-- Use an empty list of integer rotations to see what rotations are needed to align the figures.
-}
testViewBoundary :: [Int] -> Boundary -> Diagram B
testViewBoundary rots bd =  lc lime bdryFig <> graphFig where 
    [bdryFig, graphFig] = center <$> rotations rots [drawEdges vpMap bdE,  drawVGraph g]
    g = recoverGraph bd
    vpMap = bvLocMap bd
    bdE = bDedges bd

-- used to discover accuracy problem of older thirdVertexLoc
-- view tha boundary after n steps of forcing (starting with boundaryGapFDart5)
-- e.g. n = 1900
inspectForce5 :: Int -> Diagram B
inspectForce5 n = padBorder $ lw ultraThin $
                  testViewBoundary [5] $ boundaryState $ stepForce n boundaryGapFDart5

inspectForce3 n = padBorder $ lw ultraThin $
                  testViewBoundary [3] $ boundaryState $ stepForce n $ dartDs !! 3


-- figures showing boundary edges of the boundary gaps graphs  
testBoundary4, testBoundary5 :: Diagram B
testBoundary4 =  padBorder $ lw ultraThin $ drawGBoundary boundaryGapFDart4 
testBoundary5 =  padBorder $ lw ultraThin $ drawGBoundary boundaryGapFDart5 

-- testing crossing boundary detection e.g. by using force on testCrossingBoundary   
testCrossingBoundary :: Tgraph
testCrossingBoundary = makeTgraph (faces foolDminus \\ [LD(6,11,13)])

-- test wholeTiles (which adds missing second halves of each face)
checkCompleteFig:: Diagram B
checkCompleteFig =  padBorder $ hsep 1 $ fmap dashJGraph [sunD4, wholeTiles sunD4] where sunD4 = sunDs !! 4

-- test graphFromVP
checkGraphFromVP :: Diagram B
checkGraphFromVP = padBorder $ (drawGraph . graphFromVP . makeVPatch) dartD4


-- figure testing selectFacesGtoVP by removing all kites
dartsOnlyFig :: Diagram B
dartsOnlyFig = dashJPatch $ dropVertices $ selectFacesGtoVP (ldarts g++rdarts g) g where g = sunDs !! 5


{- *******************
   testing SubTgraphs
  ********************
-}
-- | subExample is a SubTgraph of a forced 5 times decomposed dart with
-- tracked faces from a forced 2 times decomposed dart
subExample:: SubTgraph
subExample = iterate (forceSub . decomposeSub) (makeSubTgraph fD2 [faces fD2]) !! 3
              where fD2 = force (dartDs !!2)

-- | subExampleFig draws subExample with the tracked faces in red
subExampleFig:: Diagram B
subExampleFig = padBorder $ lw thin drawSubTgraph1 subExample

-- hollowGraph happens to be a valid Tgraph after removing the tracked faces from subExample
-- This is not generally the case
hollowGraph::Tgraph
hollowGraph = removeFaces (concat (trackedSubsets subExample)) (fullGraph subExample)

-- figure showing hollowGraph and result of forcing
forceHollowFig:: Diagram B
forceHollowFig = padBorder $  lw ultraThin $ hsep 1 $ fmap drawGraph [hollowGraph, force hollowGraph]

-- | drawing non tracked faces in general
drawWithoutTracked:: SubTgraph -> Diagram B
drawWithoutTracked sub = 
  drawPatch $ dropVertices $ removeFacesGtoVP (concat (trackedSubsets sub)) (fullGraph sub) 

-- | example using drawWithoutTracked  
removeTrackedFig:: Diagram B
removeTrackedFig = padBorder $  lw ultraThin $ drawWithoutTracked subExample


{- **************************************
   Viewing choice results with SubTgraphs
  ***************************************
-}

{-
N.B.  Changes to forcing (or decomposing) can affect the vertex numbers chosen in twoChoices...
They should be the (reversed) long edge of the left dart on the left of a group of 3 darts
Middle of top edge of dartDs!!4.  Use e.g
checkChoiceEdge (force $ dartDs !!4)
to view the vertex numbers
-}
checkChoiceEdge g = padBorder $ lw ultraThin $ drawVGraph $ force g

-- given a boundary directed edge of a forced graph (either direction)
-- construct two SubTgraphs with dart/kite addition on the edge respectively
-- track the resulting faces and also the singleton new face, then force both SubTgraphs
trackTwoChoices:: Tgraph -> DEdge -> [SubTgraph]
trackTwoChoices g de = fmap forceSub [sub1,sub2] where
          g' = addHalfDart g de
          g'' = addHalfKite g de
          sub1 = makeSubTgraph g' [faces g', faces g' \\ faces g]
          sub2 = makeSubTgraph g'' [faces g'', faces g'' \\ faces g]
          

-- | Take a forced, 4 times decomposed dart, then track the two choices
twoChoices:: [SubTgraph]
twoChoices = trackTwoChoices (force $ dartDs !!4) (233,202)
         
-- | show the (tracked) twoChoices with drawSubTgraph2 (tracked faces in red, new face filled black)  
twoChoicesFig:: Diagram B
twoChoicesFig  = padBorder $ lw ultraThin $ hsep 1 $ fmap drawSubTgraph2 twoChoices

-- | track two further choices with the first of twoChoices (fullgraph)  
moreChoices0:: [SubTgraph]
moreChoices0 = trackTwoChoices (fullGraph $ twoChoices !! 0) (178,219)

-- | track two further choices with the second of twoChoices (fullgraph)  
moreChoices1:: [SubTgraph]
moreChoices1 = trackTwoChoices (fullGraph $ twoChoices !! 1) (178,219)

-- | figures for 4 further choices
moreChoicesFig0,moreChoicesFig1,moreChoicesFig:: Diagram B
moreChoicesFig =  vsep 1 [moreChoicesFig0,moreChoicesFig1]  
moreChoicesFig0 =  padBorder $ lw ultraThin $ hsep 10 $ fmap drawSubTgraph2 moreChoices0
moreChoicesFig1 =  padBorder $ lw ultraThin $ hsep 1 $ fmap drawSubTgraph2 moreChoices1

-- | Trying to find which extensions to the starting dart correspond to the twoChoicesFig
dartHalfDart,dartHalfKite,dartPlusDart,dartPlusKite :: Tgraph
-- | a dart with another half dart on a long edge
dartHalfDart = addHalfDart dartGraph (1,2)
-- | a dart with a half kite on a long edge
dartHalfKite = addHalfKite dartGraph (1,2)
-- | two darts sharing a long edge
dartPlusDart = addHalfDart dartHalfDart (1,5)
-- | a dart and a kite sharing a long edge
dartPlusKite = addHalfKite dartHalfKite (2,5)
-- | two kites sharing a long edge
kitePlusKite = addHalfKite (addHalfKite kiteGraph (1,3)) (1,5)

-- | A sun with a single complete dart on the boundary
sunPlusDart = addHalfDart (addHalfDart sunGraph (2,3)) (3,4)
-- | A sun with 2 darts adjacent on the boundary
sunPlus2Dart = addHalfDart (addHalfDart sunPlusDart (4,5)) (5,6)
-- | A sun with 2 darts NOT adjacent on the boundary
sunPlus2Dart' = addHalfDart (addHalfDart sunPlusDart (6,7)) (7,8)
-- | A sun with 3 darts adjacent on the boundary
sunPlus3Dart = addHalfDart (addHalfDart sunPlus2Dart (6,7)) (7,8)
-- | A sun with 3 darts on the boundary NOT all adjacent
-- This example has an emplacement that does not include the original but is still a correct Tgraph
-- View with forceEmpTest
sunPlus3Dart' = addHalfDart (addHalfDart sunPlus2Dart (8,9)) (9,10)

-- | halfWholeFig shows that a whole dart/kite needs to be added to get the same result as twoChoicesFig
-- Adding a half tile has no effect on the forced decomposition
halfWholeFig:: Diagram B
halfWholeFig =  padBorder $ lw ultraThin $ vsep 1 $ fmap (hsep 1) [take 2 gs, drop 2 gs]
  where                        
    gs = [redEmbed dd fdd, redEmbed dk fdk, redEmbed dhd fdhd, redEmbed dhk fdhk]
    redEmbed g1 g2 = lc red (lw medium $ dashJPatch $ dropVertices g1) <> lw ultraThin (drawPatch $ dropVertices g2)
    [fdd,fdk,fdhd,fdhk] = alignAll (1,3) $ fmap (makeVPatch . force . decomp4) [dartPlusDart, dartPlusKite, dartHalfDart, dartHalfKite]
    decomp4 g = decompositionsG g !! 4
    dd  = alignXaxis (1,3) $ scale (phi^4) $ makeVPatch dartPlusDart    
    dk  = alignXaxis (1,3) $ scale (phi^4) $ makeVPatch dartPlusKite    
    dhd = alignXaxis (1,3) $ scale (phi^4) $ makeVPatch dartHalfDart    
    dhk = alignXaxis (1,3) $ scale (phi^4) $ makeVPatch dartHalfKite    
          

kkEmpsFig = padBorder $ lw ultraThin $ vsep 1 $ rotations [0,9,9] $ 
            fmap drawGraph  [kk, kkD, kkD2] where
              kk = kitePlusKite
              kkD = force $ decomposeG kk
              kkD2 = force $ decomposeG kkD
             
maxShapesFig = relatedVTypeFig ||| kkEmpsFig

drawForceEmplace g = padBorder $ hsep 1 $ fmap drawVGraph [g, force g, emplace g]
tester g = padBorder $ hsep 1 $ fmap drawVGraph [g, force g, maxFCompose g]
       