--{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# LANGUAGE FlexibleContexts          #-}
--{-# LANGUAGE TypeFamilies              #-}

module GraphFigExamples where
        
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import HalfTile
import TileLib

import Tgraphs
import GraphConvert

import Data.List ((\\), nub)

{- 
    **********************
    Some display functions
    **********************
-}

padBorder = pad 1.2 . centerXY


-- use checkEmbed first to find a common pair of vertices to align (for use with drawEmbed)
checkEmbed :: Tgraph -> Tgraph -> Diagram B
checkEmbed g1 g2 = padBorder $ lw ultraThin $ vsep 1 $
                   fmap drawVGraph [g1, g2]

-- | drawEmbed (a,b) g1 g2 embeds  g1 (coloured red) onto g2, by aligning with (a,b)
-- vertices a and b must be common to g1 and g2 and scales should match for length of (a,b)
drawEmbed :: (Vertex, Vertex) -> Tgraph -> Tgraph -> Diagram B
drawEmbed (a,b) g1 g2 = 
    (lc red $ lw thin $ drawPatch $ dropVertices vp1) <> (lw ultraThin $ drawPatch $ dropVertices vp2)  where
    vp1 = alignXaxis (a,b) $ makeVPatch g1
    vp2 = alignXaxis (a,b) $ makeVPatch g2

-- | drawEmbed' alms g1 g2 embeds  g1 (coloured red) onto g2, by aligning with alignments alms - a list of 2 pairs
drawEmbed' :: [(Vertex, Vertex)] -> Tgraph -> Tgraph -> Diagram B
drawEmbed' alms g1 g2 = 
    (lc red $ lw thin $ drawPatch $ dropVertices vp1) <> (lw ultraThin $ drawPatch $ dropVertices vp2)  where
    [vp1,vp2] = alignments alms $ fmap makeVPatch [g1, g2]

-- | dashJEmbed (a,b) g1 g2 sames as drawEmbed (a,b) g1 g2 but dashJ for g1
dashJEmbed:: (Vertex, Vertex) -> Tgraph -> Tgraph -> Diagram B
dashJEmbed (a,b) g1 g2 = 
    (lc red $ lw thin $ dashJPatch $ dropVertices vp1) <> (lw ultraThin $ drawPatch $ dropVertices vp2)  where
    vp1 = alignXaxis (a,b) $ makeVPatch g1
    vp2 = alignXaxis (a,b) $ makeVPatch g2



{-
    **************************
    Some Tgraphs and Figures
    *************************
-}

-- fool: fool's kite - a decomposed left and right kite back-to-back (i.e. not sharing join edge)
fool = checkTgraph [ RD(1,2,3)
                   , LD(1,3,4)
                   , RK(6,2,5)
                   , LK(6,3,2)
                   , RK(6,4,3)
                   , LK(6,7,4)
                   ]
foolFig :: Diagram B
foolFig = padBorder $ drawVGraph fool

foolDs = decompositionsG fool
foolD = decomposeG fool -- or foolDs!!1

-- foolDminus: 3 faces removed from foolD
foolDminus:: Tgraph
foolDminus = removeFaces [RD(6,14,11), LD(6,12,14), RK(5,13,2)] foolD

foolAndFoolD :: Diagram B
foolAndFoolD = padBorder $ hsep 1 $ [(drawVPatch . scale phi . makeVPatch) fool, drawVGraph foolD]




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
        padBorder $ hsep 1 $ [drawVPatch $ selectFacesGtoVP fcs g, scale phi $ drawVGraph g'] where
        (fcs,g') = partCompose g

{- | showPCompose g (a,b)  applies partCompose to g, then aligns and draws the composed graph with the remainder faces (in lime)
It can cope with an empty composed graph.
Vertices a and b must be common to composed g and g
(use checkPCompose to see possible vertices)                               
-}
showPCompose ::  Tgraph -> (Vertex, Vertex) -> Diagram B
showPCompose g (a,b) = case nullGraph g' of
    True -> lc lime $ dashJPatch $ dropVertices $ remg
    False -> (lw ultraThin $ drawPatch $ dropVertices compg)
             <> (lc lime $ dashJPatch $ dropVertices remainder)
                 where [remainder,compg] = alignAll (a,b) [remg , scale phi $ makeVPatch g']
  where (fcs,g') = partCompose g
        remg = selectFacesGtoVP fcs g
 

-- checkPCompose  (force $ dartDs !! 3) 
pCompFig1 :: Diagram B
pCompFig1 = padBorder $ showPCompose (force $ dartDs !! 3) (1,3)        
-- pCompFig1 = padBorder $ showPCompose (emplacements dartGraph !! 3) (1,3)        
-- checkPCompose (emplacements kiteGraph !! 3)
pCompFig2 :: Diagram B
pCompFig2 = padBorder $ showPCompose (force $ kiteDs !! 3) (1,2)

pCompFig :: Diagram B
pCompFig = padBorder $ vsep 3 $ 
              [hsep 10 $ rotations []  $ [drawGraph fk3 # lw ultraThin, showPCompose fk3 (1,2)]
              ,hsep 10 $ rotations [1] $ [drawGraph fd3 # lw ultraThin, showPCompose fd3 (1,3)]
              ] where
                  fk3 = force $ kiteDs !! 3
                  fd3 = force $ dartDs !! 3


{-
  *********************************
  Forced graphs and forcing figures
  *********************************
-}
              
forceFoolDminus :: Diagram B              
forceFoolDminus = padBorder $ hsep 1 $ fmap drawVGraph [foolDminus, force foolDminus]

showForce :: Tgraph -> (Vertex, Vertex) -> Diagram B
showForce g (a,b) = drawEmbed (a,b) g (force g)

forceDartD,forceKiteD,forceSunD :: Int -> Diagram B
forceDartD n = showForce (dartDs !! n) (1,3)
forceKiteD n = showForce (kiteDs !! n) (1,2)
forceSunD  n = showForce (sunDs  !! n) (1,2)

forceDartD3Fig = padBorder $ forceDartD 3 
forceDartD5Fig = padBorder $ forceDartD 5

forceKiteD3Fig = padBorder $ forceKiteD 3
forceKiteD5Fig = padBorder $ forceKiteD 5

forceSunD5Fig = padBorder $ forceSunD 5

{-
  *************************************
  Emplacements and emplacement figures
  *************************************
-}

{- | showEmplace g n (a,b)
    g should be a TOP graph (empty composition) 
    (a,b) a pair of distinct vertices in g 
    produces emplacement diagram for nth decomposition of g               
-}
showEmplace :: Tgraph -> Int -> (Vertex, Vertex) -> Diagram B
showEmplace g n (a,b) = drawEmbed (a,b) (decompositionsG g !!n) (emplacements g !!n)

{- | showForceEmplace rots g 
     shows a graph (with dashJ) followed its forced then emplaced versions shown with draw
     rots is a list of integer rotations (use [] first to work out what rotations are needed)
-}
showForceEmplace :: [Int] -> Tgraph -> Diagram B
showForceEmplace rots g = 
    padBorder $ hsep 1 $ fmap center $ rotations rots
    [ dashJGraph g
    , drawGraph $ forced
    , drawGraph $ emplace forced
    ] where forced = force g

-- nth decomposition and its emplacement for dart,kite,fool,sun
empDartD n = showEmplace dartGraph n (1,3)
empKiteD n = showEmplace kiteGraph n (1,2)
empFoolD n = showEmplace fool n (1,3)
empSunD  n = showEmplace sunGraph n (1,2)

-- example emplacement figures
empDartD3Fig,empDartD5Fig,empKiteD3Fig,empKiteD5Fig,empSunD5Fig:: Diagram B
empDartD3Fig = padBorder $ empDartD 3 
empDartD5Fig = padBorder $ empDartD 5

empKiteD3Fig = padBorder $ empKiteD 3
empKiteD5Fig = padBorder $ empKiteD 5

empSunD5Fig = padBorder $ empSunD 5


{-
  *************************************
  Forcing and Composing Subgraphs
  *************************************
-}

dartD4 = dartDs!!4

-- brokenDart will now get repaired and be included in emplaceSimple
brokenDart :: Tgraph
brokenDart = removeFaces deleted dartD4 where
  deleted = [RK(2,15,31),LD(20,31,15),RK(15,41,20),LK(15,30,41),LK(5,20,41)] 

-- badlyBrokenDart will cause emplaceSimple to fail by producung a graph which is not face connected
-- Tgraph {vertices = [4,6,3,1,5], faces = [LD (4,6,3),LK (1,5,3)]} - which breaks on forcing
-- HOWEVER emplace still works and (emplaceSimple . force) gives the same result
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
  *************************
  Multi emplace and choices
  *************************
-}

-- four choices for composing fool
foolChoices :: Diagram B
foolChoices = padBorder $ vsep 1 
              [hsep 1 $ fmap (redFool <>) $ fmap dashJGraph choices
              ,hsep 1 $ rotations [1,1,9,4] $ scale phi $ fmap (dashJGraph . composeG) choices
              ] where choices = makeChoices fool
                      redFool = dashJGraph fool # lc red
                         

-- emplacement choices for foolD
emplaceFoolDChoices :: Diagram B
emplaceFoolDChoices = padBorder $ hsep 1 $
        fmap (((lc red . dashJPatch . dropVertices) d1 <>) . lw ultraThin . drawPatch . dropVertices) rest where
        (d1:rest) = alignments [(1,6),(1,6),(1,6),(1,6),(34,6)] (fmap makeVPatch (g:emplaceChoices g))
        g = foolDs !! 1


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
mistake = checkTgraph [RK(1,2,4), LK(1,3,2), RD(3,1,5), LD(4,6,1), LD(3,5,7), RD(4,8,6)]

-- | partially forced mistake (at the point of error)
partForcedMistake = 
    makeTgraph [RK (9,1,11),LK (9,10,7),RK (9,7,5),LK (9,5,1),RK (1,2,4)
               ,LK (1,3,2),RD (3,1,5),LD (4,6,1),LD (3,5,7),RD (4,8,6)
               ]

-- mistake and the point at which forcing fails                
pfMistakeFig :: Diagram B
pfMistakeFig  = padBorder $ hsep 1 [drawVGraph mistake, drawVGraph partForcedMistake]

-- decomposeG mistake and the point at which forcing fails  with  RK (6,26,1)              
pfDecompMistakeFig :: Diagram B
pfDecompMistakeFig = padBorder $ hsep 1 [drawVGraph (decomposeG mistake), drawVGraph part] where
    part =  makeTgraph [RK (26,24,1),RK (5,24,25),LK (5,1,24),RK (3,23,2),LK (3,22,23)
                       ,RK (3,21,22),LK (3,15,21),LK (4,2,20),RK (4,20,19),LK (4,19,18),RK (4,18,17)
                       ,LK (4,17,16),RK (4,16,12),LD (8,12,16),RK (3,14,15),LK (3,11,14),RD (7,14,11)
                       ,RK (4,13,2),LK (4,9,13),RD (1,13,9),LK (3,2,13),RK (3,13,10),LD (1,10,13)
                       ,LK (3,10,5),RD (1,5,10),RK (4,6,9),LD (1,9,6),RK (3,5,11),LD (7,11,5)
                       ,LK (4,12,6),RD (8,6,12)
                       ]

-- | mistake1 is a kite bordered by 2 half kites (subgraph of mistake and still erroneous)
mistake1 = checkTgraph [RK(1,2,4), LK(1,3,2), RD(3,1,5), LD(4,6,1)]
-- | partially forced mistake1 (at the point of error)
partForcedfMistake1 = makeTgraph [RK (8,1,6),LK (7,5,1),RK (1,2,4),LK (1,3,2),RD (3,1,5),LD (4,6,1)]

-- decomposed mistake1 is no longer erroneous and can be forced and recomposed
cdMistake1Fig :: Diagram B
cdMistake1Fig = padBorder $ hsep 1 $ fmap drawVPatch $ scales [phi,1,1,phi] $ alignAll (1,2) $ fmap makeVPatch
               [ mistake1 , mistake1D, force mistake1D, composeG mistake1D]
               where mistake1D = decomposeG mistake1

{-
  *************************
  Other Figures
  *************************
-}

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
  ****************************
  Figures of 7 types of vertex
  ****************************
-}
   
{- | vertexTypesFig is 7 vertex types single diagram as a row -}
vertexTypesFig = padBorder $ hsep 1 $ lTypeFigs
 where
 lTypeFigs = zipWith labelD ["sun","star","jack","queen","king","ace","deuce"] vTypeFigs
 vTypeFigs = zipWith drawVertex [sunV, starV, jackV, queenV, kingV, aceV,  deuceV]
                                [(1,2),(1,2), (1,2), (1,2),  (1,2), (3,6), (2,6)] -- alignments
 drawVertex g alm = showOrigin $ dashJPatch $ dropVertices $ alignXaxis alm $ makeVPatch g

labelD :: String -> Diagram B -> Diagram B
labelD l d = baselineText l # fontSize (local 0.5) # fc blue <> d # moveTo (p2(0,2.2))

{- graphs for the 7 vertex types sunV, starV, jackV, queenV, kingV, aceV,  deuceV -}
jackV = makeTgraph [LK (1,9,11),RK (1,11,2),LK (7,8,1),RD (9,1,8),RK (1,3,4)
                   ,LK (1,2,3),RK (7,1,5),LD (4,5,1),LD (9,8,10),RD (4,6,5)
                   ]
kingV = makeTgraph [LD (1,2,3),RD (1,11,2),LD (1,4,5),RD (1,3,4),LD (1,10,11)
                   ,RD (1,9,10),LK (9,1,7),RK (9,7,8),RK (5,7,1),LK (5,6,7)
                   ]
queenV = makeTgraph [LK (7,1,5),RK (3,5,1),LD (1,2,3),RK (7,9,1),LK (11,1,9)
                    ,RD (1,11,2),RK (7,5,6),LK (7,8,9),LK (3,4,5),RK (11,9,10)
                    ]

aceV = fool -- centre 3
deuceV = makeTgraph [LK (7,8,2),RK (7,2,6),RK (5,2,4),LK (5,6,2),LD (1,4,2)
                    ,RD (1,2,8),RD (1,3,4),LD (1,8,9)
                    ] -- centre 2

starV = makeTgraph [LD (1,2,3),RD (1,11,2),LD (1,10,11),RD (1,9,10),LD (1,8,9)
                   ,RD (1,7,8),LD (1,6,7),RD (1,5,6),LD (1,4,5),RD (1,3,4)
                   ]
sunV = sunGraph

{- |  forceVFigures is a list of 7 diagrams - force of 7 vertex types -}
forceVFigures :: [Diagram B]
forceVFigures =
  zipWith showForce [sunV,starV,jackV,queenV,kingV,aceV,deuceV]
                    [(1,2), (1,2), (1,2), (1,2), (1,2), (3,6), (2,6)] -- alignments

{- | forceVsFig shows emplacements of 7 vertex types in a row as single diagram -}
forceVsFig :: Diagram B
forceVsFig = padBorder $ hsep 1 forceVFigures

{- | relatedVTypeFig lays out figures from forceVFigures plus a kite as single diagram with 3 columns -}
relatedVTypeFig = padBorder $
 atPoints [p2(0,15),p2(0,10),   p2(10,15),p2(10,10),p2(10,0),  p2(19,15),p2(20,10),p2(20,0) ]
          [sunE, starE,         ace, jackE, kingE,             kite, deuceE, queenE]
 where kite = drawGraph kiteGraph # lw thin
       sunE = forceVFigures!!0
       starE = forceVFigures!!1
       jackE = forceVFigures!!2
       queenE = forceVFigures!!3
       kingE = forceVFigures!!4
       ace = forceVFigures!!5
       deuceE = forceVFigures!!6

-- graphs of the boundary faces only of a forced graph
boundaryFDart4, boundaryFDart5 :: Tgraph
boundaryFDart4 =  
    checkTgraph $ nub $ concat $ fmap snd $ vFaceAssoc $ makeBoundary $ force (dartDs!!4)
boundaryFDart5 =  
    checkTgraph $ nub $ concat $ fmap snd $ vFaceAssoc $ makeBoundary $ force (dartDs!!5)

-- graphs of the boundary faces only of a forced graph - with extra faces removed to make a gap
boundaryGapFDart4, boundaryGapFDart5 :: Tgraph
boundaryGapFDart4 =   
    checkTgraph $ filter ((/=200).originV)  (faces boundaryFDart4)
boundaryGapFDart5 =   
    checkTgraph $ filter ((/=1410).originV) (faces boundaryFDart5)

-- figures for the above boundary gap graphs
boundaryGap4Fig, boundaryGap5Fig :: Diagram B
boundaryGap4Fig = lw ultraThin $ drawVGraph $ boundaryGapFDart4
boundaryGap5Fig = lw ultraThin $ drawVGraph $ boundaryGapFDart5

-- showing intermediate state of filling the inlet and closing the gap of boundaryGapFDart5
-- using stepForce 2200
progressFigure :: Diagram B
progressFigure = lw ultraThin $ vsep 1 $ fmap center $ rotations [1,1]
    [ dashJGraph g
    , drawGraph $ recoverGraph $ stepForce 2200 g
    ] where g = boundaryGapFDart5


{-  test for bigPic without arrows -}
bigPic0:: Diagram B
bigPic0 = (padBorder $ position $ concat $
          [ zip pointsR1 $ rotations [] $ fmap compD [4,3,2,1,0]
          , zip pointsR2 $ zipWith named ["e4", "e3","e2","e1","e0"] (dots : rotations [] (fmap empD [3,2,1,0]))
          , zip pointsR3 $ zipWith named ["d4", "d3","d2","d1","d0"] (dots : rotations [1,1] (fmap drts [3,2,1,0]))
          ])
          where
              compD n = lw thin $ scale (phi ^ (4-n)) $ showPCompose (emplacements dartGraph !! n) (1,5)
              empD n = center $ scale (phi ^ (4-n)) $ empDartD n
              drts n = center . lw thin . scale (phi ^ (4-n)) $ dashJGraph $ dartDs !! n
              dots = center $ hsep 1 $ take 4 $ repeat $ ((circle 0.5) # fc gray # lw none)
              pointsR1 = map p2 [ (0, 70), (52, 70), (100, 70), (150, 70), (190, 70)]
              pointsR2 = map p2 [ (0, 40), (42, 40), (95, 40), (140, 40), (186, 40)]
              pointsR3 = map p2 [ (0, 0),  (42, 0),  (95, 0),  (140, 0),  (186, 0) ]
    
{- | bigPic is a diagram illustrating emplacement relationships for decomposed darts -}
bigPic :: Diagram B
bigPic = (padBorder $ position $ concat $
         [ zip pointsR1 $ rotations [] $ fmap compD [4,3,2,1,0]
         , zip pointsR2 $ zipWith named ["e4", "e3","e2","e1","e0"] (dots : rotations [] (fmap empD [3,2,1,0]))
         , zip pointsR3 $ zipWith named ["d4", "d3","d2","d1","d0"] (dots : rotations [1,1] (fmap drts [3,2,1,0]))
         ])  # connectPerim' arrowStyleG "e3" "e2" (1/10 @@ turn) (4/10 @@ turn)
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
             
        where compD n = lw thin $ scale (phi ^ (4-n)) $ showPCompose (emplacements dartGraph !! n) (1,3)
              empD n = center $ scale (phi ^ (4-n)) $ empDartD n
              drts n = center . lw thin . scale (phi ^ (4-n)) $ dashJGraph $ dartDs !! n
              dots = center $ hsep 1 $ take 4 $ repeat $ ((circle 0.5) # fc gray # lw none)
              pointsR1 = map p2 [ (0, 70), (52, 70), (100, 70), (150, 70), (190, 70)]
              pointsR2 = map p2 [ (0, 40), (42, 40), (95, 40), (140, 40), (186, 40)]
              pointsR3 = map p2 [ (0, 0),  (42, 0),  (95, 0),  (140, 0),  (186, 0) ]
              arrowStyleG = (with  & arrowShaft .~ shaft & headLength .~ verySmall & headStyle %~ fc green & shaftStyle %~ lc green & headGap .~ large & tailGap .~ large)
              arrowStyleB1 = (with  & headLength .~ verySmall & headStyle %~ fc blue & shaftStyle %~ lc blue & headGap .~ small & tailGap .~ small)
              arrowStyleB2 = (with  & headLength .~ verySmall & headStyle %~ fc blue  & shaftStyle %~ dashingG [1.5, 1.5] 0  & shaftStyle %~ lc blue & headGap .~ large & tailGap .~ large)
              arrowStyleE = (with & headLength .~ verySmall  & headGap .~ small & tailGap .~ large)
              shaft = arc xDir (-1/10 @@ turn)
 


{-
  *********************************************
  Testing functions and figures and experiments
  *********************************************
-}
          
-- Testing newest force rule (10)
testQueen = padBorder $ drawVGraph $ force partQueen
partQueen = makeTgraph [LK (7,1,5),RK (3,5,1),RK (7,9,1)
                       ,RK (7,5,6),LK (7,8,9),LK (3,4,5)
                       ]

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

-- used to discover accuracy problem of older thirdVertexLoc
-- view tha boundary after n steps of forcing (starting with boundaryGapFDart5)
-- e.g. n = 1900
inspectForce5 :: Int -> Diagram B
inspectForce5 n = padBorder $ lw ultraThin $
                viewBoundary [] $ stepForce n $ boundaryGapFDart5


-- figures showing boundary edges of the boundary gaps graphs  
testBoundary4, testBoundary5 :: Diagram B
testBoundary4 =  padBorder $ lw ultraThin $ showGBoundary $ boundaryGapFDart4 
testBoundary5 =  padBorder $ lw ultraThin $ showGBoundary $ boundaryGapFDart5 

-- testing crossing boundary detection e.g. by using force on testCrossingBoundary   
testCrossingBoundary :: Tgraph
testCrossingBoundary = makeTgraph (faces foolDminus \\ [LD(6,11,13)])
         
-- simple force test
checkForceFig :: Diagram B
checkForceFig =  padBorder $ hsep 1 $ fmap dashJGraph [dartD4, force dartD4]

-- test wholeTiles (which adds missing second halves of each face)
checkCompleteFig:: Diagram B
checkCompleteFig =  padBorder $ hsep 1 $ fmap dashJGraph [dartD4, wholeTiles dartD4]

-- test graphFromVP
checkGraphFromVP :: Diagram B
checkGraphFromVP = padBorder $ (drawGraph . graphFromVP . makeVPatch) dartD4


-- testing selectFacesGtoVP figure testing selectFacesGtoVP
dartsOnlyFig :: Diagram B
dartsOnlyFig = dashJPatch $ dropVertices $ selectFacesGtoVP (ldarts g++rdarts g) g where g = sunDs !! 5


{-
Another test of new forcing
Starting from the comma shaped gap (commaGap after 2200 steps of forcing boundaryGapFDart5)
get the filled in gap only (gComma = difference after forcing)
then force gComma.
The result is not the full original force (dartDs!!5)
-}
commaFig :: Diagram B
commaFig = padBorder $ lw ultraThin $ showForce gComma (3937,3936)
           where
              commaGap = recoverGraph $ stepForce 2200 boundaryGapFDart5    
              gComma = removeFaces (faces commaGap) (force commaGap)
