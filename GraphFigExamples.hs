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

import Data.List ((\\))

{- 
    **********************
    Some display functions
    **********************
-}

padBorder = pad 1.2 . centerXY

-- prograssively increase scales by phi down a list of diagrams
phiScaling :: [Diagram B] -> [Diagram B]
phiScaling ds = zipWith scale (iterate (*phi) 1) ds


-- | drawEmbed (a,b) g1 g2 embeds  g1 (coloured red) onto g2, by aligning with (a,b)
-- vertices a and b must be common to g and g1 and scales should match for length of (a,b)
drawEmbed :: (Vertex, Vertex) -> Tgraph -> Tgraph -> Diagram B
drawEmbed (a,b) g1 g2 = 
    (lc red $ lw thin $ drawPatch $ dropVertices vp1) <> (lw ultraThin $ drawPatch $ dropVertices vp2)  where
    [vp1,vp2] = alignGtoVP (a,b) [g1, g2]

-- | dashJEmbed (a,b) g1 g2 sames as drawEmbed (a,b) g1 g2 but dashJ for g1
dashJEmbed:: (Vertex, Vertex) -> Tgraph -> Tgraph -> Diagram B
dashJEmbed (a,b) g1 g2 = 
    (lc red $ lw thin $ dashJPatch $ dropVertices vp1) <> (lw ultraThin $ drawPatch $ dropVertices vp2)  where
    [vp1,vp2] = alignGtoVP (a,b) [g1, g2]

-- | alignGtoV (a,b) glist converts Tgraphs to VPatches then aligns with (a,b)
-- vertices a and b must be common to all graphs and scales should match
alignGtoVP :: (Vertex, Vertex) -> [Tgraph] -> [VPatch]
alignGtoVP (a,b) gs = alignAll (a,b) $ fmap makeVPatch gs

{-
-- | drawEmbed (a,b) g1 g2 embeds  g1 (coloured red) onto g2, by aligning with (a,b)
-- vertices a and b must be common to g and g1 and scales should match for length of (a,b)
drawEmbed :: (Vertex, Vertex) -> Tgraph -> Tgraph -> Diagram B
drawEmbed (a,b) g1 g2 = drawEmbedVP (a,b) (makeVPatch g1) (makeVPatch g2)

-- | drawEmbedVP (a,b) vp1 vp2 embeds  vp1 (coloured red) into vp2, by aligning with (a,b)
-- vertices a and b must be common to vp1 and vp2 and scales should match for length of (a,b)
drawEmbedVP :: (Vertex, Vertex) -> VPatch -> VPatch -> Diagram B
drawEmbedVP (a,b) vp1 vp2 =
    (lc red $ lw thin $ drawPatch $ dropVertices newvp1) <> (lw ultraThin $ drawPatch $ dropVertices newvp2)  where
    [newvp1,newvp2] =  alignAll (a,b) [vp1,vp2]
-}
    
-- use checkEmbed first to find a common pair of vertices to align (for use with drawEmbed)
checkEmbed :: Tgraph -> Tgraph -> Diagram B
checkEmbed g1 g2 = padBorder $ lw ultraThin $ vsep 1 $
                   fmap drawVGraph [g1, g2]




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

foolDs = graphDecompositions fool
foolD = graphDecompose fool -- or foolDs!!1

-- foolDminus: 3 faces removed from foolD to see limits of forcing
foolDminus = removeFaces [RD(6,14,11), LD(6,12,14), RK(5,13,2)] foolD

-- figures for fool...
foolAndFoolD, forceFoolDminus :: Diagram B
foolAndFoolD = padBorder $ hsep 1 $ [(drawVPatch . scale phi . makeVPatch) fool, drawVGraph foolD]
forceFoolDminus = padBorder $ hsep 1 $ fmap drawVGraph [foolDminus, force foolDminus]




-- sun and sunDs
sunGraph = checkTgraph [ RK(1,2,11), LK(1,3,2)
                       , RK(1,4,3) , LK(1,5,4)
                       , RK(1,6,5) , LK(1,7,6)
                       , RK(1,8,7) , LK(1,9,8)
                       , RK(1,10,9), LK(1,11,10)
                       ]
sunDs =  graphDecompositions sunGraph

figSunD3D2,figSunD2D:: Diagram B
figSunD3D2 = padBorder $ hsep 1 [drawVGraph $ sunDs !! 3, scale phi $ drawVGraph $ sunDs !! 2]
figSunD2D = padBorder  $ hsep 1 [drawVGraph $ sunDs !! 2, scale phi $ drawVGraph $ sunDs !! 1]


-- kite and kiteDs
kiteGraph = checkTgraph [ RK(1,2,4), LK(1,3,2)]
kiteDs = graphDecompositions kiteGraph

-- dart and dartDs
dartGraph =  checkTgraph [ RD(1,2,3), LD(1,3,4)]
dartDs =  graphDecompositions dartGraph



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
Vertices a and b must be common to composed g and remainder patch
(use checkPCompose to see possible vertices)                               
-}
showPCompose ::  Tgraph -> (Vertex, Vertex) -> Diagram B
showPCompose g (a,b) = case emptyGraph g' of
        True -> lc lime $ dashJPatch $ dropVertices $ selectFacesGtoVP fcs g
        False -> (lw ultraThin $ drawPatch $ dropVertices large) <> (lc lime $ dashJPatch $ dropVertices rem) where
                 [rem,large] = alignAll (a,b) [ selectFacesGtoVP fcs g
                                              , scale phi $ makeVPatch g'
                                              ]
      where (fcs,g') = partCompose g


-- checkPCompose  (emplacements dartGraph !! 3) 
pCompFig1 :: Diagram B
pCompFig1 = padBorder $ showPCompose (emplacements dartGraph !! 3) (1,3)        
-- checkPCompose (emplacements kiteGraph !! 3)
pCompFig2 :: Diagram B
pCompFig2 = padBorder $ showPCompose (emplacements kiteGraph !! 3) (1,2)

pCompFig :: Diagram B
pCompFig = padBorder $ vsep 3 $ 
              [hsep 10 $ rotations [4] $ [drawGraph ek3 # lw ultraThin, showPCompose ek3 (1,2)]
              ,hsep 10 $ rotations [3] $ [drawGraph ed3 # lw ultraThin, showPCompose ed3 (1,3)]
              ] where
                  ek3 = emplacements kiteGraph !! 3
                  ed3 = emplacements dartGraph !! 3



{-
  *************************************
  Emplacements and emplacement figures
  *************************************
-}

{- | showEmplace g (a,b) n
    g should be a TOP graph (empty composition) 
    (a,b) a pair of distinct vertices in g 
    produces emplacement diagram for nth decomposition of g               
-}
showEmplace :: Tgraph -> (Vertex, Vertex) -> Int -> Diagram B
showEmplace g (a,b) n = drawEmbed (a,b) (graphDecompositions g !!n) (emplacements g !!n)

-- nth decomposition and its emplacement for dart,kite,fool,sun
empDartD n = showEmplace dartGraph (1,3) n
empKiteD n = showEmplace kiteGraph (1,2) n
empFoolD n = showEmplace fool (1,3) n
empSunD  n = showEmplace sunGraph (1,2) n

-- example emplacement figures
empDartD3Fig,empDartD5Fig,empKiteD3Fig,empKiteD5Fig,empSunD5Fig:: Diagram B
empDartD3Fig = padBorder $ empDartD 3 
empDartD5Fig = padBorder $ empDartD 5

empKiteD3Fig = padBorder $ empKiteD 3
empKiteD5Fig = padBorder $ empKiteD 5

empSunD5Fig = padBorder $ empSunD 5


{-
  *************************************
  Forcing and Composing experiments
  *************************************
-}

dart4 = dartDs!!4

-- brokenDart4 will now get repaired and be included in emplaceSimple
brokenDart4 = removeFaces deleted dart4 where
  deleted = [RK(2,15,31),LD(20,31,15),RK(15,41,20),LK(15,30,41),LK(5,20,41)] 

-- badlyBrokenDart4 will cause emplaceSimple to fail by producung a graph which is not face connected
-- Tgraph {vertices = [4,6,3,1,5], faces = [LD (4,6,3),LK (1,5,3)]} - which breaks on forcing
-- HOWEVER emplace still works and (emplaceSimple . force) gives the same result
badlyBrokenDart4 = removeFaces deleted dart4 where
  deleted = [RK(2,15,31),LD(20,31,15),RK(15,41,20),LK(15,30,41),LK(5,20,41)] 
            ++[LK(11,21,44),RK(11,44,34),LD(9,34,44),RD(9,44,35),LK(17,35,44),RK(17,44,21),RK(6,17,33)]
 
-- brokenDartFig shows the faces removed from dart4 to make brokenDart and badlyBrokenDart
brokenDartFig :: Diagram B
brokenDartFig = padBorder $ lw thin $ hsep 1 $ fmap drawGraph [dart4, brokenDart4, badlyBrokenDart4]

-- brokenDartFig2 illustrates that emplaceSimple does not fully include the starting graph
brokenDartFig2 :: Diagram B
brokenDartFig2 = padBorder $ lw ultraThin $
                 vsep 10 [center $ hsep 1 $ rotations [] $ phiScaling row2
                         ,center $ hsep 10 $ rotations [0,1,1] $ phiScaling (fmap dashJGraph row1)
                         ] where
    row2 = fmap (\g -> dashJEmbed (1,3) g (emplaceSimple g)) row1
    row1 = allComps brokenDart4

-- brokenDartFig2F illustrates that full emplace does include the starting graph (so repairs it)
brokenDartFig2F :: Diagram B
brokenDartFig2F = padBorder $ lw ultraThin $
                  vsep 10 [center $ hsep 1 $ rotations [] $ phiScaling row2
                          ,center $ hsep 13 $ rotations [0,1,1] $ phiScaling (fmap dashJGraph row1)
                         ] where
    row2 = fmap (\g -> dashJEmbed (1,3) g (emplace g)) row1
    row1 = allFComps brokenDart4

-- brokenDartFig3 illustrates that a single force before emplaceSimple
-- also includes the starting graph (so repairs it) for both brokenDart and badlyBrokenDart
brokenDartFig3 :: Diagram B
brokenDartFig3 = padBorder $ lw ultraThin $ vsep (-2) [row1,row2,row3] where
    row1 = hsep 5 $ [drawGraph dart4, drawEmbed (1,3) dart4 (force dart4)]
    row2 = hsep 5 $ [drawGraph brokenDart4, drawEmbed (1,3) brokenDart4 fb, rotate (ttangle 4) $ drawGraph (emplaceSimple fb)]
    row3 = hsep 5 $ [drawGraph badlyBrokenDart4, drawEmbed (1,3) badlyBrokenDart4 fb]
    fb = force badlyBrokenDart4 -- same as force brokenDart4

brokenDartFig4 :: Diagram B
brokenDartFig4 = padBorder $ vsep 1 [ hsep 1 $ phiScaling $ fmap dashJGraph $ allComps dart4 
                                    , hsep 1 $ phiScaling $ fmap dashJGraph $ allComps brokenDart4
                                    , hsep 1 $ phiScaling $ fmap dashJGraph $ allFComps brokenDart4
                                    ] 

-- illustrates that brokenKites is repaired by emplace
-- NOTE third graphCompose of brokenKites produces a graph with crossing boundaries
-- so force is essential before composing
brokenKitesDFig :: Diagram B
brokenKitesDFig = padBorder $ hsep 1 $ fmap drawVPatch $ alignAll(1,3) $ scales [1,1,phi] $ fmap makeVPatch 
                 [graphDecompose twoKites,brokenKites, graphCompose brokenKites, emplace brokenKites]
brokenKites = removeFaces [LD(1,14,16),LK(5,4,16),RK(5,16,14)] $ graphDecompose twoKites
twoKites = checkTgraph [ RK(1,2,11), LK(1,3,2)
                       , RK(1,4,3) , LK(1,5,4)
                       ]

forceAndCompDart4  = padBorder $ lw ultraThin $ allPosition
                     $ fixRotations [[0,1,1],[0,1,1],[1,1],[1]] 
                     (fComps dart4)         
forceAndCompBrokenDart4  = padBorder $ lw ultraThin $ allPosition
                     $ fixRotations [[0,1,1],[0,1,1],[1,1],[1]] 
                     (fComps brokenDart4)         

gapSize = 25.0
insertHead x [] = [[x]]
insertHead x (a:as) = (x:a):as

-- Messy but works on rows bottom up and ok with shorter list than rows
fixRotations:: [[Int]] -> [(Diagram B,[Diagram B])] -> [(Diagram B,[Diagram B])]
fixRotations [] more = more
fixRotations [rots1] ((f,r):more) = (f,rotations rots1 r):more
fixRotations (rots1:(rot:rots2):rotsmore) ((f,r):more) = 
    (rotate (ttangle rot) f, rotations rots1 r):fixRotations (rots2:rotsmore) more
fixRotations (rots1:[]:rotsmore) ((f,r):more) = 
    (f,rotations rots1 r):fixRotations ([]:rotsmore) more

allPosition:: [(Diagram B,[Diagram B])] -> Diagram B
allPosition [] = mempty
allPosition ((f,r):more) = rowPosition r <> 
                           translate (gapSize*unitY) (f <> translate (gapSize*unitX) (allPosition more))

rowPosition:: [Diagram B] -> Diagram B
rowPosition [] = mempty
rowPosition (g:gs) = g<>(translate (gapSize*unitX) $ rowPosition gs)

fComps:: Tgraph -> [(Diagram B,[Diagram B])]
fComps g = let fg = force g
               g' = graphCompose fg
           in if emptyGraph g'
               then [(dashJGraph fg, [dashJGraph g])]
               else (dashJGraph fg, phiScaling $ fmap dashJGraph (allComps g)) : scale phi (fComps g')


{-
  *************************
  Multi emplace and choices
  *************************
-}

-- four choices for composing fool
foolChoices :: Diagram B
foolChoices = padBorder $ vsep 1 
              [hsep 1 $ fmap (redFool <>) $ fmap dashJGraph choices
              ,hsep 1 $ rotations [1,1,9,4] $ scale phi $ fmap (dashJGraph . graphCompose) choices
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

-- graphDecompose mistake and the point at which forcing fails  with  RK (6,26,1)              
pfDecompMistakeFig :: Diagram B
pfDecompMistakeFig = padBorder $ hsep 1 [drawVGraph (graphDecompose mistake), drawVGraph part] where
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
               [ mistake1 , mistake1D, force mistake1D, graphCompose mistake1D]
               where mistake1D = graphDecompose mistake1

{-
  *************************
  Other Figures
  *************************
-}

-- | figire showing ordering of a decomposed kite and test with with an extra LK(3,6,8)
graphOrder1 = padBorder $ hsep 2 [center $ vsep 1 [ft,t,dcft], cft] where
              [cft,dcft,ft,t] = fmap drawVPatch $ scales [phi] $ alignAll (1,2) $ fmap makeVPatch 
                                [cftest, dcftest, ftest, test]
              dcftest = graphDecompose cftest
              cftest = graphCompose ftest
              ftest = force test
              test = Tgraph { vertices = [8,4,7,2,5,1,3,6] 
                            , faces = [RK (4,7,2),LK (4,5,7),RD (1,7,5),LK (3,2,7)
                                      ,RK (3,7,6),LD (1,6,7), LK(3,6,8)]
                            }

labelD :: String -> Diagram B -> Diagram B
labelD l d = baselineText l # fontSize (local 0.4) # fc blue <> d # moveTo (p2(0,2.2))

-- | vertexTypesFig selects 7 subgraphs from sunD3 illustrating the 7 types of vertex
vertexTypesFig = pad 1.2 $ vsep 1 [hsep 1 $ take 3 lTypeFigs, hsep 1 $ drop 3 lTypeFigs]
 where
 lTypeFigs = zipWith labelD ["k5","k4d1","k3d2","k2d1","k2d2","k2d3","d5"] vTypeFigs
 vTypeFigs = zipWith drawVertex [k5,k4d1,k3d2,k2d1,k2d2,k2d3,d5] -- subgraph lists
                                [21,49,  34,  62,  101, 14,  1] -- centered vertices
 drawVertex list ctr = showOrigin $ dashJPatch $ dropVertices $ centerOn ctr $ selectFacesVP list $ sunD3
 sunD3 = makeVPatch (sunDs!!3)
 k2d3 = [LD (14,70,106),RD (14,108,70),LD (14,69,28),RD (14,106,69),LD (14,74,108)
        ,RD (14,32,74),LK (32,14,109),RK (32,109,75),RK (28,109,14),LK (28,66,109)
        ]
 k2d1 = [RD (4,102,62),LD (4,62,105),LK (18,62,102),LK (18,45,105),RK (18,102,44),RK (18,105,62)
        ]
 k2d2 = [LK (17,60,101),RK (17,101,43),RK (26,101,61),LK (26,43,101),LD (13,61,101)
        ,RD (13,101,60),RD (13,103,61),LD (13,60,24)
        ]
 d5 =   [LD (1,57,99),RD (1,104,57),LD (1,66,104),RD (1,109,66),LD (1,75,109)
        ,RD (1,114,75),LD (1,84,114),RD (1,119,84),LD (1,93,119),RD (1,99,93)
        ]
 k4d1 = [LK (20,49,115),RK (37,115,49),LD (49,86,37),RK (20,116,49),LK (38,49,116)
        ,RD (49,38,86),RK (20,115,80),LK (20,87,116),LK (37,85,115),RK (38,116,88)
        ]
 k3d2 = [LK (34,47,111),RK (34,111,79),LK (7,77,34),RD (47,34,77),RK (34,113,48)
        ,LK (34,79,113),RK (7,34,82),LD (48,82,34),LD (47,77,33),RD (48,35,82)
        ]
 k5 =   [RK (21,117,50),LK (21,89,117),LK (21,50,118),RK (21,118,92),LK (21,92,40)
        ,LK (21,51,120),RK (21,120,89),RK (21,121,51),LK (21,96,121),RK (21,40,96)
        ]
        


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
  *************************************
  Testing functions and figures and experiments
  *************************************
-}
         
-- testing crossing boundary detection e.g. by using force on testCrossingBoundary   
testCrossingBoundary = makeTgraph (faces foolDminus \\ [LD(6,11,13)])
         
-- testing
checkForceFig =  padBorder $ hsep 1 $ fmap dashJGraph [dart4, force dart4]

touchingProblem = padBorder $ (drawVPatch vpLeft <> (dashJPatch (dropVertices vpGone) # lc lime)) where
    vpLeft = removeFacesVP deleted vp
    vpGone = selectFacesVP deleted vp
    vp = makeVPatch sunD2
    sunD2 = sunDs!!2
    deleted = filter ((==1).originV) (faces sunD2) ++
              filter ((==20).originV) (faces sunD2) ++
              [RK(16,49,20),LK(8,20,49),RK(8,49,37)]
{-
              filter ((==19).originV) (faces sunD3) ++
              filter ((==20).originV) (faces sunD3) ++
              filter ((==15).originV) (faces sunD3) ++
              filter ((==8).originV) (faces sunD3)
-}
-- testing selectFacesGtoVP figure testing selectFacesGtoVP
dartsOnlyFig = dashJPatch $ dropVertices $ selectFacesGtoVP (ldarts g++rdarts g) g where g = sunDs !! 5


maxAndEmplace g = scales [1,1,phi^n] $ fmap drawGraph [g,empg,maxg]
                  where (maxg,empg,n) = countEmplace g

maxEdart4Fig :: Diagram B
maxEdart4Fig = vsep 1 $ lw ultraThin $ fmap (hsep 1 . rotations [0,4,0])
               [maxAndEmplace dart4, maxAndEmplace brokenDart4, maxAndEmplace badlyBrokenDart4]

     