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

{- ****************
    Some Tgraphs
******************-}

-- fool: fool's kite - a decomposed left and right kite back-to-back (i.e. not sharing join edge)
fool = checkTgraph [ RD(1,2,3)
                   , LD(1,3,4)
                   , RK(6,2,5)
                   , LK(6,3,2)
                   , RK(6,4,3)
                   , LK(6,7,4)
                   ]
foolD = graphDecompose fool

-- foolDminus: 3 faces removed from foolD to see limits of forcing
foolDminus = removeFaces [RD(6,14,11), LD(6,12,14), RK(5,13,2)] foolD
--foolDminus = checkTgraph (faces foolD \\ [RD(6,14,11), LD(6,12,14), RK(5,13,2)])

foolDs = graphDecompositions fool

sunGraph = checkTgraph [ RK(1,2,11), LK(1,3,2)
                       , RK(1,4,3) , LK(1,5,4)
                       , RK(1,6,5) , LK(1,7,6)
                       , RK(1,8,7) , LK(1,9,8)
                       , RK(1,10,9), LK(1,11,10)
                       ]
sunDs =  graphDecompositions sunGraph

kiteGraph = checkTgraph [ RK(1,2,4), LK(1,3,2)]
kiteDs = graphDecompositions kiteGraph

dartGraph =  checkTgraph [ RD(1,2,3), LD(1,3,4)]
dartDs =  graphDecompositions dartGraph

testMultiGaps = makeTgraph (faces foolDminus \\ [LD(6,11,13)])

mistake1 = checkTgraph [RK(1,2,4), LK(1,3,2), RD(3,1,5), LD(4,6,1)]
-- | partially forced mistake1 (at the point of error)
pfMistake1 = Tgraph { vertices = [8,1,6,7,5,2,4,3]
                    , faces = [RK (8,1,6),LK (7,5,1),RK (1,2,4),LK (1,3,2),RD (3,1,5),LD (4,6,1)]
                    }

mistake = checkTgraph [RK(1,2,4), LK(1,3,2), RD(3,1,5), LD(4,6,1), LD(3,5,7), RD(4,8,6)]
-- | partially forced mistake (at the point of error)
pfMistake = Tgraph { vertices = [9,1,6,10,7,5,8,2,4,3]
                   , faces = [RK (9,1,6),RK (10,7,5),LK (10,5,1),LK (9,6,8),RK (1,2,4),LK (1,3,2)
                             ,RD (3,1,5) ,LD (4,6,1),LD (3,5,7),RD (4,8,6)]
                   }


{- ************************
    Some display functions
**************************-}

padBorder = pad 1.2 . centerXY

-- | use checkEmplace to inspect which vertices to use for embedding using alignEmplace
-- shows g followed by emplace g
checkEmplace :: Tgraph -> Diagram B
checkEmplace g = padBorder $ lw ultraThin $ vsep 1 $
                 fmap drawVGraph [g, emplace g]

-- | alignEmplace (a,b) g applies emplace to g, then aligns result with g using (a,b) then embeds g onto this
-- vertices a and b must be common to g and emplace g (use checkEmplace to see possible vertices)
alignEmplace :: (Vertex, Vertex) -> Tgraph -> Diagram B
alignEmplace (a,b) g = -- padBorder $ hsep 1 $ 
           (lc red $ lw thin $ drawPatch $ asPatch d1) <> (lw ultraThin $ drawPatch $ asPatch d2)  where
           [d1,d2] = alignAll (a,b) $ fmap makeVPatch [g, emplace g]


-- | use checkEmplaceMulti to inspect which vertices to use for embedding using alignEmplaceMulti
-- shows g followed by list of results for emplace g
checkEmplaceMulti :: Tgraph -> [Diagram B]
checkEmplaceMulti g = fmap drawVGraph (g: multiEmplace g)

-- | alignEmplaceMulti (a,b) g applies multiEmplace to g, then aligns results using (a,b) 
-- then embeds g into each of the aligned results and returns a list of diagrams
alignEmplaceMulti :: (Vertex, Vertex) -> Tgraph -> [Diagram B]
alignEmplaceMulti (a,b) g = -- padBorder $ hsep 1 $ 
           fmap (((lc red . dashJPatch . asPatch) d1 <>) . lw ultraThin . drawPatch . asPatch) rest where
           (d1:rest) = alignAll (a,b) (fmap makeVPatch (g:multiEmplace g))


-- | Shows labelled vertices For checking partCompose to get alignment vertices
checkPCompose :: Tgraph -> Diagram B
checkPCompose g = 
        padBorder $ hsep 1 $ [drawVPatch $ selectFacesGtoVP fcs g, scale phi $ drawVGraph g'] where
        (fcs,g') = partCompose g

{- | alignCompose (a,b) g  applies pCompose to g, then aligns and draws the composed graph with the remainder faces (in lime)
It can cope with an empty composed graph.
Vertices a and b must be common to composed g and remainder patch
(use checkPCompose to see possible vertices)                               
-}
alignPCompose :: (Vertex, Vertex) -> Tgraph -> Diagram B
alignPCompose (a,b) g = case emptyGraph g' of
        True -> lc lime $ dashJPatch $ asPatch $ selectFacesGtoVP fcs g
        False -> (lw ultraThin $ drawPatch $ asPatch large) <> (lc lime $ dashJPatch $ asPatch rem) where
                 [rem,large] = alignAll (a,b) [ selectFacesGtoVP fcs g
                                              , scale phi $ makeVPatch g'
                                              ]
      where (fcs,g') = partCompose g

-- checkEmplace (dartDs !! 3)
empDartD n = alignEmplace (1,3) (dartDs !! n)
-- checkEmplace (kiteDs !! 5)
empKiteD n = alignEmplace (1,2) (kiteDs !! n)

-- checkEmplace (foolDs !! 1)
empFoolD n = alignEmplace (1,3) (foolDs !! n)

-- checkEmplace (sunDs !! 3)
empSunD n = alignEmplace (1,2) (sunDs !! n)


{- *******************
    Some figures
********************* -}
  
foolAndFoolD,forceFoolDminus,foolChoices,figSunD3D2,empDartD3Fig,empDartD5Fig,empKiteD3Fig,empKiteD5Fig:: Diagram B
testDartsOnly,pCompFig1,pCompFig2,pCompFig,bigPic0,bigPic:: Diagram B

foolAndFoolD = padBorder $ hsep 1 $ [(drawVPatch . scale phi . makeVPatch) fool, drawVGraph foolD]

forceFoolDminus = padBorder $ hsep 1 $ fmap drawVGraph [foolDminus, force foolDminus]

foolChoices = padBorder $ vsep 1 
              [hsep 1 $ fmap (redFool <>) $ fmap dashJGraph choices
              ,hsep 1 $ rotations [1,1,9,4] $ scale phi $ fmap (dashJGraph . graphCompose) choices
              ] where choices = makeChoices fool
                      redFool = dashJGraph fool # lc red
                         
figSunD3D2 = padBorder $ hsep 1 [drawVGraph $ sunDs !! 3, scale phi $ drawVGraph $ sunDs !! 2]

empDartD3Fig = padBorder $ empDartD 3 
empDartD5Fig = padBorder $ empDartD 5

empKiteD3Fig = padBorder $ empKiteD 3
empKiteD5Fig = padBorder $ empKiteD 5

empSunD5Fig = padBorder $ empSunD 5

embedFoolDMulti = padBorder $ hsep 1 $ alignEmplaceMulti (1,6) (foolDs !! 1)
embedFoolD2Multi = padBorder $ hsep 1 $ alignEmplaceMulti (1,6) (foolDs !! 2)

multiGapsFig = padBorder $ hsep 1 [d1,d2]
       where vp = makeVPatch foolD
             d1 = drawVPatch $ removeFacesVP [LK(3,11,14), RK(3,14,4), RK(3,13,11)] vp
             d2 = drawVPatch $ removeFacesVP [RK(5,13,2), LD(6,11,13), RD(6,14,11), LD(6,12,14)] vp

pfMistakeFig  = padBorder $ hsep 1 [drawVGraph mistake, drawVGraph pfMistake]
fdMistake = padBorder $ drawVGraph $ force $ graphDecompose mistake

cdMistake1Fig = padBorder $ hsep 1 [ drawVGraph mistake1
                                       , (drawVGraph . graphCompose . graphDecompose) mistake1]
           
testAlignments = padBorder $ hsep 1 $ fmap drawVPatch $ 
                  alignments [(1,6),(1,6),(12,6),(12,6)] $ fmap makeVPatch $ exploreSeq $ graphCompose (makeChoices fool !! 3)
-- | figure testing selectFacesGtoVP
testDartsOnly = drawVPatch $ selectFacesGtoVP (ldarts g++rdarts g) g where g = sunDs !! 5



moreForceFig = padBorder $ hsep 1 $ fmap (drawVPatch . rotate (ttangle 1)) $ 
                    alignAll (1,6) $ fmap (makeVPatch . force . graphCompose) $ makeChoices fool

moreEmplace = padBorder $ hsep 1 $ rotations [1,1,7,4]
                     $ fmap (drawGraph . emplace . force . graphCompose) $ makeChoices fool

-- | test is a decomposed kite with an extra LK(3,6,8)
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


exploreSeq g = [g, fg, emplace fg, graphCompose fg] where fg = force g
                 
exploreFool0 =  padBorder $ hsep 1 $ fmap drawVPatch $ rotations [1,1,1] 
                    $ fmap makeVPatch $ exploreSeq $ graphCompose (makeChoices fool !! 0)              
exploreFool1 =  padBorder $ hsep 1 $ fmap (drawVPatch . centerOn 1) $ scales [1,1,1,phi] $ rotations [1,1,1,2] 
                    $ fmap makeVPatch $ exploreSeq $ graphCompose (makeChoices fool !! 1)               
exploreFool3 =  padBorder $ hsep 1 $ fmap (drawVPatch) $ scales [1,1,1,phi] $ rotations [4,4,4,5] 
                    $ fmap makeVPatch $ exploreSeq $ graphCompose (makeChoices fool !! 3)               


forceAndCompsDarts = padBorder $ lw ultraThin $ allPosition
                     $ fixRotations [[0,1,1],[0,1,1],[1,1],[1]] 
                     (fComps (dartDs !! 4))         

gapSize = 20.0
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
{-
fixRotations:: [[Int]] -> [(Diagram B,[Diagram B])] -> [(Diagram B,[Diagram B])]
fixRotations [] more = more
fixRotations (rots1:_) [(f,r)] = [(f,rotations rots1 r)]
fixRotations (rots1:(rot:rots2):rotsmore) ((f,r):more) = 
    (rotate (ttangle rot) f, rotations rots1 r):fixRotations (rots2:rotsmore) more
fixRotations (rots1:[]:rotsmore) ((f,r):more) = 
    (f,rotations rots1 r):fixRotations ([]:rotsmore) more
-}

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
               else (dashJGraph fg, allComps g) : scale phi (fComps g')

allComps:: Tgraph -> [Diagram B]
allComps g = if emptyGraph g 
             then [] 
             else dashJGraph g : (scale phi $ allComps $ graphCompose g)


-- checkPCompose $ emplace $ kiteDs !! 3
pCompFig2 = padBorder $ alignPCompose (1,2) (emplace (kiteDs !! 3))
-- checkPCompose $ emplace $ dartDs !! 3 
pCompFig1 = padBorder $ alignPCompose (1,3) (emplace (dartDs !! 3))

pCompFig = padBorder $ vsep 3 $ 
              [hsep 10 $ rotations [4] $ [drawGraph ek3 # lw ultraThin, alignPCompose (1,2) ek3]
              ,hsep 10 $ rotations [3] $ [drawGraph ed3 # lw ultraThin, alignPCompose (1,3) ed3]
              ] where
                  ek3 = emplace (kiteDs !! 3)
                  ed3 = emplace (dartDs !! 3)

bigPic0 = (padBorder $ position $ concat $
          [ zip pointsR1 $ rotations [] $ fmap compD [4,3,2,1,0]
          , zip pointsR2 $ zipWith named ["e4", "e3","e2","e1","e0"] (dots : rotations [] (fmap empD [3,2,1,0]))
          , zip pointsR3 $ zipWith named ["d4", "d3","d2","d1","d0"] (dots : rotations [1,1] (fmap drts [3,2,1,0]))
          ])
          where
              compD n = lw thin $ scale (phi ^ (4-n)) $ alignPCompose (1,5) $ emplace $ dartDs !! n 
              empD n = center $ scale (phi ^ (4-n)) $ empDartD n
              drts n = center . lw thin . scale (phi ^ (4-n)) $ dashJGraph $ dartDs !! n
              dots = center $ hsep 1 $ take 4 $ repeat $ ((circle 0.5) # fc gray # lw none)
              pointsR1 = map p2 [ (0, 70), (52, 70), (100, 70), (150, 70), (190, 70)]
              pointsR2 = map p2 [ (0, 40), (42, 40), (95, 40), (140, 40), (186, 40)]
              pointsR3 = map p2 [ (0, 0),  (42, 0),  (95, 0),  (140, 0),  (186, 0) ]
    
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
             
        where compD n = lw thin $ scale (phi ^ (4-n)) $ alignPCompose (1,3) $ emplace $ dartDs !! n 
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
 
         
         
         