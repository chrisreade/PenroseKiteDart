import Test.Hspec
import Control.Exception (evaluate)

import Tgraphs
import TgraphExamples
import TestIllustrate (touchErrorFaces,testCrossingBoundary)
main :: IO ()
main = hspec spec

spec :: Spec
spec = do graphPropSpec
          graphOpSpec

-- Example lists of tile-faces

-- x0 has a face with a repeated vertex
x0 =  [LK (1,8,3),RD (2,3,8),RK (1,3,9),LD (4,9,3),LK (5,10,13)
      ,RD (6,13,10),LK (3,2,13),LD (6,11,13),LK (7,14,14),RK (7,14,12)
      ]

-- x1 has crossing boundaries
x1 = [LK (1,8,3),RD (2,3,8),RK (1,3,9),LD (4,9,3),LK (5,10,13)
     ,RD (6,13,10),LK (3,2,13),LD (6,11,13),LK (7,4,14),RK (7,14,12)
     ]
     
-- x2 is not connected
x2 = x1 ++ [LK (15,16,17),RK (15,17,18)]

-- x3 has edge conflicts 
x3 = [LK (3,8,1),RD (2,3,8),RK (1,3,9),LD (4,9,3),LK (5,10,13)
     ,RD (6,13,10),LK (3,2,13),LD (6,11,13),LK (7,4,14),RK (7,14,12)
     ]

-- x4 has edge conflicts 
x4 = [LD(1,2,3),RD(1,4,2),RD(4,1,5),LD(4,5,6)]

-- x5 has enon-positive vertex number 
x5 = [LD(0,1,2)]

-- Test example Tgraphs

-- g1 has crossing boundaries
g1 = makeUncheckedTgraph x1

-- g2 is not connected
g2 = makeUncheckedTgraph x2

-- dD6 is a 6 times decomposed dartGraph
dD6 = dartDs !!6


graphPropSpec :: Spec
graphPropSpec = describe "Test Properties of Tgraphs" $ do
    context "When fcs (a list of tile-faces) has any face with a repeated vertex" $
      it "hasEdgeLoops fcs should return True" $
        hasEdgeLoops x0 `shouldBe` True
    context "When fcs has no face with a repeated vertex" $
      it "hasEdgeLoops fcs should return False" $
        hasEdgeLoops x1 `shouldBe` False
    context "When fcs has crossing boundaries" $
      it "crossingBoundaries fcs should return True" $
        crossingBoundaries g1 `shouldBe` True
    context "When fcs has no crossing boundaries" $
      it "crossingBoundaries fcs should return False" $
        crossingBoundaries foolD `shouldBe` False
    context "When a Tgraph g is connected" $
      it "connected g should return True" $
        connected g1 `shouldBe` True
    context "When a Tgraph g is not connected" $
      it "connected g should return False" $
        connected g2 `shouldBe` False
    context "When fcs has illegal edge conflicts" $
      it "illegalTiling fcs should return True" $
        illegalTiling x3 `shouldBe` True
    context "When fcs has illegal edge conflicts" $
      it "illegalTiling fcs should return True" $
        illegalTiling x4 `shouldBe` True
    context "When fcs has no illegal edge conflicts" $
      it "illegalTiling fcs should return False" $
        illegalTiling x1 `shouldBe` False
    context "When fcs contains a non-positive vertex number" $
      it "makeTgraph fcs should throw an exception" $ do
          evaluate (makeTgraph x5) `shouldThrow` anyException
    context "When fcs have a touching vertex" $
      it "makeTgraph fcs should throw an exception" $ do
          evaluate (makeTgraph touchErrorFaces) `shouldThrow` anyException
    context "When fcs do not form a valid Tgraph" $
      it "makeTgraph fcs should throw an exception" $ do
          evaluate (makeTgraph testCrossingBoundary) `shouldThrow` anyException

graphOpSpec :: Spec
graphOpSpec = describe "Main Tgraph Operations Test" $ do
    context "Decomposition of Tgraphs" $
      it "Number of faces of dartDs !!6 should be 466" $
         length(faces dD6)  `shouldBe` 466
    context "Composing Tgraphs" $
      it "Number of faces of maxComp (dartDs !!6) should be 6" $
         length(faces(maxComp dD6)) `shouldBe` 6
    context "Forcing Tgraphs" $
      it "Number of faces of force (dartDs !!6) should be 7546" $
         length(faces(force dD6)) `shouldBe` 7546
  