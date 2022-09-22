import Test.Hspec
import Control.Exception (evaluate)

import Tgraphs
import GraphFigExamples

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

-- Test example Tgraphs

-- g1 has crossing boundaries
g1 = makeUncheckedTgraph x1

-- g2 is not connected
g2 = makeUncheckedTgraph x2

-- dD6 is a 6 times decomposed dartGraph
dD6 = dartDs !!6


graphPropSpec :: Spec
graphPropSpec = describe "Test Properties of Tgraphs" $ do
    context "When a list of tile-faces has any face with a repeated vertex" $
      it "hasEdgeLoops should return True" $
        hasEdgeLoops x0 `shouldBe` True
    context "When a list of tile-faces has no face with a repeated vertex" $
      it "hasEdgeLoops should return False" $
        hasEdgeLoops x1 `shouldBe` False
    context "When there are crossing boundaries" $
      it "crossingBoundaries should return True" $
        crossingBoundaries g1 `shouldBe` True
    context "When there are no crossing boundaries" $
      it "crossingBoundaries should return False" $
        crossingBoundaries foolD `shouldBe` False
    context "When a Tgraph is connected" $
      it "connected should return True" $
        connected g1 `shouldBe` True
    context "When a Tgraph is not connected" $
      it "connected should return False" $
        connected g2 `shouldBe` False
    context "When a Tgraph has illegal edge conflicts" $
      it "illegalTiling should return True" $
        illegalTiling x3 `shouldBe` True
    context "When a Tgraph has illegal edge conflicts" $
      it "illegalTiling should return True" $
        illegalTiling x4 `shouldBe` True
    context "When a Tgraph has no illegal edge conflicts" $
      it "illegalTiling should return False" $
        illegalTiling x1 `shouldBe` False
    context "When faces do not form a valid Tgraph" $
      it "checkedTgraph should throw an exception" $ do
          evaluate (checkedTgraph testCrossingBoundary) `shouldThrow` anyException

graphOpSpec :: Spec
graphOpSpec = describe "Main Tgraph Operations Test" $ do
    context "Decomposition of Tgraphs" $
      it "Number of faces of dartDs !!6 should be 466" $
         length(faces dD6)  `shouldBe` 466
    context "Composing Tgraphs" $
      it "Number of faces of maxCompose (dartDs !!6) should be 2" $
         length(faces(maxCompose dD6)) `shouldBe` 2
    context "Forcing Tgraphs" $
      it "Number of faces of force (dartDs !!6) should be 7546" $
         length(faces(force dD6)) `shouldBe` 7546
  