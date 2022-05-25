import Test.Hspec
import Tgraphs
import GraphFigExamples

main :: IO ()
main = hspec spec

spec :: Spec
spec = do graphPropSpec
          graphOpSpec

-- Test Example Tgraphs and non-Tgraphs

-- x1 has crossing boundaries
x1 = makeTgraph [LK (1,8,3),RD (2,3,8),RK (1,3,9),LD (4,9,3),LK (5,10,13)
                ,RD (6,13,10),LK (3,2,13),LD (6,11,13),LK (7,4,14),RK (7,14,12)
                ]

-- x2 is not connected
x2 = makeTgraph (faces x1 ++ [LK (15,16,17),RK (15,17,18)])

-- x3 has edge conflicts 
x3 = makeTgraph [LK (3,8,1),RD (2,3,8),RK (1,3,9),LD (4,9,3),LK (5,10,13)
                ,RD (6,13,10),LK (3,2,13),LD (6,11,13),LK (7,4,14),RK (7,14,12)
                ]

-- dD6 is a 6 times decomposed dartGraph
dD6 = dartDs !!6


graphPropSpec :: Spec
graphPropSpec = describe "Test Properties of Tgraphs" $ do
    context "When there are crossing boundaries" $
      it "crossingBoundaries should return True" $
        crossingBoundaries x1 `shouldBe` True
    context "When there are no crossing boundaries" $
      it "crossingBoundaries should return False" $
        crossingBoundaries foolD `shouldBe` False
    context "When a Tgraph is connected" $
      it "connected should return True" $
        connected x1 `shouldBe` True
    context "When a Tgraph is not connected" $
      it "connected should return False" $
        connected x2 `shouldBe` False
    context "When a Tgraph has edge conflicts" $
      it "edgeConflicts should return True" $
        edgeConflicts x3 `shouldBe` True
    context "When a Tgraph has no edge conflicts" $
      it "edgeConflicts should return False" $
        edgeConflicts x1 `shouldBe` False

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
  