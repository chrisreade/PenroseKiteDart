module HalfTile where
{-
Representing Half Tile Pieces Polymorphicly
Common code for both graphs and vector representations of tiling 
For vectors - rep is V2 Double
For Tgraphs rep is (Vertex,Vertex,Vertex) 
-}
data HalfTile rep = LD rep
                  | RD rep
                  | LK rep
                  | RK rep
                  deriving (Show,Eq)

instance Functor HalfTile where
    fmap f (LD rep) = LD (f rep)
    fmap f (RD rep) = RD (f rep)
    fmap f (LK rep) = LK (f rep)
    fmap f (RK rep) = RK (f rep)

tileRep (LD r) = r
tileRep (RD r) = r
tileRep (LK r) = r
tileRep (RK r) = r

isLD,isRD,isLK,isRK,isDart,isKite :: HalfTile rep -> Bool
isLD (LD _) = True
isLD _      = False
isRD (RD _) = True
isRD _      = False
isLK (LK _) = True
isLK _      = False
isRK (RK _) = True
isRK _      = False
isDart x = isLD x || isRD x
isKite x = isLK x || isRK x

