{-|
Module      : HalfTile
Description : Introducing a generic type for half tiles of darts and kites
Copyright   : (c) Chris Reade, 2021
License     : MIT
Maintainer  : chrisreade@mac.com
Stability   : experimental

-}
module HalfTile where

{-|
Representing Half Tile Pieces Polymorphicly.
Common code for both graphs and vector representations of tilings. 
For vectors - rep is V2 Double
For Tgraphs rep is (Vertex,Vertex,Vertex)
-}
data HalfTile rep = LD rep -- ^ Left Dart
                  | RD rep -- ^ Right Dart
                  | LK rep -- ^ Left Kite
                  | RK rep -- ^ Right Kite
                  deriving (Show,Eq)

instance Functor HalfTile where
    fmap f (LD rep) = LD (f rep)
    fmap f (RD rep) = RD (f rep)
    fmap f (LK rep) = LK (f rep)
    fmap f (RK rep) = RK (f rep)

-- |return the representation of a half-tile
tileRep:: HalfTile rep -> rep
tileRep (LD r) = r
tileRep (RD r) = r
tileRep (LK r) = r
tileRep (RK r) = r

-- |half-tile predicate
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



