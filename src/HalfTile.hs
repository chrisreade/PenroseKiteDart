{-|
Module      : HalfTile
Description : Introducing a generic type for half tiles of darts and kites
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

-}
{-# LANGUAGE TypeFamilies              #-} -- needed for Transformable Instance
{-# LANGUAGE FlexibleInstances         #-} -- needed for Transformable Instance

module HalfTile 
  ( HalfTile(..)
  , tileRep
  , isLD
  , isRD
  , isLK
  , isRK
  , isDart
  , isKite
  , HalfTileLabel
  , tileLabel
  , isMatched
  ) where
    
import Diagrams.Prelude (V,N, Transformable(..)) -- needed to make HalfTile a Transformable when a is Transformable

{-|
Representing Half Tile Pieces Polymorphicly.
Common code for both graphs and vector representations of tilings. 
For Pieces - rep is V2 Double
For TileFaces (in Tgraphs) rep is (Vertex,Vertex,Vertex)
-}
data HalfTile rep = LD rep -- ^ Left Dart
                  | RD rep -- ^ Right Dart
                  | LK rep -- ^ Left Kite
                  | RK rep -- ^ Right Kite
                  deriving (Show,Eq)

-- | Note this ignores the tileLabels when comparing.
-- However we should never have 2 different HalfTiles with the same rep
instance Ord rep => Ord (HalfTile rep) where
    compare t1 t2 = compare (tileRep t1) (tileRep t2)

-- |Make Halftile a Functor
instance Functor HalfTile where
    fmap f (LD rep) = LD (f rep)
    fmap f (RD rep) = RD (f rep)
    fmap f (LK rep) = LK (f rep)
    fmap f (RK rep) = RK (f rep)

-- |Needed for Transformable instance of HalfTile - requires TypeFamilies
type instance N (HalfTile a) = N a
-- |Needed for Transformable instance of HalfTile - requires TypeFamilies
type instance V (HalfTile a) = V a
-- |HalfTile inherits Transformable  - Requires FlexibleInstances
instance Transformable a => Transformable (HalfTile a) where
    transform t = fmap (transform t)






{-# INLINE tileRep #-}
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

-- |By having () as the half tile representation we treat the constructors as just labels
type HalfTileLabel = HalfTile ()
-- |convert a half tile to its label (HalfTileLabel can be compared for equality)
tileLabel :: HalfTile a -> HalfTileLabel
tileLabel = fmap $ const () -- functor HalfTile

-- | isMatched t1 t2 is True if t1 and t2 have the same HalfTileLabel 
-- (i.e. use the same constructor - both LD or both RD or both LK or both RK)
isMatched :: HalfTile rep1 -> HalfTile rep2 -> Bool
isMatched t1 t2 = tileLabel t1 == tileLabel t2



