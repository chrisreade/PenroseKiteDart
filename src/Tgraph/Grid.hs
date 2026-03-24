{-|
Module      : Tgraph.Grid
Description : A type for recording 2D locations with fast check for touching locations
Copyright   : (c) Chris Reade, 2026
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module defines a Grid type recording 2D points with fast lookup by grid coordinates.
It is used to quickly identify touching vertices when forcing.
-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TupleSections             #-}

module Tgraph.Grid
(
    insertGridCheck
  , createGridCheck
  , createGrid
  , createPointGrid
  , emptyGrid
  , Grid
  , touching
  
) where

import Diagrams.Prelude


import Data.List ( find )
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IMap (alter,insert,empty,lookup)

-- | A (Grid a) maps from Int x and y coordinates to a list of valued points in the unit square
-- with bottom left corner given by the coordinates, each valued point is a pair with a value and point.
-- For a Grid with just points (and no associated values), use Grid ().
newtype Grid a = Grid{ gridmap::IntMap (IntMap [(a,P2 Double)])} deriving (Show)

-- | An empty grid.
emptyGrid :: Grid a
emptyGrid = Grid IMap.empty

-- | get the Int x and Int y coordinates of a 2D point (using floor).
gridCoords :: P2 Double -> (Int,Int)
gridCoords p = (floor x, floor y) 
               where x = p ^. _x 
                     y = p ^. _y 

-- | insert a new (value,point) in a grid, but check the 9 neighbouring cells for any
-- points touching the new point.  If a touching point is found, a (Left v) is returned,
-- where v was associated with the (first found) touching point.
-- Otherwise Right gd' is returned where gd' is an updated grid with the new (valued) point.
insertGridCheck :: (a, P2 Double) -> Grid a -> Either a (Grid a)
insertGridCheck ap@(_,p) gd =
    case checkGridNear p gd of
        Just b  -> Left b
        Nothing -> Right $ insertGrid ap gd

 -- | insert a new (valued) point in a grid without any checks.
 -- This is used to initialise a grid with (valued) where the points are known not to be touching.       
insertGrid :: (a, P2 Double) -> Grid a -> Grid a
insertGrid ap gd = Grid $ IMap.alter column n (gridmap gd)
   where (n,m) = gridCoords (snd ap)
         column Nothing = Just $ IMap.insert n [ap] IMap.empty
         column (Just imp) = Just $ IMap.alter ht m imp
         ht Nothing = Just [ap]
         ht (Just aps) = Just (ap:aps)

-- | get the list of (valued) points in a grid cell (with Int coords x y)         
fromGrid :: Grid a -> Int -> Int -> [(a,P2 Double)]
fromGrid gd n = 
    case IMap.lookup n (gridmap gd) of
      Nothing ->  (\_ -> [])
      Just imp -> (\m -> case IMap.lookup m imp of
                          Nothing -> []
                          Just aps -> aps)

-- | get the list of (valued) points from 9 grid cells (aound the one with Int coords x y)         
fromGridNear :: Grid a -> Int -> Int -> [(a,P2 Double)]
fromGridNear gd n m =  
    let g0 = fromGrid gd n
        g1 = fromGrid gd (n-1)
        g2 = fromGrid gd (n+1)
    in concat [g0 m, g0 (m-1), g0 (m+1)
              ,g1 m, g1 (m-1), g1 (m+1)
              ,g2 m, g2 (m-1), g2 (m+1)
              ]

-- | check a point for any touching locations in the grid
-- (checking with points in the 9 grid cells near the point).
-- This will return Nothing if there are no touching locations
-- and Just v otherwise where v is the value paired with the first touching location found.       
checkGridNear :: P2 Double -> Grid a -> Maybe a             
checkGridNear p gd = fmap fst $ find touchingItem (fromGridNear gd n m) where
                 (n,m) = gridCoords p
                 touchingItem (_,p') = touching p p'

{-|touching checks if two points are considered close.
Close means the square of the distance between them is less than a certain number (currently 0.25) so they cannot be
vertex locations for 2 different vertices in a VPatch using unit scale for short edges.
-}
touching :: P2 Double -> P2 Double -> Bool
touching p p1 = quadrance (p .-. p1) < 0.25 -- quadrance is square of length of a vector
-- quadrance 0.1 represents a distance of about 0.316 units (= sqrt 0.1)
-- quadrance 0.25 represents a distance of 0.5 units

                 
-- | insert (valued) points into a new grid without any checks.
-- This is used to initialise a grid with (valued) points known not to be touching.       
createGrid :: [(a,P2 Double)] -> Grid a
createGrid aps = foldl' (flip insertGrid) (Grid IMap.empty) aps

-- | insert points into a new grid without any checks.
-- This uses () for values.      
createPointGrid :: [P2 Double] -> Grid ()
createPointGrid = createGrid . map ((),) 

-- | insert (valued) points into a new grid with checks.
-- This will return Left v if there is a touching location with value v,
-- and Just gd otherwise where gd is a grid containiing the (valued) points.
createGridCheck :: [(a, P2 Double)] -> Either a (Grid a)
createGridCheck aps = insertAll aps (Grid IMap.empty) where
  insertAll [] gd = Right gd
  insertAll (ap:more) gd = 
    do gd1 <- insertGridCheck ap gd
       insertAll more gd1
