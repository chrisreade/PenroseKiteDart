{-|
Module      : Tgraph.Grid
Description : A type for recording 2D locations with fast check for touching locations
Copyright   : (c) Chris Reade, 2026
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module defines a Grid type recording 2D points with fast lookup by grid coordinates.
It is used to quickly identify touching vertices (e.g. when forcing).
-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TupleSections             #-}


module Tgraph.Grid
(
    insertGridCheck
  , createGridCheck
  , insertGrid
  , createGrid
  , emptyGrid
  , Grid
  , touching
  , ValuedPoint
  , allClashes

) where

import Diagrams.Prelude


import Data.List ( find )
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IMap (alter,insert,empty,lookup)

-- | A (Grid a) maps from Int x and y coordinates to a list of a in the unit square
-- with bottom left corner given by the coordinates of a.
-- The type a should be a ValuedPoint for Grid access functions to work.
newtype Grid a = Grid{ gridmap::IntMap (IntMap [a])} deriving (Show)

-- | An empty grid.
emptyGrid :: Grid a
emptyGrid = Grid IMap.empty

-- |A class for items which have a 2D point (so can be added to a grid).
class ValuedPoint a where
    getPoint:: a -> P2 Double

-- | A 2D point is considered a ValuedPoint (with no associated value)
instance ValuedPoint (P2 Double) where
    getPoint p = p

-- | A pair of a value and a 2D point is considered a ValuedPoint
instance ValuedPoint (a, P2 Double) where
    getPoint ((_,p)) = p

-- | get the Int x and Int y grid coordinates for a ValuedPoint (using floor).
gridCoords :: ValuedPoint a => a -> (Int,Int)
gridCoords a = (n,m) 
   where p = getPoint a
         !n = floor (p ^. _x) 
         !m = floor (p ^. _y)

-- | insert a new valued point in a grid, but check the 9 neighbouring cells for any
-- points touching the new point.  If a touching point is found, a (Left vpt) is returned,
-- where vpt is the (first found) grid entry with a touching point.
-- Otherwise Right gd' is returned where gd' is an updated grid with the new (valued) point.
insertGridCheck :: ValuedPoint a => a -> Grid a -> Either a (Grid a)
insertGridCheck a gd =
    case checkGridNear (getPoint a) gd of
        Just b  -> Left b
        Nothing -> Right $ insertGrid a gd

-- | insert a new (valued) point in a grid without any checks.
-- This is used to initialise a grid with (valued) points where the points are known not to be touching.       
insertGrid :: ValuedPoint a => a -> Grid a -> Grid a
insertGrid ap gd = Grid $ IMap.alter column n (gridmap gd)
   where (n,m) = gridCoords ap
         column Nothing = Just $ IMap.insert m [ap] IMap.empty
         column (Just imp) = Just $ IMap.alter ht m imp
         ht Nothing = Just [ap]
         ht (Just aps) = Just (ap:aps)

-- | get the list of (valued) points in a grid cell (with the given Int coords)         
fromGrid :: Grid a -> Int -> Int -> [a]
fromGrid gd n = 
    case IMap.lookup n (gridmap gd) of
      Nothing ->  (\_ -> [])
      Just imp -> (\m -> case IMap.lookup m imp of
                          Nothing -> []
                          Just aps -> aps)

-- | get the list of (valued) points from 9 grid cells (around the one with given Int coords)         
fromGridNear :: Grid a -> Int -> Int -> [a]
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
-- This will return Nothing if there are no touching locations and Just vpt otherwise
-- where vpt is the the first valued point found in the grid with a touching location.       
checkGridNear :: ValuedPoint a => P2 Double -> Grid a -> Maybe a             
checkGridNear p gd = find touchingItem (fromGridNear gd n m)
  where (n,m) = gridCoords p
        touchingItem a = touching p (getPoint a)

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
createGrid :: ValuedPoint a => [a] -> Grid a
createGrid aps = foldl' (flip insertGrid) (Grid IMap.empty) aps

-- | insert (valued) points into a new grid with checks.
-- This will return Left vpt if vpt is found in the grid with a touching location,
-- and Just gd otherwise where gd is a grid containiing the (valued) points.
createGridCheck :: ValuedPoint a => [a] -> Either a (Grid a)
createGridCheck aps = insertAll aps (Grid IMap.empty) where
  insertAll [] gd = Right gd
  insertAll (ap:more) gd = 
    do gd1 <- insertGridCheck ap gd
       insertAll more gd1

-- |Use a grid to find all clashing (touching) valued points.
-- The function argument is used to extract values from valued points.
allClashes :: ValuedPoint a => (a -> b) -> [a] -> [(b,b)]
allClashes f aps = check aps (Grid IMap.empty) []  where
  check [] _ cs = cs
  check (ap:more) gd cs =
      case insertGridCheck ap gd of
          Right gd' -> check more gd' cs
          Left ap'  -> check more gd ((f ap, f ap'):cs) 
          -- note ap' earlier than ap in the incoming list
