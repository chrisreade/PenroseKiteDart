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
  , createGrid
  , Grid
  
) where

import Diagrams.Prelude
import Tgraph.Prelude ( touching )

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IMap (alter,insert,empty,lookup)

-- | A Grid maps from Int x and y coordinates to a list of points in the unit square
-- with bottom left at the coordinates.
newtype Grid = Grid{ gridmap::IntMap (IntMap [P2 Double])} deriving (Show)

-- | get the Int x and Int y coordinates of a 2D point (using floor).
gridCoords :: P2 Double -> (Int,Int)
gridCoords p = (floor x, floor y) 
               where x = p ^. _x 
                     y = p ^. _y 

-- | insert a new point in a grid, but check the 9 neighbouring cells for any
-- points touching the new point.  If a touching point is found, Nothing is returned,
-- otherwise Just gd' is returned where gd' is an updated grid with the new point.
insertGridCheck :: P2 Double -> Grid -> Maybe Grid
insertGridCheck p gd =
    case checkNear p gd of
        True -> Nothing
        False -> Just $ insertGrid p gd

 -- | insert a new point in a grid without any checks.
 -- This is used to initialise a grid with points known not to be touching.       
insertGrid :: P2 Double -> Grid -> Grid
insertGrid p gd = Grid $ IMap.alter column n (gridmap gd)
   where (n,m) = gridCoords p
         column Nothing = Just $ IMap.insert n [p] IMap.empty
         column (Just imp) = Just $ IMap.alter ht m imp
         ht Nothing = Just [p]
         ht (Just ps) = Just (p:ps)

-- | get the list of points in a grid cell (with Int coords x y)         
fromGrid :: Grid -> Int -> Int -> [P2 Double]
fromGrid gd n = 
    case IMap.lookup n (gridmap gd) of
      Nothing ->  (\_ -> [])
      Just imp -> (\m -> case IMap.lookup m imp of
                          Nothing -> []
                          Just ps -> ps)

-- | get the list of points from 9 grid cells (aound the one with Int coords x y)         
fromGridNear :: Grid -> Int -> Int -> [P2 Double]
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
checkNear :: P2 Double -> Grid -> Bool             
checkNear p gd = any (touching p) (fromGridNear gd n m) where
                 (n,m) = gridCoords p

 -- | insert points into a new grid without any checks.
 -- This is used to initialise a grid with points known not to be touching.       
createGrid :: [P2 Double] -> Grid
createGrid ps = foldl' (flip insertGrid) (Grid IMap.empty) ps
