{-|
Module      : Tgraph.Grid
Description : A type for comparing 2D locations
Copyright   : (c) Chris Reade, 2026
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module defines a Grid type for use in tracking positions and identifying touching vertices
when forcing.
-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TupleSections             #-}

module Grid
(
    Grid
  , insertCheck
  , createGrid
) where

import Diagrams.Prelude
import Tgraph.Prelude ( touching )
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IMap (alter,insert,empty,lookup)



type Grid = IntMap (IntMap [P2 Double])

scalefactor :: Double
scalefactor = 2.0

gridCoords :: P2 Double -> (Int,Int)
gridCoords p = (floor $ scl * x, floor $ scl * y) 
               where x = p ^. _x 
                     y = p ^. _y 
                     scl = 1/scalefactor

insertCheck :: P2 Double -> Grid -> Maybe Grid
insertCheck p cs =
    case checkNear p cs of
        True -> Nothing
        False -> Just $ insertNew p cs
        
insertNew :: P2 Double -> Grid -> Grid
insertNew p cs =
   IMap.alter column n cs
   where (n,m) = gridCoords p
         column Nothing = Just $ IMap.insert n [p] IMap.empty
         column (Just imp) = Just $ IMap.alter ht m imp
         ht Nothing = Just [p]
         ht (Just ps) = Just (p:ps)
         
fromGrid :: Grid -> Int -> Int -> [P2 Double]
fromGrid cs n = 
    case IMap.lookup n cs of
      Nothing ->  (\_ -> [])
      Just imp -> (\m -> case IMap.lookup m imp of
                          Nothing -> []
                          Just ps -> ps)
                
fromGridNear :: Grid -> Int -> Int -> [P2 Double]
fromGridNear cs n m =  
    let g0 = fromGrid cs n
        g1 = fromGrid cs (n-1)
        g2 = fromGrid cs (n+1)
    in concat [g0 m, g0 (m-1), g0 (m+1)
              ,g1 m, g1 (m-1), g1 (m+1)
              ,g2 m, g2 (m-1), g2 (m+1)
              ]

checkNear :: P2 Double -> Grid -> Bool             
checkNear p cs = any (touching p) (fromGridNear cs n m) where
                 (n,m) = gridCoords p

createGrid :: [P2 Double] -> Grid
createGrid ps = foldl' (flip insertNew) IMap.empty ps
