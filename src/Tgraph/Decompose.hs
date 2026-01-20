{-|
Module      : Tgraph.Decompose
Description : A decompose operation for Tgraphs
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module defines decompose and decompositions for Tgraphs, but also exposes 
two auxiliary functions for debugging and experimenting.
-}

{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE Strict                   #-}
{-# OPTIONS_GHC -Wno-deprecations     #-}

module Tgraph.Decompose
  ( decompose
  , decomposeFaces
  , decompositions
  -- * Exported auxiliary functions 
  , phiVMap
  , decompFace
  ) where

import qualified Data.Map.Strict as Map (Map, (!), fromList)
import Data.List(sort)

import Tgraph.Prelude



{------------------------------- 
**************************************
DECOMPOSING - decompose
**************************************
----------------------------------}


-- |Decompose a Tgraph.
decompose :: Tgraph -> Tgraph
decompose = makeUncheckedTgraph . decomposeFaces

-- |Decompose all the faces (using a phiVMap for new vertices).
decomposeFaces :: HasFaces a => a -> [TileFace]
decomposeFaces a = newFaces where
    pvmap = phiVMap a
    newFaces = concatMap (decompFace pvmap) (faces a)

-- |phiVMap fcs produces a finite map from the phi edges (the long edges including kite joins) to assigned new vertices not in fcs.
-- Both (a,b) and (b,a) get the same new vertex number. This is used(in decompFace, decompFaces and decompose.
-- (Sort is used to fix order of assigned numbers).
-- (Exported for use in TrackedTgraphs in Tgraphs module).
phiVMap :: HasFaces a => a -> Map.Map Dedge Vertex
phiVMap fcs = edgeVMap where
  phiReps = sort [e | e@(a,b) <- phiEdges fcs, a<b]
  newVs = [v+1..v+n]
  !n = length phiReps
  !v = maxV fcs
  edgeVMap = Map.fromList $ zip phiReps newVs ++ zip (map reverseD phiReps) newVs 

-- |Decompose a face producing new faces. 
-- This requires an edge to vertex map to get a unique new vertex assigned to each phi edge
-- (as created by phiVMap).
-- (Exported for use in TrackedTgraphs in Tgraph.Extras module).
decompFace:: Map.Map Dedge Vertex -> TileFace -> [TileFace]
decompFace newVFor fc = case fc of
      RK(a,b,c) -> [RK(c,x,b), LK(c,y,x), RD(a,x,y)]
        where !x = (Map.!) newVFor (a,b)
              !y = (Map.!) newVFor (c,a)
      LK(a,b,c) -> [LK(b,c,y), RK(b,y,x), LD(a,x,y)]
        where !x = (Map.!) newVFor (a,b)
              !y = (Map.!) newVFor (c,a)       
      RD(a,b,c) -> [LK(a,x,c), RD(b,c,x)]
        where !x = (Map.!) newVFor (a,b)
      LD(a,b,c) -> [RK(a,b,x), LD(c,x,b)]
        where !x = (Map.!) newVFor (a,c)
   
-- |infinite list of decompositions of a Tgraph     
decompositions :: Tgraph -> [Tgraph]
decompositions = iterate decompose



