{-|
Module      : Tgraph.Decompose
Description : A decompose operation for Tgraphs
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module includes the main decomposition operation decompose but also exposes 
some auxiliary functions for debugging and experimenting.
-}
module Tgraph.Decompose where

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
decompose g = makeUncheckedTgraph newFaces where
    newFaces = concatMap (decompFace (phiVMap g)) (faces g)

-- |phiVMap g produces a function mapping each phi edge (the long edges including kite joins) to an assigned new vertex not in g.
-- Both (a,b) and (b,a) get the same vertex number.
-- (Also uses sort to fix order of assigned numbers).  
phiVMap :: Tgraph -> Map.Map Dedge Vertex
phiVMap g = edgeVMap where
  phiReps = sort [(a,b) | (a,b) <- phiEdges g, a<b]
  newVs = (length phiReps) `newVsAfter` (maxV g)
  edgeVMap = Map.fromList $ zip phiReps newVs ++ zip (fmap reverseD phiReps) newVs 

-- |Decompose a face producing new faces. 
-- This requires a function to get the unique new vertex assigned to each phi edge
-- (as created by phiVMap).
decompFace:: Map.Map Dedge Vertex -> TileFace -> [TileFace]
decompFace newVFor fc = case fc of
      RK(a,b,c) -> [RK(c,x,b), LK(c,y,x), RD(a,x,y)]
        where x = (Map.!) newVFor (a,b)
              y = (Map.!) newVFor (c,a)
      LK(a,b,c) -> [LK(b,c,y), RK(b,y,x), LD(a,x,y)]
        where x = (Map.!) newVFor (a,b)
              y = (Map.!) newVFor (c,a)       
      RD(a,b,c) -> [LK(a,x,c), RD(b,c,x)]
        where x = (Map.!) newVFor (a,b)
      LD(a,b,c) -> [RK(a,b,x), LD(c,x,b)]
        where x = (Map.!) newVFor (a,c)
   
-- |infinite list of decompositions of a Tgraph     
decompositions :: Tgraph -> [Tgraph]
decompositions = iterate decompose



