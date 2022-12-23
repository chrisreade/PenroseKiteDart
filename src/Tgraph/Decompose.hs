{-|
Module      : Tgraph.Decompose
Description : A decompose operation for Tgraphs
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module includes the main decomposition operation decompose but also exposes 
some auxilliary functions for debugging, experimenting and use with SubTgraphs
-}
module Tgraph.Decompose where

import qualified Data.Map.Strict as Map (Map, lookup, insert, empty, (!))
import Data.List(sort,foldl')

import Tgraph.Prelude



{------------------------------- 
**************************************
DECOMPOSING - decompose
**************************************
----------------------------------}


-- |Decompose a Tgraph. This is uniquely determined.
decompose :: Tgraph -> Tgraph
decompose g = Tgraph{ maxV = newMax
                     , faces = newFaces
                     } where
    (newMax , newVFor) = maxAndPhiVMap g
    newFaces = concatMap (decompFace newVFor) (faces g)

-- |maxAndPhiVMap g produces a new maxV and
-- a function mapping each phi edge to an assigned new vertex.
-- Both (a,b) and (b,a) get the same v.
-- (Also sorted phiEdges to reduce arbitrariness of numbering).  
maxAndPhiVMap :: Tgraph -> (Vertex, Dedge -> Vertex)
maxAndPhiVMap g = (oldMax+sizeNew, (Map.!) edgeVMap) where
  phiReps = sort [(a,b) | (a,b) <- phiEdges g, a<b]
  oldMax = maxV g
  sizeNew = length phiReps
  newVs = sizeNew `newVsAfter` oldMax
--BEWARE: Changing foldl' may alter the order of numbering
  (_, edgeVMap) = foldl' insert2 (newVs, Map.empty) phiReps
  insert2 (v:vs,emap) e = (vs, Map.insert e v $ Map.insert (reverseD e) v emap)


-- |Decompose a face producing new faces. 
-- This requires a function to get the unique vertex assigned to each phi edge
-- (as created by newPhiVMap)
decompFace:: ((Vertex,Vertex)->Vertex) -> TileFace -> [TileFace]
decompFace newVFor fc = case fc of
      RK(a,b,c) -> [RK(c,x,b), LK(c,y,x), RD(a,x,y)]
        where x = newVFor (a,b)
              y = newVFor (c,a)
      LK(a,b,c) -> [LK(b,c,y), RK(b,y,x), LD(a,x,y)]
        where x = newVFor (a,b)
              y = newVFor (c,a)       
      RD(a,b,c) -> [LK(a,x,c), RD(b,c,x)]
        where x = newVFor (a,b)
      LD(a,b,c) -> [RK(a,b,x), LD(c,x,b)]
        where x = newVFor (a,c)
     
-- |infinite list of decompositions of a Tgraph     
decompositions :: Tgraph -> [Tgraph]
decompositions = iterate decompose



