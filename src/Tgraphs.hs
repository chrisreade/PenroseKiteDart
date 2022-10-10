{-|
Module      : Tgraphs
Description : Collects and exports the various Tgraph modules 
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This is the main module for Tgraphs which collects and exports the various other Tgraph modules 
and includes a definition of emplace and other experimental combinations.
-}
module Tgraphs ( module Tgraphs
               , module Tgraph.Prelude
               , module Tgraph.Decompose
               , module Tgraph.Compose
               , module Tgraph.Force
               , module Tgraph.Convert
               , module Tgraph.Relabelling
               ) where

import Tgraph.Prelude
import Tgraph.Decompose
import Tgraph.Compose
import Tgraph.Force
import Tgraph.Convert
import Tgraph.Relabelling



{-|
makeTgraph performs the same checks for Tgraph properties as checkedTgraph but in addition
it also checks that there are no touching vertices (distinct labels for the same vertex)
using Tgraph.Convert.touchingVertices (which calculates vertex locations).
It produces an error if either check fails.
Note that the other Tgraph properties are checked first, to ensure that calculation of 
vertex locations can be done.
-}
makeTgraph :: [TileFace] -> Tgraph
makeTgraph fcs = getResult $ onFail "makeTgraph: (failed):\n" $
 do g <- checkTgraphProps fcs
    let touchVs = touchingVertices (faces g)
    if null touchVs 
    then Right g 
    else Left ("Found touching vertices: " 
               ++ show touchVs
               ++ "\n(To fix, use: correctTouchingVs)\n"
              )

{-
makeTgraph :: [TileFace] -> Tgraph
makeTgraph fcs =
    g `seq` if null touchVs 
            then g
            else error $ "makeTgraph: touching vertices " ++ show touchVs
                          ++ "\n(To fix, use: correctTouchingVs)\n"
  where g = checkedTgraph fcs
        touchVs = touchingVertices fcs
-}

{----------------------------
********************************************
EXPERIMENTAL BITS
********************************************
------------------------------}

{----------------------------
EMPLACEMENTS
------------------------------}

-- |emplace does maximal composing with force and composeG, 
-- then applies decomposeG and force repeatedly back to the starting level.
-- It produces the emplacement of influence of the argument graph.   
emplace:: Tgraph -> Tgraph
emplace g = if nullGraph g'
            then fg 
            else (force . decomposeG . emplace) g'
    where fg = force g
          g' = composeG fg 
            
-- |emplacements is best supplied with a maximally composed or near maximally composed graph
-- It produces an infinite list of emplacements of the starting graph and its decompositions.
emplacements :: Tgraph -> [Tgraph]
emplacements = iterate (force . decomposeG) . emplace -- was .force

{-------------------------------------------------------------------------
 makeChoices, emplaceChoices
------------------------------------------------------------------------------}

-- |a version of emplace using makeChoices at the top level.
-- after makeChoices we use emplace to attempt further compositions but with no further choices
emplaceChoices:: Tgraph -> [Tgraph]
emplaceChoices g = 
       let fg = force g
           g' = composeG fg 
       in
           if nullGraph g'
           then emplace <$> makeChoices g
           else force . decomposeG <$> emplaceChoices g'
                                 
{-| makeChoices is a temporary tool which does not attempt to analyse choices for correctness.
It can thus create some choices which will be incorrect.
The unknowns returned from classifyDartWings can become largeKiteCentres or largeDartBases.
This produces 2^n choices where n is the number of unknowns (excluding lone dart wing tips with valency 2).
-}
makeChoices :: Tgraph -> [Tgraph]
makeChoices g = choices unks [g] where
    unks = filter ((>2).graphValency g) (unknowns (classifyDartWings g))
    choices [] gs = gs
    choices (v:more) gs = choices more (fmap (forceLKC v) gs ++ fmap (forceLDB v) gs)


{-------------------------------------------------------------------------
 compForce, allCompFs, allComps, maxCompose, maxFCompose
------------------------------------------------------------------------------}

-- |compForce does a force then composeG. 
-- (the connectedNoCross check may be redundant on the composed graph because the argument was forced.)
compForce:: Tgraph -> Tgraph
compForce = composeG . force
        
-- |allCompFs g produces a list of all forced compositions starting from g up to but excluding the empty graph
allCompFs:: Tgraph -> [Tgraph]
allCompFs g = takeWhile (not . nullGraph) $ iterate compForce g

-- |allComps g produces a list of all compositions starting from g up to but excluding the empty graph.
-- This is not safe in general as it can fail by producing
-- a non-connected graph or graph with crossing boundaries.
allComps:: Tgraph -> [Tgraph]
allComps g = takeWhile (not . nullGraph) $ iterate composeG g


-- |maxCompose and maxFCompose produce a maximal graph.
maxCompose, maxFCompose:: Tgraph -> Tgraph
maxCompose g = last $ allComps g
maxFCompose g = force $ last $ allCompFs g

-- | produces an infinite list of forced decompositions
forceDecomps:: Tgraph -> [Tgraph]
forceDecomps = iterate (force . decomposeG)

-- |remove halftile faces that do not have their matching half tile
removeIncompleteTiles:: Tgraph -> Tgraph
removeIncompleteTiles g = removeFaces halfTiles g
       where bdry = makeBoundary g
             halfTiles = fmap snd $ incompleteHalves bdry $ bDedges bdry







 