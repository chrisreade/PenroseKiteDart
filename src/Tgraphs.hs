{-|
Module      : Tgraphs
Description : Collects and exports the various Tgraph modules 
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This is the main module for Tgraph operations which collects and exports the other Tgraph modules. 
It exports makeTgraph for constructing checked Tgraphs and excludes data constructor Tgraph.
It also includes a definition of emplace and other experimental combinations.
-}
module Tgraphs ( module Tgraphs
               , module Tgraph.Prelude -- excludes data constructor Tgraph
               , module Tgraph.Decompose
               , module Tgraph.Compose
               , module Tgraph.Force
               , module Tgraph.Convert
               , module Tgraph.Relabelling
               ) where

import Tgraph.Prelude hiding (Tgraph(Tgraph)) -- hides Tgraph as type and data constructor
import Tgraph.Prelude (Tgraph) -- re-includes Tgraph as type constructor only
import Tgraph.Decompose
import Tgraph.Compose
import Tgraph.Force
import Tgraph.Convert
import Tgraph.Relabelling

{- *
Making valid Tgraphs (checked for no touching vertices).
-}


{-|
makeTgraph performs a touching vertex check as well as using checkedTgraph for other required properties.
It produces an error if either check fails.
Note that the other Tgraph properties are checked first, to ensure that calculation of 
vertex locations can be done for a touching vertex check.
-}
makeTgraph :: [TileFace] -> Tgraph
makeTgraph fcs = getResult $ onFail "makeTgraph: (failed):\n" $ touchCheckProps fcs

{-|
touchCheckProps performs the same checks for Tgraph properties as checkedTgraph but in addition
it also checks that there are no touching vertices (distinct labels for the same vertex)
using Tgraph.Convert.touchingVertices (which calculates vertex locations).
It produces Left ... if either check fails and Right g otherwise where g is the Tgraph.
Note that the other Tgraph properties are checked first, to ensure that calculation of 
vertex locations can be done.
-}
touchCheckProps :: [TileFace] -> ReportFail Tgraph
touchCheckProps fcs =
 do g <- checkTgraphProps fcs
    let touchVs = touchingVertices (faces g)
    if null touchVs 
    then Right g 
    else Left ("Found touching vertices: " 
               ++ show touchVs
               ++ "\n(To fix, use: correctTouchingVs)\n"
              )

{- *
Used in drawing Tgraphs.
-}

-- |select the halftile faces of a Tgraph with a join edge on the boundary 
boundaryJoinFaces :: Tgraph -> [TileFace]
boundaryJoinFaces g = fmap snd $ incompleteHalves bdry $ bDedges bdry where
    bdry = makeBoundary g


{- *
Combining force, composeG, decomposeG
-}

-- |compForced does a force then composeG. 
-- (the connectedNoCross check may be redundant on the composed graph because the argument was forced.)
compForced:: Tgraph -> Tgraph
compForced = composeG . force

-- |force after a decomposition
forcedDecomp:: Tgraph -> Tgraph
forcedDecomp = force . decomposeG
        
-- |allCompForced g produces a list of all forced compositions starting from g up to but excluding the empty graph
allCompForced:: Tgraph -> [Tgraph]
allCompForced g = takeWhile (not . nullGraph) $ iterate compForced g

-- |allCompositions g produces a list of all compositions starting from g up to but excluding the empty graph.
-- This is not safe in general as it can fail by producing
-- a non-connected graph or graph with crossing boundaries.
allCompositions:: Tgraph -> [Tgraph]
allCompositions g = takeWhile (not . nullGraph) $ iterate composeG g

-- | produces an infinite list of forced decompositions
allForcedDecomps:: Tgraph -> [Tgraph]
allForcedDecomps = iterate forcedDecomp

-- |maxCompose and maxFCompose produce a maximal graph.
maxCompose, maxFCompose:: Tgraph -> Tgraph
maxCompose g = last $ allCompositions g
maxFCompose g = force $ last $ allCompForced g

{- *
Emplacements
-}

-- |emplace does maximal composing with force and composeG, 
-- then applies decomposeG and force repeatedly back to the starting level.
-- It produces the emplacement of influence of the argument graph.   
emplace:: Tgraph -> Tgraph
emplace g | nullGraph g' = fg
          | otherwise = (forcedDecomp . emplace) g'
  where fg = force g
        g' = composeG fg 
            
-- |emplacements is best supplied with a maximally composed or near maximally composed graph
-- It produces an infinite list of emplacements of the starting graph and its decompositions.
emplacements :: Tgraph -> [Tgraph]
emplacements = iterate forcedDecomp . emplace -- was .force

-- |a version of emplace using makeChoices at the top level.
-- after makeChoices we use emplace to attempt further compositions but with no further choices
emplaceChoices:: Tgraph -> [Tgraph]
emplaceChoices g | nullGraph g' = emplace <$> makeChoices fg
                 | otherwise = forcedDecomp <$> emplaceChoices g'
  where fg = force g
        g' = composeG fg 
                                 
{-| makeChoices should only be used on a forced Tgraph.
It is a temporary tool which does not attempt to analyse choices for correctness.
It can thus create some choices which will be incorrect.
The unknowns returned from classifyDartWings can become largeKiteCentres or largeDartBases.
This produces 2^n choices where n is the number of unknowns.
(There will not be any dart wing tips with valency 2 in a forced graph).
-}
makeChoices :: Tgraph -> [Tgraph]
makeChoices g = choices unks [g] where
    unks = unknowns (classifyDartWings g) -- g not forced may allow solitary wing tips which will fail
    choices [] gs = gs
    choices (v:more) gs = choices more (fmap (forceLKC v) gs ++ fmap (forceLDB v) gs)







 