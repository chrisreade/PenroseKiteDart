module Tgraphs ( module Tgraphs
               , module Tgraph.Prelude
               , module Tgraph.Decompose
               , module Tgraph.Compose
               , module Tgraph.Force
               ) where
-- exports all the Tgraph Modules

import Data.List (intersect)

import Tgraph.Prelude
import Tgraph.Decompose
import Tgraph.Compose
import Tgraph.Force

{----------------------------
********************************************
EXPERIMENTAL BITS
********************************************
------------------------------}

{----------------------------
EMPLACEMENTS
------------------------------}

-- | emplace does maximal composing with force and composeG, 
-- then applies decomposeG and force repeatedly back to the starting level.
-- It produces the 'emplacement' of influence of the argument graph.   
emplace:: Tgraph -> Tgraph
emplace g = if nullGraph g'
            then fg 
            else (force . decomposeG . emplace) g'
    where fg = force g
          g' = composeG fg 
            
-- emplacements is best supplied with a maximally composed or near maximally composed graph
-- It produces an infinite list of emplacements of the starting graph and its decompositions.
emplacements :: Tgraph -> [Tgraph]
emplacements = iterate (force . decomposeG) . emplace -- was .force

{-
-- countEmplace g finds a maximally composed graph (maxg) for g and counts the number (n) of compsitions
-- needed.  It returns a triple of maxg, the nth emplacement of maxg, and n)
countEmplace :: Tgraph -> (Tgraph,Tgraph,Int)
countEmplace g = (maxg, emplacements maxg !! n, n) where (maxg,n) = maxFCompose g
-}



{-------------------------------------------------------------------------
 makeChoices, emplaceChoices
------------------------------------------------------------------------------}

-- | a version of emplace using makeChoices at the top level.
-- after makeChoices we use emplace to attempt further compositions but with no further choices
emplaceChoices:: Tgraph -> [Tgraph]
emplaceChoices g = 
       let fg = force g
           g' = composeG fg 
       in
           if nullGraph g'
           then emplace <$> makeChoices g
           else force . decomposeG <$> emplaceChoices g'
                                 
{- | makeChoices is a temporary tool which does not attempt to analyse choices for correctness.
It can thus create some choices which will be incorrect.
The unknowns returned from classifyDartWings can become largeKiteCentres or largeDartBases.
This produces 2^n choices where n is the number of unknowns (excluding lone dart wing tips with valencyD 2).
-}
makeChoices :: Tgraph -> [Tgraph]
makeChoices g = choices unks [g] where
    unks = filter ((>2).valencyD g) (unknowns (classifyDartWings g))
    choices [] gs = gs
    choices (v:more) gs = choices more (fmap (forceLKC v) gs ++ fmap (forceLDB v) gs)              



{-------------------------------------------------------------------------
 compForce, allFComps, allComps, maxCompose, maxFCompose
------------------------------------------------------------------------------}

-- | compForce does a force then composeG but it
-- by-passes the check on the composed graph because it is forced
compForce:: Tgraph -> Tgraph
compForce = snd . partCompose . force
    
    
-- allFComps g produces a list of all forced compositions starting from g up to but excluding the empty graph
allFComps:: Tgraph -> [Tgraph]
allFComps g = takeWhile (not . nullGraph) $ iterate compForce g

-- | allComps g produces a list of all compositions starting from g up to but excluding the empty graph
-- This is not safe in general
allComps:: Tgraph -> [Tgraph]
allComps g = takeWhile (not . nullGraph) $ iterate composeG g


-- maxCompose and maxFCompose produce a maximal graph.
maxCompose, maxFCompose:: Tgraph -> Tgraph
maxCompose g = last $ allComps g
maxFCompose g = last $ allFComps g

-- | remove haftile faces that do not have their matching half tile
removeIncompleteTiles:: Tgraph -> Tgraph
removeIncompleteTiles g = removeFaces halfTiles g
       where bdry = makeBoundary g
             halfTiles = fmap snd $ incompleteHalves bdry $ bDedges bdry
 



