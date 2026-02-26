{-|
Module      : Tgraph.Compose
Description : A compose operation for Tgraphs
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module includes the main composition operations compose, partCompose,
tryPartCompose and the more efficient versions composeF, partComposeF (for explicitly forced Tgraphs).
It also exposes 
auxiliary functions
tryGetDartWingInfo, getDartWingInfoForced (and type DartWingInfo)
for debugging but getDartWingInfoForced is no longer used internally.
-}
-- {-# LANGUAGE Strict                #-} 
{-# OPTIONS_GHC -Wno-deprecations  #-}

module Tgraph.Compose 
  ( -- * Composing forced Tgraphs 
    composeF
  , partComposeF
    -- * General compose operations 
  , compose
  , partCompose
  , tryPartCompose
  , tryPartComposeFaces
  -- * Exported auxiliary functions (and type)
  , partComposeDWI
  , partComposeFaces
  , DartWingInfo(..)
  -- , getDWIassumeF
  -- , getDartWingInfo
  , tryGetDartWingInfo
  , getDartWingInfoForced
  ) where

import Data.List (find,(\\),partition,nub)
import Prelude hiding (Foldable(..))
import Data.Foldable (Foldable(..))
import qualified Data.IntMap.Strict as VMap (lookup,(!),alter,empty,elems,keys)
import Data.Maybe (catMaybes,mapMaybe)
import qualified Data.IntSet as IntSet (empty,insert,toList,member)

import Tgraph.Prelude
import Tgraph.Force ( Forced(), forgetF, labelAsForced, tryForceF )
{-------------------------------------------------------------------------
***************************************************************************              
COMPOSING compose, partCompose, tryPartCompose, ...
***************************************************************************
---------------------------------------------------------------------------}

-- |The main compose (partial) function which simply drops the remainder faces from partCompose to return just
-- the composed Tgraph.  It will raise an error if the result is not a valid Tgraph
-- (i.e. if it fails the connectedness, no crossing boundary check).
-- It does not assume the given Tgraph is forced.
-- It can raise an error if the Tgraph is found to be incorrect (when getting dartwing info).
compose:: Tgraph -> Tgraph
compose = snd . partCompose

-- |partCompose g is a partial function producing a pair consisting of remainder faces (faces from g which will not compose) 
-- and a composed Tgraph. 
-- It checks the composed Tgraph for connectedness and no crossing boundaries
-- raising an error if this check fails.
-- It does not assume the given Tgraph is forced.
-- It can raise an error if the Tgraph is found to be incorrect (when getting dartwing info).
partCompose:: Tgraph -> ([TileFace],Tgraph)
partCompose g = runTry $ onFail "partCompose:\n" $ tryPartCompose g

-- |tryPartCompose g tries to produce a Tgraph by composing faces which uniquely compose in g,
-- It uses tryGetDartWingInfo g which can fail if g is found to be incorrect when forced.
-- It checks the resulting new faces for connectedness and no crossing boundaries.
-- If both the above succeed, the result is Right (remainder, g')
-- where g' is the composed Tgraph and remainder is a list
-- of faces from g which will not compose. 
tryPartCompose:: Tgraph -> Try ([TileFace],Tgraph)
tryPartCompose g = 
  do (remainder,newFaces) <- tryPartComposeFaces g
     checked <- onFail "tryPartCompose:\n" $ tryConnectedNoCross newFaces
     return (remainder,checked)

-- |Get the remainder and composed faces (without checking the composed faces make a valid Tgraph)
-- It uses tryGetDartWingInfo g which can fail if g is found to be incorrect when forced.
tryPartComposeFaces:: Tgraph -> Try ([TileFace],[TileFace])
tryPartComposeFaces g = 
  do dwInfo <- tryGetDartWingInfo g 
     return $ partComposeFaces dwInfo
-- tryPartComposeFaces is used in an example showing failure of the connected, no crossing boundary check.


-- |partComposeF fg - produces a pair consisting of remainder faces (faces from fg which will not compose) 
-- and a composed (Forced) Tgraph.
-- Since fg is a forced Tgraph it does not need a check for validity of the composed Tgraph.
-- The fact that the function is total and the result is also Forced relies on theorems
-- established for composing.
-- The calculation of remainder faces is also more efficient with a known forced Tgraph.
-- Also dartWingInfo does not need to be calculated for composing a forced Tgraph.
partComposeF:: Forced Tgraph -> ([TileFace], Forced Tgraph)
partComposeF fg = (remainder, labelAsForced $ makeUncheckedTgraph newfaces) where
  -- !evalnewfaces = evalFaces newfaces
  (_,dwFMap,unused) = dartsMapUnused (forgetF fg)
  (remainder,newfaces) = process (VMap.keys dwFMap) (unused,[])
  process [] res = res
  process (w:more) (rems, nfcs) = 
      let fcs = dwFMap  VMap.! w
      in case length fcs of
           -- 8 faces = large dart base, 6 faces = lrge kite centre, 3 faces = unknown on boundary
           8 -> process more (rems, catMaybes [largeRD fcs, largeLD fcs] ++ nfcs)
           6 -> process more (rems, catMaybes [largeRK fcs, largeLK fcs] ++ nfcs)
           3 -> process more (fcs++rems, nfcs)
           other -> error $ 
                     "partComposeF: Not possible for a forced Tgraph\n" ++
                     "Number of faces should be 8,6,or 3 but found " ++ show other ++
                     "\nat dart wing vertex: " ++ show w ++ "\n"
  largeRD fcs = do rd <- find isRD fcs
                   lk <- find ((==oppV rd) . wingV) fcs
                   let f = evalFace $ makeRD (originV lk) (originV rd) (wingV rd)
                   return f
  largeLD fcs = do ld <- find isLD fcs
                   rk <- find ((==oppV ld) . wingV) fcs
                   let f = evalFace $ makeLD (originV rk) (wingV ld) (originV ld)
                   return f
  largeRK fcs = do rd  <- find isRD fcs
                   lk <- find ((==oppV rd) . wingV) fcs
                   rk <- find (matchingJoinE lk) fcs
                   let f = evalFace $ makeRK (originV rd) (wingV rk) (originV lk)
                   return f
  largeLK fcs = do ld  <- find isLD fcs
                   rk <- find ((==oppV ld) . wingV) fcs
                   lk <- find (matchingJoinE rk) fcs
                   let f = evalFace $ makeLK (originV ld) (originV rk) (wingV lk)
                   return f


-- |composeF - produces a composed Forced Tgraph from a Forced Tgraph.
-- Since the argument is a forced Tgraph it does not need a check for validity of the composed Tgraph.
-- The fact that the function is total and the result is also Forced relies on theorems
-- established for composing.
composeF:: Forced Tgraph -> Forced Tgraph
composeF = snd . partComposeF

-- |DartWingInfo is a record type for the result of classifying dart wings in a Tgraph.
-- Faces at a largeKiteCentre vertex will form kite faces when composed.
-- Faces at a largeDartBase vertex will form dart faces when composed.
-- Faces at an unknown vertex cannot be composed.
-- The record includes a faceMap from dart wings to faces at that vertex.
-- and a list of any faces (necessarily kites) not included in the faceMap (unMapped)
data DartWingInfo =  DartWingInfo 
     { largeKiteCentres  :: [Vertex] -- ^ dart wing vertices classified as large kite centres.
     , largeDartBases  :: [Vertex]  -- ^ dart wing vertices classified as large dart bases.
     , unknowns :: [Vertex] -- ^ unclassified (boundary) dart wing vertices.
     , faceMap :: VertexMap [TileFace] -- ^ a mapping from dart wing vertices to faces at the vertex.
     , unMapped :: [TileFace] -- ^ any faces not at a dart wing vertex (necessarily kites)
     } deriving Show

-- |Recover a list of faces (no repetitions) contained in the dart wing info.
-- (These should be all faces of the Tgraph used to make the dart wing info.)
recoverFaces :: DartWingInfo -> [TileFace]
recoverFaces dwInfo =  nub $ concat (unMapped dwInfo : VMap.elems (faceMap dwInfo))


-- |The given Tgraph is not assumed to be forced.
-- Getting the dart wing information makes use of the forced version
-- as well as the Tgraph so this uses tryForce first which can fail if
-- the Tgraph is found to be incorrect.
tryGetDartWingInfo :: Tgraph -> Try DartWingInfo
tryGetDartWingInfo g =
    do fg <- onFail "tryGetDartWingInfo: incorrect Tgraph found.\n" $ tryForceF g
       return $ getDWIassumeF False g fg

-- | getDartWingInfoForced fg (fg an explicitly Forced Tgraph) classifies the dart wings in fg
-- and calculates a faceMap for each dart wing, returning as DartWingInfo.
-- The classification is much simplified knowing that the Tgraph is forced.
getDartWingInfoForced :: Forced Tgraph -> DartWingInfo
getDartWingInfoForced fg = getDWIassumeF True (forgetF fg) fg

-- | getDWIassumeF (not exported but used to define 2 cases getDartWingInfoForced and tryGetDartWingInfo).
-- getDWIassumeF isForced g fg (where fg is forceF g), classifies the dart wings in g and calculates a faceMap for each dart wing,
-- returning as DartWingInfo. The boolean isForced is used to decide if g can be assumed to be forced.
-- When this is True, the classification is simpler and does not use fg.
getDWIassumeF:: Bool -> Tgraph -> Forced Tgraph -> DartWingInfo
getDWIassumeF isForced g fg =  
  DartWingInfo { largeKiteCentres = IntSet.toList allKcs
               , largeDartBases = IntSet.toList allDbs
               , unknowns = IntSet.toList allUnks
               , faceMap = dwFMap
               , unMapped = unused
               } where
  (drts,dwFMap,unused) = dartsMapUnused g
  (allKcs,allDbs,allUnks) = foldl' processD (IntSet.empty, IntSet.empty, IntSet.empty) drts  
-- kcs = kite centres of larger kites,
-- dbs = dart bases of larger darts,
-- unks = unclassified dart wing vertices
-- Uses a triple of IntSets rather than lists
  processD (kcs, dbs, unks) drt =
    let w = wingV drt
        revLongE = reverseD (longE drt)
    in
        if w `IntSet.member` kcs || w `IntSet.member` dbs then (kcs, dbs, unks) else-- already classified
        let
            fcs = dwFMap VMap.! w -- list of  faces at w
        in
            if w `elem` map originV (filter isKite fcs) then (kcs,IntSet.insert w dbs,unks) else 
                    -- wing is a half kite origin => largeDartBase
 --           if revLongE `elem` map longE (filter isDart fcs) then (IntSet.insert w kcs,dbs,unks) else 
            if any (matchingLongE drt) (filter isDart fcs) then (IntSet.insert w kcs,dbs,unks) else 
                     -- long edge drt shared with another dart => largeKiteCentre
            if isForced then (kcs, dbs, IntSet.insert w unks) else
            let     -- (when not already forced) do same checks but with forced faces 
                ffcs = filter (isAtV w) (faces fg)
            in
                if w `elem` map originV (filter isKite ffcs) then (kcs,IntSet.insert w dbs,unks) else 
                    -- wing is a half kite origin => largeDartBase
                if revLongE `elem` map longE (filter isDart ffcs) then (IntSet.insert w kcs,dbs,unks) else 
                    -- long edge drt shared with another dart => largeKiteCentre
                (kcs,dbs,IntSet.insert w unks) -- on the forced boundary so must be unknown


-- |Not exported - used in partComposeF and in getDWIassumeF.
-- Returns a triple of:
--   list of all half-darts,
--   a dart wing to faces map, and 
--   left over faces (not at a dartwing)
dartsMapUnused :: Tgraph -> ([TileFace], VertexMap [TileFace],[TileFace])
dartsMapUnused g = (drts,dwFMap,unused) where
    (drts,kts) = partition isDart (faces g)
  -- special case of vertexFacesMap for dart wings only
  -- using only relevant vertices where there is a dart wing.
  -- i.e only wings for darts and only oppVs and originVs for kites.
  -- The map is built first from darts, then kites are added.
    dartWMap = foldl' insertD VMap.empty drts
                    -- maps all dart wing vertices to 1 or 2 half darts.
    (dwFMap,unused) = foldl' insertK (dartWMap,[]) kts 
                    -- all kite halves added to relevant dart wings of the dart wing map.
                    -- the unused list records half kites not added to any dart wing.
    insertD vmap f = VMap.alter (addD f) (wingV f) vmap
    addD f Nothing = Just [f]
    addD f (Just fs) = Just (f:fs)
    insertK (vmap,unsd) f = 
      let opp = oppV f
          org = originV f  -- cannot have a kite wingV at a dart originV
          -- kite origin cases not needed for forced Tgraph, but included for completeness of the map
      in  case (VMap.lookup opp vmap, VMap.lookup org vmap) of
            (Just _ ,Just _)     ->  (VMap.alter (addK f) opp $ VMap.alter (addK f) org vmap, unsd)
            (Just _ , Nothing)   ->  (VMap.alter (addK f) opp vmap, unsd)
            (Nothing, Just _ )   ->  (VMap.alter (addK f) org vmap, unsd)
            (Nothing, Nothing)   ->  (vmap, f:unsd) -- kite face not at any dart wing

    addK _ Nothing = Nothing  -- not added to map if it is not a dart wing vertex
    addK f (Just fs) = Just (f:fs)

{-# DEPRECATED partComposeFaces "Use partComposeDWI" #-}
-- |partComposeFaces has been renamed as partComposeDWI.
partComposeFaces :: DartWingInfo -> ([TileFace],[TileFace])
partComposeFaces = partComposeDWI --dwInfo = (remainder, evalFaces newFaces) where
 
-- |partComposeDWI constructs a pair of (remainder,composedfaces) from dart wing information (DWI).
-- This is used in defining tryPartComposeFaces but also exported
-- for use in the composeK example in Extras.
-- It does not assume the dart wing info has come from a forced Tgraph
-- so the resulting composed faces may not form a valid Tgraph.
partComposeDWI :: DartWingInfo -> ([TileFace],[TileFace])
partComposeDWI dwInfo = (remainder, evalFaces newFaces) where
    remainder = recoverFaces dwInfo \\ concatMap concat [groupRDs, groupLDs, groupRKs, groupLKs]
     -- all faces except those successfully used in making composed faces.   
    newFaces = newRDs ++ newLDs ++ newRKs ++ newLKs

    newRDs = map makenewRD groupRDs 
    groupRDs = mapMaybe groupRD (largeDartBases dwInfo)
    makenewRD [rd,lk] = makeRD (originV lk) (originV rd) (oppV lk) 
    makenewRD _       = error "partComposeFaces: RD case"
    groupRD v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    rd <- find isRD fcs
                    lk <- find (matchingShortE rd) fcs
                    return [rd,lk]

    newLDs = map makenewLD groupLDs 
    groupLDs = mapMaybe groupLD (largeDartBases dwInfo) 
    makenewLD [ld,rk] = makeLD (originV rk) (oppV rk) (originV ld)
    makenewLD _       = error "partComposeFaces: LD case"
    groupLD v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    ld <- find isLD fcs
                    rk <- find (matchingShortE ld) fcs
                    return [ld,rk]

    newRKs = map makenewRK groupRKs 
    groupRKs = mapMaybe groupRK (largeKiteCentres dwInfo) 
    makenewRK [rd,_,rk] = makeRK (originV rd) (wingV rk) (originV rk)
    makenewRK _         = error "cpartComposeFaces: RK case"
    groupRK v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    rd <- find isRD fcs
                    lk <- find (matchingShortE rd) fcs
                    rk <- find (matchingJoinE lk) fcs
                    return [rd,lk,rk]

    newLKs = map makenewLK groupLKs 
    groupLKs = mapMaybe groupLK (largeKiteCentres dwInfo) 
    makenewLK [ld,_,lk] = makeLK (originV ld) (originV lk) (wingV lk)
    makenewLK _         = error "partComposeFaces: LK case"
    groupLK v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    ld <- find isLD fcs
                    rk <- find (matchingShortE ld) fcs
                    lk <- find (matchingJoinE rk) fcs
                    return [ld,rk,lk]

