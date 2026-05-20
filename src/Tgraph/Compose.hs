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
{-# LANGUAGE Strict                #-} 
{-# OPTIONS_GHC -Wno-deprecations  #-}

module Tgraph.Compose 
  ( -- * Composing forced Tgraphs 
    composeF
  , partComposeF
    -- * General compose operations 
  , compose
  , partCompose
  , quickCompose
  , quickPartCompose
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

import Data.List (find,(\\),partition)
import Prelude hiding (Foldable(..))
import Data.Foldable (Foldable(..))
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as VMap (lookup,(!),alter,empty,keys)
import Data.Maybe (catMaybes, fromMaybe)
import Data.IntSet(IntSet)
import qualified Data.IntSet as IntSet (empty,insert,toList)
import Tgraph.Prelude
import Tgraph.Force ( Forced(), forgetF, labelAsForced, tryForce )
{-------------------------------------------------------------------------
***************************************************************************              
COMPOSING compose, partCompose, tryPartCompose, ...
***************************************************************************
---------------------------------------------------------------------------}

-- |The main compose (partial) function which simply drops the remainder faces from partCompose to return just
-- the composed Tgraph.  It will raise an error if the result is not a valid Tgraph
-- (i.e. if it fails the connectedness, no crossing boundary check).
-- It does not assume the given Tgraph is forced.
-- It inspects the forced version to classify dart wings (hence costing a force operation).
-- It can raise an error if the Tgraph is found to be incorrect when calculating the forced version).
-- (See also composeF and compForce.)
compose:: HasGraph a => a -> Tgraph
compose = snd . partCompose

-- |partCompose g is a partial function producing a pair consisting of remainder faces (faces from g which will not compose) 
-- and a composed Tgraph. 
-- It checks the composed Tgraph for connectedness and no crossing boundaries
-- raising an error if this check fails.
-- It inspects the forced version to classify dart wings (hence costing a force operation).
-- It can raise an error if the Tgraph is found to be incorrect when calculating the forced version).
-- (See also quickPartCompose)
partCompose:: HasGraph a => a -> ([TileFace],Tgraph)
partCompose = runTry . onFail "partCompose:\n" . tryPartCompose . recoverGraph

-- |tryPartCompose g tries to produce a Tgraph by composing faces which uniquely compose in g,
-- It uses tryGetDartWingInfo g which can fail if g is found to be incorrect when forced.
-- It checks the resulting new faces for connectedness and no crossing boundaries.
-- If both the above succeed, the result is Right (remainder, g')
-- where g' is the composed Tgraph and remainder is a list
-- of faces from g which will not compose. 
tryPartCompose:: HasGraph a => a -> Try ([TileFace],Tgraph)
tryPartCompose g = 
  do (remainder,newFaces) <- tryPartComposeFaces g
     checked <- onFail "tryPartCompose:\n" $ tryConnectedNoCross newFaces
     return (remainder,checked)

-- |Get the remainder and composed faces (without checking the composed faces make a valid Tgraph)
-- It uses tryGetDartWingInfo g which can fail if g is found to be incorrect when forced.
tryPartComposeFaces:: HasGraph a => a -> Try ([TileFace],[TileFace])
tryPartComposeFaces g = 
  do dwInfo <- tryGetDartWingInfo g 
     return $ partComposeDWI dwInfo
-- tryPartComposeFaces is used in an example showing failure of the connected, no crossing boundary check.


-- |partComposeF fg - produces a pair consisting of remainder faces (faces from fg which will not compose) 
-- and a composed (Forced) Tgraph.
-- Since fg is a forced Tgraph it does not need a check for validity (connected and no crossing boundaries) of the composed Tgraph.
-- The fact that the function is total and the result is also Forced relies on theorems
-- established for composing.
-- The calculation of remainder faces is also more efficient with a known forced Tgraph.
-- Also dartWingInfo does not need to be calculated for composing a forced Tgraph.
partComposeF:: HasGraph a => Forced a -> ([TileFace], Forced Tgraph)
partComposeF fg = (remainder, labelAsForced $ makeUncheckedTgraph evalnewfaces) where
  !evalnewfaces = evalFaces newfaces
  (_,dwFMap,unused) = dartsMapUnused (recoverGraph fg)
  (remainder,newfaces) = foldl' checkDW (unused,[]) (VMap.keys dwFMap)
  checkDW (rems, nfcs) w = 
     let fcs = dwFMap  VMap.! w
     in case length fcs of
          -- 8 faces = large dart base
          8 -> (rems, catMaybes [largeRD fcs, largeLD fcs] ++ nfcs)
          -- 6 faces = lrge kite centre
          6 -> (rems, catMaybes [largeRK fcs, largeLK fcs] ++ nfcs)
          -- 3 faces = unknown on boundary
          3 -> (fcs++rems, nfcs)
          other -> error $ 
                    "partComposeF: Not possible for a forced Tgraph\n" ++
                    "Number of faces should be 8,6,or 3 but found " ++ show other ++
                    "\nat dart wing vertex: " ++ show w ++ "\n"

  largeRD fcs = do rd <- find isRD fcs
                   lk <- find ((==oppV rd) . wingV) fcs
                   return $ RD (originV lk, originV rd, wingV rd)
  largeLD fcs = do ld <- find isLD fcs
                   rk <- find ((==oppV ld) . wingV) fcs
                   return $ LD (originV rk, wingV ld, originV ld)
  largeRK fcs = do rd  <- find isRD fcs
                   lk <- find ((==oppV rd) . wingV) fcs
                   rk <- find (matchingJoinE lk) fcs
                   return $ RK (originV rd, wingV rk, originV lk)
  largeLK fcs = do ld  <- find isLD fcs
                   rk <- find ((==oppV ld) . wingV) fcs
                   lk <- find (matchingJoinE rk) fcs
                   return $ LK (originV ld, originV rk, wingV lk)


-- |composeF - produces a composed Forced Tgraph from a Forced Tgraph.
-- Since the argument is a forced Tgraph it does not need a check for validity of the composed Tgraph.
-- The fact that the function is total and the result is also Forced relies on theorems
-- established for composing.
composeF:: HasGraph a => Forced a -> Forced Tgraph
composeF = snd . partComposeF

-- |DartWingInfo is a record type for the result of classifying dart wings in a Tgraph.
-- Faces at a largeKiteCentre vertex will form kite faces when composed.
-- Faces at a largeDartBase vertex will form dart faces when composed (excludin kites with origin at the largeDartBase).
-- Faces at an unknown vertex cannot be composed.
-- The record includes a faceMap from dart wings to faces at that vertex.
-- A list (unMapped) which includes any kites not in the faceMap and those only in the faceMap at the kite origin.
--
-- NB. Kites that only have a dart wing at their origin, are added to the map for dart wing classification purposes
-- but not used when composing at largeDartBases, hence they need to be recorded as unMapped as well.
-- Such kites cannot exist in a forced Tgraph, so this only arises when composing unforced Tgraphs.
data DartWingInfo =  DartWingInfo 
     { largeKiteCentres  :: [Vertex] -- ^ dart wing vertices classified as large kite centres.
     , largeDartBases  :: [Vertex]  -- ^ dart wing vertices classified as large dart bases.
     , unknowns :: [Vertex] -- ^ unclassified (boundary) dart wing vertices.
     , faceMap :: VertexMap [TileFace] -- ^ a mapping from dart wing vertices to faces at the vertex.
     , unMapped :: [TileFace] -- ^ any kites whose oppV is not at a dart wing vertex.
     } deriving Show

{- -- |Recover a list of faces (no repetitions) contained in the dart wing info.
-- (These should be all faces of the Tgraph used to make the dart wing info.)
recoverFaces :: DartWingInfo -> [TileFace]
recoverFaces dwInfo =  nub $ concat (unMapped dwInfo : VMap.elems (faceMap dwInfo))
 -}

-- | Not exported. (Used by tryGetDartWingInfo and getDartWingInfoForced.)
-- Requires a dart wing IntMap which must be from a forced Tgraph
-- Returns a triple of (largeKiteCentres, largeDartBases, Unknowns)
classifyDartWings :: IntMap [TileFace]-> [Vertex] -> (IntSet,IntSet,IntSet)
classifyDartWings dwMap = foldl' classifyDartWing (IntSet.empty, IntSet.empty, IntSet.empty) where
-- kcs = kite centres of larger kites,
-- dbs = dart bases of larger darts,
-- unks = unclassified dart wing vertices
-- Uses a triple of IntSets rather than lists
-- Uses the forced version to classify dart wings from the original 
  classifyDartWing (kcs, dbs, unks) w =
    let fcs = dwMap VMap.! w
    in case length fcs of
        -- 8 faces = large dart base
        8 -> (kcs,IntSet.insert w dbs,unks)
        -- 6 faces = lrge kite centre
        6 -> (IntSet.insert w kcs,dbs,unks)
        -- 3 faces = unknown on boundary
        3 -> (kcs,dbs,IntSet.insert w unks)
        other -> error $ 
                 "classifyDartWings: Not possible for forced version of Tgraph\n" ++
                 "(called by either tryGetDartWingInfo or getDartWingInfoForced).\n " ++
                 "Number of faces should be 8,6,or 3 in forced version of Tgraph but found " ++ show other ++
                 "\nat dart wing vertex: " ++ show w ++ "\n"

-- |The given Tgraph is not assumed to be forced.
-- Getting the dart wing information makes use of the forced version
-- as well as the Tgraph so this uses tryForce first which can fail if
-- the Tgraph is found to be incorrect.
tryGetDartWingInfo :: HasGraph a => a -> Try DartWingInfo
tryGetDartWingInfo a =
  do let g = recoverGraph a
         (drts,dwFMap,unused) = dartsMapUnused g 
     fg <- onFail "tryGetDartWingInfo: incorrect Tgraph (found during forcing).\n" $ tryForce g
     let (_,~dwFMapForced,_) = dartsMapUnused fg
         -- forced version is used for classifying (but darts from original graph)
         (allKcs,allDbs,allUnks) = classifyDartWings dwFMapForced (map wingV drts)  
     return $ DartWingInfo 
               { largeKiteCentres = IntSet.toList allKcs
               , largeDartBases = IntSet.toList allDbs
               , unknowns = IntSet.toList allUnks
               , faceMap = dwFMap -- original map (not forced version)
               , unMapped = unused -- from original Tgraph
               }


{- tryGetDartWingInfo :: HasGraph a => a -> Try DartWingInfo
tryGetDartWingInfo a =
    do let g = recoverGraph a
       fg <- onFail "tryGetDartWingInfo: incorrect Tgraph found.\n" $ tryForceF g
       return $ getDWIassumeF False g fg
 -}
-- | getDartWingInfoForced fg (fg an explicitly Forced Tgraph) classifies the dart wings in fg
-- and calculates a faceMap for each dart wing, returning as DartWingInfo.
-- The classification is much simplified knowing that the Tgraph is forced.
getDartWingInfoForced:: Forced Tgraph -> DartWingInfo
getDartWingInfoForced fg =  
  DartWingInfo { largeKiteCentres = IntSet.toList allKcs
               , largeDartBases = IntSet.toList allDbs
               , unknowns = IntSet.toList allUnks
               , faceMap = dwFMap
               , unMapped = unused
               } where
  (drts,dwFMap,unused) = dartsMapUnused (forgetF fg)
  (allKcs,allDbs,allUnks) = classifyDartWings dwFMap (map wingV drts)  

-- | A faster version of compose which may underestimate the composable faces.
-- It does not use force to check classification of dart wings, so some dartwings may be
-- incorrectly classified as unknown (leaving fewer composed faces).
-- It checks the resulting Tgraph for connected and no crossing boundaries, so
-- it can raise an error if this check fails.
-- It will always be correct on a forced Tgraph.
quickCompose :: HasGraph a => a -> Tgraph
quickCompose a = snd (quickPartCompose a)

-- | A faster version of partCompose which may underestimate the composable faces.
-- It does not use force to check classification of dart wings, so some dartwings may be
-- incorrectly classified as unknown (leaving more remainder faces and fewer composed faces).
-- It checks the resulting Tgraph for connected and no crossing boundaries, so
-- it can raise an error if this check fails.
-- It will always be correct on a forced Tgraph.
quickPartCompose:: HasGraph a => a -> ([TileFace], Tgraph)
quickPartCompose a = (remainder, checked) where
  checked = runTry $ onFail "quickPartCompose:\n" $ tryConnectedNoCross newfaces
  (_,dwFMap,unused) = dartsMapUnused (recoverGraph a)
  (remainder,newfaces) = foldl' checkDW (unused,[]) (VMap.keys dwFMap)
  checkDW (rems, nfcs) w = 
     let fcs = dwFMap  VMap.! w
     in  if w `elem` map originV (filter isKite fcs) 
                    -- dart wing is also a half kite origin => largeDartBase
         then (rems, catMaybes [largeRD fcs, largeLD fcs] ++ nfcs) 
         else case darts fcs of
            [d1,d2] -> if matchingLongE d1 d2
                            -- two darts share long edge => largekiteCentre
                       then (rems, catMaybes [largeRK fcs, largeLK fcs] ++ nfcs)
                            -- two darts, no matching long edge => largeDartBase
                       else (rems, catMaybes [largeRD fcs, largeLD fcs] ++ nfcs)
                  -- otherwise unknown (add faces to remainder faces)
            _ -> (fcs++rems, nfcs)

  largeRD fcs = do rd <- find isRD fcs
                   lk <- find ((==oppV rd) . wingV) fcs
                   return $ RD (originV lk, originV rd, wingV rd)
  largeLD fcs = do ld <- find isLD fcs
                   rk <- find ((==oppV ld) . wingV) fcs
                   return $ LD (originV rk, wingV ld, originV ld)
  largeRK fcs = do rd  <- find isRD fcs
                   lk <- find ((==oppV rd) . wingV) fcs
                   rk <- find (matchingJoinE lk) fcs
                   return $ RK (originV rd, wingV rk, originV lk)
  largeLK fcs = do ld  <- find isLD fcs
                   rk <- find ((==oppV ld) . wingV) fcs
                   lk <- find (matchingJoinE rk) fcs
                   return $ LK (originV ld, originV rk, wingV lk)

{- HISTORICAL keep for info usied in processD

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
  (_,~dwFMapForced,_) = if isForced then ([],dwFMap,[]) else dartsMapUnused (forgetF fg)
  (allKcs,allDbs,allUnks) = foldl' processD (IntSet.empty, IntSet.empty, IntSet.empty) drts  
-- kcs = kite centres of larger kites,
-- dbs = dart bases of larger darts,
-- unks = unclassified dart wing vertices
-- Uses a triple of IntSets rather than lists
  processD (kcs, dbs, unks) drt =
    let w = wingV drt
--        revLongE = reverseD (longE drt)
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
            let     -- (when not already forced) check forced faces 
                ffcs = dwFMapForced VMap.! w --filter (isAtV w) (faces fg)
            in case length ffcs of
                 -- 8 faces = large dart base
                 8 -> (kcs,IntSet.insert w dbs,unks)
                 -- 6 faces = lrge kite centre
                 6 -> (IntSet.insert w kcs,dbs,unks)
                 -- 3 faces = unknown on boundary
                 3 -> (kcs,dbs,IntSet.insert w unks)
                 other -> error $ 
                           "getDWIassumeF: Not possible for a forced Tgraph\n" ++
                           "Number of faces should be 8,6,or 3 in forced version of Tgraph but found " ++ show other ++
                           "\nat dart wing vertex: " ++ show w ++ "\n"
 -}
 
{- |Not exported - used in partComposeF, getDartWingInfoForced and tryGetDartWingInfo.
Returns a triple of:
 list of all half-darts,
 a dart wing to faces map, and 
 unused - kites guaranteed to be remainder in a composition.
-}
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
                    -- the unused list records half kites that will become remainder.
    insertD vmap f = VMap.alter (addD f) (wingV f) vmap
    addD f Nothing = Just [f]
    addD f (Just fs) = Just (f:fs)
    insertK (vmap,unsd) f = 
      let opp = oppV f
          org = originV f  -- cannot have a kite wingV at a dart originV
          -- kite origin cases not needed for forced Tgraph, but included for completeness of the map
      in  case (VMap.lookup opp vmap, VMap.lookup org vmap) of
        -- neither - add to unused 
            (Nothing, Nothing)   ->  (vmap, f:unsd) -- kite face not at any dart wing
       -- both - add to both
            (Just _ ,Just _)     ->  (VMap.alter (addK f) opp $ VMap.alter (addK f) org vmap, unsd)
        -- opp only - add to opp
            (Just _ , Nothing)   ->  (VMap.alter (addK f) opp vmap, unsd)
        -- origin only - add to origin BUT also add to unused.
        -- (Kites with only origin at dart wing cannot form part of a composed face.
        -- This case cannot arise in forced Tgraphs.)
            (Nothing, Just _ )   ->  (VMap.alter (addK f) org vmap, f:unsd) -- kite will not form part of a new face (not possible for forced Tgraph)
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
--
-- This version relies on kites that only have a dart wing at their origin, being included in unMapped.
-- Such kites are also recorded in the dart wing/(kite origin) for classification purposes but then
-- filtered out when composing at a largeDartBase.
partComposeDWI :: DartWingInfo -> ([TileFace],[TileFace])
partComposeDWI dwInfo = (remainder, evalFaces newfaces) where
    ~remainder0 = unMapped dwInfo ++ concatMap facesFor (unknowns dwInfo)
    (~remainder1,newfaces1) = foldl' collectDarts (remainder0,[]) (largeDartBases dwInfo)
    (~remainder,newfaces) = foldl' collectKites (remainder1,newfaces1) (largeKiteCentres dwInfo)
    facesFor v = faceMap dwInfo VMap.! v
    collectDarts :: ([TileFace], [TileFace]) -> Vertex -> ([TileFace], [TileFace])
    collectDarts (rems, newfs) v = (fcs''++rems, newfs'') where
      wanted f = isDart f || originV f /=v -- ignore kites with origins at v
      fcs = filter wanted (facesFor v)
      (newfs' , fcs') = fromMaybe (newfs,fcs) $ groupRD fcs newfs
      (newfs'' , fcs'') = fromMaybe (newfs',fcs') $ groupLD fcs' newfs'
      groupRD fs nfs =
         do rd <- find isRD fs
            lk <- find (matchingShortE rd) fs
            return (RD (originV lk, originV rd, oppV lk):nfs, fs\\[rd,lk])
      groupLD fs nfs = 
         do ld <- find isLD fs
            rk <- find (matchingShortE ld) fs
            return (LD (originV rk, oppV rk, originV ld):nfs, fs\\[ld,rk])
    collectKites :: ([TileFace], [TileFace]) -> Vertex -> ([TileFace], [TileFace])
    collectKites (rems, newfs) v = (fcs''++rems, newfs'') where
      fcs = facesFor v
      (newfs' , fcs') = fromMaybe (newfs,fcs) $ groupRK fcs newfs
      (newfs'' , fcs'') = fromMaybe (newfs',fcs') $ groupLK fcs' newfs'
      groupRK fs nfs =
         do rd <- find isRD fs
            lk <- find (matchingShortE rd) fs
            rk <- find (matchingJoinE lk) fs
            return (RK (originV rd, wingV rk, originV rk):nfs, fs\\[rd,lk,rk])
      groupLK fs nfs = 
          do ld <- find isLD fs
             rk <- find (matchingShortE ld) fs
             lk <- find (matchingJoinE rk) fs
             return (LK (originV ld, originV lk, wingV lk):nfs, fs\\[ld,rk,lk])

{- partComposeDWI :: DartWingInfo -> ([TileFace],[TileFace])
partComposeDWI dwInfo = (remainder, evalFaces newFaces) where
    remainder = nub $ recoverFaces dwInfo \\ concatMap concat [groupRDs, groupLDs, groupRKs, groupLKs]
     -- all faces except those successfully used in making composed faces.   
    newFaces = newRDs ++ newLDs ++ newRKs ++ newLKs

    newRDs = map makenewRD groupRDs 
    groupRDs = mapMaybe groupRD (largeDartBases dwInfo)
    makenewRD [rd,lk] = RD (originV lk, originV rd, oppV lk) 
    makenewRD _       = error "partComposeFaces: RD case"
    groupRD v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    rd <- find isRD fcs
                    lk <- find (matchingShortE rd) fcs
                    return [rd,lk]

    newLDs = map makenewLD groupLDs 
    groupLDs = mapMaybe groupLD (largeDartBases dwInfo) 
    makenewLD [ld,rk] = LD (originV rk, oppV rk, originV ld)
    makenewLD _       = error "partComposeFaces: LD case"
    groupLD v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    ld <- find isLD fcs
                    rk <- find (matchingShortE ld) fcs
                    return [ld,rk]

    newRKs = map makenewRK groupRKs 
    groupRKs = mapMaybe groupRK (largeKiteCentres dwInfo) 
    makenewRK [rd,_,rk] = RK (originV rd, wingV rk, originV rk)
    makenewRK _         = error "cpartComposeFaces: RK case"
    groupRK v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    rd <- find isRD fcs
                    lk <- find (matchingShortE rd) fcs
                    rk <- find (matchingJoinE lk) fcs
                    return [rd,lk,rk]

    newLKs = map makenewLK groupLKs 
    groupLKs = mapMaybe groupLK (largeKiteCentres dwInfo) 
    makenewLK [ld,_,lk] = LK (originV ld, originV lk, wingV lk)
    makenewLK _         = error "partComposeFaces: LK case"
    groupLK v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    ld <- find isLD fcs
                    rk <- find (matchingShortE ld) fcs
                    lk <- find (matchingJoinE rk) fcs
                    return [ld,rk,lk]
 -}
