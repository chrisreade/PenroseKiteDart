{-|
Module      : Tgraph.Compose
Description : A compose operation for Tgraphs
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module includes the main composition operations compose, partCompose,
tryPartCompose and the more efficient versions composeF, partComposeF (for explicitly forced Tgraphs).
It also exposes some auxiliary functions for debugging/experimental purposes.
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
  , tryPartCompose
  , quickCompose
  , quickPartCompose
  -- * Exported auxiliary functions (and type)
  , tryPartComposeFaces
  , partComposeDWI
  , DartWingInfo(..)
  , tryGetDartWingInfo
  , tryGetDartWingInfoLocal
  , getDartWingInfoForced
  ) where

import Data.List (find,(\\),partition)
import Prelude hiding (Foldable(..))
import Data.Foldable (Foldable(..))
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as VMap (lookup,(!),alter,empty,keys,member)
import Data.Maybe (catMaybes, fromMaybe)
import Data.IntSet(IntSet)
import qualified Data.IntSet as IntSet (empty,insert,toList)
import Tgraph.Prelude
import Tgraph.Force ( Forced(), forgetF, labelAsForced, tryForce, tryForceAt )

-- |The general compose (partial) function which simply drops the remainder faces from partCompose to return just
-- the composed Tgraph. 
-- It does not assume the given Tgraph is forced which makes it an expensive operation on large Tgraphs.
-- It inspects the forced version to classify dart wings (hence costing a force operation).
-- It can raise an error if the Tgraph is found to be incorrect when calculating the forced version).
-- It will raise an error if the result is not a valid Tgraph
-- (i.e. if it fails the connectedness, no crossing boundary check at the end).
-- (See also composeF and compForce and quickCompose.)
compose:: HasGraph a => a -> Tgraph
compose = snd . partCompose

-- |partCompose g is a partial function producing a pair consisting of remainder faces (faces from g which will not compose) 
-- and a composed Tgraph.
-- It does not assume the given Tgraph is forced which makes it an expensive operation on large Tgraphs.
-- It inspects the forced version to classify dart wings (hence costing a force operation).
-- It can raise an error if the Tgraph is found to be incorrect when calculating the forced version).
-- It checks the composed faces for connectedness and no crossing boundaries
-- raising an error if this check fails.
-- (See also partComposeF and quickPartCompose.)
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
     return $ partComposeFacesDWI dwInfo
-- tryPartComposeFaces is used in an example showing failure of the connected, no crossing boundary check.


-- |partComposeF fg - produces a pair consisting of remainder faces (faces from fg which will not compose) 
-- and a composed (Forced) Tgraph.
-- Since fg is a forced Tgraph it does not need a check for validity (connected and no crossing boundaries) of the composed Tgraph.
-- The fact that the function is total and the result is also Forced relies on theorems
-- established for composing.
partComposeF:: HasGraph a => Forced a -> ([TileFace], Forced Tgraph)
partComposeF fg = (remainder, labelAsForced $ makeUncheckedTgraph newfaces) where
  -- !evalnewfaces = evalFaces newfaces
  (dwFMap,unused) = dwMapUnused (recoverGraph fg)
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
                   return $ makeRD (originV lk) (originV rd) (wingV rd)
  largeLD fcs = do ld <- find isLD fcs
                   rk <- find ((==oppV ld) . wingV) fcs
                   return $ makeLD (originV rk) (wingV ld) (originV ld)
  largeRK fcs = do rd  <- find isRD fcs
                   lk <- find ((==oppV rd) . wingV) fcs
                   rk <- find (sharedJoinE lk) fcs
                   return $ makeRK (originV rd) (wingV rk) (originV lk)
  largeLK fcs = do ld  <- find isLD fcs
                   rk <- find ((==oppV ld) . wingV) fcs
                   lk <- find (sharedJoinE rk) fcs
                   return $ makeLK (originV ld) (originV rk) (wingV lk)


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
-- A list (remaining) of faces that will necessarily be remainder faces.
-- (this includes kite-halves not in the faceMap and those kite halves only in the faceMap at the kite origin.
--
-- NB. Kites that only have a dart wing at their origin, are added to the map for dart wing classification purposes
-- but not used when composing at largeDartBases, hence they need to be recorded as remaining as well.
-- Such kites cannot exist in a forced Tgraph, so this only arises when composing unforced Tgraphs.
data DartWingInfo =  DartWingInfo 
     { largeKiteCentres  :: [Vertex] -- ^ dart wing vertices classified as large kite centres.
     , largeDartBases  :: [Vertex]  -- ^ dart wing vertices classified as large dart bases.
     , unknowns :: [Vertex] -- ^ unclassified (boundary) dart wing vertices.
     , faceMap :: VertexMap [TileFace] -- ^ a mapping from dart wing vertices to faces at the vertex.
     , remaining :: [TileFace] -- ^ any kites whose oppV is not at a dart wing vertex.
     } deriving Show

{- -- |Recover a list of faces (no repetitions) contained in the dart wing info.
-- (These should be all faces of the Tgraph used to make the dart wing info.)
recoverFaces :: DartWingInfo -> [TileFace]
recoverFaces dwInfo =  nub $ concat (remaining dwInfo : VMap.elems (faceMap dwInfo))
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
         (dwFMap,unused) = dwMapUnused g 
     fg <- onFail "tryGetDartWingInfo: incorrect Tgraph (found during forcing).\n" $ tryForce g
     let dwFMapForced = extendMap dwFMap (extraFaces (faces fg) (faces g)) -- dwMapUnused fg
         -- forced version is used for classifying (but darts from original graph)
         (allKcs,allDbs,allUnks) = classifyDartWings dwFMapForced (VMap.keys dwFMap) --(map wingV drts)  
     return $ DartWingInfo 
               { largeKiteCentres = IntSet.toList allKcs
               , largeDartBases = IntSet.toList allDbs
               , unknowns = IntSet.toList allUnks
               , faceMap = dwFMap -- original map (not forced version)
               , remaining = unused -- from original Tgraph
               }

-- | Not exported , used by tryGetDartWingInfo to extend an existing dartwing faces map
-- only at the existing dart wings using new faces.
extendMap :: VertexMap [TileFace] -> [TileFace] -> VertexMap [TileFace]
extendMap dwm fcs = dwmFinal where
  (drts,kts) = partition isDart fcs
  dwm1 = foldl' insertDIF dwm drts
  dwmFinal = foldl' insertKIF dwm1 kts
  insertDIF m d = VMap.alter (addF d) (wingV d) m
  insertKIF m k = VMap.alter (addF k) (oppV k) $ VMap.alter (addF k) (originV k) m
  addF _ Nothing = Nothing  -- not added to map if it is not a dart wing vertex
  addF f (Just fs) = Just (f:fs)

-- | Not exported , used by tryGetDartWingInfo to get the new faces added to a forced tgraph.
-- It assumes the first list of faces extends the second list only by adding to the front.
-- It returns the front part of the list that has been added.
extraFaces :: [TileFace] -> [TileFace] -> [TileFace]
extraFaces fg [] = fg
extraFaces fg (f:_) = takeWhile (/= f) fg

-- |Experimental version of tryGetDartWingInfo that only forces at the dart wings
-- rather than a complete force to retrieve dart wing information.
tryGetDartWingInfoLocal :: HasGraph a => a -> Try DartWingInfo
tryGetDartWingInfoLocal a =
  do let g = recoverGraph a
         (dwFMap,unused) = dwMapUnused g
         wings = VMap.keys dwFMap
     fg <- onFail "tryGetDartWingInfoLocal: incorrect Tgraph (found during forcing).\n" $ tryForceAt wings g
     let dwFMapExtra = extendMap dwFMap (extraFaces (faces fg) (faces g)) --dwMapUnused fg
         -- forced version is used for classifying (but darts from original graph)
         (allKcs,allDbs,allUnks) = classifyDartWings dwFMapExtra (VMap.keys dwFMap)  
     return $ DartWingInfo 
               { largeKiteCentres = IntSet.toList allKcs
               , largeDartBases = IntSet.toList allDbs
               , unknowns = IntSet.toList allUnks
               , faceMap = dwFMap -- original map (not forced version)
               , remaining = unused -- from original Tgraph
               }

-- | getDartWingInfoForced fg (fg an explicitly Forced Tgraph) classifies the dart wings in fg
-- and calculates a faceMap for each dart wing, returning as DartWingInfo.
-- The classification is much simplified knowing that the Tgraph is forced.
getDartWingInfoForced:: Forced Tgraph -> DartWingInfo
getDartWingInfoForced fg =  
  DartWingInfo { largeKiteCentres = IntSet.toList allKcs
               , largeDartBases = IntSet.toList allDbs
               , unknowns = IntSet.toList allUnks
               , faceMap = dwFMap
               , remaining = unused
               } where
  (dwFMap,unused) = dwMapUnused (forgetF fg)
  (allKcs,allDbs,allUnks) = classifyDartWings dwFMap  (VMap.keys dwFMap) --(map wingV drts)  

-- |A faster version of compose which may underestimate the composable faces.
-- It does not use force to check classification of dart wings, so some dart wings may be
-- incorrectly classified as unknown (leaving fewer composed faces).
-- It checks the resulting Tgraph for connected and no crossing boundaries, so
-- it can raise an error if this check fails.
-- It will always be correct on a forced Tgraph.
quickCompose :: HasGraph a => a -> Tgraph
quickCompose = snd . quickPartCompose

-- | A faster version of partCompose which may underestimate the composable faces.
-- It does not use force to check classification of dart wings, so some dartwings may be
-- incorrectly classified as unknown (leaving more remainder faces and fewer composed faces).
-- It checks the resulting Tgraph for connected and no crossing boundaries, so
-- it can raise an error if this check fails.
-- It will always be correct on a forced Tgraph.
quickPartCompose:: HasGraph a => a -> ([TileFace], Tgraph)
quickPartCompose a = (remainder, checked) where
  checked = runTry $ onFail "quickPartCompose:\n" $ tryConnectedNoCross newfaces
  (dwFMap,unused) = dwMapUnused (recoverGraph a)
  (remainder,newfaces) = foldl' checkDW (unused,[]) (VMap.keys dwFMap)
  checkDW (rems, nfcs) w = 
     let fcs = dwFMap  VMap.! w
         (thekites,thedarts) = partition isKite fcs
         hasKiteOppWithOriginInMap v = 
             -- does some kite (attached at its oppV) have its origin at a largeDartBase.
               case find ((==v) . oppV) thekites of
                   Nothing -> False
                   Just k -> VMap.member (originV k) dwFMap
         
     in  if w `elem` map originV thekites 
         -- dart wing is also a half kite origin => largeDartBase
         then collectDarts (rems,nfcs) (filter (wanted w) fcs) else
         if shortMatch $ filter ((==w) . oppV) thekites
         -- two half kites, with oppV at w, share a short edge => largeKiteCentre
         then collectKites (rems, nfcs) fcs  else
         case thedarts of
            [d1,d2] -> if sharedLongE d1 d2
                            -- two darts share long edge => largekiteCentre
                       then collectKites (rems, nfcs) fcs
                            -- two darts, no matching long edge => largeDartBase
                       else collectDarts (rems,nfcs) (filter (wanted w) fcs)
            _ -> if hasKiteOppWithOriginInMap w
                     -- must be a largeKiteCentre
                 then collectKites (rems, nfcs) fcs
                    -- otherwise assume unknown
                 else (fcs++rems, nfcs) 

  wanted v f = isDart f || originV f /=v -- ignore kites with origin at v after classifying v
  shortMatch [] = False
  shortMatch [ _ ] =  False
  shortMatch (k:more) = any (sharedShortE k) more || shortMatch more
  
  collectDarts :: ([TileFace], [TileFace]) -> [TileFace] -> ([TileFace], [TileFace])
  collectDarts (rems, newfs) fcs = (fcs''++rems, newfs'') where
    (newfs' , fcs') = fromMaybe (newfs,fcs) $ groupRD fcs newfs
    (newfs'' , fcs'') = fromMaybe (newfs',fcs') $ groupLD fcs' newfs'
    groupRD fs nfs =
       do rd <- find isRD fs
          lk <- find (sharedShortE rd) fs
          return (makeRD (originV lk) (originV rd) (oppV lk):nfs, fs\\[rd,lk])
    groupLD fs nfs = 
       do ld <- find isLD fs
          rk <- find (sharedShortE ld) fs
          return (makeLD (originV rk) (oppV rk) (originV ld):nfs, fs\\[ld,rk])
  collectKites :: ([TileFace], [TileFace]) -> [TileFace] -> ([TileFace], [TileFace])
  collectKites (rems, newfs) fcs = (fcs''++rems, newfs'') where
    (newfs' , fcs') = fromMaybe (newfs,fcs) $ groupRK fcs newfs
    (newfs'' , fcs'') = fromMaybe (newfs',fcs') $ groupLK fcs' newfs'
    groupRK fs nfs =
       do rd <- find isRD fs
          lk <- find (sharedShortE rd) fs
          rk <- find (sharedJoinE lk) fs
          return (makeRK (originV rd) (wingV rk) (originV rk):nfs, fs\\[rd,lk,rk])
    groupLK fs nfs = 
        do ld <- find isLD fs
           rk <- find (sharedShortE ld) fs
           lk <- find (sharedJoinE rk) fs
           return (makeLK (originV ld) (originV lk) (wingV lk):nfs, fs\\[ld,rk,lk])

 
{- |Not exported - used in partComposeF, getDartWingInfoForced and tryGetDartWingInfo.
Returns a pair of:
 a dart wing to faces map, and 
 unused - kites guaranteed to be remainder in a composition.
A dart wing list (without duplicates) can be recovered from the map keys.
-}
dwMapUnused :: Tgraph -> (VertexMap [TileFace],[TileFace])
dwMapUnused g = (dwFMap,unused) where
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

-- |partComposeDWI constructs a pair of remainder faces and a composed Tgraph
-- from dart wing information (DWI).
-- It does not assume the dart wing info has come from a forced Tgraph
-- so a check on connected and no crossing boundaries is performed on the composed faces
-- and will raise an error if this fails.
partComposeDWI :: DartWingInfo -> ([TileFace],Tgraph)
-- This is used in defining tryPartCompose but also exported (used in the composeK example in Extras).
partComposeDWI dwi = (remainder,g) where
  g = runTry $ tryConnectedNoCross fcs
  (remainder,fcs) = partComposeFacesDWI dwi

-- |Not Exported: partComposeFacesDWI (used in partComposeDWI and tryPartComposeFaces)
-- constructs a pair of (remainder,composedfaces) from dart wing information (DWI).
-- This is used in defining tryPartComposeFaces but also exported
-- for use in the composeK example in Extras.
-- It does not assume the dart wing info has come from a forced Tgraph
-- so the resulting composed faces may not form a valid Tgraph.
--
-- This version relies on kites that only have a dart wing at their origin, being included in remaining.
-- Such kites are also recorded in the dart wing/(kite origin) for classification purposes but then
-- filtered out when composing at a largeDartBase.
partComposeFacesDWI :: DartWingInfo -> ([TileFace],[TileFace])
partComposeFacesDWI dwInfo = (remainder, newfaces) where
    ~remainder0 = remaining dwInfo ++ concatMap facesFor (unknowns dwInfo)
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
            lk <- find (sharedShortE rd) fs
            return (makeRD (originV lk) (originV rd) (oppV lk):nfs, fs\\[rd,lk])
      groupLD fs nfs = 
         do ld <- find isLD fs
            rk <- find (sharedShortE ld) fs
            return (makeLD (originV rk) (oppV rk) (originV ld):nfs, fs\\[ld,rk])
    collectKites :: ([TileFace], [TileFace]) -> Vertex -> ([TileFace], [TileFace])
    collectKites (rems, newfs) v = (fcs''++rems, newfs'') where
      fcs = facesFor v
      (newfs' , fcs') = fromMaybe (newfs,fcs) $ groupRK fcs newfs
      (newfs'' , fcs'') = fromMaybe (newfs',fcs') $ groupLK fcs' newfs'
      groupRK fs nfs =
         do rd <- find isRD fs
            lk <- find (sharedShortE rd) fs
            rk <- find (sharedJoinE lk) fs
            return (makeRK (originV rd) (wingV rk) (originV rk):nfs, fs\\[rd,lk,rk])
      groupLK fs nfs = 
          do ld <- find isLD fs
             rk <- find (sharedShortE ld) fs
             lk <- find (sharedJoinE rk) fs
             return (makeLK (originV ld) (originV lk) (wingV lk):nfs, fs\\[ld,rk,lk])

