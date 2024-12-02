{-|
Module      : Tgraph.Compose
Description : A compose operation for Tgraphs
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module includes the main composition operations compose, partCompose, tryPartCompose but also exposes 
getDartWingInfo, getDartWingInfoForced (and type DartWingInfo) and composedFaceGroups for debugging and experimenting.
-}
{-# LANGUAGE StrictData             #-} 

module Tgraph.Compose 
  ( compose
  , partCompose
  , tryPartCompose
  , uncheckedCompose
  , uncheckedPartCompose
  , partComposeFaces
  -- , partComposeFacesWith
 -- , composedFaces
  , DartWingInfo(..)
  , getDartWingInfo
  , getDartWingInfoForced
  , composedFaceGroups
  ) where

import Data.List ((\\), find, foldl',nub)
import qualified Data.IntMap.Strict as VMap (IntMap,lookup,(!))
import Data.Maybe (mapMaybe)
import qualified Data.IntSet as IntSet (empty,insert,toList,member)

import Tgraph.Prelude

{-------------------------------------------------------------------------
***************************************************************************              
COMPOSING compose, partCompose, tryPartCompose, uncheckedPartCompose
***************************************************************************
---------------------------------------------------------------------------}

-- |The main compose function which simply drops the remainder faces from partCompose to return just
-- the composed Tgraph.  It will raise an error if the result is not a valid Tgraph
-- (i.e. if it fails the connectedness, no crossing boundary check)
-- It does not assume the given Tgraph is forced and is inefficient on large Tgraphs
compose:: Tgraph -> Tgraph
compose = snd . partCompose

-- |This does the same as compose but more efficiently because it assumes the given Tgraph is forced.
-- It uses getDartWingInfoForced and it does not perform checks for connectedness and no crossing boundaries in the result.
-- This relies on a proof that the checks are not needed for forced Tgraphs.
-- (The result is a forced Tgraph.)
uncheckedCompose:: Tgraph -> Tgraph
uncheckedCompose = snd . uncheckedPartCompose

-- |partCompose g produces a pair consisting of remainder faces (faces from g which will not compose) 
-- and a composed Tgraph. It does not assume the given Tgraph is forced and can be inefficient on large Tgraphs.
-- It checks the composed Tgraph for connectedness and no crossing boundaries raising an error if this check fails.
partCompose:: Tgraph -> ([TileFace],Tgraph)
partCompose g = runTry $ onFail "partCompose:\n" $ tryPartCompose g

-- |tryPartCompose g tries to produce a Tgraph by composing faces which uniquely compose in g,
-- It checks the resulting new faces for connectedness and no crossing boundaries.
-- If the check is OK it produces Right (remainder, g') where g' is the composed Tgraph and remainder is a list
-- of faces from g which will not compose.  If the check fails it produces Left s where s is a failure report.
-- It does not assume the given Tgraph is forced and is inefficient on large Tgraphs.
tryPartCompose:: Tgraph -> Try ([TileFace],Tgraph)
tryPartCompose g = 
  do let (remainder,newFaces) = partComposeFaces g
     checked <- onFail "tryPartCompose:/n" $ tryConnectedNoCross newFaces
     return (remainder,checked)

-- |uncheckedPartCompose g - assumes g is forced. It produces a pair of the remainder faces (faces from g which will not compose)
-- and a Tgraph made from the composed faces without checking for connectedness and no crossing boundaries.
-- This relies on a proof that the result of composing a forced Tgraph does not require these checks.
uncheckedPartCompose:: Tgraph -> ([TileFace],Tgraph)
uncheckedPartCompose g = (remainder, makeUncheckedTgraph newfaces) where
  (remainder,newfaces) = partComposeFacesWith getDartWingInfoForced g

-- |partComposeFaces g - produces a pair of the remainder faces (faces from g which will not compose)
-- and the composed faces (which may or may not constitute faces of a valid Tgraph).
-- It does not assume that g is forced.
partComposeFaces:: Tgraph -> ([TileFace],[TileFace])
partComposeFaces = partComposeFacesWith getDartWingInfo

-- |partComposeFacesWith gtdwi g, 
-- (where gtdwi gets dart wing info from g - either getDartWingInfo or getDartWingInfoForced)
-- produces a pair of the remainder faces (faces from the original which will not compose)
-- and the composed faces (which may or may not constitute faces of a valid Tgraph).
partComposeFacesWith:: (Tgraph -> DartWingInfo) -> Tgraph -> ([TileFace],[TileFace])
partComposeFacesWith getdwi g = (remainder,newfaces) where
  compositions = composedFaceGroups $ getdwi g
  newfaces = map fst compositions
  remainder = faces g \\ concatMap snd compositions


{- 
-- |composedFaces g produces the composed faces of g (which may or may not constitute faces of a valid Tgraph).
composedFaces:: Tgraph -> [TileFace]
composedFaces = snd . partComposeFaces
 -}

-- |DartWingInfo is a record type for the result of classifying dart wings in a Tgraph.
-- It includes a faceMap from dart wings to faces at that vertex.
data DartWingInfo =  DartWingInfo 
     { largeKiteCentres  :: [Vertex]
     , largeDartBases  :: [Vertex]
     , unknowns :: [Vertex]
     , faceMap :: VMap.IntMap [TileFace] 
     } deriving Show

-- | getDartWingInfo g, classifies the dart wings in g and calculates a faceMap for each dart wing,
-- returning as DartWingInfo. It does not assume g is forced and is more expensive than getDartWingInfoForced
getDartWingInfo:: Tgraph -> DartWingInfo
getDartWingInfo = getDWIassumeF False

-- | getDartWingInfoForced g, classifies the dart wings in g and calculates a faceMap for each dart wing,
-- returning as DartWingInfo. It assume g is forced.
getDartWingInfoForced :: Tgraph -> DartWingInfo
getDartWingInfoForced = getDWIassumeF True

-- | getDWIassumeF isForced g, classifies the dart wings in g and calculates a faceMap for each dart wing,
-- returning as DartWingInfo. The boolean isForced is used to decide if g can be assumed to be forced.
getDWIassumeF:: Bool -> Tgraph -> DartWingInfo
getDWIassumeF isForced g =  
  DartWingInfo { largeKiteCentres = IntSet.toList allKcs
               , largeDartBases = IntSet.toList allDbs
               , unknowns = IntSet.toList allUnks
               , faceMap = dwFMap
               } where
  drts  = darts g
  dwFMap = vertexFacesMap (nub $ fmap wingV drts) (faces g)
  (allKcs,allDbs,allUnks) = foldl' processD (IntSet.empty, IntSet.empty, IntSet.empty) drts  
-- kcs = kite centres of larger kites,
-- dbs = dart bases of larger darts,
-- unks = unclassified dart wing tips
-- processD now uses a triple of IntSets rather than lists
  processD (kcs, dbs, unks) rd@(RD (orig, w, _)) = -- classify wing tip w
    if w `IntSet.member` kcs || w `IntSet.member` dbs then (kcs, dbs, unks) else-- already classified
    let
        fcs = dwFMap VMap.! w -- faces at w
--        Just fcs = VMap.lookup w dwFMap -- faces at w
    in
        if length fcs ==1 then (kcs, dbs, IntSet.insert w unks) else -- lone dart wing => unknown
        if w `elem` fmap originV (filter isKite fcs) then (kcs,IntSet.insert w dbs,unks) else 
                -- wing is a half kite origin => largeDartBases
        if (w,orig) `elem` fmap longE (filter isLD fcs) then (IntSet.insert w kcs,dbs,unks) else 
                -- long edge rd shared with an ld => largeKiteCentres
        if isForced then (kcs, dbs, IntSet.insert w unks) else
        case findFarK rd fcs of
        Nothing -> (kcs,dbs,IntSet.insert w unks) -- unknown if incomplete kite attached to short edge of rd
        Just rk@(RK _)  ->  
            case find (matchingShortE rk) fcs of
            Just (LK _) -> (IntSet.insert w kcs,dbs,unks) -- short edge rk shared with an lk => largeKiteCentres
            Just (LD _) -> (kcs,IntSet.insert w dbs,unks) -- short edge rk shared with an ld => largeDartBases
            _ -> let 
                     newfcs = filter (isAtV (wingV rk)) (faces g)   -- faces at rk wing    
                 in
                 case find (matchingLongE rk) newfcs of  -- short edge rk has nothing attached
                 Nothing -> (kcs,dbs,IntSet.insert w unks)  -- long edge of rk has nothing attached => unknown
                 Just (LD _) -> (IntSet.insert w kcs,dbs,unks) -- long edge rk shared with ld => largeKiteCentres
                 Just lk@(LK _) ->               -- long edge rk shared with lk
                      case find (matchingShortE lk) newfcs of
                      Just (RK _) -> (IntSet.insert w kcs,dbs,unks)
                              -- short edge of this lk shared with another rk => largeKiteCentres
                      Just (RD _) -> (kcs,IntSet.insert w dbs,unks) 
                              -- short edge of this lk shared with rd => largeDartBases
                      _ -> (kcs,dbs,IntSet.insert w unks) 
                 Just _ ->  error "getDartWingInfo: illegal case for matchingLongE of a right kite"
                              -- short edge of this lk has nothing attached => unknown
        Just _ -> error "getDartWingInfo: non-kite returned by findFarK"

-- processD now uses a triple of IntSets rather than lists
  processD (kcs, dbs, unks) ld@(LD (orig, _, w)) = -- classify wing tip w
    if w `IntSet.member` kcs || w `IntSet.member` dbs then (kcs, dbs, unks) else  -- already classified
    let
        fcs = dwFMap VMap.! w -- faces at w
    in
        if length fcs ==1 then (kcs, dbs, IntSet.insert w unks) else -- lone dart wing => unknown
        if w `elem` fmap originV (filter isKite fcs) then (kcs,IntSet.insert w dbs,unks) else
                   -- wing is a half kite origin => nodeDB
        if (w,orig) `elem` fmap longE (filter isRD fcs) then (IntSet.insert w kcs,dbs,unks) else
                   -- long edge ld shared with an rd => nodeKC
        if isForced then (kcs, dbs, IntSet.insert w unks) else
        case findFarK ld fcs of
          Nothing -> (kcs,dbs,IntSet.insert w unks) -- unknown if incomplete kite attached to short edge of ld
          Just lk@(LK _)  ->  
            case find (matchingShortE lk) fcs of
            Just (RK _) -> (IntSet.insert w kcs,dbs,unks) -- short edge lk shared with an rk => largeKiteCentres
            Just (RD _) -> (kcs,IntSet.insert w dbs,unks) -- short edge lk shared with an rd => largeDartBases
            _ -> let 
                     newfcs = filter (isAtV (wingV lk)) (faces g)   -- faces at lk wing  
                 in
                 case find (matchingLongE lk) newfcs of -- short edge lk has nothing attached
                 Nothing -> (kcs,dbs,IntSet.insert w unks)  -- long edge of lk has nothing attached => unknown
                 Just (RD _) -> (IntSet.insert w kcs,dbs,unks) -- long edge lk shared with rd => largeKiteCentres
                 Just rk@(RK _) ->               -- long edge lk is shared with an rk
                     case find (matchingShortE rk) newfcs of
                     Just (LK _) -> (IntSet.insert w kcs,dbs,unks)
                             -- short edge of this rk shared with another lk => largeKiteCentres
                     Just (LD _) -> (kcs,IntSet.insert w dbs,unks)
                             -- short edge of this rk shared with ld => largeDartBases
                     _ -> (kcs,dbs,IntSet.insert w unks) -- short edge of this rk has nothing attached => unknown
                 Just _ ->  error "getDartWingInfo: illegal case for matchingLongE of a left kite"

          Just _ -> error "getDartWingInfo: non-kite returned by findFarK"

  processD _ _ = error "getDartWingInfo: processD applied to non-dart"

    -- find the two kite halves below a dart half, return the half kite furthest away (not attached to dart).
    -- Returns a Maybe.   rd produces an rk (or Nothing) ld produces an lk (or Nothing)
  findFarK :: TileFace -> [TileFace] -> Maybe TileFace
  findFarK rd@(RD _) fcs = do lk <- find (matchingShortE rd) (filter isLK fcs)
                              find (matchingJoinE lk) (filter isRK fcs)
  findFarK ld@(LD _) fcs = do rk <- find (matchingShortE ld) (filter isRK fcs)
                              find (matchingJoinE rk)  (filter isLK fcs)
  findFarK _ _ = error "getDartWingInfo: findFarK applied to non-dart face"

-- | Auxiliary function for partComposeFacesWith.
-- Creates a list of new composed faces, each paired with a list of old faces (components of the new face)
-- using dart wing information.
composedFaceGroups :: DartWingInfo -> [(TileFace,[TileFace])]
composedFaceGroups dwInfo = faceGroupRDs ++ faceGroupLDs ++ faceGroupRKs ++ faceGroupLKs where

    faceGroupRDs = fmap (\gp -> (makeRD gp,gp)) groupRDs 
    groupRDs = mapMaybe groupRD (largeDartBases dwInfo)
    makeRD [rd,lk] = RD(originV lk, originV rd, oppV lk) 
    makeRD _       = error "composedFaceGroups: RD case"
    groupRD v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    rd <- find isRD fcs
                    lk <- find (matchingShortE rd) fcs
                    return [rd,lk]

    faceGroupLDs = fmap (\gp -> (makeLD gp,gp)) groupLDs 
    groupLDs = mapMaybe groupLD (largeDartBases dwInfo) 
    makeLD [ld,rk] = LD(originV rk, oppV rk, originV ld)
    makeLD _       = error "composedFaceGroups: LD case"
    groupLD v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    ld <- find isLD fcs
                    rk <- find (matchingShortE ld) fcs
                    return [ld,rk]

    faceGroupRKs = fmap (\gp -> (makeRK gp,gp)) groupRKs 
    groupRKs = mapMaybe groupRK (largeKiteCentres dwInfo) 
    makeRK [rd,_,rk] = RK(originV rd, wingV rk, originV rk)
    makeRK _         = error "composedFaceGroups: RK case"
    groupRK v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    rd <- find isRD fcs
                    lk <- find (matchingShortE rd) fcs
                    rk <- find (matchingJoinE lk) fcs
                    return [rd,lk,rk]

    faceGroupLKs = fmap (\gp -> (makeLK gp,gp)) groupLKs 
    groupLKs = mapMaybe groupLK (largeKiteCentres dwInfo) 
    makeLK [ld,_,lk] = LK(originV ld, originV lk, wingV lk)
    makeLK _         = error "composedFaceGroups: LK case"
    groupLK v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    ld <- find isLD fcs
                    rk <- find (matchingShortE ld) fcs
                    lk <- find (matchingJoinE rk) fcs
                    return [ld,rk,lk]




