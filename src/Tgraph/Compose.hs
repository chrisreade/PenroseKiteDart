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
and partComposeFacesFrom for debugging.
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
  , tryPartComposeFaces
  -- * Exported auxiliary functions (and type)
  -- , partCompFacesAssumeF
  -- , partComposeFaces
  -- , partComposeFacesF
  , partComposeFacesFrom --new
  , DartWingInfo(..)
  -- , getDWIassumeF
  -- , getDartWingInfo
  , tryGetDartWingInfo
  , getDartWingInfoForced
 -- , composedFaceGroups
   -- * Older versions (for debugging/comparison)
  , oldGetDartWingInfo
  , oldPartCompose
  ) where

import Data.List (find,(\\),partition,nub)
import Prelude hiding (Foldable(..))
import Data.Foldable (Foldable(..))
import qualified Data.IntMap.Strict as VMap (IntMap,lookup,(!),alter,empty,elems)
import Data.Maybe (mapMaybe)
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
  do dwInfo <- tryGetDartWingInfo g 
     let (~remainder,newFaces) = partComposeFacesFrom dwInfo
     checked <- onFail "tryPartCompose:\n" $ tryConnectedNoCross newFaces
     return (remainder,checked)

-- |Get the remainder and composed faces (without checking the composed faces make a valid Tgraph)
-- It uses tryGetDartWingInfo g which can fail if g is found to be incorrect when forced.
tryPartComposeFaces:: Tgraph -> Try ([TileFace],[TileFace])
tryPartComposeFaces g = 
  do dwInfo <- tryGetDartWingInfo g 
     return $ partComposeFacesFrom dwInfo
-- tryPartComposeFaces is used in an example showing failure of the connected, no crossing boundary check.


-- |Uses supplied dartwing info to get remainder faces and composed faces.
-- Does not assume forced and does not check the composed faces for connected/no crossing boundaries
partComposeFacesFrom :: DartWingInfo -> ([TileFace], [TileFace])
partComposeFacesFrom = partCompFacesAssumeF False

{- 
-- |partComposeFaces g - produces a pair of the remainder faces (faces from g which will not compose)
-- and the composed faces (which may or may not constitute faces of a valid Tgraph).
-- It does not assume that g is forced which makes it less efficient than partComposeFacesF.
partComposeFaces:: Tgraph -> ([TileFace],[TileFace])
partComposeFaces = partCompFacesAssumeF False

-- |partComposeFacesF (does the same as partComposeFaces for a Forced Tgraph).
-- It produces a pair of the remainder faces (faces which will not compose)
-- and the composed faces.
partComposeFacesF :: Forced Tgraph -> ([TileFace],[TileFace])
partComposeFacesF = partCompFacesAssumeF True . forgetF

 -}

-- |partComposeF fg - produces a pair consisting of remainder faces (faces from fg which will not compose) 
-- and a composed (Forced) Tgraph.
-- Since fg is a forced Tgraph it does not need a check for validity of the composed Tgraph.
-- The fact that the function is total and the result is also Forced relies on theorems
-- established for composing.
-- The calculation of remainder faces is also more efficient with a known forced Tgraph.
partComposeF:: Forced Tgraph -> ([TileFace], Forced Tgraph)
partComposeF fg = (remainder, labelAsForced $ makeUncheckedTgraph newfaces) where
  (~remainder,newfaces) = partCompFacesAssumeF True $ getDartWingInfoForced fg
  
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
     , faceMap :: VMap.IntMap [TileFace] -- ^ a mapping from dart wing vertices to faces at the vertex.
     , unMapped :: [TileFace] -- ^ any faces not at a dart wing vertex (necessarily kites)
     } deriving Show

-- |Recover a list of faces (no repetitions) contained in the dart wing info.
-- (These should be all faces of the Tgraph used to make the dart wing info.)
recoverFaces :: DartWingInfo -> [TileFace]
recoverFaces dwInfo =  nub $ concat (unMapped dwInfo : VMap.elems (faceMap dwInfo))


{- -- | getDartWingInfo g, classifies the dart wings in g and calculates a faceMap for each dart wing,
-- returning as DartWingInfo. It does not assume g is forced and is more expensive than getDartWingInfoForced
getDartWingInfo:: Tgraph -> DartWingInfo
getDartWingInfo = getDWIassumeF False
 -}

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
        !revLongE = reverseD (longE drt)
    in
        if w `IntSet.member` kcs || w `IntSet.member` dbs then (kcs, dbs, unks) else-- already classified
        let
            !fcs = dwFMap VMap.! w -- list of  faces at w
        in
            if w `elem` map originV (filter isKite fcs) then (kcs,IntSet.insert w dbs,unks) else 
                    -- wing is a half kite origin => largeDartBase
            if revLongE `elem` map longE (filter isDart fcs) then (IntSet.insert w kcs,dbs,unks) else 
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

-- |(not exported - only used in getDWIassumeF)
-- Returns a triple of:
--   list of all half-darts,
--   a dart wing to faces map, and 
--   left over faces (not at a dartwing)
dartsMapUnused :: Tgraph -> ([TileFace], VMap.IntMap [TileFace],[TileFace])
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
          org = originV f
      in  case (VMap.lookup opp vmap, VMap.lookup org vmap) of
            (Just _ ,Just _)     ->  (VMap.alter (addK f) opp $ VMap.alter (addK f) org vmap, unsd)
            (Just _ , Nothing)   ->  (VMap.alter (addK f) opp vmap, unsd)
            (Nothing, Just _ )   ->  (VMap.alter (addK f) org vmap, unsd)
            (Nothing, Nothing)   ->  (vmap, f:unsd) -- kite face not at any dart wing

    addK _ Nothing = Nothing  -- not added to map if it is not a dart wing vertex
    addK f (Just fs) = Just (f:fs)


-- |partCompFacesAssumeF
-- (not exported but used to build 2 cases: partComposeFacesFrom, partComposeF)
-- If the boolean is True then assumptions are made that the DartWingIno
-- has come from a forced Tgraph,
-- making the remainder faces calculation more efficient.
partCompFacesAssumeF :: Bool ->  DartWingInfo -> ([TileFace],[TileFace])
partCompFacesAssumeF isForced dwInfo = (remainder, newFaces) where
    ~remainder = 
        if isForced
        then -- unMapped faces plus all faces at unknowns.
            unMapped dwInfo ++ concatMap (faceMap dwInfo VMap.!) (unknowns dwInfo)
        else -- all faces except those successfully used in making composed faces.
            recoverFaces dwInfo \\ concatMap concat [groupRDs, groupLDs, groupRKs, groupLKs]
    
    newFaces = newRDs ++ newLDs ++ newRKs ++ newLKs

    newRDs = map makenewRD groupRDs 
    groupRDs = mapMaybe groupRD (largeDartBases dwInfo)
    makenewRD [rd,lk] = makeRD (originV lk) (originV rd) (oppV lk) 
    makenewRD _       = error "composedFaceGroups: RD case"
    groupRD v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    rd <- find isRD fcs
                    lk <- find (matchingShortE rd) fcs
                    return [rd,lk]

    newLDs = map makenewLD groupLDs 
    groupLDs = mapMaybe groupLD (largeDartBases dwInfo) 
    makenewLD [ld,rk] = makeLD (originV rk) (oppV rk) (originV ld)
    makenewLD _       = error "composedFaceGroups: LD case"
    groupLD v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    ld <- find isLD fcs
                    rk <- find (matchingShortE ld) fcs
                    return [ld,rk]

    newRKs = map makenewRK groupRKs 
    groupRKs = mapMaybe groupRK (largeKiteCentres dwInfo) 
    makenewRK [rd,_,rk] = makeRK (originV rd) (wingV rk) (originV rk)
    makenewRK _         = error "composedFaceGroups: RK case"
    groupRK v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    rd <- find isRD fcs
                    lk <- find (matchingShortE rd) fcs
                    rk <- find (matchingJoinE lk) fcs
                    return [rd,lk,rk]

    newLKs = map makenewLK groupLKs 
    groupLKs = mapMaybe groupLK (largeKiteCentres dwInfo) 
    makenewLK [ld,_,lk] = makeLK (originV ld) (originV lk) (wingV lk)
    makenewLK _         = error "composedFaceGroups: LK case"
    groupLK v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    ld <- find isLD fcs
                    rk <- find (matchingShortE ld) fcs
                    lk <- find (matchingJoinE rk) fcs
                    return [ld,rk,lk]


-- |oldPartCompose g is a partial function producing a pair consisting of remainder faces (faces from g which will not compose) 
-- and a composed Tgraph. 
-- It checks the composed Tgraph for connectedness and no crossing boundaries raising an error if this check fails.
-- It does not assume the given Tgraph is forced.
-- It can raise an error if the Tgraph is found to be incorrect (when getting dartwing info).
oldPartCompose:: Tgraph -> ([TileFace],Tgraph)
oldPartCompose g = runTry $ onFail "oldPartCompose:\n" $
  do let dwInfo = oldGetDartWingInfo g 
         (remainder,newFaces) = partComposeFacesFrom dwInfo
     checked <- tryConnectedNoCross newFaces
     return (remainder,checked)


-- | oldGetDartWingInfo g, classifies the dart wings in g and calculates a faceMap for each dart wing,
-- returning as DartWingInfo. If only uses local information to classify each dart wing and can
-- therefore sometimes classify a dart wing as unknown unnecessarily.
-- In contrast tryGetDartWingInfo is accurate using information from forcing (so is not local)
oldGetDartWingInfo:: Tgraph -> DartWingInfo
oldGetDartWingInfo = oldGetDWIassumeF False

-- | oldGetDWIassumeF (not exported but used to define oldGetDartWingInfo).
-- oldGetDWIassumeF isForced g, classifies the dart wings in g and calculates a faceMap for each dart wing,
-- returning as DartWingInfo. The boolean isForced is used to decide if g can be assumed to be forced.
oldGetDWIassumeF:: Bool -> Tgraph -> DartWingInfo
oldGetDWIassumeF isForced g =  
  DartWingInfo { largeKiteCentres = IntSet.toList allKcs
               , largeDartBases = IntSet.toList allDbs
               , unknowns = IntSet.toList allUnks
               , faceMap = dwFMap
               , unMapped = unused
               } where
  (drts,kts) = partition isDart (faces g)
  -- special case of vertexFacesMap for dart wings only
  -- using only relevant vertices where there is a dart wing.
  -- i.e only wingVs for darts and only oppVs and originVs for kites.
  -- The map is built first from darts, then kites are added.
  (dwFMap,unused) = foldl' insertK (dartWMap,[]) kts
                    -- all kite halves added to relevant dart wings of the dart wing faces map
    where           -- the unused list records half kites not added to any dart wing
    dartWMap = foldl' insertD VMap.empty drts
                     -- maps all dart wing vertices to 1 or 2 half darts
    insertD vmap f = VMap.alter (addD f) (wingV f) vmap
    addD f Nothing = Just [f]
    addD f (Just fs) = Just (f:fs)
    insertK (vmap,unsd) f = 
      let opp = oppV f
          org = originV f
      in  case (VMap.lookup opp vmap, VMap.lookup org vmap) of
            (Just _ ,Just _)     ->  (VMap.alter (addK f) opp $ VMap.alter (addK f) org vmap, unsd)
            (Just _ , Nothing)   ->  (VMap.alter (addK f) opp vmap, unsd)
            (Nothing, Just _ )   ->  (VMap.alter (addK f) org vmap, unsd)
            (Nothing, Nothing)   ->  (vmap, f:unsd)

    addK _ Nothing = Nothing  -- not added to map if it is not a dart wing vertex
    addK f (Just fs) = Just (f:fs)

  (allKcs,allDbs,allUnks) = foldl' processD (IntSet.empty, IntSet.empty, IntSet.empty) drts  
-- kcs = kite centres of larger kites,
-- dbs = dart bases of larger darts,
-- unks = unclassified dart wing vertices
-- Uses a triple of IntSets rather than lists
  processD (kcs, dbs, unks) rd@(RD (orig, w, _)) = -- classify wing tip w
    if w `IntSet.member` kcs || w `IntSet.member` dbs then (kcs, dbs, unks) else-- already classified
    let
        fcs = dwFMap VMap.! w -- faces at w
--        Just fcs = VMap.lookup w dwFMap -- faces at w
    in
        if w `elem` map originV (filter isKite fcs) then (kcs,IntSet.insert w dbs,unks) else 
                -- wing is a half kite origin => largeDartBase
        if (w,orig) `elem` map longE (filter isLD fcs) then (IntSet.insert w kcs,dbs,unks) else 
                -- long edge rd shared with an ld => largeKiteCentre
        if isForced || length fcs == 1 then (kcs, dbs, IntSet.insert w unks) else
        case findFarK rd fcs of -- extra inspection only needed for unforced Tgraphs
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
        if w `elem` map originV (filter isKite fcs) then (kcs,IntSet.insert w dbs,unks) else
                   -- wing is a half kite origin => largeDartBase
        if (orig,w) `elem` map longE (filter isRD fcs) then (IntSet.insert w kcs,dbs,unks) else
                   -- long edge ld shared with an rd => largeKiteCentre
        if isForced || length fcs == 1 then (kcs, dbs, IntSet.insert w unks) else
        case findFarK ld fcs of -- extra inspection only needed for unforced Tgraphs
          Nothing -> (kcs,dbs,IntSet.insert w unks) -- unknown if incomplete kite attached to short edge of ld
          Just lk@(LK _)  ->  
            case find (matchingShortE lk) fcs of
            Just (RK _) -> (IntSet.insert w kcs,dbs,unks) 
                  -- short edge lk shared with an rk => largeKiteCentres
            Just (RD _) -> (kcs,IntSet.insert w dbs,unks)
                  -- short edge lk shared with an rd => largeDartBases
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

{- 
-- |Creates a list of new composed faces, each paired with a list of old faces (components of the new face)
-- using dart wing information. No longer used.
composedFaceGroups :: DartWingInfo -> [(TileFace,[TileFace])]
composedFaceGroups dwInfo = faceGroupRDs ++ faceGroupLDs ++ faceGroupRKs ++ faceGroupLKs where

    faceGroupRDs = map (\gp -> (makenewRD gp,gp)) groupRDs 
    groupRDs = mapMaybe groupRD (largeDartBases dwInfo)
    makenewRD [rd,lk] = makeRD (originV lk) (originV rd) (oppV lk) 
    makenewRD _       = error "composedFaceGroups: RD case"
    groupRD v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    rd <- find isRD fcs
                    lk <- find (matchingShortE rd) fcs
                    return [rd,lk]

    faceGroupLDs = map (\gp -> (makenewLD gp,gp)) groupLDs 
    groupLDs = mapMaybe groupLD (largeDartBases dwInfo) 
    makenewLD [ld,rk] = makeLD (originV rk) (oppV rk) (originV ld)
    makenewLD _       = error "composedFaceGroups: LD case"
    groupLD v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    ld <- find isLD fcs
                    rk <- find (matchingShortE ld) fcs
                    return [ld,rk]

    faceGroupRKs = map (\gp -> (makenewRK gp,gp)) groupRKs 
    groupRKs = mapMaybe groupRK (largeKiteCentres dwInfo) 
    makenewRK [rd,_,rk] = makeRK (originV rd) (wingV rk) (originV rk)
    makenewRK _         = error "composedFaceGroups: RK case"
    groupRK v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    rd <- find isRD fcs
                    lk <- find (matchingShortE rd) fcs
                    rk <- find (matchingJoinE lk) fcs
                    return [rd,lk,rk]

    faceGroupLKs = map (\gp -> (makenewLK gp,gp)) groupLKs 
    groupLKs = mapMaybe groupLK (largeKiteCentres dwInfo) 
    makenewLK [ld,_,lk] = makeLK (originV ld) (originV lk) (wingV lk)
    makenewLK _         = error "composedFaceGroups: LK case"
    groupLK v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    ld <- find isLD fcs
                    rk <- find (matchingShortE ld) fcs
                    lk <- find (matchingJoinE rk) fcs
                    return [ld,rk,lk]

-}

