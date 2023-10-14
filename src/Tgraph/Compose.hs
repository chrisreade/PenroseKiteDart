{-|
Module      : Tgraph.Compose
Description : A compose operation for Tgraphs
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module includes the main composition operations compose and partCompose but also exposes 
getDartWingInfo (and type DartWingInfo) for debugging and experimenting.
-}
module Tgraph.Compose where

import Data.List ((\\), find, foldl',nub)
import qualified Data.IntMap.Strict as VMap (IntMap,lookup) -- used in partCompose
import Data.Maybe (mapMaybe)

import Tgraph.Prelude

{-------------------------------------------------------------------------
***************************************************************************              
COMPOSING compose, partCompose, tryPartCompose, uncheckedPartCompose
***************************************************************************
---------------------------------------------------------------------------}

-- |The main compose function which simply drops the remainder faces from partCompose to return just
-- the composed Tgraph.  It will raise an error if the result is not a valid Tgraph
-- (i.e. if it fails the connectedness, no crossing boundary check)
compose:: Tgraph -> Tgraph
compose = snd . partCompose

-- |This does the same as compose but without checks for connectedness and no crossing boundaries in the result.
-- It is intended for use on forced Tgraphs where we have a proof that the checks are not needed.
uncheckedCompose:: Tgraph -> Tgraph
uncheckedCompose = snd . uncheckedPartCompose

-- |partCompose g produces a pair consisting of remainder faces (faces from g which will not compose) 
-- and a composed Tgraph.
-- It checks the composed Tgraph for connectedness and no crossing boundaries raising an error if this check fails.
partCompose:: Tgraph -> ([TileFace],Tgraph)
partCompose g = runTry $ onFail "partCompose:\n" $ tryPartCompose g

-- |tryPartCompose g tries to produce a Tgraph by composing faces which uniquely compose in g,
-- It checks the resulting new faces for connectedness and no crossing boundaries.
-- If the check is OK it produces Right (remainder, g') where g' is the composed Tgraph and remainder is a list
-- of faces from g which will not compose.  If the check fails it produces Left s where s is a failure report.
tryPartCompose:: Tgraph -> Try ([TileFace],Tgraph)
tryPartCompose g = 
  do let (remainder,newGraph) = uncheckedPartCompose g
     checked <- onFail "tryPartCompose:/n" $ checkConnectedNoCross newGraph
     pure (remainder,checked)

-- |uncheckedPartCompose g produces a pair of the remainder faces (faces from g which will not compose)
-- and a Tgraph made from the composed faces without checking that the Tgraph is valid.
-- I.e. it does NOT check the composition Tgraph for connectedness and no crossing boundaries.
-- This is intended for use when we know the check is not needed (e.g. when g is forced).
uncheckedPartCompose:: Tgraph -> ([TileFace],Tgraph)
uncheckedPartCompose g = (remainder, makeUncheckedTgraph newfaces) where
  (remainder,newfaces) = partComposeFaces g

-- |partComposeFaces produces a pair of the remainder faces (faces from the original which will not compose)
-- and the composed faces (which may or may not constitute faces of a valid Tgraph).
partComposeFaces:: Tgraph -> ([TileFace],[TileFace])
partComposeFaces g = (remainder,newfaces) where
  compositions = composedFaceGroups $ getDartWingInfo g
  newfaces = map fst compositions
  groups = map snd compositions
  remainder = faces g \\ concat groups

-- |composedFaces g produces the composed faces of g (which may or may not constitute faces of a valid Tgraph).
composedFaces:: Tgraph -> [TileFace]
composedFaces = snd . partComposeFaces


-- |DartWingInfo is a record type for the result of classifying dart wings in a Tgraph.
-- It includes a faceMap from dart wings to faces at that vertex.
data DartWingInfo =  DartWingInfo 
     { largeKiteCentres  :: [Vertex]
     , largeDartBases  :: [Vertex]
     , unknowns :: [Vertex]
     , faceMap :: VMap.IntMap [TileFace] 
     } deriving Show

-- | getDartWingInfo g, classifies the dart wings in g and calculates a faceMap for each dart wing,
-- returning as DartWingInfo.
getDartWingInfo:: Tgraph -> DartWingInfo
getDartWingInfo g =  DartWingInfo {largeKiteCentres = kcs, largeDartBases = dbs, unknowns = unks, faceMap = dwFMap} where
  darts  = filter isDart (faces g)
  dwFMap = vertexFacesMap (nub $ fmap wingV darts) (faces g)
  (kcs,dbs,unks) = foldl' processD ([],[],[]) darts  
-- kcs = kite centres of larger kites,
-- dbs = dart bases of larger darts,
-- unks = unclassified dart wing tips
-- gps is a mapping of dart wing tips to the group of faces found at that vertex
  processD (kcs, dbs, unks) rd@(RD (orig, w, _)) = -- classify wing tip w
    if w `elem` kcs || w `elem` dbs then (kcs, dbs, unks) else-- already classified
    let
        Just fcs = VMap.lookup w dwFMap -- faces at w
    in
        if length fcs ==1 then (kcs, dbs, w:unks) else -- lone dart wing => unknown
        if w `elem` fmap originV (filter isKite fcs) then (kcs,w:dbs,unks) else 
                -- wing is a half kite origin => largeDartBases
        if (w,orig) `elem` fmap longE (filter isLD fcs) then (w:kcs,dbs,unks) else 
                -- long edge rd shared with an ld => largeKiteCentres
        case findFarK rd fcs of
        Nothing -> (kcs,dbs,w:unks) -- unknown if incomplete kite attached to short edge of rd
        Just rk@(RK _)  ->  
            case find (matchingShortE rk) fcs of
            Just (LK _) -> (w:kcs,dbs,unks) -- short edge rk shared with an lk => largeKiteCentres
            Just (LD _) -> (kcs,w:dbs,unks) -- short edge rk shared with an ld => largeDartBases
            _ -> let 
                     newfcs = filter (isAtV (wingV rk)) (faces g)   -- faces at rk wing    
                 in
                 case find (matchingLongE rk) newfcs of  -- short edge rk has nothing attached
                 Nothing -> (kcs,dbs,w:unks)  -- long edge of rk has nothing attached => unknown
                 Just (LD _) -> (w:kcs,dbs,unks) -- long edge rk shared with ld => largeKiteCentres
                 Just lk@(LK _) ->               -- long edge rk shared with lk
                      case find (matchingShortE lk) newfcs of
                      Just (RK _) -> (w:kcs,dbs,unks)
                              -- short edge of this lk shared with another rk => largeKiteCentres
                      Just (RD _) -> (kcs,w:dbs,unks) 
                              -- short edge of this lk shared with rd => largeDartBases
                      _ -> (kcs,dbs,w:unks) 
                              -- short edge of this lk has nothing attached => unknown

  processD (kcs, dbs, unks) ld@(LD (orig, _, w)) = -- classify wing tip w
    if w `elem` kcs || w `elem` dbs then (kcs, dbs, unks) else  -- already classified
    let
        Just fcs = VMap.lookup w dwFMap -- faces at w
    in
        if length fcs ==1 then (kcs, dbs, w:unks) else -- lone dart wing => unknown
        if w `elem` fmap originV (filter isKite fcs) then (kcs,w:dbs,unks) else
                   -- wing is a half kite origin => nodeDB
        if (w,orig) `elem` fmap longE (filter isRD fcs) then (w:kcs,dbs,unks) else
                   -- long edge ld shared with an rd => nodeKC
        case findFarK ld fcs of
          Nothing -> (kcs,dbs,w:unks) -- unknown if incomplete kite attached to short edge of ld
          Just lk@(LK _)  ->  
            case find (matchingShortE lk) fcs of
            Just (RK _) -> (w:kcs,dbs,unks) -- short edge lk shared with an rk => largeKiteCentres
            Just (RD _) -> (kcs,w:dbs,unks) -- short edge lk shared with an rd => largeDartBases
            _ -> let 
                     newfcs = filter (isAtV (wingV lk)) (faces g)   -- faces at lk wing  
                 in
                 case find (matchingLongE lk) newfcs of -- short edge lk has nothing attached
                 Nothing -> (kcs,dbs,w:unks)  -- long edge of lk has nothing attached => unknown
                 Just (RD _) -> (w:kcs,dbs,unks) -- long edge lk shared with rd => largeKiteCentres
                 Just rk@(RK _) ->               -- long edge lk is shared with an rk
                     case find (matchingShortE rk) newfcs of
                     Just (LK _) -> (w:kcs,dbs,unks)
                             -- short edge of this rk shared with another lk => largeKiteCentres
                     Just (LD _) -> (kcs,w:dbs,unks)
                             -- short edge of this rk shared with ld => largeDartBases
                     _ -> (kcs,dbs,w:unks) -- short edge of this rk has nothing attached => unknown
    -- find the two kite halves below a dart half, return the half kite furthest away (not attached to dart).
    -- Returns a Maybe.   rd produces an rk (or Nothing) ld produces an lk (or Nothing)
  findFarK :: TileFace -> [TileFace] -> Maybe TileFace
  findFarK rd@(RD _) fcs = do lk <- find (matchingShortE rd) (filter isLK fcs)
                              find (matchingJoinE lk) (filter isRK fcs)
  findFarK ld@(LD _) fcs = do rk <- find (matchingShortE ld) (filter isRK fcs)
                              find (matchingJoinE rk)  (filter isLK fcs)
  findFarK _ _ = error "getDartWingInfo: findFarK applied to non-dart face"

-- | Auxiliary function for uncheckedPartCompose.
-- Creates a list of new composed faces, each paired with a list of old faces (components of the new face)
-- using dart wing information.
composedFaceGroups :: DartWingInfo -> [(TileFace,[TileFace])]
composedFaceGroups dwInfo = faceGroupRDs ++ faceGroupLDs ++ faceGroupRKs ++ faceGroupLKs where

    faceGroupRDs = fmap (\gp -> (makeRD gp,gp)) groupRDs 
    groupRDs = mapMaybe groupRD (largeDartBases dwInfo)
    makeRD [rd,lk] = RD(originV lk, originV rd, oppV lk) 
    groupRD v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    rd <- find isRD fcs
                    lk <- find (matchingShortE rd) fcs
                    pure [rd,lk]

    faceGroupLDs = fmap (\gp -> (makeLD gp,gp)) groupLDs 
    groupLDs = mapMaybe groupLD (largeDartBases dwInfo) 
    makeLD [ld,rk] = LD(originV rk, oppV rk, originV ld)
    groupLD v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    ld <- find isLD fcs
                    rk <- find (matchingShortE ld) fcs
                    pure [ld,rk]

    faceGroupRKs = fmap (\gp -> (makeRK gp,gp)) groupRKs 
    groupRKs = mapMaybe groupRK (largeKiteCentres dwInfo) 
    makeRK [rd,lk,rk] = RK(originV rd, wingV rk, originV rk)
    groupRK v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    rd <- find isRD fcs
                    lk <- find (matchingShortE rd) fcs
                    rk <- find (matchingJoinE lk) fcs
                    pure [rd,lk,rk]

    faceGroupLKs = fmap (\gp -> (makeLK gp,gp)) groupLKs 
    groupLKs = mapMaybe groupLK (largeKiteCentres dwInfo) 
    makeLK [ld,rk,lk] = LK(originV ld, originV lk, wingV lk)
    groupLK v = do  fcs <- VMap.lookup v (faceMap dwInfo)
                    ld <- find isLD fcs
                    rk <- find (matchingShortE ld) fcs
                    lk <- find (matchingJoinE rk) fcs
                    pure [ld,rk,lk]




