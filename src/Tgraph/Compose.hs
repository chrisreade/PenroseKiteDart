{-|
Module      : Tgraph.Compose
Description : A compose operation for Tgraphs
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module includes the main composition operation composeG but also exposes 
classifyDartWings (and type DWClass) for debugging and experimenting
-}
module Tgraph.Compose where

import Data.List ((\\), find, foldl',nub)
import qualified Data.IntMap.Strict as VMap (lookup) -- used in partCompose
import Data.Maybe (mapMaybe)

import Tgraph.Prelude


{-------------------------------------------------------------------------
******************************************** *****************************              
COMPOSING composeG and partCompose 
***************************************************************************
---------------------------------------------------------------------------}

-- |A deterministic function for composing is composeG which makes no choices when composing
-- (producing the meet of possible choices).
-- which is essentially partCompose after uncomposed faces are ignored.
-- If the result fails to be connected or has crossing boundaries an error is raised.
composeG:: Tgraph -> Tgraph
composeG = snd . partCompose

-- |partCompose produces a Tgraph by composing faces which uniquely compose,
-- returning a pair consisting of unused faces of the original graph along with the composed Tgraph.
-- It checks the Tgraph for connectedness and no crossing boundaries raising an error if this check fails.
-- (It makes use of classifying dart wings)
partCompose:: Tgraph -> ([TileFace],Tgraph)
partCompose g = (remainder,g')
  where
    g' = getResult $ checkConnectedNoCross $ 
          Tgraph { faces = newFaces, maxV = maxVertex }
    newFaces = newRDs ++ newLDs ++ newRKs ++ newLKs
    remainder = faces g \\ concat (groupRDs ++ groupLDs ++ groupRKs ++ groupLKs)
    maxVertex = if null newFaces then 0 else facesMaxV newFaces

    darts  = filter isDart (faces g)
    dwFMap = makeVFMapFor (nub $ fmap wingV darts) (faces g)
    dwClass = classifyWith g dwFMap darts
    -- ignore unknowns

    newRDs = fmap makeRD groupRDs 
    groupRDs = mapMaybe groupRD (largeDartBases dwClass)
    makeRD [rd,lk] = RD(originV lk, originV rd, oppV lk) 
    groupRD v = do  fcs <- VMap.lookup v dwFMap
                    rd <- find isRD fcs
                    lk <- find (matchingShortE rd) fcs
                    return [rd,lk]

    newLDs = fmap makeLD groupLDs
    groupLDs = mapMaybe groupLD (largeDartBases dwClass) 
    makeLD [ld,rk] = LD(originV rk, oppV rk, originV ld)
    groupLD v = do  fcs <- VMap.lookup v dwFMap
                    ld <- find isLD fcs
                    rk <- find (matchingShortE ld) fcs
                    return [ld,rk]

    newRKs = fmap makeRK groupRKs 
    groupRKs = mapMaybe groupRK (largeKiteCentres dwClass) 
    makeRK [rd,lk,rk] = RK(originV rd, wingV rk, originV rk)
    groupRK v = do  fcs <- VMap.lookup v dwFMap
                    rd <- find isRD fcs
                    lk <- find (matchingShortE rd) fcs
                    rk <- find (matchingJoinE lk) fcs
                    return [rd,lk,rk]

    newLKs = fmap makeLK groupLKs 
    groupLKs = mapMaybe groupLK (largeKiteCentres dwClass) 
    makeLK [ld,rk,lk] = LK(originV ld, originV lk, wingV lk)
    groupLK v = do  fcs <- VMap.lookup v dwFMap
                    ld <- find isLD fcs
                    rk <- find (matchingShortE ld) fcs
                    lk <- find (matchingJoinE rk) fcs
                    return [ld,rk,lk]


-- |An experimental version of composition which defaults to kites when there are choices (unknowns).
-- This can create an incorrect Tgraph from a correct Tgraph.
-- It uses partkCompose.
composeK :: Tgraph -> Tgraph
composeK = snd . partComposeK

-- |partkCompose is experimental and only used by composeK.
-- It produces a Tgraph by composing faces which can be composed and defaulting to half kites
-- in preference to half darts when there is a choice (unknowns).
-- It returns a pair consisting of unused faces of the original graph along with the composed Tgraph.
-- It checks the Tgraph for connectedness and no crossing boundaries raising an error if this check fails.
-- (It makes use of classifying dart wings)
partComposeK:: Tgraph -> ([TileFace],Tgraph)
partComposeK g = (remainder,g')
  where
    g' = getResult $ checkConnectedNoCross $ 
          Tgraph { faces = newFaces, maxV = maxVertex }
    newFaces = newRDs ++ newLDs ++ newRKs ++ newLKs
    remainder = faces g \\ concat (groupRDs ++ groupLDs ++ groupRKs ++ groupLKs)
    maxVertex = if null newFaces then 0 else facesMaxV newFaces

    darts  = filter isDart (faces g)
    dwFMap = makeVFMapFor (nub $ fmap wingV darts) (faces g)
    dwClass = classifyWith g dwFMap darts
    -- ignore unknowns

    newRDs = fmap makeRD groupRDs 
    groupRDs = mapMaybe groupRD (largeDartBases dwClass)
    makeRD [rd,lk] = RD(originV lk, originV rd, oppV lk) 
    groupRD v = do  fcs <- VMap.lookup v dwFMap
                    rd <- find isRD fcs
                    lk <- find (matchingShortE rd) fcs
                    return [rd,lk]

    newLDs = fmap makeLD groupLDs
    groupLDs = mapMaybe groupLD (largeDartBases dwClass) 
    makeLD [ld,rk] = LD(originV rk, oppV rk, originV ld)
    groupLD v = do  fcs <- VMap.lookup v dwFMap
                    ld <- find isLD fcs
                    rk <- find (matchingShortE ld) fcs
                    return [ld,rk]

    newRKs = fmap makeRK groupRKs 
    groupRKs = mapMaybe groupRK (largeKiteCentres dwClass ++ unknowns dwClass) 
    makeRK [rd,lk,rk] = RK(originV rd, wingV rk, originV rk)
    groupRK v = do  fcs <- VMap.lookup v dwFMap
                    rd <- find isRD fcs
                    lk <- find (matchingShortE rd) fcs
                    rk <- find (matchingJoinE lk) fcs
                    return [rd,lk,rk]

    newLKs = fmap makeLK groupLKs 
    groupLKs = mapMaybe groupLK (largeKiteCentres dwClass ++ unknowns dwClass) 
    makeLK [ld,rk,lk] = LK(originV ld, originV lk, wingV lk)
    groupLK v = do  fcs <- VMap.lookup v dwFMap
                    ld <- find isLD fcs
                    rk <- find (matchingShortE ld) fcs
                    lk <- find (matchingJoinE rk) fcs
                    return [ld,rk,lk]


-- |DartWingClass is a record type for the result of classifying dart wings in a Tgraph.
data DartWingClass = 
     DartWingClass { largeKiteCentres  :: [Vertex]
                   , largeDartBases  :: [Vertex]
                   , unknowns :: [Vertex]
                   } deriving Show

-- | classifyWith g dwFMap darts, where darts are the darts from Tgraph g and dwFMap is a VertexMap
-- from the dart wings to faces at that vertex in g.
-- It produces a classification of the dart wings as a DartWingClass
classifyWith :: Tgraph -> VertexMap [TileFace] -> [TileFace] -> DartWingClass
classifyWith g dwFMap darts =
  DartWingClass {largeKiteCentres = kcs, largeDartBases = dbs, unknowns = unks} where
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
  findFarK _ _ = error "findFarK: applied to non-dart face"


-- |For interactive use, classifyDartWings g produces the classification of the dart wings in g
classifyDartWings :: Tgraph -> DartWingClass
classifyDartWings g = classifyWith g dwFMap darts where
   darts  = filter isDart (faces g)
   dwFMap = makeVFMapFor (nub $ fmap wingV darts) (faces g)
   






