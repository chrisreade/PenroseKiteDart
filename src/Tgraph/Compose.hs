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

import Data.List ((\\), find)
import qualified Data.Map as Map (Map, lookup, insert, empty)
import Data.Maybe (mapMaybe)

import Tgraph.Prelude


{-------------------------------------------------------------------------
******************************************** *****************************              
COMPOSING composeG and partCompose 
***************************************************************************
---------------------------------------------------------------------------}

-- |The main deterministic function for composing is composeG
-- which is essentially partCompose after unused faces are ignored.
composeG:: Tgraph -> Tgraph
composeG g = checkedTgraph (faces g') where
    (_, g') = partCompose g
-- composeG = snd . partCompose 

-- |partCompose produces a graph by composing faces which uniquely compose,
-- returning a pair consisting of unused faces of the original graph along with the composed graph
-- it makes use of classifyDartWings which also returns an association of faces incident with each dart wing
-- so these do not need to be reclculated.
partCompose:: Tgraph -> ([TileFace],Tgraph)
partCompose g = (remainder,checkedTgraph newFaces)
  where
    dwClass = classifyDartWings g
-- ignores unknowns
    newFaces = newRDs ++ newLDs ++ newRKs ++ newLKs
    remainder = faces g \\ concat (groupRDs ++ groupLDs ++ groupRKs ++ groupLKs)

    newRDs = fmap makeRD groupRDs 
    groupRDs = mapMaybe groupRD (largeDartBases dwClass)
    makeRD [rd,lk] = RD(originV lk, originV rd, oppV lk) 
    groupRD v = do  fcs <- Map.lookup v (vGroup dwClass)
                    rd <- find isRD fcs
                    lk <- find (matchingShortE rd) fcs
                    return [rd,lk]

    newLDs = fmap makeLD groupLDs
    groupLDs = mapMaybe groupLD (largeDartBases dwClass) 
    makeLD [ld,rk] = LD(originV rk, oppV rk, originV ld)
    groupLD v = do  fcs <- Map.lookup v (vGroup dwClass)
                    ld <- find isLD fcs
                    rk <- find (matchingShortE ld) fcs
                    return [ld,rk]

    newRKs = fmap makeRK groupRKs 
    groupRKs = mapMaybe groupRK (largeKiteCentres dwClass) 
    makeRK [rd,lk,rk] = RK(originV rd, wingV rk, originV rk)
    groupRK v = do  fcs <- Map.lookup v (vGroup dwClass)
                    rd <- find isRD fcs
                    lk <- find (matchingShortE rd) fcs
                    rk <- find (matchingJoinE lk) fcs
                    return [rd,lk,rk]

    newLKs = fmap makeLK groupLKs 
    groupLKs = mapMaybe groupLK (largeKiteCentres dwClass) 
    makeLK [ld,rk,lk] = LK(originV ld, originV lk, wingV lk)
    groupLK v = do  fcs <- Map.lookup v (vGroup dwClass)
                    ld <- find isLD fcs
                    rk <- find (matchingShortE ld) fcs
                    lk <- find (matchingJoinE rk) fcs
                    return [ld,rk,lk]

-- |DWClass is a record type for the result of classifying dart wings in a Tgraph.
--  It also records vGroup - a mapping of each dart wing vertex
--  in the Tgraph to the faces incident with the vertex. 
-- This is to save recalculating the face groups when composing with partCompose.
data DWClass = DWClass { largeKiteCentres  :: [Vertex]
                       , largeDartBases  :: [Vertex]
                       , unknowns :: [Vertex]
                       , vGroup :: Mapping Vertex [TileFace]
                       } deriving Show
                       
-- |classifyDartWings classifies all dart wing tips.
-- The result is a DWClass record of largeKiteCentres, largeDartBases, unknowns
-- and a mapping where
-- largeKiteCentres are new kite centres, largeDartBases are new dart bases
-- unknowns cannot be classified, and
-- the mapping identifies the faces found at each vertex in both largeKiteCentres and largeDartBases.
-- This information is recorded to make partCompose more efficient.
classifyDartWings :: Tgraph -> DWClass
classifyDartWings g = DWClass {largeKiteCentres = kcs, largeDartBases = dbs, unknowns = unks
                              , vGroup = gps
                              } where
  (kcs,dbs,unks,gps) = foldl (processD g) ([],[],[],Map.empty) (rdarts g ++ ldarts g)

-- kcs = kite centres of larger kites,
-- dbs = dart bases of larger darts,
-- unks = unclassified dart wing tips
-- gps is a mapping of dart wing tips to the group of faces found at that vertex
  processD g (kcs, dbs, unks, gps) rd@(RD (orig, w, _))-- classify wing tip w
    | valencyD g w == 2 = (kcs, dbs, w : unks, gps) -- lone dart wing => unknown
    | w `elem` kcs || w `elem` dbs = (kcs, dbs, unks, gps) -- already classified
    | otherwise
    = let
        fcs = filter (isAtV w) (faces g)  -- faces at w
        newgps = Map.insert w fcs gps -- (w,fcs):gps
      in
        if w `elem` fmap originV (filter isKite fcs) then (kcs,w:dbs,unks,newgps) else 
                -- wing is a half kite origin => largeDartBases
        if (w,orig) `elem` fmap longE (filter isLD fcs) then (w:kcs,dbs,unks,newgps) else 
                -- long edge rd shared with an ld => largeKiteCentres
        case findFarK rd fcs of
        Nothing -> (kcs,dbs,w:unks,gps) -- unknown if incomplete kite attached to short edge of rd
        Just rk@(RK _)  ->  
            case find (matchingShortE rk) fcs of
            Just (LK _) -> (w:kcs,dbs,unks,newgps) -- short edge rk shared with an lk => largeKiteCentres
            Just (LD _) -> (kcs,w:dbs,unks,newgps) -- short edge rk shared with an ld => largeDartBases
            _ -> let 
                     newfcs = filter (isAtV (wingV rk)) (faces g)   -- faces at rk wing    
                 in
                 case find (matchingLongE rk) newfcs of  -- short edge rk has nothing attached
                 Nothing -> (kcs,dbs,w:unks,gps)  -- long edge of rk has nothing attached => unknown
                 Just (LD _) -> (w:kcs,dbs,unks,newgps) -- long edge rk shared with ld => largeKiteCentres
                 Just lk@(LK _) ->               -- long edge rk shared with lk
                      case find (matchingShortE lk) newfcs of
                      Just (RK _) -> (w:kcs,dbs,unks,newgps)
                              -- short edge of this lk shared with another rk => largeKiteCentres
                      Just (RD _) -> (kcs,w:dbs,unks,newgps) 
                              -- short edge of this lk shared with rd => largeDartBases
                      _ -> (kcs,dbs,w:unks,gps) 
                              -- short edge of this lk has nothing attached => unknown

  processD g (kcs, dbs, unks, gps) ld@(LD (orig, _, w))-- classify wing tip w
    | valencyD g w == 2 = (kcs, dbs, w : unks, gps) -- lone dart wing => unknown
    | w `elem` kcs || w `elem` dbs = (kcs, dbs, unks, gps) -- already classified
    | otherwise
    = let
        fcs = filter (isAtV w) (faces g) -- faces at w
        newgps = Map.insert w fcs gps -- (w,fcs):gps
      in
        if w `elem` fmap originV (filter isKite fcs) then (kcs,w:dbs,unks,newgps) else
                   -- wing is a half kite origin => nodeDB
        if (w,orig) `elem` fmap longE (filter isRD fcs) then (w:kcs,dbs,unks,newgps) else
                   -- long edge ld shared with an rd => nodeKC
        case findFarK ld fcs of
          Nothing -> (kcs,dbs,w:unks,gps) -- unknown if incomplete kite attached to short edge of ld
          Just lk@(LK _)  ->  
            case find (matchingShortE lk) fcs of
            Just (RK _) -> (w:kcs,dbs,unks,newgps) -- short edge lk shared with an rk => largeKiteCentres
            Just (RD _) -> (kcs,w:dbs,unks,newgps) -- short edge lk shared with an rd => largeDartBases
            _ -> let 
                     newfcs = filter (isAtV (wingV lk)) (faces g)   -- faces at lk wing  
                 in
                 case find (matchingLongE lk) newfcs of -- short edge lk has nothing attached
                 Nothing -> (kcs,dbs,w:unks,gps)  -- long edge of lk has nothing attached => unknown
                 Just (RD _) -> (w:kcs,dbs,unks,newgps) -- long edge lk shared with rd => largeKiteCentres
                 Just rk@(RK _) ->               -- long edge lk is shared with an rk
                     case find (matchingShortE rk) newfcs of
                     Just (LK _) -> (w:kcs,dbs,unks,newgps)
                             -- short edge of this rk shared with another lk => largeKiteCentres
                     Just (LD _) -> (kcs,w:dbs,unks,newgps)
                             -- short edge of this rk shared with ld => largeDartBases
                     _ -> (kcs,dbs,w:unks,gps) -- short edge of this rk has nothing attached => unknown


    -- find the two kite halves below a dart half, return the half kite furthest away (not attached to dart).
    -- Returns a Maybe.   rd produces an rk (or Nothing) ld produces an lk (or Nothing)
  findFarK :: TileFace -> [TileFace] -> Maybe TileFace
  findFarK rd@(RD _) fcs = do lk <- find (matchingShortE rd) (filter isLK fcs)
                              find (matchingJoinE lk) (filter isRK fcs)
  findFarK ld@(LD _) fcs = do rk <- find (matchingShortE ld) (filter isRK fcs)
                              find (matchingJoinE rk)  (filter isLK fcs)
  findFarK _ _ = error "findFarK: applied to non-dart face"



