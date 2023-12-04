{-|
Module      : Tgraph.Force
Description : The force functions for Tgraphs 
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module includes force and tryForce plus related operations for testing and experimenting.
It introduces BoundaryState and ForceState types and includes a Forcible class with instances for
Tgraph, BoundaryState, and ForceState.
It exposes the calculation of relative angle of edges at boundary vertices used to find existing edges.
It imports a touching check for adding new vertices (with locateVertices and addVPoint).
-}
-- {-# LANGUAGE BangPatterns #-}

module Tgraph.Force  where

import Data.List ((\\), intersect, nub, find,foldl')
import qualified Data.Map as Map (Map, empty, delete, elems, insert, union, keys) -- used for UpdateMap
import qualified Data.IntMap.Strict as VMap (elems, filterWithKey, alter, delete, lookup, (!))
            -- used for BoundaryState locations AND faces at boundary vertices
import Diagrams.Prelude (Point, V2) -- necessary for touch check (touchCheck) used in tryUnsafeUpdate 
import Tgraph.Convert(touching, locateVertices, addVPoint)
import Tgraph.Prelude

{-
***************************************************************************   
Efficient FORCING with 
  BoundaryState, ForceState 
  Touching Vertex Check
  Incremented Update Maps
***************************************************************************
-}

{-*
Touching vertex checking
-}

{-------------------------
*************************             
Touching vertex checking 
********************************************
requires Diagrams.Prelude for Point and V2
--------------------------------------------}

-- |touchCheck p vpMap - check if a vertex location p touches (is too close to) any other vertex location in the mapping vpMap
touchCheck:: Point V2 Double -> VertexMap (Point V2 Double) -> Bool
touchCheck p vpMap = any (touching p) (VMap.elems vpMap)

{-*
BoundaryState operations
-}
{-| A BoundaryState records
the boundary directed edges (directed so that faces are on LHS and exterior is on RHS)
plus 
a mapping of boundary vertices to their incident faces, plus
a mapping of boundary vertices to positions (using Tgraph.Prelude.locateVertices).
It also keeps track of all the faces
and the next vertex label to be used when adding a new vertex.
-}
data BoundaryState 
   = BoundaryState
     { boundary:: [Dedge]  -- ^ boundary directed edges (face on LHS, exterior on RHS)
     , bvFacesMap:: VertexMap [TileFace] -- ^faces at each boundary vertex.
     , bvLocMap:: VertexMap (Point V2 Double)  -- ^ position of each boundary vertex.
     , allFaces:: [TileFace] -- ^ all the tile faces
     , nextVertex:: Vertex -- ^ next vertex number
     } deriving (Show)

-- |Calculates BoundaryState information from a Tgraph
-- also checks for no crossing boundaries as these could cause difficult to trace errors in forcing.
makeBoundaryState:: Tgraph -> BoundaryState
makeBoundaryState g = 
  let bdes = graphBoundary g
      bvs = fmap fst bdes -- (fmap snd bdes would also do) for all boundary vertices
      bvLocs = VMap.filterWithKey (\k _ -> k `elem` bvs) $ locateVertices $ faces g
  in if not $ null $ crossingVertices bdes then error $ "makeBoundaryState: found crossing boundary in faces:\n"++show (faces g)++"\n"
     else
      BoundaryState
      { boundary = bdes
      , bvFacesMap = vertexFacesMap bvs (faces g)
      , bvLocMap = bvLocs 
      , allFaces = faces g
      , nextVertex = 1+ maxV g
      }
      
-- |Converts a BoundaryState back to a Tgraph
recoverGraph:: BoundaryState -> Tgraph
recoverGraph bd = makeUncheckedTgraph (allFaces bd)

-- |changeVFMap f vfmap - adds f to the list of faces associated with each v in f, returning a revised vfmap
changeVFMap::  TileFace -> VertexMap [TileFace] -> VertexMap [TileFace]
changeVFMap f vfm = foldl' insertf vfm (faceVList f) where
   insertf vmap v = VMap.alter consf v vmap
   consf Nothing = Just [f]
   consf (Just fs) = Just (f:fs)
   
-- |facesAtBV bd v - returns the faces found at v (which must be a boundary vertex)
facesAtBV:: BoundaryState -> Vertex -> [TileFace]
facesAtBV bd v = case VMap.lookup v (bvFacesMap bd) of
  Just fcs -> fcs
  Nothing -> error $ "facesAtBV: Not a boundary vertex? No faces found at " ++ show v ++ "\n"

-- |return a list of faces which have a boundary vertex from a BoundaryState
boundaryFaces :: BoundaryState -> [TileFace]
boundaryFaces bd = nub $ concatMap (facesAtBV bd) bvs where
    bvs = fmap fst $ boundary bd
-- boundaryFaces = nub . concat . VMap.elems . bvFacesMap 
-- relies on the map containing no extra info for non boundary vertices



{-*
Updates and ForceState types
-}

-- |An Update is either safe or unsafe.
-- A safe update has a new face involving 3 existing vertices.
-- An unsafe update has a makeFace function to create the new face when given a fresh third vertex.
data Update = SafeUpdate TileFace 
            | UnsafeUpdate (Vertex -> TileFace)

-- | 0 is used as a dummy variable to show unsafe updates (to display the function explicitly)
instance Show Update where
    show (SafeUpdate f) = "SafeUpdate (" ++ show f ++ ")"
    show (UnsafeUpdate mf) = "UnsafeUpdate (\0 -> " ++ show (mf 0)++ ")"
    
-- |UpdateMap: partial map associating updates with (some) boundary directed edges.
type UpdateMap = Map.Map Dedge Update

-- |ForceState: The force state records information between executing single face updates during forcing
-- (a BoundaryState and an UpdateMap).
data ForceState = ForceState 
                   { boundaryState:: BoundaryState
                   , updateMap:: UpdateMap 
                   }

{-|UpdateGenerator abbreviates the type of functions which capture one or more of the forcing rules.
They produce a (Try) UpdateMap when given a BoundaryState and a focus list of particular directed boundary edges.  
Each forcing rule has a particular UpdateGenerator,
but they can also be combined (e.g in sequence - allUGenerator or otherwise - defaultAllUGenerator).
-}
type UpdateGenerator = BoundaryState -> [Dedge] -> Try UpdateMap


{-*
Forcible class 
-}

-- | Forcible class has operations to (indirectly) implement forcing and single step forcing
-- (tryForceWith, tryStepForceWith) for any Forcible. The class operations are more general to allow for other
-- force related operations to be generalised for use on any Forcible.
-- Both tryForceWith and tryStepForceWith are implemented using tryFSOpWith, and
-- tryAddHalfKite and tryAddHalfDart are implemented using tryChangeBoundaryWith.
--
-- To improve performance of a sequence of force related operations, express each as a
-- ForceState -> Try ForceState, then use (<=<) or (>=>) to combine and pass to tryFSOpWith.
-- This ensures there are no unnecessary conversions between steps.
class Forcible a where
    -- | tryFSOpWith (try ForseState Operation with) when given an update generator, generalises a ForceState operation to a Forcible operation.
    -- The update generator is only used to initialise a ForceState when there is not one
    -- already available (i.e not used when the Forcible is a ForceState)
    tryFSOpWith :: UpdateGenerator -> (ForceState -> Try ForceState) -> a -> Try a
    -- | tryInitFSWith (try initialising a ForceState with) when given an update generator tries to create an initial ForceState (ready for forcing) from a Forcible.
    -- Again, the update generator is not used when the instance is a ForceState.
    tryInitFSWith :: UpdateGenerator -> a -> Try ForceState
    -- | tryChangeBoundaryWith when given an update generator, converts a (try) BoundaryState changing operation to a (try) Forcible operation.
    -- The update generator is only used when the instance is a ForceState (to revise the update map in the result).
    tryChangeBoundaryWith :: UpdateGenerator -> (BoundaryState -> Try BoundaryChange) -> a -> Try a
    -- | construct or recover the BoundaryState from a Forcible
    getBoundaryState :: a -> BoundaryState

-- |ForceStates are Forcible
instance Forcible ForceState where
    tryFSOpWith _ = id  -- update generator not used
    tryInitFSWith _  = return  -- update generator not used
    tryChangeBoundaryWith ugen f fs = do
        bdC <- f (boundaryState fs)
        tryReviseFSWith ugen bdC fs
    getBoundaryState = boundaryState
    
-- | BoundaryStates are Forcible    
instance Forcible BoundaryState where
    tryFSOpWith ugen f bd = do
        fs <- tryInitFSWith ugen bd
        fs' <- f fs
        return $ boundaryState fs'
    tryInitFSWith ugen bd = do
        umap <- ugen bd (boundary bd)
        return $ ForceState { boundaryState = bd , updateMap = umap }
    tryChangeBoundaryWith _ f bd = do -- update generator not used
        bdC <- f bd
        return $ newBoundaryState bdC
    getBoundaryState = id

-- | Tgraphs are Forcible    
instance Forcible Tgraph where
    tryFSOpWith ugen f g = recoverGraph <$> tryFSOpWith ugen f (makeBoundaryState g)
    tryInitFSWith ugen g = tryInitFSWith ugen (makeBoundaryState g)
    tryChangeBoundaryWith ugen f g = -- update generator not used
        recoverGraph <$> tryChangeBoundaryWith ugen f (makeBoundaryState g)
    getBoundaryState = makeBoundaryState

{-*
Generalised forcing operations
-}

-- | try forcing using a given UpdateGenerator.
--  tryForceWith uGen fs - recursively does updates using uGen until there are no more updates.
--  It produces Left report if it encounters a Forcible representing a stuck/incorrect Tgraph.
tryForceWith :: Forcible a => UpdateGenerator -> a -> Try a
tryForceWith ugen = tryFSOpWith ugen (tryForceStateWith ugen) where
--    tryForceStateWith :: UpdateGenerator -> ForceState -> Try ForceState
    tryForceStateWith uGen = retry where
      retry fs = case findSafeUpdate (updateMap fs) of
                 Just u -> do bdChange <- trySafeUpdate (boundaryState fs) u
                              fs' <- tryReviseFSWith uGen bdChange fs
                              retry fs'
                 _  -> do maybeBdC <- tryUnsafes fs
                          case maybeBdC of
                            Nothing  -> Right fs -- no more updates
                         -- Nothing -> tryFinalStuckCheck fs  -- No Longer used
                            Just bdC -> do fs' <- tryReviseFSWith uGen bdC fs
                                           retry fs'
    
-- | try a given number of force steps using a given UpdateGenerator.
tryStepForceWith :: Forcible a => UpdateGenerator -> Int -> a -> Try a
tryStepForceWith ugen n = tryFSOpWith ugen $ tryStepForceStateWith ugen n where
--    tryStepForceStateWith :: UpdateGenerator -> Int -> ForceState -> Try ForceState
    tryStepForceStateWith updateGen n = count n where
      count 0 fs = return fs
      count n fs = do result <- tryOneStepWith updateGen fs
                      case result of
                       Nothing -> return fs
                       Just (fs', _) ->  count (n-1) fs' 
    
-- |A version of tryFSOpWith using defaultAllUGen representing all 10 rules for updates.
tryFSOp :: Forcible a => (ForceState -> Try ForceState) -> a -> Try a
tryFSOp = tryFSOpWith defaultAllUGen

-- |A version of the main force function using defaultAllUGen representing all 10 rules for updates.
-- This returns Left report on discovering a stuck Tgraph and Right a (with a the resulting forcible) otherwise.
tryForce:: Forcible a => a -> Try a
tryForce = tryForceWith defaultAllUGen

-- |The main force function using defaultAllUGen representing all 10 rules for updates.
-- This raises an error on discovering a stuck/incorrect Forcible.
force:: Forcible a => a -> a
force = runTry . tryForce

-- |special case of forcing only half tiles to whole tiles
wholeTiles:: Forcible a => a -> a
wholeTiles = forceWith wholeTileUpdates 

-- | forceWith ugen: force using the given UpdateGenerator
forceWith:: Forcible a => UpdateGenerator -> a -> a
forceWith ugen = runTry . tryForceWith ugen

-- | try to initialize a force state with the default UpdateGenerator.
-- Returns a Left report if it finds a stuck forcible.
tryInitFS :: Forcible a => a -> Try ForceState
tryInitFS = tryInitFSWith defaultAllUGen

-- | initialize a force state with the default UpdateGenerator.
-- Raises aan error if it finds a stuck forcible.
initFS :: Forcible a => a -> ForceState
initFS = runTry . tryInitFS

-- |tryStepForce produces a (Right) intermediate state after a given number of steps (face additions)
-- or a Left report if it encounters a stuck/incorrect Tgraph/forcible
tryStepForce :: Forcible a => Int -> a -> Try a 
tryStepForce = tryStepForceWith defaultAllUGen-- Was called tryStepForceFrom

-- |stepForce  produces an intermediate state after a given number of steps (face additions).
-- It raises an error if it encounters a stuck/incorrect Forcible
stepForce :: Forcible a => Int -> a ->  a
stepForce n = runTry . tryStepForce n

-- |specialises tryChangeBoundaryWith to the default update generator.
tryChangeBoundary:: Forcible a => (BoundaryState -> Try BoundaryChange) -> a -> Try a
tryChangeBoundary = tryChangeBoundaryWith defaultAllUGen

{-*
Force Related Functions:
addHalfKite, addHalfDart, tryAddHalfKite, tryAddHalfDart
-}


-- |addHalfKite is for adding a single half kite on a chosen boundary Dedge of a Forcible.
-- The Dedge must be a boundary edge but the direction is not important as
-- the correct direction is automatically calculated.
-- It will raise an error if the edge is a dart join or if a conflict (stuck graph) is detected
-- or if the edge is not a boundary edge.
addHalfKite :: Forcible a => Dedge -> a -> a
addHalfKite e  = runTry . tryAddHalfKite e

-- |tryAddHalfKite is a version of addHalfKite which returns a Try
-- with a Left report if it finds a stuck/incorrect graph, or 
-- if the edge is a dart join, or
-- if the edge is not a boundary edge.   
tryAddHalfKite :: Forcible a => Dedge -> a -> Try a
tryAddHalfKite = tryChangeBoundary . tryAddHalfKiteBoundary where
-- |tryAddHalfKiteBoundary implements tryAddHalfKite as a BoundaryState change
-- tryAddHalfKiteBoundary :: Dedge -> BoundaryState -> Try BoundaryChange
    tryAddHalfKiteBoundary e bd = 
      do de <- case [e, reverseD e] `intersect` boundary bd of
                 [de] -> Right de
                 _ -> Left $ "tryAddHalfKiteBoundary:  on non-boundary edge " ++ show e ++ "\n"
         let (fc,etype) = inspectBDedge bd de
         let tryU | etype == Long = addKiteLongE bd fc
                  | etype == Short = addKiteShortE bd fc
                  | etype == Join && isKite fc = completeHalf bd fc
                  | otherwise = Left "tryAddHalfKiteBoundary: applied to dart join (not possible).\n"
         u <- tryU
         tryUpdate bd u

-- |addHalfDart is for adding a single half dart on a chosen boundary Dedge of a Forcible.
-- The Dedge must be a boundary edge but the direction is not important as
-- the correct direction is automatically calculated.
-- It will raise an error if the edge is a dart short edge or kite join
-- or if a conflict (stuck graph) is detected or if
-- the edge is not a boundary edge.
addHalfDart :: Forcible a => Dedge -> a -> a
addHalfDart e = runTry . tryAddHalfDart e
  
-- |tryAddHalfDart is a version of addHalfDart which returns a Try
-- with a Left report if it finds a stuck/incorrect graph, or
-- if the edge is a dart short edge or kite join, or
-- if the edge is not a boundary edge.
tryAddHalfDart :: Forcible a => Dedge -> a -> Try a
tryAddHalfDart = tryChangeBoundary . tryAddHalfDartBoundary where
-- |tryAddHalfDartBoundary implements tryAddHalfDart as a BoundaryState change
-- tryAddHalfDartBoundary :: Dedge -> BoundaryState -> Try BoundaryChange
    tryAddHalfDartBoundary e bd = 
      do de <- case [e, reverseD e] `intersect` boundary bd of
                [de] -> Right de
                _ -> Left $ "tryAddHalfDartBoundary:  on non-boundary edge " ++ show e  ++ "\n"
         let (fc,etype) = inspectBDedge bd de
         let tryU | etype == Long = addDartLongE bd fc
                  | etype == Short && isKite fc = addDartShortE bd fc
                  | etype == Join && isDart fc = completeHalf bd fc
                  | otherwise = Left "tryAddHalfDartBoundary: applied to short edge of dart or to kite join (not possible).\n"
         u <- tryU
         tryUpdate bd u

{-*
Specialised forcing operations (used for inspecting steps)
-}

-- |tryOneStepWith uGen fs does one force step.
-- It returns a (Try maybe) with a new force sate paired with the boundary change for debugging purposes.
-- It uses uGen to revise updates in the final ForceState. 
-- It produces Left report for a stuck/incorrect graph.
-- A result of Right Nothing indicates forcing has finished and there are no more updates.
tryOneStepWith :: UpdateGenerator -> ForceState -> Try (Maybe (ForceState,BoundaryChange))
tryOneStepWith uGen fs = 
      case findSafeUpdate (updateMap fs) of
      Just u -> do bdChange <- trySafeUpdate (boundaryState fs) u
                   fs' <- tryReviseFSWith uGen bdChange fs
                   return $ Just (fs',bdChange)
      _  -> do maybeBdC <- tryUnsafes fs
               case maybeBdC of
                Just bdC -> do fs' <- tryReviseFSWith uGen bdC fs
                               return $ Just (fs',bdC)
                Nothing  -> return Nothing           -- no more updates

-- |tryOneStepF is a special case of tryOneStepWith only used for debugging
tryOneStepF :: ForceState -> Try (Maybe (ForceState,BoundaryChange))
tryOneStepF = tryOneStepWith defaultAllUGen



{-*
Updating BoundaryState and ForceState after a single force step
-}


{-| BoundaryChange records the new boundary state after completing an update (by either trySafeUpdate or tryUnsafeUpdate)
     along with a list of directed edges which are no longer on the boundary,
     plus a list of boundary edges revised (and requiring updates to be recalculated).
     See affectedBoundary.
-}
data BoundaryChange = BoundaryChange 
                       { newBoundaryState:: BoundaryState -- ^ resulting boundary state
                       , removedEdges:: [Dedge] -- ^ edges no longer on the boundary
                       , revisedEdges :: [Dedge]  -- ^ boundary edges requiring new update calculations
                       , newFace :: TileFace -- ^ face added in the change
                       } deriving (Show)

{-| Given a BoundaryState with a list of one new boundary edge or
     two adjacent new boundary edges (or exceptionally no new boundary edges) to be added,
     it extends the list with adjacent boundary edges (to produce 3 or 4 or none).
     (Used to calculate revisedEdges in a BoundaryChange)
     (When a face is fitted in to a hole with 3 sides there is no new boundary.)
-}
affectedBoundary :: BoundaryState -> [Dedge] -> [Dedge]
affectedBoundary bd [(a,b)] = [(x,a),(a,b),(b,y)] where
           bdry = boundary bd
           (x,_) = mustFind ((==a).snd) bdry (error $ "affectedBoundary: boundary edge not found with snd = " ++ show a ++ "\n")
           (_,y) = mustFind ((==b).fst) bdry (error $ "affectedBoundary: boundary edge not found with fst = " ++ show b ++ "\n")
affectedBoundary bd [(a,b),(c,d)] | b==c = [(x,a),(a,b),(c,d),(d,y)] where
           bdry = boundary bd
           (x,_) = mustFind ((==a).snd) bdry (error $ "affectedBoundary: boundary edge not found with snd = " ++ show a ++ "\n")
           (_,y) = mustFind ((==d).fst) bdry (error $ "affectedBoundary: boundary edge not found with fst = " ++ show d ++ "\n")
affectedBoundary bd [(a,b),(c,d)] | a==d = [(x,c),(c,d),(a,b),(b,y)] where
           bdry = boundary bd
           (x,_) = mustFind ((==c).snd) bdry (error $ "affectedBoundary: boundary edge not found with snd = " ++ show c ++ "\n")
           (_,y) = mustFind ((==b).fst) bdry (error $ "affectedBoundary: boundary edge not found with fst = " ++ show b ++ "\n")
affectedBoundary _ [] = []
affectedBoundary _ edges = error $ "affectedBoundary: unexpected new boundary edges " ++ show edges ++ "\n"

{-| mustFind is an auxiliary function used to search with definite result.
mustFind p ls err returns the first item in ls satisfying predicate p and returns
err argument when none found (in finite cases).    
-}
mustFind :: Foldable t => (p -> Bool) -> t p -> p -> p
mustFind p ls err
  = maybe err id (find p ls)

-- |tryReviseUpdates uGen bdChange: revises the UpdateMap after boundary change (bdChange)
-- using uGen to calculate new updates.
tryReviseUpdates:: UpdateGenerator -> BoundaryChange -> UpdateMap -> Try UpdateMap
tryReviseUpdates uGen bdChange umap = 
  do let umap' = foldr Map.delete umap (removedEdges bdChange)
     umap'' <- uGen (newBoundaryState bdChange) (revisedEdges bdChange) 
     return (Map.union umap'' umap')

-- |tryReviseFSWith ugen bdC fs tries to revise fs after a boundary change (bdC) by calculating
-- the revised updates with ugen (and using the new boundary state in bdC).
tryReviseFSWith :: UpdateGenerator -> BoundaryChange -> ForceState -> Try ForceState
tryReviseFSWith ugen bdC fs =
    do umap <- tryReviseUpdates ugen bdC (updateMap fs)
       return $ ForceState{ boundaryState = newBoundaryState bdC, updateMap = umap}

{-*
Auxiliary Functions for doing a force step
-}

-- |finds the first safe update - Nothing if there are none (ordering is directed edge key ordering)
findSafeUpdate:: UpdateMap -> Maybe Update 
findSafeUpdate umap = find isSafeUpdate (Map.elems umap) where
  isSafeUpdate (SafeUpdate _ ) = True
  isSafeUpdate (UnsafeUpdate _ ) = False

{-| tryUnsafes: Should only be used when there are no Safe updates in the UpdateMap.
   tryUnsafes works through the unsafe updates in (directed edge) key order and
   completes the first unsafe update that is not blocked (by a touching vertex), returning Right (Just bdC)
   where bdC is the resulting boundary change (if there is one).
   It returns Right Nothing if there are no unsafe updates but
   Left report if there are unsafes but all unsafes are blocked, where report describes the problem.
-}
tryUnsafes:: ForceState -> Try (Maybe BoundaryChange)
tryUnsafes fs = checkBlocked False $ Map.elems $ updateMap fs where
  bd = boundaryState fs
  -- the boolean records whether a blocked case has been found so far
  checkBlocked True  [] = Left $ "tryUnsafes: There are unsafe updates but ALL unsafe updates are blocked (by touching vertices)\n" ++
                                 "This should not happen!\n"
  checkBlocked False [] = return Nothing
  checkBlocked _ (u: more) = case checkUnsafeUpdate bd u of
                               Nothing -> checkBlocked True more
                               other -> return other

{-| checkUnsafeUpdate bd u, calculates the resulting boundary change for an unsafe update (u) with a new vertex
     (raising an error if u is a safe update).
     It performs a touching vertex check with the new vertex
     returning Nothing if there is a touching vertex (blocked case).
     Otherwise it returns Just bdc with bdc a boundary change.
    [Note: Try is not used as a conflict cannot be found in the unsafe case, and blocking is only a problem
    when all unsafe updates are blocked (and there is at least one) - see tryUnsafes]
-}
checkUnsafeUpdate:: BoundaryState -> Update -> Maybe BoundaryChange
checkUnsafeUpdate _  (SafeUpdate _) = error  "checkUnsafeUpdate: applied to safe update.\n"
checkUnsafeUpdate bd (UnsafeUpdate makeFace) = 
   let v = nextVertex bd       
       newface = makeFace v
       oldVPoints = bvLocMap bd
       newVPoints = addVPoint newface oldVPoints
       Just vPosition = VMap.lookup v newVPoints
       fDedges = faceDedges newface
       matchedDedges = filter (\(x,y) -> x /= v && y /= v) fDedges -- singleton
       newDedges = fmap reverseD (fDedges \\ matchedDedges) -- two edges
       resultBd = BoundaryState
                    { boundary = newDedges ++ (boundary bd \\ matchedDedges)
                    , bvFacesMap = changeVFMap newface (bvFacesMap bd)
                    , bvLocMap = newVPoints
                    , allFaces = newface:allFaces bd
                    , nextVertex = v+1
                    }
       bdChange = BoundaryChange 
                    { newBoundaryState = resultBd
                    , removedEdges = matchedDedges
                    , revisedEdges = affectedBoundary resultBd newDedges
                    , newFace = newface
                    }
   in if touchCheck vPosition oldVPoints -- true if new vertex is blocked because it touches the boundary elsewhere
      then Nothing -- don't proceed when v is a touching vertex
      else Just bdChange 

{-| trySafeUpdate bd u adds a new face by completing a safe update u on BoundaryState bd
    (raising an error if u is an unsafe update).
     It returns a Right BoundaryChange (containing a new BoundaryState, removed boundary edges and
     revised boundary edge list), unless a stuck/incorrect graph is found.
     It checks that the new face is not in conflict with existing faces,
     producing (Left report) if there is a conflict.
    It should cater for the exceptional case where the update removes 3 boundary edges
    in a triangle (and removes 3 boundary vertices), closing a hole.
-}
trySafeUpdate:: BoundaryState -> Update -> Try BoundaryChange
trySafeUpdate _  (UnsafeUpdate _) = error "trySafeUpdate: applied to non-safe update.\n"
trySafeUpdate bd (SafeUpdate newface) = 
   let fDedges = faceDedges newface
       matchedDedges = fDedges `intersect` boundary bd -- list of 2 or 3
       removedBVs = commonVs matchedDedges -- usually 1 vertex no longer on boundary (exceptionally 3)
       newDedges = fmap reverseD (fDedges \\ matchedDedges) -- one or none
       nbrFaces = nub $ concatMap (facesAtBV bd) removedBVs
       resultBd = BoundaryState 
                   { boundary = newDedges ++ (boundary bd \\ matchedDedges)
                   , bvFacesMap = foldr VMap.delete (changeVFMap newface $ bvFacesMap bd) removedBVs
--                   , bvFacesMap = changeVFMap newface (bvFacesMap bd)
                   , allFaces = newface:allFaces bd
                   , bvLocMap = foldr VMap.delete (bvLocMap bd) removedBVs
                               --remove vertex/vertices no longer on boundary
                   , nextVertex = nextVertex bd
                   }
       bdChange = BoundaryChange 
                   { newBoundaryState = resultBd
                   , removedEdges = matchedDedges
                   , revisedEdges = affectedBoundary resultBd newDedges
                   , newFace = newface
                   }
   in if newNoConflict newface nbrFaces 
      then Right bdChange 
      else Left $ "trySafeUpdate:(incorrect tiling)\nConflicting new face  "
                   ++ show newface
                   ++ "\nwith neighbouring faces\n"
                   ++ show nbrFaces
                   ++ "\n"

                 
-- | given 2 consecutive directed edges (not necessarily in the right order),
-- this returns the common vertex (as a singleton list).
-- Exceptionally it may be given 3 consecutive directed edges forming a triangle
-- and returns the 3 vertices of the triangle.
-- It raises an error if the argument is not one of these 2 cases.
commonVs :: [Dedge] -> [Vertex]
commonVs [(a,b),(c,d)] | b==c = [b] 
                       | d==a = [a]
                       | otherwise = error $ "commonVs: 2 directed edges not consecutive: " ++ show [(a,b),(c,d)] ++ "\n"
commonVs [(a,b),(c,d),(e,f)] | length (nub [a,b,c,d,e,f]) == 3 = [a,c,e] 
commonVs es = error $ "commonVs: unexpected argument edges (not 2 consecutive directed edges or 3 round triangle): " ++ show es  ++ "\n"

-- |tryUpdate: tries a single update (safe or unsafe),
-- producing Left report if the update creates a touching vertex in the unsafe case,
-- or if a stuck/incorrect Tgraph is discovered in the safe case.
tryUpdate:: BoundaryState -> Update -> Try BoundaryChange
tryUpdate bd u@(SafeUpdate _) = trySafeUpdate bd u
tryUpdate bd u@(UnsafeUpdate _) = 
  case checkUnsafeUpdate bd u of
       Just bdC -> return bdC
       Nothing ->  Left "tryUpdate: crossing boundary (touching vertices).\n"


{-*
Now unused: final stuck check
-}


{- |
tryFinalStuckCheck was designed to check a final force state (in tryForceStateWith).
The final state is rejected as having a stuck Tgraph if any boundary vertex external angle is less than 4 (tenth turns).
This check was included in tryForceStateWith to catch examples like

  makeTgraph [LK(1,2,3),RK(4,3,2),RK(1,3,5),LK(4,6,3),RK(1,7,2),LK(4,2,8)] 

Previously forcing would not discover that the result was stuck without the check.
However a check for a false queen has been added to both allUGenerator and defaultAllUGenerator
to avoid the need for this check. 
-}
tryFinalStuckCheck:: ForceState -> Try ForceState
tryFinalStuckCheck fs =
  case find ((<4) . externalAngle bs) bvs of
     Nothing -> Right fs
     Just v -> Left $ "tryFinalStuckCheck: stuck/incorrect tiling: external angle problem found at vertex " ++ show v ++
                      "\nwith local faces:" ++ show (facesAtBV bs v) ++
                      "\nand boundary edges:" ++ show (boundary bs `intersect` (fmap reverseD $ facesDedges $ facesAtBV bs v)) ++ "\n"
  where bs = boundaryState fs
        bvs = fmap fst (boundary bs)

    
{-*
Forcing Rules and Update Generators (with Individual generators for each rule)
-}
                                    
{- $rules
Forcing rules

1. (wholeTileUpdates) When a join edge is on the boundary - add the missing half tile to make a whole tile.    
2. (aceKiteUpdates) When a half dart has its short edge on the boundary
   add the half kite that must be on the short edge
   (this is at ace vertices but also helps with jack and deuce vertices).  
3. (queenOrKingUpdates) When a vertex is both a dart origin and a kite wing it must be a queen or king vertex.
   If there is a boundary short edge of a kite half at the vertex, 
   add another kite half sharing the short edge. 
   (This converts 1 kite to 2 and 3 kites to 4 in combination with the first rule).
4. (deuceDartUpdates) When two half kites share a short edge their oppV vertex must be a deuce vertex.
   Add any missing half darts needed to complete the vertex.
5. (jackDartUpdates) When a single dart wing is at a vertex which is recognised as an incomplete jack vertex
   and has a complete kite below the dart wing, 
   add a second dart half touching at the vertex (sharing the kite below).
   This is also known as a *largeDartBase* vertex (= new dart base next level up - see later)
6. (sunStarUpdates) When a vertex has 3 or 4 whole kite origins (= 6 or 8 half kite origins)
   it must be a sun centre. Also if a vertex has 4 whole dart origins (= 8 half dart origins)
   it must be a star centre.
   Add an appropriate half kite/dart on a boundary long edge at the vertex.
   (This will complete suns (resp. stars) along with rule 1),
7. (jackKiteUpdates) When a dart half has its wing recognised as a jack (largeDartBase) vertex
   add a missing kite half on its long edge.
8. (kingDartUpdates) When a vertex is a kite wing and also an origin for exactly 4 dart halves
   it must be a king vertex.
   Add a missing dart half (on any boundary long edge of a dart at the vertex).
9. (queenDartUpdates) If there are 4 kite wings at a vertex (necessarily a queen)
   add any missing half dart on a boundary kite long edge
10.(queenKiteUpdates) If there are 3 kite wings at a vertex (necessarily a queen)
   add any missing fourth half kite on a boundary kite short edge
11.(stuckFalseQueen) If there are 4 kite wings at a vertex with a kite short edge on the boundary
   fail with a (Left) stuck Tgraph report.
-}
           
{-------------------  FORCING RULES and Generators --------------------------
7 vertex types are:
sun, queen, jack (largeDartBase), ace (fool), deuce (largeKiteCentre), king, star
-}
                


{-| allUGenerator combines all the 11 rule update generators.
    They are combined in sequence (keeping the rule order) after applying each to the
    supplied BoundaryState and a focus edge list. (See also defaultAllUGen).
    This version returns a Left..(fail report) for the first generator that produces a Left..(fail report)
    See $rules
-}
allUGenerator :: UpdateGenerator 
allUGenerator bd focus =
  do  (_ , umap) <- foldl' addGen (Right (focus,Map.empty)) generators
      return umap
  where
    addGen (Right (es,umap)) gen = do umap' <- gen bd es
                                      let es' = es \\ Map.keys umap'
                                      return (es',Map.union umap' umap) 
    addGen other _  = other  -- fails with first failing generator
    generators = [ wholeTileUpdates          -- (rule 1)
                 , aceKiteUpdates            -- (rule 2)
                 , queenOrKingUpdates        -- (rule 3)
                 , deuceDartUpdates          -- (rule 4)
                 , jackDartUpdates           -- (rule 5)
                 , sunStarUpdates            -- (rule 6)
                 , jackKiteUpdates           -- (rule 7)
                 , kingDartUpdates           -- (rule 8)
                 , queenDartUpdates          -- (rule 9)
                 , queenKiteUpdates          -- (rule 10)
                 , stuckFalseQueen           -- (new: rule 11)
                 ]

-- |UFinder (Update case finder functions). Given a BoundaryState and a list of (focus) boundary directed edges,
-- such a function returns each focus edge satisfying the particular update case paired with the tileface
-- matching that edge. For example, if the function is looking for dart short edges on the boundary,
-- it will return only those focus edges which are half-dart short edges,
-- each paired with its half-dart face.
type UFinder = BoundaryState -> [Dedge] -> [(Dedge,TileFace)]

-- |UChecker (Update checker functions). Given a BoundaryState and a particular tileface (on the boundary),
-- such functions try to produce particular updates on the boundary edge of the given tileface.
-- [They are called update checkers because they may uncover an incorrect/stuck tiling (using tryFindThirdV)
-- when creating the update.]
-- As an example, addKiteShortE will try to produce an update to add a half-kite with short edge against the boundary.
-- Such a function can be used with a UFinder that either returns dart halves with short edge on the boundary
-- (nonKDarts in rule 2) or returns kite halves with short edge on the boundary
-- (kitesWingDartOrigin in rule 3).
type UChecker = BoundaryState -> TileFace -> Try Update      

{-|This is a general purpose filter used to create UFinder functions for each force rule.
 It requires a face predicate.
 The face predicate takes a BoundaryState bd, a boundary Dedge (a,b) and the TileFace with the edge (b,a)
 and decides whether the face is wanted or not (True = wanted)
 This will be used to filter all the faces at the focus edges 
 (when given a BoundaryState and list of focus edges).
 For some predicates the BoundaryState argument is not used (eg boundaryJoin in incompleteHalves), 
 but for others it is used to look at other faces at b or at a besides the supplied face 
 (eg kiteWDO in kitesWingDartOrigin) 
-}
boundaryFilter::  (BoundaryState -> Dedge -> TileFace -> Bool) -> UFinder
boundaryFilter predF bd focus = 
    [ (e,fc) | e <- focus 
             , fc <- facesAtBV bd (fst e)
             , fc `elem` facesAtBV bd (snd e)
             , predF bd e fc
             ]

-- |makeUpdate f x constructs a safe update if x is Just .. and an unsafe update if x is Nothing
makeUpdate:: (Vertex -> TileFace) -> Maybe Vertex ->  Update
makeUpdate f (Just v) = SafeUpdate (f v)
makeUpdate f Nothing  = UnsafeUpdate f

{-
-- |checkUpdate f lifts makeUpdate f to apply to Try results from third vertex search.
checkUpdate:: (Vertex -> TileFace) -> Try (Maybe Vertex) -> Try Update
checkUpdate f  = fmap (makeUpdate f) 
-}

{-*
BoundaryState vertex predicates and properties
-}
         
-- |A vertex on the boundary must be a star if it has 7 or more dart origins
mustbeStar:: BoundaryState -> Vertex -> Bool
mustbeStar bd v = length (filter ((==v) . originV) $ filter isDart $ facesAtBV bd v) >= 7

-- |A vertex on the boundary must be a sun if it has 5 or more kite origins
mustbeSun:: BoundaryState -> Vertex -> Bool
mustbeSun bd v = length (filter ((==v) . originV) $ filter isKite $ facesAtBV bd v) >= 5

-- |A vertex on the boundary which is an oppV of a kite must be a deuce
-- if there is a shared kite short edge at the vertex.
mustbeDeuce:: BoundaryState -> Vertex -> Bool
mustbeDeuce bd v = isKiteOppV bd v &&
                   hasAnyMatchingE (fmap shortE $ filter isKite $ facesAtBV bd v)

-- |A boundary vertex which is a kite wing and has 4 dart origins must be a king vertex
mustbeKing:: BoundaryState -> Vertex -> Bool
mustbeKing bd v = isKiteWing bd v && length dartOrigins ==4
   where  dartOrigins = filter ((==v) . originV) $ filter isDart $ facesAtBV bd v

{-
-- |A boundary vertex which is a kite wing and dart origin must be either a king or queen
mustbeQorK:: BoundaryState -> Vertex -> Bool
mustbeQorK bd v = isDartOrigin bd v && isKiteWing bd v
-}

-- |isKiteWing bd v - Vertex v is a kite wing in BoundaryState bd
isKiteWing:: BoundaryState -> Vertex -> Bool
isKiteWing bd v = v `elem` fmap wingV (filter isKite (facesAtBV bd v))

-- |isKiteOppV bd v - Vertex v is a kite oppV in BoundaryState bd
isKiteOppV:: BoundaryState -> Vertex -> Bool
isKiteOppV bd v = v `elem` fmap oppV (filter isKite (facesAtBV bd v))

-- |isDartOrigin bd v - Vertex v is a dart origin in BoundaryState bd
isDartOrigin:: BoundaryState -> Vertex -> Bool
isDartOrigin bd v = v `elem` fmap originV (filter isDart (facesAtBV bd v))

-- |A boundary vertex with 4 kite wings is a queen vertex (maybe needing a dart)
mustbeQueen4Kite:: BoundaryState -> Vertex -> Bool
mustbeQueen4Kite bd v = kiteWingCount bd v ==4

-- |A boundary vertex with 3 kite wings is a queen vertex (needing a fourth kite)
mustbeQueen3Kite:: BoundaryState -> Vertex -> Bool
mustbeQueen3Kite bd v = kiteWingCount bd v ==3

-- |kiteWingCount bd v - the number of kite wings at v in BoundaryState bd
kiteWingCount:: BoundaryState -> Vertex -> Int
kiteWingCount bd v = length $ filter ((==v) . wingV) $ filter isKite (facesAtBV bd v)

-- |mustbeJack (large dart base / jack) is true of a boundary vertex if
-- it is the wing of two darts not sharing a long edge or
-- it is a wing of a dart and also a kite origin
-- (false means it is either undetermined or is a large kite centre  - deuce)
mustbeJack :: BoundaryState -> Vertex -> Bool
mustbeJack bd v =
  (length dWings == 2 && not (hasAnyMatchingE (fmap longE dWings))) || -- 2 dart wings and dart long edges not shared.
  (length dWings == 1 && isKiteOrigin) 
  where fcs = facesAtBV bd v
        dWings = filter ((==v) . wingV) $ filter isDart fcs
        isKiteOrigin = v `elem` fmap originV (filter isKite fcs)

-- |hasMatching asks if a directed edge list has any two matching (=opposing) directed edges.
hasAnyMatchingE :: [Dedge] -> Bool
hasAnyMatchingE ((x,y):more) = (y,x) `elem` more || hasAnyMatchingE more
hasAnyMatchingE [] = False


{-*
Making generators for each rule.
-}


{-| makeGenerator combines an update case finder (UFinder) with its corresponding update checker (UChecker)
    to produce an update generator function.
    This is used to make each of the 10 update generators corresponding to 10 rules. 
    
    When the generator is given a BoundaryState and list of focus edges,
    the finder produces a list of pairs of dedge and face,
    the checker is used to convert the face in each pair to an update (which can fail with a Left report),
    and the new updates are returned as a map (with the dedges as key) in a Right result.
-}
makeGenerator :: UChecker -> UFinder -> UpdateGenerator
makeGenerator checker finder = gen where
  gen bd edges = foldr addU (Right Map.empty) (finder bd edges) where
                 addU _      (Left x) = Left x 
                 addU (e,fc) (Right ump) = do u <- checker bd fc
                                              return (Map.insert e u ump)

                         
{-*
Ten Update Generators (with corresponding Finders)
-}

-- |Update generator for rule (1)
wholeTileUpdates:: UpdateGenerator
wholeTileUpdates = makeGenerator completeHalf incompleteHalves

-- |Find faces with missing opposite face (mirror face)  
incompleteHalves :: UFinder 
incompleteHalves = boundaryFilter boundaryJoin where
    boundaryJoin _ (a,b) fc = joinE fc == (b,a)


-- |Update generator for rule (2)
aceKiteUpdates :: UpdateGenerator
aceKiteUpdates = makeGenerator addKiteShortE nonKDarts

-- |Find half darts with boundary short edge
nonKDarts :: UFinder            
nonKDarts = boundaryFilter bShortDarts where
    bShortDarts _ (a,b) fc = isDart fc && shortE fc == (b,a)


-- |Update generator for rule (3)
 -- queen and king vertices add a missing kite half (on a boundary kite short edge)
queenOrKingUpdates :: UpdateGenerator
queenOrKingUpdates = makeGenerator addKiteShortE kitesWingDartOrigin

-- |Find kites with boundary short edge where the wing is also a dart origin
kitesWingDartOrigin :: UFinder              
kitesWingDartOrigin = boundaryFilter kiteWDO where
   kiteWDO bd (a,b) fc = shortE fc == (b,a) 
                         && isKite fc && isDartOrigin bd (wingV fc)


{-| Update generator for rule (4)
     (for deuce vertices = largeKiteCentres)
     Kites whose short edge (b,a) matches a boundary edge (a,b) where their oppV 
     has 2 other kite halves sharing a shortE.
     These need a dart adding on the short edge.
-}
deuceDartUpdates :: UpdateGenerator
deuceDartUpdates = makeGenerator addDartShortE kiteGaps

-- |Find kite halves with a short edge on the boundary (a,b) 
-- where there are 2 other kite halves sharing a short edge
-- at oppV of the kite half (a for left kite and b for right kite)
kiteGaps :: UFinder              
kiteGaps = boundaryFilter kiteGap where
  kiteGap bd (a,b) fc = shortE fc == (b,a)
                        && isKite fc && mustbeDeuce bd (oppV fc)


-- |Update generator for rule (5)
-- jackDartUpdates - jack vertex add a missing second dart
jackDartUpdates :: UpdateGenerator
jackDartUpdates = makeGenerator addDartShortE noTouchingDart

-- |Find kite halves with a short edge on the boundary (a,b) where oppV is a largeDartBase vertex
-- (oppV is a for left kite and b for right kite).
-- The function mustbeJack determines if a vertex must be a a largeDartBase / jack
noTouchingDart :: UFinder              
noTouchingDart = boundaryFilter farKOfDarts where
   farKOfDarts bd (a,b) fc  = shortE fc == (b,a)
                              && isKite fc && mustbeJack bd (oppV fc)

{-| Update generator for rule (6)
sunStarUpdates is for vertices that must be either sun or star 
almostSunStar finds half-kites/half-darts with a long edge on the boundary
where their origin vertex has 8 total half-kites/half-darts respectively
or their origin vertex has 6 total half-kites in the case of kites only
completeSunStar will add a new face of the same type (dart/kite) 
sharing the long edge.
-}
sunStarUpdates :: UpdateGenerator
sunStarUpdates = makeGenerator completeSunStar almostSunStar

-- |Find a boundary long edge of either
-- a dart where there are at least 7 dart origins, or
-- a kite where there are at least 5 kite origins
almostSunStar :: UFinder                  
almostSunStar = boundaryFilter multiples57 where
    multiples57 bd (a,b) fc = longE fc == (b,a) &&           
        ((isDart fc && mustbeStar bd (originV fc)) ||
         (isKite fc && mustbeSun bd (originV fc))
        )

-- |Update generator for rule (7)
-- jack vertices (largeDartBases) with dart long edge on boundary - add missing kite top
jackKiteUpdates :: UpdateGenerator
jackKiteUpdates = makeGenerator addKiteLongE jackMissingKite

-- |Find jack vertices (largeDartBases) with dart long edge on the boundary
jackMissingKite :: UFinder                  
jackMissingKite = boundaryFilter dartsWingDB where
    dartsWingDB bd (a,b) fc = longE fc == (b,a) &&
                              isDart fc && mustbeJack bd (wingV fc)

-- |Update generator for rule (8)
-- king vertices with 2 of the 3 darts  - add another half dart on a boundary long edge of existing darts
kingDartUpdates :: UpdateGenerator
kingDartUpdates = makeGenerator addDartLongE kingMissingThirdDart

-- |Find king vertices with a dart long edge on the boundary
-- and 2 of the 3 darts at its origin plus a kite wing at its origin
kingMissingThirdDart :: UFinder                    
kingMissingThirdDart = boundaryFilter pred where
    pred bd (a,b) fc = longE fc == (b,a) &&
        isDart fc && mustbeKing bd (originV fc)


-- |Update generator for rule (9)
-- queen vertices (with 4 kite wings) -- add any missing half dart on a boundary kite long edge
queenDartUpdates :: UpdateGenerator
queenDartUpdates = makeGenerator addDartLongE queenMissingDarts

-- |Find queen vertices (with 4 kite wings) and a boundary kite long edge
queenMissingDarts :: UFinder                      
queenMissingDarts = boundaryFilter pred where
    pred bd (a,b) fc = longE fc == (b,a) && isKite fc && length kiteWings ==4
                        where fcWing = wingV fc
                              kiteWings = filter ((==fcWing) . wingV) $ 
                                          filter isKite $ facesAtBV bd fcWing

-- |find a false queen vertex (with 4 kite wings) and a boundary kite short edge.
-- This is an incorrect Tgraph.
stuckFalseQueen  :: UpdateGenerator
stuckFalseQueen = makeGenerator abortFalseQueen falseQueen where
    abortFalseQueen bd fc = Left $ "stuckFalseQueen: stuck/incorrect Tgraph found at vertex " ++ show (wingV fc) ++ 
                                   "\nwith faces: " ++ show (facesAtBV bd (wingV fc)) ++ "\n"

-- |A vertex with 4 kite wings and a boundary kite short edge is a stck/incorrect Tgraph
falseQueen :: UFinder                      
falseQueen = boundaryFilter pred where
    pred bd (a,b) fc = shortE fc == (b,a) && isKite fc && length kiteWings ==4
                        where fcWing = wingV fc
                              kiteWings = filter ((==fcWing) . wingV) $ 
                                          filter isKite $ facesAtBV bd fcWing

-- |Update generator for rule (10)
-- queen vertices with 3 kite wings -- add missing fourth half kite on a boundary kite short edge
queenKiteUpdates :: UpdateGenerator
queenKiteUpdates = makeGenerator addKiteShortE queenMissingKite

-- |Find queen vertices with only 3 kite wings and a kite short edge on the boundary
queenMissingKite :: UFinder                        
queenMissingKite = boundaryFilter pred where
    pred bd (a,b) fc = shortE fc == (b,a) && isKite fc && length kiteWings ==3
                        where
                          fcWing = wingV fc
                          kiteWings = filter ((==fcWing) . wingV) $ filter isKite (facesAtBV bd fcWing)


{-*
Six Update Checkers
-}

-- |completeHalf will check an update to
--  add a symmetric (mirror) face for a given face at a boundary join edge.
completeHalf :: UChecker      
completeHalf bd (LD(a,b,_)) = makeUpdate makeFace <$> x where
        makeFace v = RD(a,v,b)
        x = tryFindThirdV bd (b,a) (3,1) --anglesForJoinRD
completeHalf bd (RD(a,_,b)) = makeUpdate makeFace <$> x where
        makeFace v = LD(a,b,v)
        x = tryFindThirdV bd (a,b) (1,3) --anglesForJoinLD
completeHalf bd (LK(a,_,b)) = makeUpdate makeFace <$> x where
        makeFace v = RK(a,b,v)
        x = tryFindThirdV bd (a,b) (1,2) --anglesForJoinRK
completeHalf bd (RK(a,b,_)) = makeUpdate makeFace <$> x where
        makeFace v = LK(a,v,b)
        x = tryFindThirdV bd (b,a) (2,1) --anglesForJoinLK

-- |add a (missing) half kite on a (boundary) short edge of a dart or kite
addKiteShortE :: UChecker         
addKiteShortE bd (RD(_,b,c)) = makeUpdate makeFace <$> x where
    makeFace v = LK(v,c,b)
    x = tryFindThirdV bd (c,b) (2,2) --anglesForShortLK
addKiteShortE bd (LD(_,b,c)) = makeUpdate makeFace <$> x where
    makeFace v = RK(v,c,b)
    x = tryFindThirdV bd (c,b) (2,2) --anglesForShortRK
addKiteShortE bd (LK(_,b,c)) = makeUpdate makeFace <$> x where
    makeFace v = RK(v,c,b)
    x = tryFindThirdV bd (c,b) (2,2) --anglesForShortRK
addKiteShortE bd (RK(_,b,c)) = makeUpdate makeFace <$> x where
    makeFace v = LK(v,c,b)
    x = tryFindThirdV bd (c,b) (2,2) --anglesForShortLK

-- |add a half dart top to a boundary short edge of a half kite.
addDartShortE :: UChecker         
addDartShortE bd (RK(_,b,c)) = makeUpdate makeFace <$> x where
        makeFace v = LD(v,c,b)
        x = tryFindThirdV bd (c,b) (3,1) --anglesForShortLD
addDartShortE bd (LK(_,b,c)) = makeUpdate makeFace <$> x where
        makeFace v = RD(v,c,b)
        x = tryFindThirdV bd (c,b) (1,3) --anglesForShortRD
addDartShortE _  _ = error "addDartShortE applied to non-kite face\n"

-- |add a kite half to a kite long edge or dart half to a dart long edge
completeSunStar :: UChecker
completeSunStar bd fc = if isKite fc 
                        then addKiteLongE bd fc
                        else addDartLongE bd fc

-- |add a kite to a long edge of a dart or kite
addKiteLongE :: UChecker            
addKiteLongE bd (LD(a,_,c)) = makeUpdate makeFace <$> x where
    makeFace v = RK(c,v,a)
    x = tryFindThirdV bd (a,c) (2,1) -- anglesForLongRK
addKiteLongE bd (RD(a,b,_)) = makeUpdate makeFace <$> x where
    makeFace v = LK(b,a,v)
    x = tryFindThirdV bd (b,a) (1,2) -- anglesForLongLK
addKiteLongE bd (RK(a,_,c)) = makeUpdate makeFace <$> x where
  makeFace v = LK(a,c,v)
  x = tryFindThirdV bd (a,c) (1,2) -- anglesForLongLK
addKiteLongE bd (LK(a,b,_)) = makeUpdate makeFace <$> x where
  makeFace v = RK(a,v,b)
  x = tryFindThirdV bd (b,a) (2,1) -- anglesForLongRK

-- |add a half dart on a boundary long edge of a dart or kite
addDartLongE :: UChecker            
addDartLongE bd (LD(a,_,c)) = makeUpdate makeFace <$> x where
  makeFace v = RD(a,c,v)
  x = tryFindThirdV bd (a,c) (1,1) -- anglesForLongRD
addDartLongE bd (RD(a,b,_)) = makeUpdate makeFace <$> x where
  makeFace v = LD(a,v,b)
  x = tryFindThirdV bd (b,a) (1,1) -- anglesForLongLD
addDartLongE bd (LK(a,b,_)) = makeUpdate makeFace <$> x where
  makeFace v = RD(b,a,v)
  x = tryFindThirdV bd (b,a) (1,1) -- anglesForLongRD
addDartLongE bd (RK(a,_,c)) = makeUpdate makeFace <$> x where
  makeFace v = LD(c,v,a)
  x = tryFindThirdV bd (a,c) (1,1) -- anglesForLongLD

-- |mnemonic for internal angles of an edge (expressed as integer units of a tenth turn (I.e 1,2 or 3)
anglesForJoinRD,anglesForJoinLD,anglesForJoinRK,anglesForJoinLK::(Int,Int)
anglesForJoinRD = (3,1)
anglesForJoinLD = (1,3)
anglesForJoinRK = (1,2)
anglesForJoinLK = (2,1)
-- |mnemonic for internal angles of an edge (expressed as integer units of a tenth turn (I.e 1,2 or 3)
anglesForLongLD,anglesForLongRD,anglesForLongRK,anglesForLongLK::(Int,Int)
anglesForLongLD = (1,1)
anglesForLongRD = (1,1)
anglesForLongRK = (2,1)
anglesForLongLK = (1,2)
-- |mnemonic for internal angles of an edge (expressed as integer units of a tenth turn (I.e 1,2 or 3)
anglesForShortLD,anglesForShortRD,anglesForShortLK,anglesForShortRK::(Int,Int)
anglesForShortLD = (3,1)
anglesForShortRD = (1,3)
anglesForShortLK = (2,2)
anglesForShortRK = (2,2)


-- |An alternative to allUGenerator, and used as the default. It uses the same rules and UCheckers,
-- but makes decisions based on the EdgeType of a boundary edge (instead of trying each UFinder in turn).
-- If there are any Left..(fail reports) for the given
-- boundary edges the result is a sigle Left.. concatenating all the failure reports (unlike allUGenerator).
defaultAllUGen :: UpdateGenerator
defaultAllUGen bd es = combine $ fmap decide es  where -- Either String is a monoid as well as Map
  decide e = decider (e,fc,etype) where (fc,etype) = inspectBDedge bd e
  decider (e,fc,Join)  = mapItem e (completeHalf bd fc) -- rule 1
  decider (e,fc,Short) 
    | isDart fc = mapItem e (addKiteShortE bd fc) -- rule 2
    | otherwise = kiteShortDecider e fc 
  decider (e,fc,Long)  
    | isDart fc = dartLongDecider e fc
    | otherwise = kiteLongDecider e fc 
  dartLongDecider e fc
    | mustbeStar bd (originV fc) = mapItem e (completeSunStar bd fc)
    | mustbeKing bd (originV fc) = mapItem e (addDartLongE bd fc)
    | mustbeJack bd (wingV fc) = mapItem e (addKiteLongE bd fc)
    | otherwise = Right Map.empty
  kiteLongDecider e fc
    | mustbeSun bd (originV fc) = mapItem e (completeSunStar bd fc)
    | mustbeQueen4Kite bd (wingV fc) = mapItem e (addDartLongE bd fc)
    | otherwise = Right Map.empty
  kiteShortDecider e fc
    | mustbeDeuce bd (oppV fc) || mustbeJack bd (oppV fc) = mapItem e (addDartShortE bd fc)
    | mustbeQueen3Kite bd (wingV fc) || isDartOrigin bd (wingV fc) = mapItem e (addKiteShortE bd fc)
    -- addewd new false queen check (4 kite wings at a vertex with a kite SHORT edge on the boundary)
    | mustbeQueen4Kite bd (wingV fc) = Left $ "defaultAllUGen: stuck/incorrect Tgraph (false Queen) found at vertex " ++ show (wingV fc) ++ 
                                              "\nwith faces: " ++ show (facesAtBV bd (wingV fc)) ++ "\n"
    | otherwise = Right Map.empty
  mapItem e = fmap (\u -> Map.insert e u Map.empty)
  combine = fmap mconcat . concatFails -- concatenates all failure reports if there are any
                                       -- otherwise combines the update maps with mconcat
  

-- |Given a BoundaryState and a directed boundary edge, this returns the same edge with
-- the unique face on that edge and the edge type for that face and edge (Short/Long/Join)
inspectBDedge:: BoundaryState -> Dedge -> (TileFace, EdgeType)
inspectBDedge bd e = (fc,edgeType (reverseD e) fc) where
    fc = case facesAtBV bd (fst e) `intersect` facesAtBV bd (snd e) of
         [fc] -> fc
         _ -> error $ "inspectBDedge: Not a boundary directed edge " ++ show e ++ "\n"




{-*
Auxiliary Functions for adding faces: externalAngle and tryFindThirdV.
-}

{-------------------------------------------
****************************
ADDING FACES with tryFindThirdV
****************************
  
The difficulty is determining if any edges of a new face already exist.
This goes beyond a simple graph operation and requires use of the internal angles of the faces.
We use a representation of angles which allows an equality test.
All angles are integer multiples of 1/10th turn (mod 10) so we use
these integers for comparing angles n where n is 0..9


No crossing boundary property:
It is important that there are no crossing boundaries, otherwise external angle calculations can be wrong.

Possible Touching Vertices.
When tryFindThirdV returns Nothing, this means a new vertex needs to be created.
This will need to have its position checked against other (boundary) vertices to avoid
creating a touching vertex/crossing boundary. (Taken care of in tryUnsafeUpdate)
---------------------------------}

{-|tryFindThirdV finds a neighbouring third vertex (if it is in the Tgraph) for a face added to
   the right hand side of a directed boundary edge.
   In tryFindThirdV bd (a,b) (n,m), the two integer arguments n and m are the INTERNAL angles
   for the new face on the boundary directed edge (a,b)
   (for a and b respectively) expressed as multiples of tt (tt being a tenth turn)
   and must both be either 1,2, or 3.
   tryFindThirdV compares these internal angles with the external angles of the boundary calculated at a and b.
   If one of them matches, then an adjacent boundary edge will lead to the required vertex.
   If either n or m is too large a Left report is returned indicating an incorrect graph (stuck tiling).
   If n and m are smaller than the respective external angles, Right Nothing is returned.
-}
tryFindThirdV:: BoundaryState -> Dedge -> (Int,Int) -> Try (Maybe Vertex)
tryFindThirdV bd (a,b) (n,m) = maybeV where
    aAngle = externalAngle bd a
    bAngle = externalAngle bd b
    maybeV | aAngle < n = err
           | bAngle < m = err
           | aAngle == n = case find ((==a) . snd) (boundary bd) of
                             Just pr -> Right $ Just (fst pr)
                             Nothing -> errB
           | bAngle == m = case find ((==b) . fst) (boundary bd) of
                             Just pr -> Right $ Just (snd pr)
                             Nothing -> errB
           | otherwise =   Right  Nothing
    err = Left $ "tryFindThirdV: Found incorrect graph (stuck tiling)\nConflict at edge: " 
                 ++ show (a,b) ++ "\n"
    errB = Left $ "tryFindThirdV: Impossible boundary. No predecessor/successor Dedge for Dedge " 
                 ++ show (a,b) ++ "\n"

-- |externalAngle bd v - calculates the external angle at boundary vertex v in BoundaryState bd as an
-- integer multiple of tt (tenth turn), so 1..9.  It relies on there being no crossing boundaries,
-- so that there is a single external angle at each boundary vertex. 
externalAngle:: BoundaryState -> Vertex -> Int
externalAngle bd v = check $ 10 - sum (map (intAngleAt v) $ facesAtBV bd v) where
  check n | n>9 || n<1 = error $ "externalAngle: vertex not on boundary "++show v
                                  ++ " with external angle " ++show n++" with faces:\n"
                                  ++ show (bvFacesMap bd VMap.! v)
  check n = n
  
-- |intAngleAt v fc gives the internal angle of the face fc at vertex v (which must be a vertex of the face)
-- in terms of tenth turns, so returning an Int (1,2,or 3).
intAngleAt :: Vertex -> TileFace -> Int
intAngleAt v fc = faceIntAngles fc !! indexV v fc

-- |faceIntAngles returns a list of the three internal angles of a face (clockwise from originV)
-- in terms of tenth turns - always 1 or 2 for kites and 1 or 3 for darts.
faceIntAngles :: TileFace -> [Int]
faceIntAngles (LD _) = [1,3,1]
faceIntAngles (RD _) = [1,1,3]
faceIntAngles _      = [1,2,2] -- LK and RK



