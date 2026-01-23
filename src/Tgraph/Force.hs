{-|
Module      : Tgraph.Force
Description : The force functions for Tgraphs 
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module includes force and tryForce plus related operations for testing and experimenting
such as tryStepForce, tryAddHalfKite and tryAddHalfDart.
It introduces BoundaryState and ForceState types and includes a Forcible class with instances for
Tgraph, BoundaryState, and ForceState.

The module is made strict (to remove many space leaks).
-}

{-# LANGUAGE Strict                 #-} 
{-# OPTIONS_GHC -Wno-deprecations   #-}

module Tgraph.Force
  ( -- *  Forcible class
   Forcible(..)
    -- *  Generalised forcing
  , force
  , forceWith
  , tryForce
  , tryForceWith
  , stepForce
  , tryStepForce
  , tryStepForceWith
  , tryFSOp
  , initFS
  , tryInitFS
  , tryChangeBoundary
  , wholeTiles
  -- *  Explicitly Forced
  , Forced()
  , forgetF
  , tryForceF
  , forceF
  , recoverGraphF
  , boundaryStateF
  , makeBoundaryStateF
  , initFSF
  , labelAsForced
   -- *  Force Related
  , addHalfKite
  , tryAddHalfKite
  , addHalfDart
  , tryAddHalfDart
    -- *  Debug Step Forcing
  , tryOneStepWith
  , tryOneStepForce
-- * Types for Forcing
  , BoundaryState(..)
  , ForceState(..)
  , BoundaryChange(..)
  , Update(..)
  , UpdateMap
  , UpdateGenerator(..)
  , UFinder
  , UChecker
    -- *  BoundaryState operations
  , makeBoundaryState
  -- , boundary (part of HasFaces class)
  , recoverGraph
--  , changeVFMap -- Now HIDDEN
  , facesAtBV
  -- , boundaryVFacesBS  (removed)
    -- *  Auxiliary Functions for a force step
  , affectedBoundary
--  , mustFind
  , tryReviseUpdates
  , tryReviseFSWith
  , findSafeUpdate
  , tryUnsafes
  , checkUnsafeUpdate
  , trySafeUpdate
--   , commonVs
  , tryUpdate
    -- *  Recalibrating force
  , recalibratingForce
  , tryRecalibratingForce
  , recalculateBVLocs
    -- * Forcing Rules and Update Generators
    -- $rules

    -- *  Combined Update Generators
  , defaultAllUGen
  , combineUpdateGenerators
  , allUGenerator
    -- * Update Generators and Finders for each Rule.
  , wholeTileUpdates
  , incompleteHalves
  , aceKiteUpdates
  , nonKDarts
  , queenOrKingUpdates
  , kitesWingDartOrigin
  , deuceDartUpdates
  , kiteGaps
  , jackDartUpdates
  , noTouchingDart
  , sunStarUpdates
  , almostSunStar
  , jackKiteUpdates
  , jackMissingKite
  , kingDartUpdates
  , kingMissingThirdDart
  , queenDartUpdates
  , queenMissingDarts
  , queenKiteUpdates
  , queenMissingKite
    -- *  Six Update Checkers
  , completeHalf
  , addKiteShortE
  , addDartShortE
  , completeSunStar
  , addKiteLongE
  , addDartLongE
    -- *  Boundary vertex properties
  , mustbeStar
  , mustbeSun
  , mustbeDeuce
  , mustbeKing
  , isKiteWing
  , isKiteOppV
  , isDartOrigin
  , mustbeQueen
  , kiteWingCount
  , mustbeJack
   -- * Other tools for making new update generators
  , newUpdateGenerator
  -- , makeGenerator
  , boundaryFilter
  , boundaryEdgeFilter
  , makeUpdate
--  , hasAnyMatchingE
--  , inspectBDedge
    -- *  Auxiliary Functions for adding faces
    -- $Additions
  -- , tryFindThirdV
  , externalAngle

  , touchCheck

  )  where



import Data.List ((\\), intersect, nub, find)
import Prelude hiding (Foldable(..))
import Data.Foldable (Foldable(..))
import qualified Data.Map as Map (Map, empty, delete, elems, insert, union, keys) -- used for UpdateMap
import qualified Data.IntMap.Strict as VMap (null, filter, filterWithKey, alter, delete, lookup, (!), keysSet)
import qualified Data.IntSet as IntSet (member,fromList)
            -- used for BoundaryState locations AND faces at boundary vertices
-- import qualified Data.Maybe(fromMaybe)  -- was used for lazy mustFind only
import Diagrams.Prelude (Point, V2) -- necessary for touch check (touchCheck) used in tryUnsafeUpdate 
import Tgraph.Prelude

{-
***************************************************************************   
Efficient FORCING with 
  BoundaryState, ForceState 
  Touching Vertex Check
  Incremented Update Maps
***************************************************************************
-}




{-| A BoundaryState records
the boundary directed edges (directed so that faces are on LHS and exterior is on RHS)
plus 
a mapping of boundary vertices to their incident faces, plus
a mapping of boundary vertices to positions (using Tgraph.Prelude.locateGraphVertices).
It also keeps track of all the faces
and the next vertex label to be used when adding a new vertex.
-}
data BoundaryState
   = BoundaryState
     { boundaryDedges:: [Dedge]  -- ^ boundary directed edges (face on LHS, exterior on RHS)
     , bvFacesMap:: VertexMap [TileFace] -- ^faces at each boundary vertex.
     , bvLocMap:: VertexMap (Point V2 Double)  -- ^ position of each boundary vertex.
     , allFaces:: [TileFace] -- ^ all the tile faces
     , nextVertex:: Vertex -- ^ next vertex number
     } deriving (Show)

-- |BoundaryState is in class HasFaces.
-- Note the default implementations are overiden to use precalculated information
instance HasFaces BoundaryState where
    faces = allFaces
    boundary = boundaryDedges  -- (overrides default) boundary already calculated
    maxV bd = nextVertex bd - 1 -- (overrides default)
    boundaryVFMap = bvFacesMap -- (overrides default) already calculated

-- |Calculates BoundaryState information from a Tgraph.
makeBoundaryState:: Tgraph -> BoundaryState
makeBoundaryState g =
  let bdes = boundary g
      bvs = IntSet.fromList (map fst bdes) --boundaryVertexSet g --map fst bdes -- (map snd bdes would also do) for all boundary vertices
      bvLocs = VMap.filterWithKey (\k _ -> k `IntSet.member` bvs) $ locateGraphVertices g
  in 
      BoundaryState
      { boundaryDedges = bdes
      , bvFacesMap = vertexFacesMap bvs g
      , bvLocMap = bvLocs
      , allFaces = faces g
      , nextVertex = 1+ maxV g
      }

-- |Converts a BoundaryState back to a Tgraph
recoverGraph:: BoundaryState -> Tgraph
recoverGraph = makeUncheckedTgraph . faces

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
  Nothing -> error $ "facesAtBV: Not a boundary vertex? No result found for vertex " ++ show v ++ "\n"

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
-- (Any boundary directed edge will have the opposite direction in some face.)
type UpdateMap = Map.Map Dedge Update

-- |ForceState: The force state records information between executing single face updates during forcing
-- (a BoundaryState and an UpdateMap).
data ForceState = ForceState
                   { boundaryState:: BoundaryState
                   , updateMap:: UpdateMap
                   } deriving (Show)

-- |ForceState is in class HasFaces.
-- (using the boundaryState implementations)
instance HasFaces ForceState where
    faces = faces . boundaryState
    boundary = boundary . boundaryState
    maxV = maxV . boundaryState
    boundaryVFMap = boundaryVFMap . boundaryState

{-|UpdateGenerator is a newtype for functions which capture one or more of the forcing rules.
The functions can be applied using the unwrapper applyUG
and produce a (Try) UpdateMap when given a BoundaryState and a focus list of particular directed boundary edges.  
Each forcing rule has a particular UpdateGenerator,
but they can also be combined (e.g in sequence - allUGenerator or otherwise - defaultAllUGenerator).
-}
newtype UpdateGenerator = UpdateGenerator {applyUG :: BoundaryState -> [Dedge] -> Try UpdateMap}


-- | Forcing a Tgraph requires conversion to a BoundaryState (which records extra data)
-- and then to a ForceState (which keeps track of possible updates during forcing).
-- Thus we introduce a Forcible class which will have Tgraph, BoundaryState, ForceState as instances.
--
-- Forcible class has operations to (indirectly) implement forcing and single step forcing
-- for any Forcible. The class operations are more general to allow for other
-- force related operations to be generalised for use on any Forcible.
-- For example tryAddHalfKite and tryAddHalfDart are implemented using tryChangeBoundaryWith.
--
-- Note that these operations are parameterised on an UpdateGenerator, which encapsulates
-- the forcing rules to be used.
class Forcible a where
    -- | tryFSOpWith (try ForseState Operation with) when given an update generator, generalises a (try) ForceState operation to a (try) Forcible operation.
    -- The update generator is only used to initialise a ForceState when there is not one
    -- already available (i.e not used when the Forcible is a ForceState)
    --
    -- To improve performance of a sequence of force related operations, express each as a
    -- ForceState -> Try ForceState, then use (\<=\<) or (\>=\>) to combine and pass to tryFSOpWith.
    -- This ensures there are no unnecessary conversions between steps.
    tryFSOpWith :: UpdateGenerator -> (ForceState -> Try ForceState) -> a -> Try a
    -- | tryInitFSWith (try initialising a ForceState with) when given an update generator tries to create an initial ForceState (ready for forcing) from a Forcible.
    -- Again, the update generator is not used when the instance is a ForceState.
    tryInitFSWith :: UpdateGenerator -> a -> Try ForceState
    -- | tryChangeBoundaryWith when given an update generator, converts a (try) BoundaryState changing operation to a (try) Forcible operation.
    -- The update generator is only used when the instance is a ForceState (to revise the update map in the result).
    tryChangeBoundaryWith :: UpdateGenerator -> (BoundaryState -> Try BoundaryChange) -> a -> Try a

-- |ForceStates are Forcible
instance Forcible ForceState where
    tryFSOpWith _ = id  -- update generator not used
    tryInitFSWith _  = return  -- update generator not used
    tryChangeBoundaryWith ugen f fs = do
        bdC <- f (boundaryState fs)
        tryReviseFSWith ugen bdC fs

-- | BoundaryStates are Forcible    
instance Forcible BoundaryState where
    tryFSOpWith ugen f bd = do
        fs <- tryInitFSWith ugen bd
        fs' <- f fs
        return $ boundaryState fs'
    tryInitFSWith ugen bd = do
        umap <- applyUG ugen bd (boundary bd)
        return $ ForceState { boundaryState = bd , updateMap = umap }
    tryChangeBoundaryWith _ f bd = do -- update generator not used
        bdC <- f bd
        return $ newBoundaryState bdC

-- | Tgraphs are Forcible    
instance Forcible Tgraph where
    tryFSOpWith ugen f g = recoverGraph <$> tryFSOpWith ugen f (makeBoundaryState g)
    tryInitFSWith ugen g = tryInitFSWith ugen (makeBoundaryState g)
    tryChangeBoundaryWith ugen f g = -- update generator not used
        recoverGraph <$> tryChangeBoundaryWith ugen f (makeBoundaryState g)



-- | try forcing using a given UpdateGenerator.
--  tryForceWith uGen fs - does updates using uGen until there are no more updates.
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
                            Just bdC -> do fs' <- tryReviseFSWith uGen bdC fs
                                           retry fs'

-- | try a given number of force steps using a given UpdateGenerator.
tryStepForceWith :: Forcible a => UpdateGenerator -> Int -> a -> Try a
tryStepForceWith ugen n =
  if n>=0
  then tryFSOpWith ugen $ count n
  else error "tryStepForceWith: used with negative number of steps\n"
  where
      count 0 fs = return fs
      count m fs = do result <- tryOneStepWith ugen fs
                      case result of
                       Nothing -> return fs
                       Just (fs', _) ->  count (m-1) fs'

-- |A version of tryFSOpWith using defaultAllUGen representing all 10 rules for updates.
tryFSOp :: Forcible a => (ForceState -> Try ForceState) -> a -> Try a
tryFSOp = tryFSOpWith defaultAllUGen

-- |A try version of the main force function using defaultAllUGen representing all 10 rules for updates.
-- This returns Left report on discovering a stuck Tgraph and Right a (with a the resulting forcible) otherwise.
tryForce:: Forcible a => a -> Try a
tryForce = tryForceWith defaultAllUGen

-- |The main force (partial) function using defaultAllUGen representing all 10 rules for updates.
-- This raises an error on discovering a stuck/incorrect Forcible.
force:: Forcible a => a -> a
force = runTry . tryForce

-- |special case of forcing only half tiles to whole tiles
wholeTiles:: Forcible a => a -> a
wholeTiles = forceWith wholeTileUpdates

-- | forceWith ugen: force using the given UpdateGenerator
-- This raises an error on discovering a stuck/incorrect Forcible.
forceWith:: Forcible a => UpdateGenerator -> a -> a
forceWith ugen = runTry . tryForceWith ugen

-- | try to initialize a force state with the default UpdateGenerator.
-- Returns a Left report if it finds a stuck Forcible.
tryInitFS :: Forcible a => a -> Try ForceState
tryInitFS = tryInitFSWith defaultAllUGen

-- | initialize a force state with the default UpdateGenerator.
-- Raises an error if it finds a stuck Forcible.
initFS :: Forcible a => a -> ForceState
initFS = runTry . tryInitFS

-- |tryStepForce n a - produces a (Right) intermediate Forcible after n steps (n face additions) starting from Forcible a.
-- or a Left report if it encounters a stuck/incorrect Forcible within n steps.
-- If forcing finishes successfully in n or fewer steps, it will return that final Forcible. 
tryStepForce :: Forcible a => Int -> a -> Try a
tryStepForce = tryStepForceWith defaultAllUGen-- Was called tryStepForceFrom

-- |stepForce n a - produces an intermediate Forcible after n steps (n face additions) starting from Forcible a.
-- It raises an error if it encounters a stuck/incorrect Forcible within n steps.
-- If forcing finishes successfully in n or fewer steps, it will return that final Forcible. 
stepForce :: Forcible a => Int -> a ->  a
stepForce n = runTry . tryStepForce n

-- |specialises tryChangeBoundaryWith to the default update generator.
tryChangeBoundary:: Forcible a => (BoundaryState -> Try BoundaryChange) -> a -> Try a
tryChangeBoundary = tryChangeBoundaryWith defaultAllUGen

-- |Forced a is a newtype (for a) to explicitly indicate that a is fully forced.
-- This enables restriction of some functions that should only be applied to a fully forced Forcible.
--
-- To access the Forcible use:  forgetF :: Forced a -> a
--
-- Create an explicitly Forced Forcible using forceF or tryForceF
newtype Forced a = Forced { -- | forget the explicit Forced labelling
                            forgetF :: a  
                          }                 
   deriving (Show)

-- |Extend HasFaces ops from a to Forced a
instance HasFaces a => HasFaces (Forced a) where
    faces = faces . forgetF
    boundary = boundary . forgetF
    maxV = maxV . forgetF
    boundaryVFMap = boundaryVFMap . forgetF

-- | Constructs an explicitly Forced type.
--
-- WARNING: this should only be used when the argument is known to be a fully forced Forcible.
-- Consider using forceF or tryForceF instead for safety reasons.
-- Used in partComposeF and composeF
labelAsForced :: a -> Forced a
labelAsForced = Forced

-- |tryForceF is the same as tryForce except that
-- the successful result is explitly indicated as Forced.
tryForceF :: Forcible a => a -> Try (Forced a)
tryForceF = fmap Forced . tryForce

-- |forceF is the same partial function as force except that
-- the (successful) result is explitly indicated as Forced.
forceF:: Forcible a => a -> Forced a
forceF = runTry . tryForceF

-- | recoverGraphF is an explicitly forced version of recoverGraph
recoverGraphF :: Forced BoundaryState -> Forced Tgraph
recoverGraphF (Forced bs) = Forced (recoverGraph bs)

-- | boundaryStateF is an explicitly forced version of boundaryState
boundaryStateF :: Forced ForceState -> Forced BoundaryState
boundaryStateF (Forced fs) = Forced (boundaryState fs)

-- | makeBoundaryStateF is an explicitly forced version of makeBoundaryState
makeBoundaryStateF :: Forced Tgraph -> Forced BoundaryState
makeBoundaryStateF (Forced g) = Forced (makeBoundaryState g)

-- | initFSF is an explicitly forced version of initFS
initFSF :: Forcible a => Forced a -> Forced ForceState
initFSF (Forced a) = Forced (initFS a)

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
                 _ ->  failReports
                          ["tryAddHalfKite:  on non-boundary edge "
                          ,show e
                          ,"\n"
                          ]
         let (fc,etype) = inspectBDedge bd de
         let tryU | etype == Long = addKiteLongE bd fc
                  | etype == Short = addKiteShortE bd fc
                  | etype == Join && isKite fc = completeHalf bd fc
                  | otherwise = failReport "tryAddHalfKite: applied to dart join (not possible).\n"
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
                _ -> failReports
                        ["tryAddHalfDart:  on non-boundary edge "
                        ,show e
                        ,"\n"
                        ]
         let (fc,etype) = inspectBDedge bd de
         let tryU | etype == Long = addDartLongE bd fc
                  | etype == Short && isKite fc = addDartShortE bd fc
                  | etype == Join && isDart fc = completeHalf bd fc
                  | otherwise = failReport "tryAddHalfDart: applied to short edge of dart or to kite join (not possible).\n"
         u <- tryU
         tryUpdate bd u


-- |tryOneStepWith uGen fs does one force step (used for debugging purposes).
-- It returns either (1) a Right(Just (f,bc)) with a new ForceState f paired with a BoundaryChange bc
-- (using uGen to revise updates in the final ForceState), or (2)
-- a Right Nothing indicating forcing has finished and there are no more updates, or (3)
-- a Left report for a stuck/incorrect graph.
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

-- |tryOneStepForce is a special case of tryOneStepWith using defaultAllUGen (used for debugging).
tryOneStepForce :: ForceState -> Try (Maybe (ForceState,BoundaryChange))
tryOneStepForce = tryOneStepWith defaultAllUGen


{-| After a face addition to a BoundaryState (by either trySafeUpdate or tryUnsafeUpdate), a BoundaryChange records
     (1) the new BoundaryState 
     (2) the list of directed edges that were, but are no longer on the boundary (1,2,or 3),
     (3) a list of boundary edges requiring updates to be recalculated - i.e the new boundary edges and their immediate neighbours (4,3,or 0).
     (4) the face that has been added.
-}
data BoundaryChange = BoundaryChange
                       { newBoundaryState:: BoundaryState -- ^ resulting boundary state
                       , removedEdges:: [Dedge] -- ^ edges no longer on the boundary
                       , revisedEdges :: [Dedge]  -- ^ new boundary edges plus immediate boundary neighbours (requiring new update calculations)
                       , newFace :: TileFace -- ^ face added in the change
                       } deriving (Show)

{-| Given a BoundaryState with a list of one boundary edge or
     two adjacent boundary edges (or exceptionally no boundary edges),
     it extends the list with adjacent boundary edges (to produce 3 or 4 or none).
     It will raise an error if given more than 2 or 2 non-adjacent boundary edges.
     (Used to calculate revisedEdges in a BoundaryChange.
     (N.B. When a new face is fitted in to a hole with 3 sides there is no new boundary. Hence the need to allow for an empty list.)
-}
affectedBoundary :: BoundaryState -> [Dedge] -> [Dedge]
affectedBoundary bd [e1@(a,b)] = [e0,e1,e2] where
           bdry = boundary bd
           e0 = mustFind ((==a).snd) bdry 
                  (\()-> error $ "affectedBoundary: boundary edge not found with snd = "
                            ++ show a ++ "\nand edges: " ++ show [e1]
                            ++ "\nwith boundary:\n" ++ show bdry ++ "\n")
           e2 = mustFind ((==b).fst) bdry
                  (\()-> error $ "affectedBoundary: boundary edge not found with fst = "
                            ++ show b ++ "\nand edges: " ++ show [e1]
                            ++ "\nwith boundary:\n" ++ show bdry ++ "\n")
affectedBoundary bd [e1@(a,b),e2@(c,d)] | c==b = [e0,e1,e2,e3] where
           bdry = boundary bd
           e0 = mustFind ((==a).snd) bdry 
                   (\()-> error $ "affectedBoundary (c==b): boundary edge not found with snd = "
                            ++ show a ++ "\nand edges: " ++ show [e1,e2]
                            ++ "\nwith boundary:\n" ++ show bdry ++ "\n")
           e3 = mustFind ((==d).fst) bdry 
                   (\()-> error $ "affectedBoundary: boundary edge not found with fst = "
                            ++ show d ++ "\nand edges: " ++ show [e1,e2]
                            ++ "\nwith boundary:\n" ++ show bdry ++ "\n")
affectedBoundary bd [e1@(a,b),e2@(c,d)] | a==d  = [e0,e2,e1,e3] where
           bdry = boundary bd
           e0 = mustFind ((==c).snd) bdry 
                   (\()-> error $ "affectedBoundary (a==d): boundary edge not found with snd = "
                            ++ show c ++  "\nand edges: " ++ show [e1,e2]
                            ++ "\nwith boundary:\n" ++ show bdry ++ "\n")
           e3 = mustFind ((==b).fst) bdry 
                   (\()-> error $ "affectedBoundary: boundary edge not found with fst = "
                            ++ show b ++  "\nand edges: " ++ show [e1,e2]
                            ++ "\nwith boundary:\n" ++ show bdry ++ "\n")
affectedBoundary _ [] = [] -- case for filling a triangular hole
affectedBoundary _ edges = error $ "affectedBoundary: unexpected boundary edges "
                             ++ show edges ++ "\n(Either more than 2 or 2 not adjacent)\n"


{-| mustFind (older version requires laziness) - an auxiliary function used to search with definite result.
mustFind p ls default returns the first item in ls satisfying predicate p and returns
default argument when none found (in finite cases).
Special case: the default arg may be used to raise an error when nothing is found.

mustFind :: Foldable t => (p -> Bool) -> t p -> p -> p
mustFind p ls dflt
  = Data.Maybe.fromMaybe dflt (find p ls)
-}

{-| mustFind (strict version) is an auxiliary function used to search with definite result.
mustFind' p ls defaulfnt returns the first item in ls satisfying predicate p and returns
defaultfn () when none found (in finite cases).
This is a replacement foran older mustFind that relied on laziness.
This version works in a strict context.
-}
mustFind :: Foldable t => (p -> Bool) -> t p -> (() -> p) -> p
mustFind p ls dflt
  = case find p ls of
    Just a -> a
    Nothing -> dflt ()

-- |tryReviseUpdates uGen bdChange: revises the UpdateMap after boundary change (bdChange)
-- using uGen to calculate new updates.
tryReviseUpdates:: UpdateGenerator -> BoundaryChange -> UpdateMap -> Try UpdateMap
tryReviseUpdates uGen bdChange umap =
  do let umap' = foldl' (flip Map.delete) umap (removedEdges bdChange)
     umap'' <- applyUG uGen (newBoundaryState bdChange) (revisedEdges bdChange)
     return (Map.union umap'' umap')

-- |tryReviseFSWith ugen bdC fs tries to revise fs after a boundary change (bdC) by calculating
-- the revised updates with ugen (and using the new boundary state in bdC).
tryReviseFSWith :: UpdateGenerator -> BoundaryChange -> ForceState -> Try ForceState
tryReviseFSWith ugen bdC fs =
    do umap <- tryReviseUpdates ugen bdC (updateMap fs)
       return $ ForceState{ boundaryState = newBoundaryState bdC, updateMap = umap}


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
tryUnsafes fs = checkBlocked 0 $ Map.elems $ updateMap fs where
  bd = boundaryState fs
  -- the integer records how many blocked cases have been found so far
  checkBlocked:: Int -> [Update]  -> Try (Maybe BoundaryChange)
  checkBlocked 0 [] = return Nothing
  checkBlocked n [] = failReports 
                        ["tryUnsafes: There are "
                        ,show n
                        ," unsafe updates but ALL unsafe updates are blocked (by touching vertices)\n"
                        ,"This should not happen! However it may arise when accuracy limits are reached on very large Tgraphs.\n"
                        ,"Total number of faces is "
                        ,show (faceCount bd)
                        ,"\n"
                        ]
  checkBlocked n (u: more) = case checkUnsafeUpdate bd u of
                               Nothing -> checkBlocked (n+1) more
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
       vPosition = newVPoints VMap.! v -- Just vPosition = VMap.lookup v newVPoints
       fDedges = faceDedges newface
       matchedDedges = filter (\(x,y) -> x /= v && y /= v) fDedges -- singleton
       newDedges = map reverseD (fDedges \\ matchedDedges) -- two edges
       resultBd = BoundaryState
                    { boundaryDedges = newDedges ++ (boundary bd \\ matchedDedges)
                    , bvFacesMap = changeVFMap newface (bvFacesMap bd)
                    , bvLocMap = newVPoints
                    , allFaces = newface:allFaces bd
                    -- allFaces = newface:faces bd <<<CAUSES SPACE LEAK>>>>
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
       localRevDedges =  [(b,a) | v <- faceVList newface, !f <- bvFacesMap bd VMap.! v, (a,b) <- faceDedges f]
       matchedDedges = fDedges `intersect` localRevDedges -- list of 2 or 3
       -- matchedDedges = fDedges `intersect` boundary bd -- list of 2 or 3
       removedBVs = commonVs matchedDedges -- usually 1 vertex no longer on boundary (exceptionally 3)
       newDedges = map reverseD (fDedges \\ matchedDedges) -- one or none
       nbrFaces = nub $ concatMap (facesAtBV bd) removedBVs
       resultBd = BoundaryState
                   { boundaryDedges = newDedges ++ (boundaryDedges bd \\ matchedDedges)
                   , bvFacesMap = foldl' (flip VMap.delete) (changeVFMap newface $ bvFacesMap bd) removedBVs
--                   , bvFacesMap = changeVFMap newface (bvFacesMap bd)
                   , allFaces = newface:allFaces bd
 --                  , bvLocMap = foldr VMap.delete (bvLocMap bd) removedBVs
                   , bvLocMap = foldl' (flip VMap.delete) (bvLocMap bd) removedBVs
                              --remove vertex/vertices no longer on boundary
                   , nextVertex = nextVertex bd
                   }
       bdChange = BoundaryChange
                   { newBoundaryState = resultBd
                   , removedEdges = matchedDedges
                   , revisedEdges = affectedBoundary resultBd newDedges
                   , newFace = newface
                   }
   in if noNewConflict newface nbrFaces
      then Right bdChange
      else failReports
              ["trySafeUpdate:(incorrect tiling)\nConflicting new face  "
              ,show newface
              ,"\nwith neighbouring faces\n"
              ,show nbrFaces
              ,"\n"
              ]


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
       Nothing ->  failReport "tryUpdate: crossing boundary (touching vertices).\n"

-- |This recalibrates a BoundaryState by recalculating boundary vertex positions from scratch with locateGraphVertices.
-- (Used at intervals in tryRecalibratingForce and recalibratingForce).
recalculateBVLocs :: BoundaryState -> BoundaryState
recalculateBVLocs bd = bd {bvLocMap = newlocs} where
    newlocs = VMap.filterWithKey (\k _ -> k `IntSet.member` bvs) $ locateGraphVertices $ recoverGraph bd
    bvs = VMap.keysSet $ bvLocMap bd -- IntSet.fromList $ fst <$> boundary bd

-- |A version of tryForce that recalibrates at 20,000 step intervals by recalculating boundary vertex positions from scratch.
-- This is needed to limit accumulated inaccuracies when large numbers of faces are added in forcing.
tryRecalibratingForce :: Forcible c => c -> Try c
tryRecalibratingForce = tryFSOp recalibrating where
   recalibrating fs = do
       fs' <- tryStepForce 20000 fs
       if null $ updateMap fs'
       then return fs'
       else recalibrating $ fs' {boundaryState = recalculateBVLocs $ boundaryState fs'}

-- |A version of force that recalibrates at 20,000 step intervals by recalculating boundary vertex positions from scratch.
-- This is needed to limit accumulation of errors when large numbers of faces are added in forcing.
-- This raises an error on discovering a stuck/incorrect Forcible.
recalibratingForce :: Forcible c => c -> c
recalibratingForce = runTry . tryRecalibratingForce



{- $rules
FORCING RULES:

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
6. (sunStarUpdates) When a vertex has 3 or 4 whole kite origins (= 6 or 8 half kite origins)
   it must be a sun centre. Also if a vertex has 4 whole dart origins (= 8 half dart origins)
   it must be a star centre.
   Add an appropriate half kite/dart on a boundary long edge at the vertex.
   (This will complete suns (resp. stars) along with rule 1),
7. (jackKiteUpdates) When a dart half has its wing recognised as a jack vertex
   add a missing kite half on its long edge.
8. (kingDartUpdates) When a vertex is a kite wing and also an origin for exactly 4 dart halves
   it must be a king vertex.
   Add a missing dart half (on any boundary long edge of a dart at the vertex).
9. (queenDartUpdates) If there are more than 2 kite wings at a vertex (necessarily a queen)
   add any missing half dart on a boundary kite long edge. (More than 2 is still valid - was =4)
10.(queenKiteUpdates) If there are more than 2 kite wings at a vertex (necessarily a queen)
   add any missing fourth half kite on a boundary kite short edge. (More than 2 rather than =3 to trap false queen case)

There is an update generator for each rule as well as combined update generators (defaultAllUGen, allUGenerator).

The rules are based on the 7 vertex types:

sun, star, jack, queen, king, ace (fool), deuce

-}

{-------------------  FORCING RULES and Update Generators --------------------------
7 vertex types are:
sun, queen, jack (largeDartBase), ace (fool), deuce (largeKiteCentre), king, star
-}

-- |combineUpdateGenerators combines a list of update generators into a single update generator.
-- When used, the generators are tried in order on each boundary edge (in the supplied focus edges),
-- and will return a Left..(fail report) for the first generator that produces a Left..(fail report) if any.
combineUpdateGenerators :: [UpdateGenerator] -> UpdateGenerator
combineUpdateGenerators gens = UpdateGenerator genf where
  genf bd focus =
    do let addGen (Right (es,umap)) gen =
             do umap' <- applyUG gen bd es
                let es' = es \\ Map.keys umap'
                return (es',Map.union umap' umap)
           addGen other _  = other  -- fails with first failing generator
       (_ , umap) <- foldl' addGen (Right (focus,Map.empty)) gens
       return umap

{-| allUGenerator was the original generator for all updates.
    It combines the individual update generators for each of the 10 rules in sequence using combineUpdateGenerators
    (See also defaultAllUGen which is defined without using combineUpdateGenerators)
-}
allUGenerator :: UpdateGenerator
allUGenerator = combineUpdateGenerators generators where
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
                 ]


-- |UFinder (Update case finder functions). Given a BoundaryState and a list of (focus) boundary directed edges,
-- such a function returns each focus edge satisfying the particular update case paired with the tileface
-- matching that edge. For example, if the function is looking for dart short edges on the boundary,
-- it will return only those focus edges which are half-dart short edges,
-- each paired with its half-dart face.
type UFinder = BoundaryState -> [Dedge] -> [(Dedge,TileFace)]

-- |UChecker (Update checker functions). Given a BoundaryState and a particular tileface (on the boundary),
-- such functions try to produce particular updates on the boundary edge of the given tileface.
-- [They are called update checkers because they may uncover an incorrect/stuck tiling
-- when creating the update.]
-- As an example, addKiteShortE will try to produce an update to add a half-kite with short edge against the boundary.
-- Such a function can be used with a UFinder that either returns dart halves with short edge on the boundary
-- (nonKDarts in rule 2) or returns kite halves with short edge on the boundary
-- (kitesWingDartOrigin in rule 3).
type UChecker = BoundaryState -> TileFace -> Try Update

{-|This is a general purpose filter (previously used to create UFinder functions for each force rule).
 It requires a face predicate where
 the face predicate takes a BoundaryState bd, a boundary Dedge (a,b) and the TileFace with the edge (b,a)
 and decides whether the face is wanted or not (True = wanted)
 This will be used to filter all the faces at the focus edges 
 (when given a BoundaryState and list of focus edges).
 For some predicates the BoundaryState argument is not used (eg boundaryJoin in incompleteHalves), 
 but for others it is used to look at other faces at b or at a besides the supplied face 
 (eg in kitesWingDartOrigin) 
-}
boundaryFilter::  (BoundaryState -> Dedge -> TileFace -> Bool) -> UFinder
boundaryFilter predF bd focus =
    [ (e,fc) | e <- focus
             , fc <- facesAtBV bd (fst e)
             , fc `elem` facesAtBV bd (snd e)
             , predF bd e fc
             ]


{-|This is a general purpose filter used to create UFinder functions for each force rule.
 It requires an edgeType and a face predicate.
 The face predicate takes a BoundaryState bd, and a TileFace
 and (assuming the boundary edge of the face matches the given edgeType)
 decides whether the face is wanted or not (True = wanted).
 This will be used to filter all the faces at the focus edges 
 (when given a BoundaryState and list of focus edges).
 For some predicates the BoundaryState argument is not used (eg boundaryJoin in incompleteHalves), 
 but for others it is used to look at other faces besides the supplied face 
 (eg in kitesWingDartOrigin) 
-}
boundaryEdgeFilter::  EdgeType -> (BoundaryState -> TileFace -> Bool) -> UFinder
boundaryEdgeFilter etype predF bd focus =
     [ (e,fc) | e <- focus
              , let (fc,etype') = inspectBDedge bd e
              , etype == etype'
              , predF bd fc
              ]

-- |makeUpdate f x constructs a safe update if x is Just(..) and an unsafe update if x is Nothing
makeUpdate:: (Vertex -> TileFace) -> Maybe Vertex ->  Update
makeUpdate f (Just !v) = fv `seq` SafeUpdate fv where fv = f v
makeUpdate f Nothing  = UnsafeUpdate f



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
                   hasAnyMatchingE (map shortE $ filter isKite $ facesAtBV bd v)

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
isKiteWing bd v = v `elem` map wingV (filter isKite (facesAtBV bd v))

-- |isKiteOppV bd v - Vertex v is a kite oppV in BoundaryState bd
isKiteOppV:: BoundaryState -> Vertex -> Bool
isKiteOppV bd v = v `elem` map oppV (filter isKite (facesAtBV bd v))

-- |isDartOrigin bd v - Vertex v is a dart origin in BoundaryState bd
isDartOrigin:: BoundaryState -> Vertex -> Bool
isDartOrigin bd v = v `elem` map originV (filter isDart (facesAtBV bd v))

-- |A boundary vertex with >2 kite wings is a queen vertex 
-- (needing a fourth kite on a kite short edge or dart on a kite long edge)
mustbeQueen:: BoundaryState -> Vertex -> Bool
mustbeQueen bd v = kiteWingCount bd v >2

-- |kiteWingCount bd v - the number of kite wings at v in BoundaryState bd
kiteWingCount:: BoundaryState -> Vertex -> Int
kiteWingCount bd v = length $ filter ((==v) . wingV) $ filter isKite (facesAtBV bd v)

-- |mustbeJack  is true of a boundary vertex if
-- it is the wing of two darts not sharing a long edge or
-- it is a wing of a dart and also a kite origin
-- (false means it is either undetermined or is a deuce).
mustbeJack :: BoundaryState -> Vertex -> Bool
mustbeJack bd v =
  (length dWings == 2 && not (hasAnyMatchingE (map longE dWings))) || -- 2 dart wings and dart long edges not shared.
  (length dWings == 1 && isKiteOrigin)
  where fcs = facesAtBV bd v
        dWings = filter ((==v) . wingV) $ filter isDart fcs
        isKiteOrigin = v `elem` map originV (filter isKite fcs)

-- |hasMatching asks if a directed edge list has any two matching (=opposing) directed edges.
hasAnyMatchingE :: [Dedge] -> Bool
hasAnyMatchingE ((x,y):more) = (y,x) `elem` more || hasAnyMatchingE more
hasAnyMatchingE [] = False

{-| newUpdateGenerator combines an update case finder (UFinder) with its corresponding update checker (UChecker)
    to produce an update generator.
    This is used to make each of the 10 update generators corresponding to 10 rules. 
    
    When the generator is applied (with applyUG) to a BoundaryState and list of focus edges,
    the finder produces a list of pairs of dedge and face,
    the checker is used to convert the face in each pair to an update (which can fail with a Left report),
    and the new updates are returned as a map (with the dedges as keys) in a Right result.
-}
newUpdateGenerator :: UChecker -> UFinder -> UpdateGenerator
newUpdateGenerator checker finder = UpdateGenerator genf where
  genf bd edges = foldl' addU (Right Map.empty) (finder bd edges) where
     addU (Left x) _          = Left x
     addU (Right ump) (e,fc)  = do u <- checker bd fc
                                   return (Map.insert e u ump)

{-  makeGenerator (deprecated) this is renamed as newUpdateGenerator.
makeGenerator :: UChecker -> UFinder -> UpdateGenerator
makeGenerator = newUpdateGenerator
 -}

--   Ten Update Generators (with corresponding Finders)


-- |Update generator for rule (1)
wholeTileUpdates:: UpdateGenerator
wholeTileUpdates = newUpdateGenerator completeHalf incompleteHalves

-- |Find faces with missing opposite face (mirror face)  
incompleteHalves :: UFinder
incompleteHalves = boundaryEdgeFilter Join anyFace where
    anyFace _ _ = True
{- incompleteHalves = boundaryFilter boundaryJoin where
    boundaryJoin _ (a,b) fc = joinE fc == (b,a)
 -}

-- |Update generator for rule (2)
aceKiteUpdates :: UpdateGenerator
aceKiteUpdates = newUpdateGenerator addKiteShortE nonKDarts

-- |Find half darts with boundary short edge
nonKDarts :: UFinder
nonKDarts = boundaryEdgeFilter Short foundDart where
    foundDart _ = isDart
{- nonKDarts = boundaryFilter bShortDarts where
    bShortDarts _ (a,b) fc = isDart fc && shortE fc == (b,a)
 -}

-- |Update generator for rule (3)
 -- queen and king vertices add a missing kite half (on a boundary kite short edge)
queenOrKingUpdates :: UpdateGenerator
queenOrKingUpdates = newUpdateGenerator addKiteShortE kitesWingDartOrigin

-- |Find kites with boundary short edge where the wing is also a dart origin
kitesWingDartOrigin :: UFinder
kitesWingDartOrigin = boundaryEdgeFilter Short kiteWDO where
   kiteWDO bd fc = isKite fc && isDartOrigin bd (wingV fc)
{- kitesWingDartOrigin = boundaryFilter kiteWDO where
   kiteWDO bd (a,b) fc = shortE fc == (b,a)
                         && isKite fc && isDartOrigin bd (wingV fc)
 -}

{-| Update generator for rule (4)
     (for deuce vertices = largeKiteCentres)
     Kites whose short edge (b,a) matches a boundary edge (a,b) where their oppV 
     has 2 other kite halves sharing a shortE.
     These need a dart adding on the short edge.
-}
deuceDartUpdates :: UpdateGenerator
deuceDartUpdates = newUpdateGenerator addDartShortE kiteGaps

-- |Find kite halves with a short edge on the boundary 
-- where there are 2 other kite halves sharing a short edge
-- at oppV of the kite half.
kiteGaps :: UFinder
kiteGaps = boundaryEdgeFilter Short kiteGap where
  kiteGap bd fc = isKite fc && mustbeDeuce bd (oppV fc)
{- kiteGaps = boundaryFilter kiteGap where
  kiteGap bd (a,b) fc = shortE fc == (b,a)
                        && isKite fc && mustbeDeuce bd (oppV fc)
 -}

-- |Update generator for rule (5)
-- jackDartUpdates - jack vertex add a missing second dart
jackDartUpdates :: UpdateGenerator
jackDartUpdates = newUpdateGenerator addDartShortE noTouchingDart

-- |Find kite halves with a short edge on the boundary where oppV must be a jack vertex
-- The function mustbeJack finds if a vertex must be a jack.
noTouchingDart :: UFinder
noTouchingDart = boundaryEdgeFilter Short farKOfDarts where
   farKOfDarts bd fc  = isKite fc && mustbeJack bd (oppV fc)
{- noTouchingDart = boundaryFilter farKOfDarts where
   farKOfDarts bd (a,b) fc  = shortE fc == (b,a)
                              && isKite fc && mustbeJack bd (oppV fc)
 -}

{-| Update generator for rule (6)
sunStarUpdates is for vertices that must be either sun or star 
almostSunStar finds half-kites/half-darts with a long edge on the boundary
where their origin vertex has 8 total half-kites/half-darts respectively
or their origin vertex has 6 total half-kites in the case of kites only
completeSunStar will add a new face of the same type (dart/kite) 
sharing the long edge.
-}
sunStarUpdates :: UpdateGenerator
sunStarUpdates = newUpdateGenerator completeSunStar almostSunStar

-- |Find a boundary long edge of either
-- a dart where there are at least 7 dart origins (mustbeStar), or
-- a kite where there are at least 5 kite origins (mustbeSun).
almostSunStar :: UFinder
almostSunStar = boundaryEdgeFilter Long multiples57 where
    multiples57 bd fc = 
        (isDart fc && mustbeStar bd (originV fc)) ||
        (isKite fc && mustbeSun bd (originV fc))
{- almostSunStar = boundaryFilter multiples57 where
    multiples57 bd (a,b) fc = longE fc == (b,a) &&
        ((isDart fc && mustbeStar bd (originV fc)) ||
         (isKite fc && mustbeSun bd (originV fc))
        )
 -}

-- |Update generator for rule (7)
-- jack vertices with dart long edge on the boundary - add missing kite top.
-- The function mustbeJack finds if a vertex must be a jack.
jackKiteUpdates :: UpdateGenerator
jackKiteUpdates = newUpdateGenerator addKiteLongE jackMissingKite

-- |Find a boundary long edge of a dart where the wingV is a jack vertex.
-- The function mustbeJack finds if a vertex must be a jack.
jackMissingKite :: UFinder
jackMissingKite = boundaryEdgeFilter Long dartsWingDB where
    dartsWingDB bd fc = isDart fc && mustbeJack bd (wingV fc)
{- jackMissingKite = boundaryFilter dartsWingDB where
    dartsWingDB bd (a,b) fc = longE fc == (b,a) &&
                              isDart fc && mustbeJack bd (wingV fc)
 -}
-- |Update generator for rule (8)
-- king vertices with 2 of the 3 darts  - add another half dart on a boundary long edge of existing darts
kingDartUpdates :: UpdateGenerator
kingDartUpdates = newUpdateGenerator addDartLongE kingMissingThirdDart

-- |Find a dart long edge on the boundary where the originV must be a king vertex.
-- The function mustbeKing finds if a vertex must be a king
-- (2 of the 3 darts at the origin plus a kite wing at the origin).
kingMissingThirdDart :: UFinder
kingMissingThirdDart = boundaryEdgeFilter Long predicate where
    predicate bd fc = isDart fc && mustbeKing bd (originV fc)
{- kingMissingThirdDart = boundaryFilter predicate where
    predicate bd (a,b) fc = longE fc == (b,a) &&
        isDart fc && mustbeKing bd (originV fc)

 -}
-- |Update generator for rule (9)
-- queen vertices (more than 2 kite wings) with a boundary kite long edge - add a half dart
queenDartUpdates :: UpdateGenerator
queenDartUpdates = newUpdateGenerator addDartLongE queenMissingDarts

-- |Find a boundary kite long edge where the wingV must be a queen vertex
-- (more than 2 kite wings at the wingV).
queenMissingDarts :: UFinder
queenMissingDarts = boundaryEdgeFilter Long predicate where
    predicate bd fc = isKite fc && length kiteWings >2
           where fcWing = wingV fc
                 kiteWings = filter ((==fcWing) . wingV) $
                             filter isKite $ facesAtBV bd fcWing
{- queenMissingDarts = boundaryFilter predicate where
    predicate bd (a,b) fc =
        longE fc == (b,a) && isKite fc && length kiteWings >2
           where fcWing = wingV fc
                 kiteWings = filter ((==fcWing) . wingV) $
                             filter isKite $ facesAtBV bd fcWing
 -}
-- |Update generator for rule (10)
-- queen vertices with more than 2 kite wings -- add missing half kite on a boundary kite short edge
queenKiteUpdates :: UpdateGenerator
queenKiteUpdates = newUpdateGenerator addKiteShortE queenMissingKite

-- |Find a kite short edge on the boundary where the wingV must be a queen vertex
-- (more than 2 kite wings at the wingV).
queenMissingKite :: UFinder
queenMissingKite = boundaryEdgeFilter Short predicate where
    predicate bd fc =
        isKite fc && length kiteWings >2
        where fcWing = wingV fc
              kiteWings = filter ((==fcWing) . wingV) $ filter isKite (facesAtBV bd fcWing)
{- queenMissingKite = boundaryFilter predicate where
    predicate bd (a,b) fc =
        shortE fc == (b,a) && isKite fc && length kiteWings >2
           where fcWing = wingV fc
                 kiteWings = filter ((==fcWing) . wingV) $ filter isKite (facesAtBV bd fcWing)
 -}

--  Six Update Checkers


-- |completeHalf will check an update to
--  add a symmetric (mirror) face for a given face at a boundary join edge.
completeHalf :: UChecker
completeHalf bd (LD(a,b,_)) = makeUpdate makeFace <$> x where
        makeFace v = makeRD a v b --RD (a,v,b)
        x = tryFindThirdV bd (b,a) (3,1) --anglesForJoinRD
completeHalf bd (RD(a,_,b)) = makeUpdate makeFace <$> x where
        makeFace v = makeLD a b v --LD (a,b,v)
        x = tryFindThirdV bd (a,b) (1,3) --anglesForJoinLD
completeHalf bd (LK(a,_,b)) = makeUpdate makeFace <$> x where
        makeFace v = makeRK a b v --RK (a,b,v)
        x = tryFindThirdV bd (a,b) (1,2) --anglesForJoinRK
completeHalf bd (RK(a,b,_)) = makeUpdate makeFace <$> x where
        makeFace v = makeLK a v b --LK (a,v,b)
        x = tryFindThirdV bd (b,a) (2,1) --anglesForJoinLK

-- |add a (missing) half kite on a (boundary) short edge of a dart or kite
addKiteShortE :: UChecker
addKiteShortE bd (RD(_,b,c)) = makeUpdate makeFace <$> x where
    makeFace v = makeLK v c b --LK (v,c,b)
    x = tryFindThirdV bd (c,b) (2,2) --anglesForShortLK
addKiteShortE bd (LD(_,b,c)) = makeUpdate makeFace <$> x where
    makeFace v = makeRK v c b --RK (v,c,b)
    x = tryFindThirdV bd (c,b) (2,2) --anglesForShortRK
addKiteShortE bd (LK(_,b,c)) = makeUpdate makeFace <$> x where
    makeFace v = makeRK v c b --RK (v,c,b)
    x = tryFindThirdV bd (c,b) (2,2) --anglesForShortRK
addKiteShortE bd (RK(_,b,c)) = makeUpdate makeFace <$> x where
    makeFace v = makeLK v c b --LK (v,c,b)
    x = tryFindThirdV bd (c,b) (2,2) --anglesForShortLK

-- |add a half dart top to a boundary short edge of a half kite.
addDartShortE :: UChecker
addDartShortE bd (RK(_,b,c)) = makeUpdate makeFace <$> x where
        makeFace v = makeLD v c b --LD (v,c,b)
        x = tryFindThirdV bd (c,b) (3,1) --anglesForShortLD
addDartShortE bd (LK(_,b,c)) = makeUpdate makeFace <$> x where
        makeFace v = makeRD v c b --RD (v,c,b)
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
    makeFace v = makeRK c v a --RK (c,v,a)
    x = tryFindThirdV bd (a,c) (2,1) -- anglesForLongRK
addKiteLongE bd (RD(a,b,_)) = makeUpdate makeFace <$> x where
    makeFace v = makeLK b a v --LK (b,a,v)
    x = tryFindThirdV bd (b,a) (1,2) -- anglesForLongLK
addKiteLongE bd (RK(a,_,c)) = makeUpdate makeFace <$> x where
  makeFace v = makeLK a c v --LK (a,c,v)
  x = tryFindThirdV bd (a,c) (1,2) -- anglesForLongLK
addKiteLongE bd (LK(a,b,_)) = makeUpdate makeFace <$> x where
  makeFace v = makeRK a v b --RK (a,v,b)
  x = tryFindThirdV bd (b,a) (2,1) -- anglesForLongRK

-- |add a half dart on a boundary long edge of a dart or kite
addDartLongE :: UChecker
addDartLongE bd (LD(a,_,c)) = makeUpdate makeFace <$> x where
  makeFace v = makeRD a c v --RD (a,c,v)
  x = tryFindThirdV bd (a,c) (1,1) -- anglesForLongRD
addDartLongE bd (RD(a,b,_)) = makeUpdate makeFace <$> x where
  makeFace v = makeLD a v b --LD (a,v,b)
  x = tryFindThirdV bd (b,a) (1,1) -- anglesForLongLD
addDartLongE bd (LK(a,b,_)) = makeUpdate makeFace <$> x where
  makeFace v = makeRD b a v --RD (b,a,v)
  x = tryFindThirdV bd (b,a) (1,1) -- anglesForLongRD
addDartLongE bd (RK(a,_,c)) = makeUpdate makeFace <$> x where
  makeFace v = makeLD c v a --LD (c,v,a)
  x = tryFindThirdV bd (a,c) (1,1) -- anglesForLongLD

{-
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
-}


--  The Default All Update Generator (defaultAllUGen)


-- |The default all update generator (see also allUGenerator). It uses the 10 rules (and the same UCheckers as allUGenerator),
-- but makes decisions based on the EdgeType of a boundary edge (instead of trying each UFinder in turn).
-- If there are any Left..(fail reports) for the given
-- boundary edges the result is a single Left, concatenating all the failure reports (unlike allUGenerator).
defaultAllUGen :: UpdateGenerator
defaultAllUGen = UpdateGenerator { applyUG = gen } where
  gen bd es = combine $ map decide es where -- Either String is a monoid as well as Map
    combine = fmap mconcat . concatFails -- concatenates all failure reports if there are any
                                         -- otherwise combines the update maps with mconcat
    decide e = decider $ inspectBDedge bd e where

      decider (f,Join)  = mapItem (completeHalf bd f) -- rule 1
      decider (f,Short)
        | isDart f = mapItem (addKiteShortE bd f) -- rule 2
        | otherwise = kiteShortDecider f
      decider (f,Long)
        | isDart f = dartLongDecider f
        | otherwise = kiteLongDecider f

      dartLongDecider f
        | mustbeStar bd (originV f) = mapItem (completeSunStar bd f)
        | mustbeKing bd (originV f) = mapItem (addDartLongE bd f)
        | mustbeJack bd (wingV f) = mapItem (addKiteLongE bd f)
        | otherwise = Right Map.empty

      kiteLongDecider f
        | mustbeSun bd (originV f) = mapItem (completeSunStar bd f)
        | mustbeQueen bd (wingV f) = mapItem (addDartLongE bd f)
        | otherwise = Right Map.empty

      kiteShortDecider f
        | mustbeDeuce bd (oppV f) || mustbeJack bd (oppV f) = mapItem (addDartShortE bd f)
        | mustbeQueen bd (wingV f) || isDartOrigin bd (wingV f) = mapItem (addKiteShortE bd f)
        | otherwise = Right Map.empty

      mapItem = fmap (\u -> Map.insert e u Map.empty)

{- defaultAllUGen :: UpdateGenerator
defaultAllUGen = UpdateGenerator { applyUG = gen } where
  gen bd es = combine $ map decide es where -- Either String is a monoid as well as Map
      decide e = decider (e,f,etype) where (f,etype) = inspectBDedge bd e

      decider (e,f,Join)  = mapItem e (completeHalf bd f) -- rule 1
      decider (e,f,Short)
        | isDart f = mapItem e (addKiteShortE bd f) -- rule 2
        | otherwise = kiteShortDecider e f
      decider (e,f,Long)
        | isDart f = dartLongDecider e f
        | otherwise = kiteLongDecider e f

      dartLongDecider e f
        | mustbeStar bd (originV f) = mapItem e (completeSunStar bd f)
        | mustbeKing bd (originV f) = mapItem e (addDartLongE bd f)
        | mustbeJack bd (wingV f) = mapItem e (addKiteLongE bd f)
        | otherwise = Right Map.empty

      kiteLongDecider e f
        | mustbeSun bd (originV f) = mapItem e (completeSunStar bd f)
        | mustbeQueen bd (wingV f) = mapItem e (addDartLongE bd f)
        | otherwise = Right Map.empty

      kiteShortDecider e f
        | mustbeDeuce bd (oppV f) || mustbeJack bd (oppV f) = mapItem e (addDartShortE bd f)
        | mustbeQueen bd (wingV f) || isDartOrigin bd (wingV f) = mapItem e (addKiteShortE bd f)
        | otherwise = Right Map.empty

      mapItem e = fmap (\u -> Map.insert e u Map.empty)
      combine = fmap mconcat . concatFails -- concatenates all failure reports if there are any
                                           -- otherwise combines the update maps with mconcat
 -}

-- |Given a BoundaryState and a directed boundary edge, this returns the same edge with
-- the unique face on that edge and the edge type for that face and edge (Short/Long/Join)
inspectBDedge:: BoundaryState -> Dedge -> (TileFace, EdgeType)
inspectBDedge bd (a,b) = (face,edgeType (b,a) face) where
      face = case filter (isAtV a) $ facesAtBV bd b of
             [f] -> f
             _   -> error $ "inspectBDedge: Not a boundary directed edge " ++ show (a,b) ++ "\n"
 {-
     face = case facesAtBV bd a `intersect` facesAtBV bd b of
         [f] -> f
         _ -> error $ "inspectBDedge: Not a boundary directed edge " ++ show (a,b) ++ "\n"
-}





{- $Additions
Note about face additions:

When adding a new face on a boundary edge we need to use some geometric information.

To check if any other edges of the new face are adjacent on the boundary, we
calculate external angles at the relevant boundary vertices,
using a representation of angles which allows an equality test.
(All angles are integer multiples of 1/10th turn (mod 10) so we use
these integers for comparing angles n where n is 0..9)

No crossing boundary property:
It is important that there are no crossing boundaries to ensure there is a unique external angle at each boundary vertex.

Touching Vertex check:
If only one edge of a new face is on the boundary, we need to create a new vertex.
This will need to have its position checked against other (boundary) vertices to avoid
creating a touching vertex/crossing boundary. This is why BoundaryStates keep track of boundary vertex positions.
(The check is done in tryUnsafeUpdate.)
-}

{-|tryFindThirdV tries to find a neighbouring third vertex on the boundary either side of the dedge
   (for a new face to be added).
   It can return a Left failreport if it discovers an incorrect Tgraph.
   Otherwise it returns a Right Nothing or Right (Just v) depending on whether it found an existing
   suitable vertex.
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
    maybeV | aAngle <1 || aAngle >9
                = failReports 
                   ["tryFindThirdV: vertex: "
                   ,show a
                   ," has (tt) external angle "
                   ,show aAngle
                   ,"\nwhen adding to boundary directed edge: "
                   ,show (a,b)
                   ,"\nwith faces at "
                   ,show a
                   ,":\n"
                   ,show (bvFacesMap bd VMap.! a)
                   ,"\nand faces at "
                   ,show b
                   ,":\n"
                   ,show (bvFacesMap bd VMap.! b), 
                   "\nand a total of "
                   ,show (length $ faces bd)
                   ," faces.\n"
                   ]
           | bAngle <1 || bAngle >9
                = failReports
                    ["tryFindThirdV: vertex: "
                    ,show b
                    ," has (tt) external angle "
                    ,show bAngle
                    ,"\nwhen adding to boundary directed edge: "
                    ,show (a,b)
                    ,"\nwith faces at "
                    ,show a
                    ,":\n"
                    ,show (bvFacesMap bd VMap.! a)
                    ,"\nand faces at "
                    ,show b
                    ,":\n"
                    ,show (bvFacesMap bd VMap.! b)
                    ,"\nand a total of "
                    ,show (faceCount bd)
                    ," faces.\n"
                    ]
           | aAngle < n
                = failReports
                    ["tryFindThirdV: Found incorrect graph (stuck tiling)\nConflict at edge: "
                    ,show (a,b)
                    ,"\n"
                    ]
           | bAngle < m
                = failReports
                    ["tryFindThirdV: Found incorrect graph (stuck tiling)\nConflict at edge: "
                    ,show (a,b)
                    ,"\n"
                    ]
           | aAngle == n = case find ((==a) . snd) (boundary bd) of
                             Just pr -> Right $ Just (fst pr)
                             Nothing -> failReports
                                          ["tryFindThirdV: Impossible boundary. No predecessor/successor Dedge for Dedge "
                                          ,show (a,b)
                                          ,"\n"
                                          ]
           | bAngle == m = case find ((==b) . fst) (boundary bd) of
                             Just pr -> Right $ Just (snd pr)
                             Nothing -> failReports
                                           ["tryFindThirdV: Impossible boundary. No predecessor/successor Dedge for Dedge "
                                           ,show (a,b)
                                           ,"\n"
                                           ]
           | otherwise =   Right  Nothing

-- |externalAngle bd v - calculates the external angle at boundary vertex v in BoundaryState bd as an
-- integer multiple of tt (tenth turn), so 1..9.  It relies on there being no crossing boundaries,
-- so that there is a single external angle at each boundary vertex. 
externalAngle:: BoundaryState -> Vertex -> Int
externalAngle bd v = 10 - sum (map (intAngleAt v) $ facesAtBV bd v)

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


{-------------------------
*************************             
Touching vertex checking 
********************************************
requires Diagrams.Prelude for Point and V2
--------------------------------------------}

-- |touchCheck p vpMap - check if a vertex location p touches (is too close to) any other vertex location in the mapping vpMap
touchCheck:: Point V2 Double -> VertexMap (Point V2 Double) -> Bool
touchCheck p vpMap = not $ VMap.null $ VMap.filter (touching p) vpMap
  -- filtering the map has better performance than checking a list
  -- touchCheck p vpMap = any (touching p) (VMap.elems vpMap)