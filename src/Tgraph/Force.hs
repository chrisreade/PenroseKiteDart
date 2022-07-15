{-|
Module      : Tgraph.Force
Description : The main force function for Tgraphs 
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

This module includes force plus related operations for testing and experimenting.
It also exposes the calculation of relative angle of edges at vertices used to find existing edges.
It uses a touching check for adding new vertices (using Tgraph.Convert.creatVPoints for vertex locations)
-}
module Tgraph.Force  where

import Data.List ((\\), intersect, nub, find,foldl')
import qualified Data.Map as Map (Map, empty, delete, elems, assocs, insert) -- used for UpdateMap
import qualified Data.IntMap.Strict as VMap (IntMap, elems, filterWithKey, insert, empty, alter, delete, lookup)-- used for Boundaries
import Diagrams.Prelude (Point, V2) -- necessary for touch check (touchCheck) used in tryUnsafeUpdate 
import Tgraph.Convert(touching, createVPoints, addVPoints)
import Tgraph.Prelude

{-
***************************************************************************   
NEW FORCING with 
  Boundaries 
  Touching Vertex Check
  Incremented Update Maps
***************************************************************************
-}

{- *
Touching vertex checking
-}
{-|
    touchCheckOn is a global variable used to switch on/off
    checks for touching vertices when new vertices are added to a graph.
    This should be True to avoid creating non-planar Tgraphs.
    However it can be changed to False to make forcing more efficient but then any
    result of forcing needs to be checked for touching vertices using touchCheck.
-}
touchCheckOn::Bool
touchCheckOn = True
{-------------------------
*************************             
Touching vertex checking 
********************************************
requires Diagrams.Prelude for Point and V2
--------------------------------------------}

-- |Abbreviation for Mapping with Vertex keys (used for Boundaries)
type VertexMap a = VMap.IntMap a

-- |check if a vertex location touches (is too close to) any other vertex location in the mapping
touchCheck:: Point V2 Double -> VertexMap (Point V2 Double) -> Bool
touchCheck p vpMap = touchCheckOn && any (touching p) (VMap.elems vpMap)

{-
-- |check if two vertex locatioons are too close (indicating touching vertices)
tooClose :: Point V2 Double  -> Point V2 Double -> Bool
tooClose p p' = quadrance (p .-. p') < 0.25 -- quadrance is square of length of a vector
-}


{- *
Boundary operations
-}
{-| A Boundary records
the boundary directed edges (directed so that faces are on LHS and exterior is on RHS)
plus 
a mapping of boundary vertices to their incident faces, plus
a mapping of boundary vertices to positions (using Tgraph.Prelude.createVPoints).
It also keeps track of all the faces and vertices, 
and the next vertex label to be used when adding a new vertex.
Note that bvFacesMap is initially only defined for boundary vertices,
but the information is not removed when a vertex is no longer on the boundary (after an update).
Similarly for bvLocMap.
-}
data Boundary 
  = Boundary
    { bDedges:: [DEdge]  -- ^ boundary directed edges (face on LHS, exterior on RHS)
    , bvFacesMap:: VertexMap [TileFace] -- ^faces at each boundary vertex
    , bvLocMap:: VertexMap (Point V2 Double)  -- ^ position of each boundary vertex
    , allFaces:: [TileFace] -- ^ all the tile faces
    , allVertices:: [Vertex] -- ^ all the vertices
    , nextVertex::  Vertex -- ^ next vertex number
    } deriving (Show)

-- |Calculates Boundary information from a Tgraph
makeBoundary:: Tgraph -> Boundary
makeBoundary g = 
  let bdes = boundaryDedges g
      bvs = fmap fst bdes -- (fmap snd bdes would also do) for all boundary vertices
      bvLocs = VMap.filterWithKey (\k _ -> k `elem` bvs) $ createVPoints $ faces g
      addFacesAt v = VMap.insert v $ filter (isAtV v) (faces g) 
  in Boundary
      { bDedges = bdes
      , bvFacesMap = foldr addFacesAt VMap.empty bvs
      , bvLocMap = bvLocs 
      , allFaces = faces g
      , allVertices = vertices g
      , nextVertex = makeNewV (vertices g)
      }
 
-- |Converts a Boundary back to a Tgraph
recoverGraph:: Boundary -> Tgraph
recoverGraph bd = 
  Tgraph{ faces = allFaces bd
        , vertices = allVertices bd
        }

-- |changeVFMap f vfmap vs - adds f to the list of faces associated with each v in vs and updates vfmap
changeVFMap::  TileFace -> VertexMap [TileFace] -> [Vertex] -> VertexMap [TileFace]
changeVFMap f = foldr (VMap.alter addf) where
   addf Nothing = Just [f]
   addf (Just fs) = Just (f:fs)
   
   
-- |facesAtBV bd v - returns the faces found at v (which must be a boundary vertex)
facesAtBV:: Boundary -> Vertex -> [TileFace]
facesAtBV bd v = case VMap.lookup v (bvFacesMap bd) of
            Just fcs -> fcs
            Nothing -> error ("facesAtBV: no faces found at boundary vertex " ++ show v)


-- |return the (set of) faces which have a boundary vertex from boundary information
boundaryFaces :: Boundary -> [TileFace]
boundaryFaces = nub . concat . VMap.elems . bvFacesMap

{- *
Updates and ForceState types
-}

-- |An Update is a pair of
-- a Maybe Vertex identifying the third vertex for a face addition (Nothing means it needs to be created)
-- and a makeFace function to create the new face when given the third vertex.
type Update = (Maybe Vertex, Vertex -> TileFace)

-- |UpdateMap: partial map associating updates with (some) boundary directed edges.
type UpdateMap = Map.Map DEdge Update

-- |ForceState: The force state records information between executing single face updates during forcing
-- (a Boundary and an UpdateMap).
data ForceState = ForceState 
                   { boundaryState:: Boundary
                   , updateMap:: UpdateMap 
                   }

{-|UpdateGenerator is the type of functions which can change the UpdateMap when given
a Boundary and a focus list of particular directed boundary edges.  
Such functions should add particular updates for those focus edges which match a rule. 
So such functions implement the forcing rules.
-}
type UpdateGenerator = Boundary -> [DEdge] -> UpdateMap -> UpdateMap

{-| BoundaryChange records the new boundary after an update (by either doSafeUpdate or tryUnsafeUpdate)
     along with a list of directed edges which are no longer on the boundary,
     plus a list of boundary edges revised (and requiring updates to be recalculated)
     See affectedBoundary.
-}
data BoundaryChange = BoundaryChange 
                       { newBoundary:: Boundary -- ^ resulting boundary
                       , removedEdges:: [DEdge] -- ^ edges no longer on the boundary
                       , revisedEdges :: [DEdge]  -- ^ boundary edges requiring new update calculations
                       } deriving (Show)

{-| Given a Boundary with a list of boundary edges that have been newly added,
     it creates a list of boundary edges
     that are affected. Namely, the boundary edges sharing a vertex with a new edge.
     For a safe update this will be
     the single new edge + edges either side on the boundary.
     For an unsafe update this will be
     4 edges including the 2 new ones. (Used to make revisedEdges in a BoundaryChange)
-}
affectedBoundary :: Boundary -> [DEdge] -> [DEdge]
affectedBoundary bd edges = filter incidentEdge (bDedges bd) where
      bvs = nub (fmap fst edges ++ fmap snd edges) -- boundary vertices affected
      incidentEdge (a,b) = a `elem` bvs || b `elem` bvs

{- *
forcing operations
-}

-- |The main force function using allUGenerator representing all 10 rules for updates
force:: Tgraph -> Tgraph
force = forceWith allUGenerator

-- |special case of forcing only half tiles to whole tiles
wholeTiles:: Tgraph -> Tgraph
wholeTiles = forceWith wholeTileUpdates 

{-| forceWith uGen: 
     initialises force state before forcing (both using uGen to generate updates).
     It recursively does all updates using forceAll uGen, 
     then gets boundary from the final state and converts back to a Tgraph
-}
forceWith:: UpdateGenerator -> Tgraph -> Tgraph
forceWith uGen = recoverGraph . boundaryState . forceAll uGen . initForceState uGen

{-| forceAll uGen recursively does updates using uGen until there are no more updates, 
-}
forceAll :: UpdateGenerator -> ForceState -> ForceState
forceAll uGen = retry where
  retry fs = case findSafeUpdate (updateMap fs) of
               Just u -> retry $ ForceState{ boundaryState = newBoundary bdChange, updateMap = umap}
                          where bdChange = doSafeUpdate (boundaryState fs) u
                                umap = reviseUpdates uGen bdChange (updateMap fs)
               _  -> case tryUnsafes fs of
                      Just bdC -> retry $ ForceState{ boundaryState = newBoundary bdC, updateMap = umap}
                                  where umap = reviseUpdates uGen bdC (updateMap fs)
                      Nothing  -> fs           -- no more updates

{-| initForceState uGen g calculates an initial force state with boundary information from g
     and uses uGen on all boundary edges to initialise updateMap.
-}
initForceState :: UpdateGenerator -> Tgraph -> ForceState
initForceState uGen g = ForceState { boundaryState = bd , updateMap = umap } where
     bd = makeBoundary g
     umap = uGen bd (bDedges bd) Map.empty 

-- |reviseUpdates uGen bdChange: updates the UpdateMap after boundary change (bdChange)
-- using uGen to calculate new updates.
reviseUpdates:: UpdateGenerator -> BoundaryChange -> UpdateMap -> UpdateMap
reviseUpdates uGen bdChange umap = umap'' where
  umap' = foldr Map.delete umap (removedEdges bdChange)
  umap'' = uGen (newBoundary bdChange) (revisedEdges bdChange) umap' 

-- |safe updates are those which do not require a new vertex, 
-- so have an identified existing vertex (Just v)
isSafeUpdate :: Update -> Bool
isSafeUpdate (Just _ , _ ) = True
isSafeUpdate (Nothing, _ ) = False

-- |finds the first safe update - Nothing if there are none (ordering is directed edge key ordering)
findSafeUpdate:: UpdateMap -> Maybe Update 
findSafeUpdate umap = find isSafeUpdate (Map.elems umap)


{- *
Inspecting Force Steps
-}
-- |show applied to a ForceState suppresses the update functions in the updateMap with "fn".
instance Show ForceState where
    show fs = "ForceState{ boundaryState = ...\nbDedges = "
               ++ show (bDedges $ boundaryState fs) 
               ++ ",\nupdateMap = "
               ++ show (fmap (\(e,(mv,_)) -> (e,(mv,"fn"))) $ Map.assocs $ updateMap fs) ++ " }\n"
-- |stepForce  produces an intermediate state after a given number of steps (face additions)
stepForce :: Int -> Tgraph -> ForceState
stepForce n g = stepForceWith allUGenerator n $ initForceState allUGenerator g


-- |do a number of force steps using a given UpdateGenerator (used by stepForce)
stepForceWith :: UpdateGenerator -> Int -> ForceState -> ForceState
stepForceWith updateGen = count where
  count 0 fs = fs
  count n fs = case oneStepWith updateGen fs of
                Nothing -> fs
                Just (fs', _) ->  count (n-1) fs'

{- *
Single Force Steps
-}

-- |oneStepWith uGen fs uses uGen to find updates and does one force step.
-- It returns a (maybe) new force sate but paired with the boundary change for debugging purposes
oneStepWith :: UpdateGenerator -> ForceState -> Maybe (ForceState,BoundaryChange)
oneStepWith uGen fs = 
      case findSafeUpdate (updateMap fs) of
      Just u -> Just (ForceState{ boundaryState = newBoundary bdChange, updateMap = umap},bdChange)
                where bdChange = doSafeUpdate (boundaryState fs) u
                      umap = reviseUpdates uGen bdChange (updateMap fs)
      _  -> case tryUnsafes fs of
            Just bdC -> Just (ForceState{ boundaryState = newBoundary bdC, updateMap = umap},bdC)
                        where umap = reviseUpdates uGen bdC (updateMap fs)
            Nothing  -> Nothing           -- no more updates

-- |oneStepF is a special case of oneStepWith only used for debugging
oneStepF :: ForceState -> Maybe (ForceState,BoundaryChange)
oneStepF = oneStepWith allUGenerator




{-| tryUnsafes: Should only be used when there are no Safe updates in the UpdateMap.
   When touchChecKOn is True any unsafe update producing a touching vertex returns Nothing.
   tryUnsafes works through the unsafe updates in (directed edge) key order and
   completes the first unsafe update that is not blocked, returning
   Just the resulting boundary change (if there was one) and Nothing if all unsafes are blocked.
-}
tryUnsafes:: ForceState -> Maybe BoundaryChange
tryUnsafes fs = tryList $ Map.elems $ updateMap fs where
    bd = boundaryState fs
--    tryList = listToMaybe . mapMaybe (tryUnsafeUpdate bd)
    tryList [] = Nothing
    tryList (u: more) = case tryUnsafeUpdate bd u of
                          Nothing -> tryList more
                          Just bdC -> Just bdC


{-| doSafeUpdate bd u adds a new face by completing a safe update u on boundary bd
    (raising an error if u is an unsafe update).
     It returns a BoundaryChange (containing a new Boundary, removed boundary edges and
     revised boundary edge list).
     It checks that the new face is not in conflict with existing faces,
     raising an error if there is a conflict.
-}
doSafeUpdate:: Boundary -> Update -> BoundaryChange
doSafeUpdate bd (Nothing, makeFace) = error "doSafeUpdate: applied to non-safe update "
doSafeUpdate bd (Just v, makeFace) = 
   let newFace = makeFace v
       fDedges = faceDedges newFace
       matchedDedges = fDedges `intersect` bDedges bd -- list of 2
       removedBV = commonV matchedDedges -- vertex no longer on boundary
       newDedges = fmap reverseD (fDedges \\ matchedDedges) -- one or none
       nbrFaces = nub $ concatMap (facesAtBV bd) (faceVList newFace)
       resultBd = Boundary 
                   { bDedges = newDedges ++ (bDedges bd \\ matchedDedges)
                   , bvFacesMap = changeVFMap newFace (bvFacesMap bd) (faceVList newFace)
                   , allFaces = newFace:allFaces bd
                   , bvLocMap = VMap.delete removedBV (bvLocMap bd)  --remove vertex no longer on boundary
                   , allVertices = allVertices bd
                   , nextVertex = nextVertex bd
                   }
       bdChange = BoundaryChange 
                   { newBoundary = resultBd
                   , removedEdges = matchedDedges
                   , revisedEdges = affectedBoundary resultBd newDedges
                   }
   in if noConflict newFace nbrFaces then bdChange else
      error ("doSafeUpdate:(incorrect tiling)\nConflicting new face  "
             ++ show newFace
             ++ "\nwith neighbouring faces\n"
             ++ show nbrFaces
             ++ "\nIn graph:\n"
             ++ show (recoverGraph resultBd)
            )

-- | given 2 adjacent directed edges, this returns the common vertex.
-- Raises an error if the argument is not 2 adjacent directed edges
commonV :: [DEdge] -> Vertex
commonV [(a,b),(c,d)] | b==c = b 
                      | d==a = a
                      | otherwise = error $ "commonV: directed edges not adjacent: " ++ show [(a,b),(c,d)]
commonV es = error $ "commonV: argument is not 2 adjacent directed edges: " ++ show es

{-| tryUnsafeUpdate bd u, calculates the resulting boundary change for an unsafe update (u) with a new vertex
     (raising an error if u is a safe update).
     It checks that the new face is not in conflict with existing faces (raising an error if there is a conflict).
     If touchCheckOn is True, it performs a touching vertex check with the new vertex
     returning Nothing if there is a touching vertex.
     Otherwise it returns Just a boundary change.
-}
tryUnsafeUpdate:: Boundary -> Update -> Maybe BoundaryChange
tryUnsafeUpdate bd (Just _ , makeFace) = error "tryUnsafeUpdate: applied to safe update "
tryUnsafeUpdate bd (Nothing, makeFace) = 
   let v = nextVertex bd       
       newFace = makeFace v
       oldVPoints = bvLocMap bd
       newVPoints = addVPoints [newFace] [] oldVPoints
       Just vPosition = VMap.lookup v newVPoints
       fDedges = faceDedges newFace
       matchedDedges = fDedges `intersect` bDedges bd -- singleton
       newDedges = fmap reverseD (fDedges \\ matchedDedges) -- two edges
       nbrFaces = nub $ concatMap (facesAtBV bd) (faceVList newFace \\ [v])
       resultBd = Boundary 
                    { bDedges = newDedges ++ (bDedges bd \\ matchedDedges)
                    , bvFacesMap = changeVFMap newFace (bvFacesMap bd) (faceVList newFace)
                    , bvLocMap = newVPoints
                    , allFaces = newFace:allFaces bd
                    , allVertices = v:allVertices bd
                    , nextVertex = v+1
                    }
       bdChange = BoundaryChange 
                    { newBoundary = resultBd
                    , removedEdges = matchedDedges
                    , revisedEdges = affectedBoundary resultBd newDedges
                    }
   in if touchCheck vPosition oldVPoints -- always False if touchCheckOn = False
      then Nothing -- don't proceed - v is a touching vertex
      else if noConflict newFace nbrFaces  -- check new face does not conflict on edges
           then Just bdChange  
           else error 
                ("tryUnsafeUpdate:(incorrect tiling)\nConflicting new face  "
                 ++ show newFace
                 ++ "\nwith neighbouring faces\n"
                 ++ show nbrFaces
                 ++ "\nIn graph:\n"
                 ++ show (recoverGraph resultBd)
                )


-- |doUpdate: do a single update (safe or unsafe)
doUpdate:: Boundary -> Update -> BoundaryChange
doUpdate bd u = if isSafeUpdate u then doSafeUpdate bd u
                else case tryUnsafeUpdate bd u of
                 Just bdC -> bdC
                 Nothing -> error "doUpdate: crossing boundary (touching vertices)"


{- *
Conflict Test
-}

-- |noConflict fc fcs  where fc is a new face and fcs are neighbouring faces.
-- There is no conflict if none of the new directed face edges of fc are already directed edges
-- of neighbouring faces fcs (in the same direction)
-- and the edge length types (phi/nonPhi) do not conflict.
noConflict :: TileFace -> [TileFace] -> Bool
noConflict fc fcs = null (faceDedges fc `intersect` concatMap faceDedges fcs) &&
                    null (faceNonPhiEdges fc `intersect` concatMap facePhiEdges fcs) &&
                    null (facePhiEdges fc `intersect` concatMap faceNonPhiEdges fcs)





{- *
Forcing Rules become Update Generators
-}
                                    
{- $rules
Forcing rules

1. (wholeTileUpdates) When a join edge is on the boundary - add the missing half tile to make a whole tile.    
2. (kiteBelowDartUpdates) When a half dart has its short edge on the boundary
   add the half kite that must be on the short edge
   (this is at ace vertices but also helps with jack and deuce vertices).  
3. (kiteWingDartOriginUpdates) When a vertex is both a dart origin and a kite wing it must be a queen or king vertex.
   If there is a boundary short edge of a kite half at the vertex, 
   add another kite half sharing the short edge. 
   (This converts 1 kite to 2 and 3 kites to 4 in combination with the first rule).
4. (kiteGapUpdates) When two half kites share a short edge their oppV vertex must be a deuce vertex.
   Add any missing half darts needed to complete the vertex.
   (In the gap between the other two short edges of the full kites).
5. (secondTouchingDartUpdates) When a single dart wing is at a vertex which is recognised as an incomplete jack vertex
   and has a complete kite below the dart wing, 
   add a second dart half touching at the vertex (sharing the kite below).
   This is also known as a *largeDartBase* vertex (= new dart base next level up - see later)
6. (sunStarUpdates) When a vertex has 3 or 4 whole kite origins (= 6 or 8 half kite origins)
   it must be a sun centre. Also if a vertex has 4 whole dart origins (= 8 half dart origins)
   it must be a star centre.
   Add an appropriate half kite/dart on a boundary long edge at the vertex.
   (This will complete suns (resp. stars) along with rule 1),
7. (dartKiteTopUpdates) When a dart half has its wing recognised as a jack (largeDartBase) vertex
   add a missing kite half on its long edge.
8. (thirdDartUpdates) When a vertex is a kite wing and also an origin for exactly 4 dart halves
   it must be a king vertex.
   Add a missing dart half (on any boundary long edge of a dart at the vertex).
9. (queenDartUpdates) If there are 4 kite wings at a vertex (necessarily a queen)
   add any missing half dart on a boundary kite long edge
10.(queenKiteUpdates) If there are 3 kite wings at a vertex (necessarily a queen)
   add any missing fourth half kite on a boundary kite short edge
-}
           
{-------------------  FORCING RULES and Generators --------------------------
7 vertex types are:
sun, queen, jack (largeDartBase), ace (fool), deuce (largeKiteCentre), king, star
-}

{-| allUGenerator combines all the 10 update generators.
     They are composed (keeping the order) after applying each to the
     supplied boundary and focus edge list arguments                        
-}
allUGenerator :: UpdateGenerator 
allUGenerator bd focus = foldl' link id generators where
    link combined g = g bd focus . combined
    generators = [ wholeTileUpdates          -- (1)
                 , kiteBelowDartUpdates      -- (2)
                 , kiteWingDartOriginUpdates -- (3)
                 , kiteGapUpdates            -- (4)
                 , secondTouchingDartUpdates -- (5)
                 , sunStarUpdates            -- (6)
                 , dartKiteTopUpdates        -- (7)
                 , thirdDartUpdates          -- (8)
                 , queenDartUpdates          -- (9)
                 , queenKiteUpdates          -- (10)
                 ]

-- |UFinder (Update case finder function). Given a Boundary and a list of (focus) boundary directed edges,
-- such a function returns each focus edge satisfying the particular update case paired with the tileface
-- matching that edge. For example, if the function is looking for dart short edges on the boundary,
-- it will return only those focus edges which have a matching half-dart short edge,
-- each paired with the corresponding half-dart.
type UFinder = Boundary -> [DEdge] -> [(DEdge,TileFace)]

-- |UMaker (Update creator function). Given a Boundary and a particular tileface (on the boundary),
-- such functions produce particular updates on the boundary edge of the given tileface.
-- For example, addKitreShortE will produce an update to add a half-kite with short edge against the boundary.
-- It can be used with a UFinder that either returns dart halves with short edge on the boundary
-- (nonKDarts in rule 2) or returns kite halves with short edge on the boundary
-- (kitesWingDartOrigin in rule 3).
type UMaker = Boundary -> TileFace -> Update

{-|This is a general purpose filter used to create UFinder functions for each force rule.
 It requires a face predicate.
 The face predicate takes a Boundary bd, a boundary DEdge (a,b) and the TileFace with the edge (b,a)
 and decides whether the face is wanted or not (True = wanted)
 This will be used to filter all the faces at the focus edges 
 (when given a Boundary and list of focus edges).
 For some predicates the boundary argument is not used (eg boundaryJoin in incompleteHalves), 
 but for others it is used to look at other faces at b or at a besides the supplied fc 
 (eg kiteWDO in kitesWingDartOrigin) 
-}
boundaryFilter::  (Boundary -> DEdge -> TileFace -> Bool) -> UFinder
boundaryFilter predF bd focus = 
    [ (e,fc) | e <- focus 
             , fc <- facesAtBV bd (fst e)
             , fc `elem` facesAtBV bd (snd e)
             , predF bd e fc
             ]


{-
------------------  FORCING CASES  ----------------------------
-}


{-| makeGenerator combines an update case finder (UFinder) with its corresponding update creator (UMaker)
    to produce an update generator function.
    This is used to make all of the 10 update generators corresponding to 10 rules 
    
    When the generator is given a boundary, list of focus edges and an update map,
    the finder produces a list of new pairs of dedge and face,
    the maker is used to convert the face in each pair to an update,
    and the updates are added to the final update map (with the dedge as key)
-}
makeGenerator :: UMaker -> UFinder -> UpdateGenerator
makeGenerator maker finder = gen where
    gen bd edges umap = foldr addU umap (finder bd edges) where
                        addU (e,fc) ump =  Map.insert e (maker bd fc) ump
{-
    gen bd edges umap = foldl' addU umap (finder bd edges) where
                        addU ump (e,fc) =  Map.insert e (maker bd fc) ump
-}

{- *
Ten Update Generators (with corresponding Finders)
-}

-- |Update generator for rule (1)
wholeTileUpdates:: UpdateGenerator
wholeTileUpdates = makeGenerator completeHalf incompleteHalves

-- |Find faces with missing opposite face (mirror face)  
incompleteHalves :: UFinder 
incompleteHalves = boundaryFilter boundaryJoin where
    boundaryJoin bd (a,b) fc = joinE fc == (b,a)


-- |Update generator for rule (2)
kiteBelowDartUpdates :: UpdateGenerator
kiteBelowDartUpdates = makeGenerator addKiteShortE nonKDarts

-- |Find half darts with boundary short edge
nonKDarts :: UFinder            
nonKDarts = boundaryFilter bShortDarts where
    bShortDarts bd (a,b) fc = isDart fc && shortE fc == (b,a)


-- |Update generator for rule (3)
 -- queen and king vertices add a missing kite half
kiteWingDartOriginUpdates :: UpdateGenerator
kiteWingDartOriginUpdates = makeGenerator addKiteShortE kitesWingDartOrigin

-- |Find kites with boundary short edge where the wing is also a dart origin
kitesWingDartOrigin :: UFinder              
kitesWingDartOrigin = boundaryFilter kiteWDO where
   kiteWDO bd (a,b) fc = shortE fc == (b,a) 
                      && ((isLK fc && isDartOrigin bd b) || (isRK fc && isDartOrigin bd a))
   isDartOrigin bd v = v `elem` fmap originV (filter isDart (facesAtBV bd v))


{-| Update generator for rule (4)
     (for deuce vertices = largeKiteCentres)
     Kites whose short edge (b,a) matches a boundary edge (a,b) where their oppV (= a for LK and = b for RK)
     has 2 other kite halves sharing a shortE.
     These need a dart adding on the short edge.
-}
kiteGapUpdates :: UpdateGenerator
kiteGapUpdates = makeGenerator addDartShortE kiteGaps

-- |Find kite halves with a short edge on the boundary (a,b) 
-- where there are 2 other kite halves sharing a short edge
-- at oppV of the kite half (a for left kite and b for right kite)
kiteGaps :: UFinder              
kiteGaps = boundaryFilter kiteGap where
  kiteGap bd (a,b) fc = shortE fc == (b,a)
                     && (isLK fc && hasKshortKat bd a || isRK fc && hasKshortKat bd b)
  hasKshortKat bd v = hasAnyMatchingE $ fmap shortE $ filter isKite $ facesAtBV bd v


-- |Update generator for rule (5)
-- secondTouchingDartUpdates - jack vertex add a missing second dart
secondTouchingDartUpdates :: UpdateGenerator
secondTouchingDartUpdates = makeGenerator addDartShortE noTouchingDarts

-- |Find kite halves with a short edge on the boundary (a,b) where oppV is a largeDartBase vertex
-- (oppV is a for left kite and b for right kite).
-- The function mustbeLDB determines if a vertex must be a a largeDartBase
noTouchingDarts :: UFinder              
noTouchingDarts = boundaryFilter farKOfDarts where
   farKOfDarts bd (a,b) fc  = shortE fc == (b,a)
                              && (isRK fc && mustbeLDB bd b || isLK fc && mustbeLDB bd a)

-- |mustbeLDB (large dart base / jack) is true of a boundary vertex if
-- it is the wing of two darts not sharing a long edge or
-- it is a wing of a dart and also a kite origin
-- (false means it is either undetermined or is a large kite centre)
mustbeLDB :: Boundary -> Vertex -> Bool
mustbeLDB bd v =
  (length dWings == 2 && not (hasAnyMatchingE (fmap longE dWings))) ||
  (length dWings == 1 && isKiteOrigin) 
  where fcs = facesAtBV bd v
        dWings = filter ((==v) . wingV) $ filter isDart fcs
        isKiteOrigin = v `elem` fmap originV (filter isKite fcs)

-- |hasMatching asks if a directed edge list has any two matching (=opposing) directed edges.
hasAnyMatchingE :: [DEdge] -> Bool
hasAnyMatchingE ((x,y):more) = (y,x) `elem` more || hasAnyMatchingE more
hasAnyMatchingE [] = False


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
    multiples57 bd (a,b) fc =               
        (isLD fc && longE fc == (b,a) && (length dartOriginsAta >=7)) ||
        (isRD fc && longE fc == (b,a) && (length dartOriginsAtb >=7)) ||
        (isLK fc && longE fc == (b,a) && (length kiteOriginsAtb >=5)) ||
        (isRK fc && longE fc == (b,a) && (length kiteOriginsAta >=5))
        where
            fcsAta = facesAtBV bd a
            fcsAtb = facesAtBV bd b
            kiteOriginsAta = filter ((==a) . originV) (filter isKite fcsAta)
            kiteOriginsAtb = filter ((==b) . originV) (filter isKite fcsAtb)
            dartOriginsAta = filter ((==a) . originV) (filter isDart fcsAta)             
            dartOriginsAtb = filter ((==b) . originV) (filter isDart fcsAtb)             


-- |Update generator for rule (7)
-- jack vertices (largeDartBases) with dart long edge on boundary - add missing kite top
dartKiteTopUpdates :: UpdateGenerator
dartKiteTopUpdates = makeGenerator addKiteLongE noKiteTopDarts

-- |Find jack vertices (largeDartBases) with dart long edge on the boundary
noKiteTopDarts :: UFinder                  
noKiteTopDarts = boundaryFilter dartsWingDB where
    dartsWingDB bd (a,b) fc = (isLD fc && longE fc == (b,a) && mustbeLDB bd b) ||
                              (isRD fc && longE fc == (b,a) && mustbeLDB bd a)


-- |Update generator for rule (8)
-- king vertices with 2 of the 3 darts  - add another half dart on a boundary long edge of existing darts
thirdDartUpdates :: UpdateGenerator
thirdDartUpdates = makeGenerator addDartLongE missingThirdDarts

-- |Find king vertices with 2 of the 3 darts (a kite wing and 4 dart origins present)
-- and a dart long edge on the boundary
missingThirdDarts :: UFinder                    
missingThirdDarts = boundaryFilter pred where
    pred bd (a,b) fc = (isLD fc && longE fc == (b,a) && aHasKiteWing && length dartOriginsAta ==4) ||
                       (isRD fc && longE fc == (b,a) && bHasKiteWing && length dartOriginsAtb ==4)
        where       
            fcsAta = facesAtBV bd a
            fcsAtb = facesAtBV bd b
            dartOriginsAta = filter ((==a) . originV) $ filter isDart fcsAta
            dartOriginsAtb = filter ((==b) . originV) $ filter isDart fcsAtb
            aHasKiteWing = a `elem` fmap wingV (filter isKite fcsAta)
            bHasKiteWing = b `elem` fmap wingV (filter isKite fcsAtb)


-- |Update generator for rule (9)
-- queen vertices (with 4 kite wings) -- add any missing half dart on a boundary kite long edge
queenDartUpdates :: UpdateGenerator
queenDartUpdates = makeGenerator addDartLongE queenMissingDarts

-- |Find queen vertices (with 4 kite wings) and a boundary kite long edge
queenMissingDarts :: UFinder                      
queenMissingDarts = boundaryFilter pred where
    pred bd (a,b) fc = (isLK fc && longE fc == (b,a) && length (kiteWingsAt a) ==4) ||
                       (isRK fc && longE fc == (b,a) && length (kiteWingsAt b) ==4)
                        where
                          kiteWingsAt x = filter ((==x) . wingV) $ filter isKite (facesAtBV bd x)



-- |Update generator for rule (10)
-- queen vertices with 3 kite wings -- add missing fourth half kite on a boundary kite short edge
queenKiteUpdates :: UpdateGenerator
queenKiteUpdates = makeGenerator addKiteShortE queenMissingKite

-- |Find queen vertices with only 3 kite wings and a kite short edge on the boundary
queenMissingKite :: UFinder                        
queenMissingKite = boundaryFilter pred where
    pred bd (a,b) fc = (isLK fc && shortE fc == (b,a) && length (kiteWingsAt b) ==3) ||
                       (isRK fc && shortE fc == (b,a) && length (kiteWingsAt a) ==3)
                        where
                          kiteWingsAt x = filter ((==x) . wingV) $ filter isKite (facesAtBV bd x)


{- *
Six Update Makers
-}

-- |completeHalf will make an update to
--  add a symmetric (mirror) face for a given face at a boundary join edge.
completeHalf :: UMaker      
completeHalf bd (LD(a,b,_)) = (x, makeFace) where
        makeFace v = RD(a,v,b)
        x = findThirdV bd (b,a) anglesForJoinRD
completeHalf bd (RD(a,_,b)) = (x, makeFace) where
        makeFace v = LD(a,b,v)
        x = findThirdV bd (a,b) anglesForJoinLD
completeHalf bd (LK(a,_,b)) = (x, makeFace) where
        makeFace v = RK(a,b,v)
        x = findThirdV bd (a,b) anglesForJoinRK
completeHalf bd (RK(a,b,_)) = (x, makeFace) where
        makeFace v = LK(a,v,b)
        x = findThirdV bd (b,a) anglesForJoinLK

-- |add a (missing) half kite on a (boundary) short edge of a dart or kite
addKiteShortE :: UMaker         
addKiteShortE bd (RD(_,b,c)) = (x, makeFace) where
    makeFace v = LK(v,c,b)
    x = findThirdV bd (c,b) anglesForShortLK
addKiteShortE bd (LD(_,b,c)) = (x, makeFace) where
    makeFace v = RK(v,c,b)
    x = findThirdV bd (c,b) anglesForShortRK
addKiteShortE bd (LK(_,b,c)) = (x, makeFace) where
    makeFace v = RK(v,c,b)
    x = findThirdV bd (c,b) anglesForShortRK
addKiteShortE bd (RK(_,b,c)) = (x, makeFace) where
    makeFace v = LK(v,c,b)
    x = findThirdV bd (c,b) anglesForShortLK

-- |add a half dart top to a boundary short edge of a half kite.
addDartShortE :: UMaker         
addDartShortE bd (RK(_,b,c)) = (x, makeFace) where
        makeFace v = LD(v,c,b)
        x = findThirdV bd (c,b) anglesForShortLD
addDartShortE bd (LK(_,b,c)) = (x, makeFace) where
        makeFace v = RD(v,c,b)
        x = findThirdV bd (c,b) anglesForShortRD
addDartShortE bd _ = error "addDartShortE applied to non-kite face"

-- |add a kite half to a kite long edge or dart half to a dart long edge
completeSunStar :: UMaker
completeSunStar bd fc = if isKite fc 
                        then addKiteLongE bd fc
                        else addDartLongE bd fc

-- |add a kite to a long edge of a dart or kite
addKiteLongE :: UMaker            
addKiteLongE bd (LD(a,_,c)) = (x, makeFace) where
    makeFace v = RK(c,v,a)
    x = findThirdV bd (a,c) anglesForLongRK
addKiteLongE bd (RD(a,b,_)) = (x, makeFace) where
    makeFace v = LK(b,a,v)
    x = findThirdV bd (b,a) anglesForLongLK
addKiteLongE bd (RK(a,_,c)) = (x, makeFace) where
  makeFace v = LK(a,c,v)
  x = findThirdV bd (a,c) anglesForLongLK
addKiteLongE bd (LK(a,b,_)) = (x, makeFace) where
  makeFace v = RK(a,v,b)
  x = findThirdV bd (b,a) anglesForLongRK

-- |add a half dart on a boundary long edge of a dart or kite
addDartLongE :: UMaker            
addDartLongE bd (LD(a,_,c)) = (x, makeFace) where
  makeFace v = RD(a,c,v)
  x = findThirdV bd (a,c) anglesForLongRD
addDartLongE bd (RD(a,b,_)) = (x, makeFace) where
  makeFace v = LD(a,v,b)
  x = findThirdV bd (b,a) anglesForLongLD

addDartLongE bd (LK(a,b,_)) = (x, makeFace) where
  makeFace v = RD(b,a,v)
  x = findThirdV bd (b,a) anglesForLongRD
addDartLongE bd (RK(a,_,c)) = (x, makeFace) where
  makeFace v = LD(c,v,a)
  x = findThirdV bd (a,c) anglesForLongLD

{-
------------------  END OF FORCING CASES  ----------------------------
-}

{- *
Other Forcing operations
-}


-- |force applied to a SubTgraph - has no effect on tracked subsets but applies force to the full Tgraph.
forceSub :: SubTgraph -> SubTgraph
forceSub (SubTgraph{ fullGraph = g, trackedSubsets = tlist}) = makeSubTgraph (force g) tlist


{-
Other Force Related Functions
addHalfKite, addHalfDart, forceLDB, forceLKC
-}


-- |[addHalfKite and addHalfDart are not efficient but used to tinker with a Tgraph by adding a single half tile
-- e.g. to see how it affects forcing. They use the update makers (UMaker).]
-- 
-- addHalfKite is for adding a single half kite on a chosen DEdge of a Tgraph.
-- The DEdge must be a boundary edge but the direction is not important as
-- the correct direction is automatically calculated.
-- It will fail if the edge is a dart join.
addHalfKite :: Tgraph -> DEdge -> Tgraph
addHalfKite g e = recoverGraph $ newBoundary $ doUpdate bd u where
  bd = makeBoundary g
  de = case [e, reverseD e] `intersect` bDedges bd of
         [de] -> de
         _ -> error ("addHalfKite:  on non-boundary edge " ++ show e)
  [fc] = facesAtBV bd (fst de) `intersect` facesAtBV bd (snd de)
  u | longE fc == reverseD de = addKiteLongE bd fc
    | shortE fc == reverseD de = addKiteShortE bd fc
    | joinE fc == reverseD de && isKite fc = completeHalf bd fc
    | otherwise = error "addHalfKite: applied to dart join (not possible)"

-- |addHalfDart is for adding a single half dart on a chosen DEdge of a Tgraph.
-- The DEdge must be a boundary edge but the direction is not important as
-- the correct direction is automatically calculated.
-- It will fail if the edge is a dart short edge or kite join.
addHalfDart :: Tgraph -> DEdge -> Tgraph
addHalfDart g e = recoverGraph $ newBoundary $ doUpdate bd u where
  bd = makeBoundary g
  de = case [e, reverseD e] `intersect` bDedges bd of
         [de] -> de
         _ -> error ("addHalfDart:  on non-boundary edge " ++ show e)
  [fc] = facesAtBV bd (fst de) `intersect` facesAtBV bd (snd de)
  u | longE fc == reverseD de = addDartLongE bd fc
    | shortE fc == reverseD de && isKite fc = addDartShortE bd fc
    | joinE fc == reverseD de && isDart fc = completeHalf bd fc
    | otherwise
    = error "addHalfDart: applied to short edge of dart or to kite join (not possible)"


-- |For an unclassifiable dart wing v in a Tgraph, force it to become a large dart base (largeDartBase) by
-- adding a second half dart face (sharing the kite below the existing half dart face at v)
-- This assumes exactly one dart wing tip is at v, and that half dart has a full kite below it.
forceLDB :: Vertex -> Tgraph -> Tgraph
forceLDB v g = recoverGraph $ newBoundary $ doUpdate bd (addDartShortE bd k) where
    bd = makeBoundary g
    vFaces = facesAtBV bd v
    d = mustFind ((v==) . wingV) (filter isDart vFaces) $
           error ("forceLDB: no dart wing at " ++ show v)
    ks = filter ((==v) . oppV) $ filter isKite vFaces
    k = mustFind ((/= oppV d) . wingV) ks $
           error ("forceLDB: incomplete kite below dart " ++ show d)

-- |For an unclassifiable dart wing v in a Tgraph, force it to become a large kite centre (largeKiteCentres) by adding
-- 3 faces - a second half dart face sharing the long edge of the existing half dart face at v,
-- and then completing the kite on the new half dart short edge.
-- This assumes exactly one dart wing tip is at v.
-- (Note: farK for a half-dart d is that half of a full kite attached to the short edge of d
-- which does not share an edge with d). 
-- It is safe to add the 3 parts because v being unknown ensures the
-- existing dart has a boundary long edge and 
-- the new farK does not already exist (attached to existing dart farK),
-- provided the existing dart half has no kite or a full kite below.
-- If it has only a half kite below, but the new farK exists, then v will already be a crossing boundary.
forceLKC :: Vertex -> Tgraph -> Tgraph
forceLKC v g = recoverGraph bd3 where
    bd0 = makeBoundary g
    vFaces0 = facesAtBV bd0 v
    d = mustFind ((v==) . wingV) (filter isDart vFaces0) $
           error ("forceLKC: no dart wing at " ++ show v)
    bd1 = newBoundary $ doUpdate bd0 (addDartLongE bd0 d)
    vFaces1 = facesAtBV bd1 v
    newd = head (vFaces1 \\ vFaces0)
    bd2 = newBoundary $ doUpdate bd1 (addKiteShortE bd1 newd)
    vFaces2 = facesAtBV bd2 v
    newk = head (vFaces2 \\ vFaces1)
    bd3 = newBoundary $ doUpdate bd2 (completeHalf bd2 newk)

{-| mustFind is used to search (in forceLKC and forceLDB)
mustFind p ls err returns the first item in ls satisfying predicate p and returns
err argument when none found       
-}
mustFind :: Foldable t => (p -> Bool) -> t p -> p -> p
mustFind p ls err = case find p ls of
                     Just a  -> a
                     Nothing -> err







{- *
Adding faces (Auxiliary operations)
-}




{-------------------------------------------
****************************
ADDING FACES with findThirdV
****************************
  
The difficulty is determining if any edges of a new face already exist.
This goes beyond a simple graph operation and requires use of the geometry of the faces.
However, we do not need to go to a full conversion to vectors (which would have equality test problems anyway).
Instead we introduce a representation of relative directions of edges at a vertex with an equality test.
All directions are integer multiples of 1/10th turn (mod 10) so we use these integers for comparing directions.
IntAngle n where n is 0..9

No crossing boundary property:
Going round a vertex starting from a boundary edge and starting towards the interior, 
there will be a boundary and gap after the last face encountered.
If there is an extra gap - i.e. faces left over when no face is found attached to the last found edge, this
indicates a crossing boundary so an error is reported as it is unsafe to assume the sought edge does not already exist.

Possible Touching Vertices.
When searchThirdV / findThirdV return Nothing, this means a new vertex needs to be created.
This will need to have its position checked against other (boundary) vertices to avoid
creating touching vertices/crossing boundary. (Taken care of in tryUnsafeUpdate)
---------------------------------}

-- |findThirdV is the main exported interface to find a third vertex for a face added to
-- the RIGHT HAND SIDE of a directed boundary edge.
-- It is a simpler interface for searchThirdV (which it uses).
-- In findThirdV bd (a,b) (n,m), the two integer arguments n and m are the INTERNAL angles
-- for the new face on the boundary edge (a,b)
-- (for a and b respectively) expressed as multiples of tt (tt being a tenth turn) and must both be either 1,2, or 3 
-- It converts n and m to the correct IntAngles,
-- subtracting n from 10 to get the antiClockwise (external) angle on the first vertex (a),
-- before calling searchThirdV to return a Maybe Vertex. (See also searchThirdV)
findThirdV:: Boundary -> DEdge -> (Int,Int) -> Maybe Vertex
findThirdV bd (a,b) (n,m) = searchThirdV bd (a,b) (IntAngle (10-n)) (IntAngle m)
{-
findThirdV bd (a,b) (n,m) = checkAngleNumbers $ searchThirdV bd (a,b) (IntAngle (10-n)) (IntAngle m)
  where checkAngleNumbers x = if n `elem` [1,2,3] && m `elem` [1,2,3]
                              then x
                              else error ("findThirdV angles should be 1,2 or 3 but found "++ show(n,m))
-}

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

-- |IntAngles are Ints mod 10
newtype IntAngle = IntAngle Int deriving(Eq,Show)

-- |make an IntAngle from an Int (using `mod` 10)
intAngle :: Int -> IntAngle
intAngle n = IntAngle (n `mod` 10)
-- |from IntAngle n, turn clockwise/anticlockwise by IntAngle m to get a new IntAngle
anti,clock:: IntAngle -> IntAngle -> IntAngle
anti  (IntAngle n) (IntAngle m) = intAngle (n+m)
clock (IntAngle n) (IntAngle m) = intAngle (n-m)

{- | searchThirdV is the main function for constructing a new face.
searchThirdV bd (a,b) nAnti nClock will find the third vertex of a face to be added to
the RIGHT HAND SIDE of boundary directed edge (a,b) by inspecting the edges found at a and b.
(To add to the LHS of (a,b) then the boundary direction must be (b,a) so add to the right of (b,a))

nAnti is the IntAngle searched for at vertex a going anticlockwise (starting with (a,b) as IntAngle 0).
nAnti should be formed from integers 7, 8, or 9 for the face to be on RHS.

nClock is the IntAngle searched for at vertex b going clockwise (starting with (b,a) as IntAngle 0).
nClock should be formed from integers 1,2, or 3 for the face to be on RHS.

If existing edges are found in the specified directions
they are used and the associated third vertex is returned (Just v). 
If nothing is found, Nothing is returned - indicating that a new vertex is needed.

A gap in faces processed round a and b (i.e. a boundary edge before all faces are checked) will indicate
a *crossing boundary* error
where it is unsafe to conclude the edges do not exist.
This problem should be detected by findClockAt and findAntiAt.
-} 
searchThirdV:: Boundary -> DEdge -> IntAngle -> IntAngle -> Maybe Vertex
searchThirdV bd (a,b) nAnti nClock  = 
    case (findAntiAt bd nAnti (a,b) (facesAtBV bd a), findClockAt bd nClock (b,a) (facesAtBV bd b)) of
         (Just c, Just d ) -> if c==d then Just c else error "searchThirdV: non-matching end-points"
         (Just c, Nothing) -> Just c
         (Nothing, Just c) -> Just c
         (Nothing, Nothing)-> Nothing


{-| findClockAt, findAntiAt
For a boundary bd and boundary directed edge (a,b) 
findAntiAt bd n (a,b) fcs will look at directed edges from a of fcs going anti-clockwise round a with
their direction starting with (a,b) in direction 0.
fcs must be the faces at a. It then returns Just c if (a,c) is found in direction n on the boundary.
It will return Nothing if no such edge (a,c) is found BUT a *crossing boundary* error
if a boundary edge is found before the sought angle.
It also produces an error if the last angle found is beyond the one searched for.
This indicates an incorrect graph, and the boundary argument is used just for reporting the error
in such cases.
Similarly findClockAt in the clockwise direction but called with the reverse directed edge,
so fcs must be faces at b.
-}
findClockAt, findAntiAt :: Boundary -> IntAngle -> (Vertex, Vertex) -> [TileFace] -> Maybe Vertex
{-| findAntiAt bd n (a,b) fcs
(for a boundary bd and boundary directed edge (a,b) and faces at a fcs)
will look at directed edges from a of fcs going anti-clockwise round a with
their direction starting with (a,b) in direction 0.
It then returns Just c if (a,c) is found in direction n on the boundary.
It will return Nothing if no such edge (a,c) is found BUT a *crossing boundary* error
if a boundary edge is found before the sought angle.
It also produces an error if the last angle found is beyond the one searched for.
This indicates an incorrect graph, and the boundary argument is used just for reporting the error
in such cases.
-}
findAntiAt  bd n (a,b) fcs = errorCheckAnti bd a n  $ allAnglesAnti  [(intAngle 0,b)] fcs
{-| findClockAt bd n (a,b) fcs
(for a boundary bd and boundary directed edge (a,b) and faces at b fcs)
findClockAt bd n (a,b) fcs will look at directed edges from b of fcs going anti-clockwise round b with
their direction starting with (b,a) in direction 0.
It then returns Just c if (b,c) is found in direction n on the boundary.
It will return Nothing if no such edge (b,c) is found BUT a *crossing boundary* error
if a boundary edge is found before the sought angle.
It also produces an error if the last angle found is below the one searched for.
This indicates an incorrect graph, and the boundary argument is used just for reporting the error
in such cases.
-}
findClockAt bd n (a,b) fcs = errorCheckClock bd a n $ allAnglesClock [(intAngle 0,b)] fcs

-- |errorCheckAnti bd v n and errorCheckClock bd v n are used instead of just lookup n, 
-- because the search for n should find it in the front pair on the list or not at all.
-- However if the first angle is smaller than n for checkClockFor or bigger than n for checkAntiFor,
-- there is an incorrect graph.  The first 2 arguments are passed only to report such errors.
errorCheckAnti,errorCheckClock::Boundary -> Vertex -> IntAngle -> [(IntAngle,Vertex)] -> Maybe Vertex
errorCheckAnti bd v n [] = Nothing
errorCheckAnti bd v n ms@((m, a) : _)
  | m == n = Just a
  | m `smallerAngle` n = Nothing
  | otherwise
    = error ("errorcheckAnti:  Found incorrect graph\nConflict at vertex: "
            ++ show v
            ++ "\nChecking (anticlockwise) for angle "
            ++ show n
            ++ " but found "
            ++ show ms
            ++ "\n In graph:\n"
            ++ show (recoverGraph bd))

errorCheckClock bd v n [] = Nothing
errorCheckClock bd v n ms@((m, a) : _)
    | m == n = Just a
    | n `smallerAngle` m = Nothing
    | otherwise
    = error ("errorcheckClock:  Found incorrect graph\nConflict at vertex: "
            ++ show v
            ++ "\nChecking (clockwise) for angle "
            ++ show n
            ++ " but found "
            ++ show ms
            ++ "\n In graph:\n"
            ++ show (recoverGraph bd))

-- | x is a smaller IntAngle than y if the (mod 10) integer of x is smaller than the (mod 10) integer of y
(IntAngle m) `smallerAngle`  (IntAngle n) = m<n-- only valid up to n==9


{-| allAnglesClock and allAnglesAnti are used by findClockAt and findAntiAt.
The second argument is the unprocessed list of faces attached to a common vertex a.
The first argument is the accumulated list of directions found so far with last one at the front of the list
e.g (n,last) means the last edge found was (a,last) at angle n.
It looks for another face sharing vertex last (and therefore edge (a,last)) and uses this
to get the next angle and edge. The found face is removed from the list of faces to be processed in the recursive call.
If there is no such face, it will return the whole (angle,vertex) list found so far provided
there are no faces left over.
Faces left over indicate a crossing boundary at the vertex and therefore an error should be reported
(because the angle list is incomplete and unsafe to use).
-}
allAnglesClock,allAnglesAnti:: [(IntAngle,Vertex)] -> [TileFace] -> [(IntAngle,Vertex)]
allAnglesClock la@((n,last):_) fcs = case filter (isAtV last) fcs of  
    [fc] ->  allAnglesClock ((clock n (intAngleAt (prevV last fc) fc), nextV last fc) :la)  $ fcs\\[fc]
    []   -> if null fcs then la else error ("allAnglesClock: crossing boundaries?\nGap after "++ show last ++ " before " ++ show fcs)
    other    -> error ("allAnglesClock: Conflicting faces found: " ++ show other)

allAnglesAnti la@((n,last):_) fcs = case filter (isAtV last) fcs of  
    [fc] ->  allAnglesAnti ((anti n (intAngleAt (nextV last fc) fc), prevV last fc) :la)  $ fcs\\[fc]
    []    -> if null fcs then la else error ("allAnglesAnti: crossing boundaries?\nGap after "++ show last ++ " before " ++ show fcs)
    other    -> error ("allAnglesAnti: Conflicting faces found: " ++ show other)

-- |intAngleAt v fc gives the internal angle of the face fc at vertex v (which must be a vertex of the face)
-- returning an IntAngle (1,2,or 3).
intAngleAt :: Vertex -> TileFace -> IntAngle
intAngleAt v fc = faceAngles fc !! indexV v fc

-- |faceAngles returns a list of the three internal angles of a face (clockwise from originV)
-- represented as IntAngles - always 1 or 2 for kites and 1 or 3 for darts
faceAngles :: TileFace -> [IntAngle]
faceAngles (LD _) = fmap intAngle [1,3,1]
faceAngles (RD _) = fmap intAngle [1,1,3]
faceAngles _      = fmap intAngle [1,2,2] -- LK and RK


{-|
For testing  allAnglesAnti and allAnglesClock on a boundary directed edge.
The RHS of directed edge (a,b) should be the exterior side.
-}
testAngles g (a,b) = (allAnglesAnti  [(intAngle 0,b)] $ filter (isAtV a) (faces g),
                      allAnglesClock [(intAngle 0,a)] $ filter (isAtV b) (faces g))






