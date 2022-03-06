module Tgraphs where

import Data.List ((\\), intersect, nub, elemIndex, find)
import qualified Data.Map as Map (Map, lookup, insert, empty, (!), elems, toList, fromAscList, fromList)
import Data.Maybe (mapMaybe, isNothing)

import HalfTile
import Diagrams.Prelude      -- necessary for New createVPoints and for Located boundaries
import TileLib (ttangle,phi) -- necessary for New createVPoints


{- |-------------------------------------------------------------------
    touchCheckOn is a global variable used to switch on/off
    checks for touching vertices when new vertices are added to a graph.
    Used by touchCheck. 
    This should be True for safety.
    If it is switched to False, all results of forcing need to be checked
    for touching vertices.
------------------------------------------------------------------------}
touchCheckOn::Bool
touchCheckOn = True




-- | Tgraph vertices
type Vertex = Int

-- | Tgraph faces  (vertices clockwise starting with tile origin vertex)
-- a specialisation of HalfTile
type TileFace = HalfTile (Vertex,Vertex,Vertex)

-- | A Tgraph contains vertices, and faces (each are lists treated as sets with no repetitions).
-- Every vertex must be a face vertex and vice versa
data Tgraph = Tgraph { vertices :: [Vertex]
                     , faces    :: [TileFace]
                     } deriving (Show)

{-------------------------------------------
********************************************
Basic Tgraph, vertex, edge, face operations
********************************************
--------------------------------------------}

{--------------------------------------
Basic operations for producing Tgraphs
---------------------------------------}
                 
-- | provided a list of faces makes sense and are face-edge-connected
-- makeTgraph will create a Tgraph from the faces by calculating vertices
makeTgraph:: [TileFace] -> Tgraph
makeTgraph fcs =
    Tgraph { vertices = nub $ concatMap faceVList fcs
           , faces = fcs
           }

-- | checkTgraph creates a graph from faces but checks for edge conflicts and
-- crossing boundaries and connectedness
-- No crossing boundaries and connected => face-connected
checkTgraph:: [TileFace] -> Tgraph
checkTgraph fcs = 
    let g = makeTgraph fcs in
    if not (connected g)    then error ("checkTgraph: \nTgraph not connected\n" ++ show g) 
    else if edgeConflicts g then error ("checkTgraph: \nConflicting face edges: " ++ show (conflictingDedges g) ++
                                        "\nConflicting length edges: " ++ show (conflictingLengthEdges g) ++
                                        "\nin\n" ++ show g
                                       )
    else if crossingBoundaries g 
         then error ("checkTgraph: crossing boundaries found at " ++ show (crossingBVs g) ++
                     "\nin\n" ++ show g
                    )
         else g

-- | select or remove faces from a Tgraph,
-- but check resulting graph for connectedness and no crossing boundaries
selectFaces, removeFaces  :: [TileFace] -> Tgraph -> Tgraph
selectFaces fcs g = checkTgraph (faces g `intersect` fcs)
removeFaces fcs g = checkTgraph (faces g \\ fcs)

{-----------------------------------------------
Some important tests and properties for Tgraphs
-----------------------------------------------}

-- | conflictingDedges g returns a list of conflicting directed edges of the faces in g
-- (which should be null)
conflictingDedges :: Tgraph -> [(Vertex,Vertex)]
conflictingDedges g = duplicates $  graphDedges g where
     duplicates es = es \\ nub es

-- | conflictingLengthEdges g returns a list of conflicting lengthed edges of the faces in g
-- (which should be null)     
conflictingLengthEdges :: Tgraph -> [(Vertex,Vertex)]
conflictingLengthEdges g = phiEdges g `intersect` nonPhiEdges g -- using undirected edges

edgeConflicts :: Tgraph -> Bool
edgeConflicts g = not $ null $ conflictingDedges g ++ conflictingLengthEdges g

-- | There are crossing boundaries if vertices occur more than once
-- at the start of all boundary directed edges
-- (or more than once at the end of all boundary directed edges).
-- crossingBVs g returns a list of vertices with crossing boundaries
-- (which should be empty).               
crossingBVs :: Tgraph -> [Vertex]
crossingBVs g = bVerts \\ nub bVerts  -- leaves any duplicates
     where bVerts = fmap fst $ boundaryDedges g -- snd could replace fst here

crossingBoundaries :: Tgraph -> Bool
crossingBoundaries g = not $ null $ crossingBVs g

connected :: Tgraph -> Bool
connected g = if nullGraph g 
              then True
              else null (vs \\ connectedTo (head vs) vs (graphEdges g))
                   where vs = vertices g

connectedTo :: Eq a => a -> [a] -> [(a, a)] -> [a]
connectedTo v unvisited edges = dfs [] [v] (unvisited \\[v]) where 
-- depth first search arguments:  processed, visited, unvisited
  dfs done vs [] = vs++done
  dfs done (x:visited) unvisited 
     = dfs (x:done) (newVs ++ visited) (unvisited \\ newVs)
       where nextVs = map snd $ filter ((== x) . fst) edges
             newVs = nextVs \\ (done++visited) -- assumes no self-loops





       
{--------------------
Other face operations
---------------------}
       
ldarts,rdarts,lkites,rkites :: Tgraph -> [TileFace]
ldarts g = filter isLD (faces g)
rdarts g = filter isRD (faces g)
lkites g = filter isLK (faces g)
rkites g = filter isRK (faces g) 

{----------------
Vertex operations
------------------}

valency :: Tgraph -> Vertex -> Int                  
valency g v = length $ filter (\(a,b) -> a==v || b ==v) (graphDedges g) -- assumes no self-loops
-- valency g v = length $ filter ((==v) . fst) (edges g) -- assumes no self-loops

-- | face vertices in order clockwise - tileRep specialised to TileFace
faceVs::TileFace -> (Vertex,Vertex,Vertex)
faceVs = tileRep

faceVList::TileFace -> [Vertex]
faceVList = (\(x,y,z) -> [x,y,z]) . faceVs
--faceVList fc = [x,y,z] where (x,y,z) =  faceVs fc

-- | Whilst first, second and third vertex of a face are obvious (clockwise), 
-- it is often more convenient to refer to the originV (=firstV),
-- oppV (the vertex at the other end of the join edge), and
-- wingV (the remaining vertex not on the join edge)
firstV,secondV,thirdV,originV,wingV,oppV:: TileFace -> Vertex
firstV  fc = a where (a,_,_) = faceVs fc
secondV fc = b where (_,b,_) = faceVs fc
thirdV  fc = c where (_,_,c) = faceVs fc

originV = firstV

wingV (LD(_,_,c)) = c
wingV (RD(_,b,_)) = b
wingV (LK(_,b,_)) = b
wingV (RK(_,_,c)) = c

oppV (LD(_,b,_)) = b
oppV (RD(_,_,c)) = c
oppV (LK(_,_,c)) = c
oppV (RK(_,b,_)) = b

-- | indexV finds the index of a vertex in a face (firstV -> 0, secondV -> 1, thirdV -> 2)
indexV :: Int -> TileFace -> Int
indexV v fc = case elemIndex v (faceVList fc) of
                  Just i -> i
                  _      -> error ("indexV: " ++ show v ++ " not found in " ++ show fc)                

-- | nextV returns the next vertex in a face going clockwise from v
-- where v must be a vertex of the face
nextV :: Vertex -> TileFace -> Vertex
nextV v fc = case indexV v fc of
                    0 -> secondV fc
                    1 -> thirdV fc
                    2 -> firstV fc
-- | prevV returns the previous vertex in a face (i.e. next going anti-clockwise) from v
-- where v must be a vertex of the face
prevV :: Vertex -> TileFace -> Vertex
prevV v fc = case indexV v fc of
                    0 -> thirdV fc
                    1 -> firstV fc
                    2 -> secondV fc

{----------------
 Edge operations
-----------------
(a,b) is regarded as a directed edge from a to b.
A list of such pairs will usually be regarded as a list of directed edges.
In the special case that the list is symmetrically closed [(b,a) is in the list whenever (a,b) is in the list]
we will refer to this as an edge list rather than a directed edge list.                  
----------------}

-- | directed edges (clockwise) round a face
faceDedges::TileFace -> [(Vertex,Vertex)]
faceDedges face = [(a,b),(b,c),(c,a)] where (a,b,c) = faceVs face

-- | opposite directed edge
reverseE:: (Vertex,Vertex) -> (Vertex,Vertex)
reverseE (a,b) = (b,a)

-- | Whilst first, second and third edges are obvious (always clockwise), 
-- it is often more convenient to refer to the joinE (join edge),
-- shortE (the short edge which is not a join edge), and
-- longE (the long edge which is not a join edge)
-- these are also directed clockwise.
-- joinOfTile also returns the join edge but in the direction away from the origin
firstE,secondE,thirdE, joinE, shortE, longE, joinOfTile:: TileFace -> (Vertex,Vertex)
firstE = head . faceDedges
secondE = head . tail . faceDedges
thirdE = head . tail . tail . faceDedges

-- | joinE preserves the clockwise direction unlike joinOfTile
joinE (LD(a,b,_)) = (a,b)
joinE (RD(a,_,c)) = (c,a)
joinE (LK(a,_,c)) = (c,a)
joinE (RK(a,b,_)) = (a,b)
-- | shortE not the join edge in the dart cases
shortE = secondE
-- | longE not the join edge in the kite cases
longE (LD(a,_,c)) = (c,a)
longE (RD(a,b,_)) = (a,b)
longE (LK(a,b,_)) = (a,b) 
longE (RK(a,_,c)) = (c,a)

facePhiEdges fc@(RD _) = [e, reverseE e] where e = longE fc
facePhiEdges fc@(LD _) = [e, reverseE e] where e = longE fc
facePhiEdges fc        = [e, reverseE e, j, reverseE j] 
                         where e = longE fc
                               j = joinE fc

faceNonPhiEdges fc = bothDir' (faceDedges fc) \\ facePhiEdges fc

-- | The directed join edge of a face but origin first (not clockwise for RD and LK)
joinOfTile fc = (originV fc, oppV fc)

-- | matchingE etype fc is a predicate on tile faces 
-- where etype finds a particular type of edge of a face
-- etype could be joinE or longE or shortE   for example
-- it maps fc' to True if fc' has an etype edge matching the (reversed) etype edge of fc
matchingE :: (TileFace -> (Vertex,Vertex)) -> TileFace -> TileFace -> Bool
matchingE etype fc = (== reverseE (etype fc)) . etype

matchingLongE,matchingShortE,matchingJoinE ::  TileFace -> TileFace -> Bool
matchingLongE  = matchingE longE
matchingShortE = matchingE shortE
matchingJoinE  = matchingE joinE

-- | all the directed edges of a graph
graphDedges :: Tgraph -> [(Vertex, Vertex)]
graphDedges g = concatMap faceDedges (faces g)

-- | phiEdges returns a list of the longer edges of all faces (both directions for each edge)
phiEdges :: Tgraph -> [(Vertex, Vertex)]
phiEdges g = bothDir $ fmap longE (faces g)
                       ++ fmap joinE (lkites g ++ rkites g) 

nonPhiEdges :: Tgraph -> [(Vertex, Vertex)]
nonPhiEdges g = bothDir $ fmap shortE (faces g)
                          ++ fmap joinE (ldarts g ++ rdarts g)

graphEdges :: Tgraph -> [(Vertex, Vertex)]
graphEdges = bothDir . graphDedges

-- | bothDir adds missing reverse directed edges to a list of directed edges and then removes duplicates
bothDir:: [(Vertex,Vertex)] -> [(Vertex,Vertex)]
bothDir = nub . bothDir'

-- | bothDir' adds the reverse directed edges to a list of directed edges without checking for duplicates 
bothDir':: [(Vertex,Vertex)] -> [(Vertex,Vertex)]
bothDir' [] = []
bothDir' (e:more) = e:reverseE e:bothDir' more

-- | boundaryDedges g are missing reverse directed edges in graphDedges g (these are single directions only)
boundaryDedges :: Tgraph -> [(Vertex, Vertex)]
boundaryDedges g = bothDir des \\ des where 
    des = graphDedges g


-- | boundary edges are face edges not shared by 2 faces (both directions)
boundaryEdges :: Tgraph -> [(Vertex, Vertex)]
boundaryEdges  = bothDir' . boundaryDedges

-- | internal edges are shared by two faces = all edges except boundary edges
internalEdges :: Tgraph -> [(Vertex, Vertex)]
internalEdges g = des \\ fmap reverseE bdes where
    des = graphDedges g
    bdes = bothDir des \\ des


{---------------------
*********************
General Purpose tools
*********************
-----------------------}

type Mapping = Map.Map -- imported from Data.Map

{- | mustFind is used frequently to search
It returns the first item satisfying predicate p and returns
err arg when none found       
-}
mustFind :: Foldable t => (p -> Bool) -> t p -> p -> p
mustFind p ls err = case find p ls of
                     Just a  -> a
                     Nothing -> err

{- |
For testing  allAnglesAnti and allAnglesClock on a boundary edge
The RHS of directed edge (a,b) should be the exterior side.
-}
testAngles g (a,b) = (allAnglesAnti  [(intAngle 0,b)] $ filter (isAtV a) (faces g),
                      allAnglesClock [(intAngle 0,a)] $ filter (isAtV b) (faces g))







 
{-------------------------------------------------------------------------
******************************************** *****************************              
COMPOSING composeG and partCompose 
***************************************************************************
---------------------------------------------------------------------------}

-- | The main deterministic function for composing is composeG
-- which is essentially partCompose after unused faces are ignored.
composeG:: Tgraph -> Tgraph
composeG = snd . partCompose 

-- | partCompose produces a graph by composing faces which uniquely compose,
-- returning a pair consisting of unused faces of the original graph along with the composed graph
-- it makes use of classifyDartWings which also returns an association of faces incident with each dart wing
-- so these do not need to be reclculated.
partCompose:: Tgraph -> ([TileFace],Tgraph)
partCompose g = (remainder,checkTgraph newFaces)
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

-- | DWClass is a record type for the result of classifying dart wings
--  It now also records vGroup - an association of faces incident with each dart wing vertex
--  to to save recalculating in partCompose
data DWClass = DWClass { largeKiteCentres  :: [Vertex]
                       , largeDartBases  :: [Vertex]
                       , unknowns :: [Vertex]
                       , vGroup :: Map.Map Vertex [TileFace] --Mapping Vertex [TileFace]
                       } deriving Show
                       
-- | classifyDartWings classifies all dart wing tips
-- the result is a DWClass record of largeKiteCentres, largeDartBases, unknowns
-- and an assoc list where
-- largeKiteCentres are new kite centres, largeDartBases are new dart bases
-- unknowns cannot be classified, and
-- the assoc list gives faces found at each vertex in both largeKiteCentres and largeDartBases
-- passed on to make partCompose more efficient
classifyDartWings :: Tgraph -> DWClass
classifyDartWings g = DWClass {largeKiteCentres = kcs, largeDartBases = dbs, unknowns = unks
                              , vGroup = gps
                              } where
   (kcs,dbs,unks,gps) = foldl (processD g) ([],[],[],Map.empty) (rdarts g ++ ldarts g)

-- kcs = kite centres of larger kites,
-- dbs = dart bases of larger darts,
-- unks = unclassified dart wing tips
-- gps is an association list of the group of faces for each dart wing tip
processD g (kcs,dbs,unks,gps) rd@(RD(orig,w,_)) -- classify wing tip w
  = if valency g w ==2 then (kcs,dbs,w:unks,gps) else -- lone dart wing => unknown
    if w `elem` kcs || w `elem` dbs then (kcs,dbs,unks,gps) else -- already classified
    let
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

processD g (kcs,dbs,unks,gps) ld@(LD(orig,_,w)) -- classify wing tip w
  = if valency g w ==2 then (kcs,dbs,w:unks,gps) else -- lone dart wing => unknown
    if w `elem` kcs || w `elem` dbs then (kcs,dbs,unks,gps) else -- already classified
    let 
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


-- | find the two kite halves below a dart half, return the half kite furthest away (not attached to dart).
-- Returns a Maybe.   rd produces an rk (or Nothing) ld produces an lk (or Nothing)
findFarK :: TileFace -> [TileFace] -> Maybe TileFace
findFarK rd@(RD _) fcs = do lk <- find (matchingShortE rd) (filter isLK fcs)
                            rk <- find (matchingJoinE lk) (filter isRK fcs)
                            return rk
findFarK ld@(LD _) fcs = do rk <- find (matchingShortE ld) (filter isRK fcs)
                            lk <- find (matchingJoinE rk)  (filter isLK fcs)
                            return lk
findFarK _ _ = error "findFarK: applied to non-dart face"







{------------------------------- 
**************************************
DECOMPOSING - decomposeG
**************************************
----------------------------------}

-- \ decomposeG is deterministic and should never fail with a correct Tgraph
decomposeG :: Tgraph -> Tgraph
decomposeG g = Tgraph{ vertices = newVs++vertices g
                     , faces = newFaces
                     } where
  allPhi = phiEdges g
  newVs = makeNewVs (length allPhi `div` 2) (vertices g)
  newVFor = (Map.!) (buildMap allPhi newVs Map.empty)
  newFaces = concatMap decompFace (faces g)
-- buildMap edges vs m 
-- build a map associating a unique vertex from vs (list of new vertices) to each edge in edges
-- assumes vs is long enough, and both (a,b) and (b,a) get the same v   
  buildMap [] vs m = m
  buildMap ((a,b):more) vs m = case Map.lookup (a,b) m  of
    Just _  -> buildMap more vs m
    Nothing -> buildMap more (tail vs) (Map.insert (a,b) v (Map.insert (b,a) v m))
               where v = head vs
  -- | decompFace to process a face in decomposition
  -- producing new faces. 
  -- It uses newVFor - a function to get the unique vertex assigned to each phi edge
  decompFace fc = case fc of
      RK(a,b,c) -> [RK(c,x,b), LK(c,y,x), RD(a,x,y)]
        where x = newVFor (a,b)
              y = newVFor (c,a)
      LK(a,b,c) -> [LK(b,c,y), RK(b,y,x), LD(a,x,y)]
        where x = newVFor (a,b)
              y = newVFor (c,a)       
      RD(a,b,c) -> [LK(a,x,c), RD(b,c,x)]
        where x = newVFor (a,b)
      LD(a,b,c) -> [RK(a,b,x), LD(c,x,b)]
        where x = newVFor (a,c)
  
-- | given existing vertices vs, create n new vertices
makeNewVs :: Int -> [Vertex] -> [Vertex]
makeNewVs n vs = [k+1..k+n] where k = maximum vs
-- | return one new vertex
makeNewV :: [Vertex] -> Vertex
makeNewV vs = 1+maximum vs


-- infinite list of decompositions of a graph     
decompositionsG :: Tgraph -> [Tgraph]
decompositionsG = iterate decomposeG





















{-
***************************************************************************   
NEW FORCING with Boundaries and Touching Vertex Check
***************************************************************************
-}
{- | force: calculate boundary information,
     then call forceAll updatesBD to do all updates, 
     then convert back to a Tgraph
     where updatesBD bd generates all local boundary updates for bd
-}
force:: Tgraph -> Tgraph
force = recoverGraph . forceAll updatesBD . makeBoundary

-- wholeTiles: special case of forcing only half tiles to whole tiles
wholeTiles:: Tgraph -> Tgraph
wholeTiles = recoverGraph . forceAll wholeTileUpdates . makeBoundary 

{- | A Boundary records
the boundary directed edges plus 
a mapping of boundary vertices to their incident faces, plus
a mapping of boundary vertices to positions.
It also keeps track of all the faces and vertices, 
and the next vertex label to be used when adding a new vertex.
Note that bvFacesMap is initially only defined for boundary vertices,
but the information is not removed when a vertex is no longer on the boundary (after an update).
Similarly for bvLocMap.
-}
data Boundary 
  = Boundary
    { bDedges:: [(Vertex,Vertex)]  -- boundary directed edges
    , bvFacesMap:: Mapping Vertex [TileFace] -- faces at each boundary vertex
    , bvLocMap:: Mapping Vertex (Point V2 Double)  -- position of each boundary vertex
    , allFaces:: [TileFace]
    , allVertices:: [Vertex]
    , nextVertex::  Vertex
    } deriving (Show)

-- | makeBoundary: calculate Boundary information from a Tgraph
makeBoundary:: Tgraph -> Boundary
makeBoundary g = 
  let bdes = boundaryDedges g
      bvs = fmap fst bdes -- (fmap snd bdes would also do) for all boundary vertices
      bvLocs = Map.fromAscList $ filter ((`elem` bvs) . fst) $ Map.toList $ createVPoints (faces g) 
--      bvLocs = filter ((`elem` bvs) . fst) $ createVPoints (faces g) 
                  -- if there were no holes, could restrict to boundary faces only
  in Boundary
      { bDedges = bdes
      , bvFacesMap = Map.fromList $ fmap (\v -> (v, filter (isAtV v) (faces g))) bvs
--      , bvFacesMap = fmap (\v -> (v, filter (isAtV v) (faces g))) bvs
      , bvLocMap = bvLocs 
      , allFaces = faces g
      , allVertices = vertices g
      , nextVertex = makeNewV (vertices g)
      }
 
-- | recoverGraph: convert a Boundary back to a Tgraph
recoverGraph:: Boundary -> Tgraph
recoverGraph bd = 
  Tgraph{ faces = allFaces bd
        , vertices = allVertices bd
        }

{- | The recursive `forceBoundary`selects safe updates first from the list of all possible updates of a boundary,
only doing an unsafe update if there are no safe ones. The update list is
recalculated at each recursive call after an update
and the recursion stops only when the new update list is empty.
-}
forceAll :: (Boundary -> [Update]) -> Boundary -> Boundary
forceAll updateGen = retry where
  retry bd = case find safeUpdate updates of
               Just u -> retry (doUpdate u)     -- first safe update then recurse
               _  -> case tryUnsafes updates of
                       Just bd' -> retry bd' 
                       Nothing  -> bd           -- no more updates
             where   updates = updateGen bd     -- list of generated updates for bd

-- | an Update is a triple of
-- a Maybe Vertex identifying the third vertex for a face addition (Nothing means it needs to be created)
-- and a makeFace function to create the new face when given a third vertex,
-- and a Boundary
-- The function doUpdate applies makeFace to v for the Just v case and to a new vertex for the Nothing case
type Update = (Maybe Vertex, Vertex -> TileFace, Boundary)

-- | safe updates are those which do not require a new vertex, 
-- so have an identified existing vertex (Just v)
safeUpdate :: Update -> Bool
safeUpdate (Just _ , _ , _) = True
safeUpdate (Nothing, _ , _) = False

-- | tryUnsafes: When touchChecKOn is True any unsafe update producing a touching vertex returns Nothing
tryUnsafes [] = Nothing
tryUnsafes (u:upds) = case tryUpdate u of 
                             Nothing -> tryUnsafes upds
                             Just bd -> Just bd

{-
------------------  FORCING RULES  ----------------------------
Each rule is a function of type Boundary -> [Update] producing a list of possible updates for a boundary
The lists for all the rules are then concatenated to form a single list of all possible updates for a boundary

7 vertex types are:
sun (k5), queen (k4d1), jack (k3d2, largeDartBase), ace (k2d1, fool),
deuce (k2d2, largeKiteCentre), king (k2d3), star (d5)
-}

-- upDatesBD bd produces a list of all the possible single updates to bd (using the 8 forcing rules)
updatesBD:: Boundary -> [Update]
updatesBD bd =
    wholeTileUpdates bd             -- (1)
    ++ kiteBelowDartUpdates bd      -- (2)
    ++ kiteWingDartOriginUpdates bd -- (3)
    ++ kiteGapUpdates bd            -- (4)
    ++ secondTouchingDartUpdates bd -- (5)
    ++ sunStarUpdates bd            -- (6)
    ++ dartKiteTopUpdates bd        -- (7)
    ++ thirdDartUpdates bd          -- (8)
    ++ queenDartUpdates bd          -- (9)
    ++ queenKiteUpdates bd          -- (10)
{- 
1. When a join edge is on the boundary - add the missing half tile to make a whole tile.    
2. When a half dart has its short edge on the boundary
   add the half kite that must be on the short edge
   (this is at ace vertices but also helps with jack and deuce vertices).  
3. When a vertex is both a dart origin and a kite wing it must be a queen or king vertex.
   If there is a boundary short edge of a kite half at the vertex, 
   add another kite half sharing the short edge. 
   (This converts 1 kite to 2 and 3 kites to 4 in combination with the first rule).
4. When two half kites share a short edge their oppV vertex must be a deuce vertex
   add any missing half darts needed to complete the vertex.
   (In the gap between the other two short edges of the full kites).
5. When a single dart wing is at a vertex which is recognised as an incomplete jack vertex
   and has a complete kite below the dart wing, 
   add a second dart half touching at the vertex (sharing the kite below).
   This is also known as a *largeDartBase* vertex (= new dart base next level up - see later)
6. When a vertex has 3 or 4 whole kite origins (= 6 or 8 half kite origins)
   it must be a sun centre. Also if a vertex has 4 whole dart origins (= 8 half dart origins)
   it must be a star centre.
   Add an appropriate half kite/dart on a boundary long edge at the vertex.
   (This will complete suns (resp. stars) along with case 1),
7. When a dart half has its wing recognised as a jack (largeDartBase) vertex
   add a missing kite half on its long edge.
8. When a vertex is a kite wing and also an origin for exactly 4 dart halves
   it must be a king vertex.
   Add a missing dart half (on any boundary long edge of a dart at the vertex).
9. If there are 4 kite wings at a vertex (necessarily a queen)
   add any missing half dart on a boundary kite long edge
10.If there are 3 kite wings at a vertex (necessarily a queen)
   add any missing fourth half kite on a boundary kite short edge
-}



-- | doSafeUpdate adds a new face by making changes to the boundary information
-- and checks that the new face is not in conflict with existing faces.
-- Safe cases have Just v with existing vertex v as third vertex.
doSafeUpdate:: Update -> Boundary
doSafeUpdate (Nothing, makeFace, bd) = error "doSafeUpdate: applied to non-safe update "
doSafeUpdate (Just v, makeFace, bd) = 
   let newFace = makeFace v
       fDedges = faceDedges newFace
       matchedDedges = fDedges `intersect` bDedges bd
       newfDedges = fDedges \\ matchedDedges
       nbrFaces = nub $ concatMap (facesAtBV bd) (faceVList newFace)
       result = Boundary { bDedges = fmap reverseE newfDedges ++ (bDedges bd \\ matchedDedges)
                         , bvFacesMap = changeVFMap (faceVList newFace) newFace (bvFacesMap bd)
                         , allFaces = newFace:allFaces bd
                         , bvLocMap = bvLocMap bd  -- no change
                         , allVertices = allVertices bd
                         , nextVertex = nextVertex bd
                         }
   in if noConflict newFace nbrFaces then result else
      error ("doSafeUpdate:(incorrect tiling)\nConflicting new face  "
             ++ show newFace
             ++ "\nwith neighbouring faces\n"
             ++ show nbrFaces
             ++ "\nin boundary\n"
             ++ show result
            )


{- | tryUpdate u, calculates the resulting boundary for an unsafe update (u) with a new vertex.
     It checks that the new face is not in conflict with existing faces.
     If touchCheckOn is True, it performs a touching vertex check with the new vertex
     returning Nothing if there is a touching vertex
     Otherwise it returns Just the resulting boundary 
 -}
tryUpdate:: Update -> Maybe Boundary
tryUpdate (Just _ , makeFace, bd) = error "tryUpdate: applied to safe update "
tryUpdate (Nothing, makeFace, bd) = 
   let v = nextVertex bd       
       newFace = makeFace v
       oldVPoints = bvLocMap bd
       newVPoints = addVPoints [newFace] [] oldVPoints
       Just vPosition = Map.lookup v newVPoints
       fDedges = faceDedges newFace
       matchedDedges = fDedges `intersect` bDedges bd
       newfDedges = fDedges \\ matchedDedges
       nbrFaces = nub $ concatMap (facesAtBV bd) (faceVList newFace \\ [v])
       result = Boundary 
                         { bDedges = fmap reverseE newfDedges ++ (bDedges bd \\ matchedDedges)
                         , bvFacesMap = changeVFMap (faceVList newFace) newFace (bvFacesMap bd)
                         , bvLocMap = newVPoints
                         , allFaces = newFace:allFaces bd
                         , allVertices = v:allVertices bd
                         , nextVertex = v+1
                         }
   in if touchCheck vPosition oldVPoints -- always False if touchCheckOn = False
      then Nothing -- don't proceed - v is a touching vertex
      else if noConflict newFace nbrFaces  -- check new face does not conflict
           then Just result 
           else error 
                ("tryUpdate:(incorrect tiling)\nConflicting new face  "
                 ++ show newFace
                 ++ "\nwith neighbouring faces\n"
                 ++ show nbrFaces
                 ++ "\nin boundary\n"
                 ++ show result
                )




















{-------------------------------------------------------------------------
******************************************** *****************************              
New Boundary Location calculation and touching vertex checking 
***************************************************************************

requires Diagrams.Prelude for points etc.  plus ttangle and phi from TileLib
------------------------------------------------------------------------------}
touchCheck:: (Point V2 Double) -> Mapping a (Point V2 Double) -> Bool
touchCheck p vpMap = 
  if touchCheckOn 
  then any (tooClose p) (Map.elems vpMap) -- (fmap snd vpMap) 
  else False

tooClose :: Point V2 Double  -> Point V2 Double -> Bool
tooClose p p' = quadrance (p .-. p') < 0.25 -- quadrance is square of length of a vector
-- tooClose p p' = sqLength (p .-. p') < 0.25

sqLength :: V2 Double  -> Double
sqLength vec = dot vec vec

{- | createVPoints: process list of faces to associate points for each vertex.
     Faces must be face connected.
     This is used both in 
       makeBoundary for touching vertex checks and also
       GraphConvert.makeVPatch to make VPatches and Patches
-}
createVPoints:: [TileFace] -> Mapping Vertex (Point V2 Double)
createVPoints [] = Map.empty
createVPoints (face:more) = addVPoints [face] more (initJoin face)

{- | addVPoints readyfaces fcOther vpMap
The first argument list of faces (readyfaces) contains the ones being processed next in order where
each will have at least two known vertex points.
The second argument list of faces (fcOther) have not yet been added and may not yet have known vertex points.
The third argument is the mapping of vertices to points.
-}
addVPoints:: [TileFace] -> [TileFace] -> Mapping Vertex (Point V2 Double) -> Mapping Vertex (Point V2 Double)
addVPoints [] [] vpMap = vpMap 
addVPoints [] fcOther vpMap = error ("addVPoints: Faces not face-edge connected " ++ show fcOther)
addVPoints (fc:fcs) fcOther vpMap = addVPoints (fcs++fcs') fcOther' vpMap' where
  vpMap' = case thirdVertexLoc fc vpMap of
             Just (v,p) -> Map.insert v p vpMap
             Nothing -> vpMap
  (fcs', fcOther')   = edgeNbs fc fcOther

-- | initJoin fc 
-- initialises a vpMap with locations for join edge vertices of fc along x axis - used to initialise createVPoints
initJoin::TileFace -> Mapping Vertex (Point V2 Double)                
initJoin (LD(a,b,_)) = Map.insert a origin $ Map.insert b (p2(1,0)) Map.empty -- [(a,origin), (b, p2(1,0))]
initJoin (RD(a,_,c)) = Map.insert a origin $ Map.insert c (p2(1,0)) Map.empty --[(a,origin), (c, p2(1,0))]
initJoin (LK(a,_,c)) = Map.insert a origin $ Map.insert c (p2(phi,0)) Map.empty --[(a,origin), (c, p2(phi,0))]
initJoin (RK(a,b,_)) = Map.insert a origin $ Map.insert b (p2(phi,0)) Map.empty -- [(a,origin), (b, p2(phi,0))]

-- lookup 3 vertex locations
find3Locs (v1,v2,v3) vpMap = (Map.lookup v1 vpMap, Map.lookup v2 vpMap, Map.lookup v3 vpMap)

{- | thirdVertexLoc fc vpMap

New Version - Assumes all edge lengths are 1 or phi
It now uses signorm to produce vectors of length 1 rather than rely on relative lengths.

     thirdVertexLoc fc vpMap
     where fc is a tileface and
     vpMap associates points with vertices (positions)
     It looks up all 3 vertices in vpMap hoping to find 2 of them, it then returns Just pr
     where pr is an association pair for the third vertex.
     If all 3 are found, returns Nothing
     If none or one found this is an error (a non tile-connected face)
-}
thirdVertexLoc:: TileFace -> Mapping Vertex (Point V2 Double) -> Maybe (Vertex, Point V2 Double)        
thirdVertexLoc fc@(LD _) vpMap = case find3Locs (faceVs fc) vpMap of
  (Just loc1, Just loc2, Nothing) -> Just (wingV fc, loc1 .+^ v)   where v = phi*^signorm (rotate (ttangle 9) (loc2 .-. loc1))
  (Nothing, Just loc2, Just loc3) -> Just (originV fc, loc2 .+^ v) where v = signorm (rotate (ttangle 7) (loc3 .-. loc2))
  (Just loc1, Nothing, Just loc3) -> Just (oppV fc, loc1 .+^ v)    where v = signorm (rotate (ttangle 1) (loc3 .-. loc1))
  (Just _ , Just _ , Just _)      -> Nothing
  _ -> error ("thirdVertexLoc: face not tile-connected?: " ++ show fc)

thirdVertexLoc fc@(RD _) vpMap = case find3Locs (faceVs fc) vpMap of
  (Just loc1, Just loc2, Nothing) -> Just (oppV fc, loc1 .+^ v)    where v = signorm (rotate (ttangle 9) (loc2 .-. loc1))
  (Nothing, Just loc2, Just loc3) -> Just (originV fc, loc3 .+^ v) where v = signorm (rotate (ttangle 3) (loc2 .-. loc3))
  (Just loc1, Nothing, Just loc3) -> Just (wingV fc, loc1 .+^ v)   where v = phi*^signorm (rotate (ttangle 1) (loc3 .-. loc1))
  (Just _ , Just _ , Just _)      -> Nothing
  _ -> error ("thirdVertexLoc: face not tile-connected?: " ++ show fc)
 
thirdVertexLoc fc@(LK _) vpMap = case find3Locs (faceVs fc) vpMap of
  (Just loc1, Just loc2, Nothing) -> Just (oppV fc, loc1 .+^ v)    where v = phi*^signorm (rotate (ttangle 9) (loc2 .-. loc1))
  (Nothing, Just loc2, Just loc3) -> Just (originV fc, loc2 .+^ v) where v = phi*^signorm (rotate (ttangle 8) (loc3 .-. loc2))
  (Just loc1, Nothing, Just loc3) -> Just (wingV fc, loc1 .+^ v)   where v = phi*^signorm (rotate (ttangle 1) (loc3 .-. loc1))
  (Just _ , Just _ , Just _)      -> Nothing
  _ -> error ("thirdVertexLoc: face not tile-connected?: " ++ show fc)
 
thirdVertexLoc fc@(RK _) vpMap = case find3Locs (faceVs fc) vpMap of
  (Just loc1, Just loc2, Nothing) -> Just (wingV fc, loc1 .+^ v)   where v = phi*^signorm (rotate (ttangle 9) (loc2 .-. loc1))
  (Nothing, Just loc2, Just loc3) -> Just (originV fc, loc2 .+^ v) where v = phi*^signorm (rotate (ttangle 8) (loc3 .-. loc2))
  (Just loc1, Nothing, Just loc3) -> Just (oppV fc, loc1 .+^ v)    where v = phi*^signorm (rotate (ttangle 1) (loc3 .-. loc1))
  (Just _ , Just _ , Just _)      -> Nothing
  _ -> error ("thirdVertexLoc: face not tile-connected?: " ++ show fc)


-- | for a given face, find edge neighbouring faces in the supplied list of faces
-- returns a pair - the list of the ones found followed by the supplied list with these removed
edgeNbs::TileFace -> [TileFace] -> ([TileFace],[TileFace])
edgeNbs fc fcOther = (fcNbs, fcOther') where
      fcNbs = filter sharedEdge fcOther
      fcOther' = fcOther \\ fcNbs
      sharedEdge fc' = any (\e -> e `elem` fmap reverseE (faceDedges fc)) (faceDedges fc')


-- | noConflict fc fcs  where fc is a new face and fcs are neighbouring faces
-- There is no conflict if none of the new directed face edges of fc are already directed edges
-- of neighbouring faces fcs (in the same direction)
-- and the edge length types (phi/nonPhi) do not conflict
noConflict :: TileFace -> [TileFace] -> Bool
noConflict fc fcs = null (faceDedges fc `intersect` concatMap faceDedges fcs) &&
                    null (faceNonPhiEdges fc `intersect` concatMap facePhiEdges fcs) &&
                    null (facePhiEdges fc `intersect` concatMap faceNonPhiEdges fcs)

-- | changeVFMap vs f vfmap  - adds f to the list of faces associated with each v in vs
changeVFMap:: [Vertex] -> TileFace -> Mapping Vertex [TileFace] -> Mapping Vertex [TileFace]
changeVFMap vs f vfmap = foldl (changeV f) vfmap vs
  where changeV f vfmap v = case Map.lookup v vfmap of
                            Just fs -> Map.insert v (f:fs) vfmap
                            Nothing -> Map.insert v [f] vfmap

-- | facesAtBV bd v - returns the faces found at v which should be a boundary vertex
facesAtBV:: Boundary -> Vertex -> [TileFace]
facesAtBV bd v = case Map.lookup v (bvFacesMap bd) of
            Just fcs -> fcs
            Nothing -> error ("facesAtBV: no faces found at boundary vertex " ++ show v)
            
boundaryFaces :: Boundary -> [TileFace]
boundaryFaces = nub . concat . Map.elems . bvFacesMap














{-
boundaryFilter: requires a predicate and a Boundary
The predicate takes a boundary bd, a boundary directed edge (a,b) and a tileface at a (the first vertex of the edge)
and decides whether the face is wanted or not (True = wanted)
This is then used to filter all the faces round the boundary by applying to all the boundary edges.
For some predicates the boundary argument is not needed (eg boundaryJoin in incompleteHalves), 
but for others it is used to look at all the faces at b or at other faces at a besides the supplied fc 
(eg kiteWDO in kitesWingDartOrigin) 
-}
boundaryFilter::  (Boundary -> (Vertex,Vertex) -> TileFace -> Bool) -> Boundary -> [TileFace]
boundaryFilter pred bd =  [ fc | e <- bDedges bd, fc <- facesAtBV bd (fst e), pred bd e fc]
    -- concatMap (\e -> filter (pred bd e) (facesAtBV bd (fst e))) (bDedges bd)


{-
------------------  FORCING CASES  ----------------------------
-}

wholeTileUpdates:: Boundary -> [Update]
wholeTileUpdates bd = fmap (completeHalf bd) (incompleteHalves bd)

-- | incompleteHalves bd returns faces with missing opposite face (mirror face)  
incompleteHalves :: Boundary -> [TileFace]
incompleteHalves = boundaryFilter boundaryJoin where
    boundaryJoin bd (a,b) fc = joinE fc == (b,a)

-- | completeHalf will add a symmetric (mirror) face for a given face at a boundary join edge.
completeHalf :: Boundary -> TileFace -> Update        
completeHalf bd (LD(a,b,_)) = (x, makeFace, bd) where
        makeFace v = RD(a,v,b)
        x = findThirdV bd (b,a) 3 1 
completeHalf bd (RD(a,_,b)) = (x, makeFace, bd) where
        makeFace v = LD(a,b,v)
        x = findThirdV bd (a,b) 1 3
completeHalf bd (LK(a,_,b)) = (x, makeFace, bd) where
        makeFace v = RK(a,b,v)
        x = findThirdV bd (a,b) 1 2
completeHalf bd (RK(a,b,_)) = (x, makeFace, bd) where
        makeFace v = LK(a,v,b)
        x = findThirdV bd (b,a) 2 1


kiteBelowDartUpdates :: Boundary -> [Update]
kiteBelowDartUpdates bd = fmap (addKiteShortE bd) (nonKDarts bd)

-- half darts with boundary short edge
nonKDarts:: Boundary -> [TileFace]
nonKDarts = boundaryFilter bShortDarts where
    bShortDarts bd (a,b) fc = isDart fc && shortE fc == (b,a)

-- | add a (missing) half kite on a (boundary) short edge of a dart or kite
addKiteShortE::  Boundary -> TileFace -> Update   
addKiteShortE bd (RD(_,b,c)) = (x, makeFace, bd) where
    makeFace v = LK(v,c,b)
    x = findThirdV bd (c,b) 2 2
addKiteShortE bd (LD(_,b,c)) = (x, makeFace, bd) where
    makeFace v = RK(v,c,b)
    x = findThirdV bd (c,b) 2 2
addKiteShortE bd (LK(_,b,c)) = (x, makeFace, bd) where
    makeFace v = RK(v,c,b)
    x = findThirdV bd (c,b) 2 2
addKiteShortE bd (RK(_,b,c)) = (x, makeFace, bd) where
    makeFace v = LK(v,c,b)
    x = findThirdV bd (c,b) 2 2


 -- queen and king vertices add a missing kite half
kiteWingDartOriginUpdates :: Boundary -> [Update]
kiteWingDartOriginUpdates bd = fmap (addKiteShortE bd) (kitesWingDartOrigin bd)

-- kites with boundary short edge where the wing is also a dart origin
kitesWingDartOrigin:: Boundary -> [TileFace]   
kitesWingDartOrigin = boundaryFilter kiteWDO where
   kiteWDO bd (a,b) fc = shortE fc == (b,a) 
                      && ((isLK fc && isDartOrigin bd b) || (isRK fc && isDartOrigin bd a))
   isDartOrigin bd v = v `elem` fmap originV (filter isDart (facesAtBV bd v))


{- | (for deuce vertices = largeKiteCentres)
Kites whose short edge (b,a) matches a boundary edge (a,b) where their oppV (= a for LK and = b for RK)
has 2 other kite halves sharing a shortE.
These need a dart adding on the short edge.
-}
kiteGapUpdates :: Boundary -> [Update]
kiteGapUpdates bd = fmap (addDartShortE bd) (kiteGaps bd)

-- | add a half dart top to a boundary short edge of a half kite.
addDartShortE :: Boundary -> TileFace -> Update
addDartShortE bd (RK(_,b,c)) = (x, makeFace, bd) where
        makeFace v = LD(v,c,b)
        x = findThirdV bd (c,b) 3 1
addDartShortE bd (LK(_,b,c)) = (x, makeFace, bd) where
        makeFace v = RD(v,c,b)
        x = findThirdV bd (c,b) 1 3
addDartShortE bd _ = error "addDartShortE applied to non-kite face"

-- | kite halves with a short edge on the boundary (a,b) 
-- when there are 2 other kite halves sharing a short edge
-- at oppV of the kite half (a for left kite and b for right kite)
kiteGaps :: Boundary -> [TileFace]
kiteGaps = boundaryFilter kiteGap where
  kiteGap bd (a,b) fc = shortE fc == (b,a)
                     && (isLK fc && hasKshortKat bd a || isRK fc && hasKshortKat bd b)
  hasKshortKat bd v = hasMatchingE $ fmap shortE $ filter isKite $ facesAtBV bd v




-- | secondTouchingDartUpdates - jack vertex add a missing second dart
secondTouchingDartUpdates :: Boundary -> [Update] 
secondTouchingDartUpdates bd = fmap (addDartShortE bd) (noTouchingDarts bd)

-- | kite halves with a short edge on the boundary (a,b) and oppV must be a largeDartBase  vertex
-- (oppV is a for left kite and b for right kite)
-- function mustbeLDB determines if a vertex must be a a largeDartBase
noTouchingDarts :: Boundary -> [TileFace]
noTouchingDarts = boundaryFilter farKOfDarts where
   farKOfDarts bd (a,b) fc  = shortE fc == (b,a)
                              && (isRK fc && mustbeLDB bd b || isLK fc && mustbeLDB bd a)



{- |  sunStarUpdates is for vertices that must be either sun or star 
almostSunStar finds half-kites/half-darts with a long edge on the boundary
where their origin vertex has 8 total half-kites/half-darts respectively
or their origin vertex has 6 total half-kites in the case of kites only
completeSunStar will add a new face of the same type (dart/kite) 
sharing the long edge.
-}
sunStarUpdates :: Boundary -> [Update] 
sunStarUpdates bd = fmap (completeSunStar bd) (almostSunStar bd)

almostSunStar :: Boundary -> [TileFace]    
almostSunStar = boundaryFilter multiples68 where
    multiples68 bd (a,b) fc =               
        (isLD fc && longE fc == (b,a) && length dartOriginsAta ==8) ||
        (isRD fc && longE fc == (b,a) && length dartOriginsAtb ==8) ||
        (isLK fc && longE fc == (b,a) && (length kiteOriginsAtb ==6 || length kiteOriginsAtb ==8)) ||
        (isRK fc && longE fc == (b,a) && (length kiteOriginsAta ==6 || length kiteOriginsAta ==8))
        where
            fcsAta = facesAtBV bd a
            fcsAtb = facesAtBV bd b
            kiteOriginsAta = filter ((==a) . originV) (filter isKite fcsAta)
            kiteOriginsAtb = filter ((==b) . originV) (filter isKite fcsAtb)
            dartOriginsAta = filter ((==a) . originV) (filter isDart fcsAta)             
            dartOriginsAtb = filter ((==b) . originV) (filter isDart fcsAtb)             

completeSunStar :: Boundary -> TileFace -> Update   
completeSunStar bd (RK(a,_,c)) = (x, makeFace, bd) where
  makeFace v = LK(a,c,v)
  x = findThirdV bd (a,c) 1 2
completeSunStar bd (LK(a,b,_)) = (x, makeFace, bd) where
  makeFace v = RK(a,v,b)
  x = findThirdV bd (b,a) 2 1
completeSunStar bd (RD(a,b,_)) = (x, makeFace, bd) where
  makeFace v = LD(a,v,b)
  x = findThirdV bd (b,a) 1 1
completeSunStar bd (LD(a,_,c)) = (x, makeFace, bd) where
  makeFace v = RD(a,c,v)
  x = findThirdV bd (a,c) 1 1



-- jack vertices (largeDartBases) with dart long edge on boundary - add missing kite top
dartKiteTopUpdates :: Boundary -> [Update] 
dartKiteTopUpdates bd = fmap (addKiteLongE bd) (noKiteTopDarts bd)

addKiteLongE :: Boundary -> TileFace -> Update
addKiteLongE bd (LD(a,_,c)) = (x, makeFace, bd) where
    makeFace v = RK(c,v,a)
    x = findThirdV bd (a,c) 2 1
addKiteLongE bd (RD(a,b,_)) = (x, makeFace, bd) where
    makeFace v = LK(b,a,v)
    x = findThirdV bd (b,a) 1 2
addKiteLongE bd _ = error "addKiteLongE: applied to kite"

-- jack vertices (largeDartBases) with dart long edge on boundary
noKiteTopDarts :: Boundary -> [TileFace]
noKiteTopDarts = boundaryFilter dartsWingDB where
    dartsWingDB bd (a,b) fc = (isLD fc && longE fc == (b,a) && mustbeLDB bd b) ||
                              (isRD fc && longE fc == (b,a) && mustbeLDB bd a)



-- king vertices with 2 of the 3 darts  - add another half dart on a boundary long edge of existing darts
thirdDartUpdates :: Boundary -> [Update] 
thirdDartUpdates bd = fmap (addDartLongE bd) (missingThirdDarts bd)

-- add a half dart on a boundary long edge of a dart or kite
addDartLongE:: Boundary -> TileFace -> Update
addDartLongE bd (LD(a,_,c)) = (x, makeFace, bd) where
  makeFace v = RD(a,c,v)
  x = findThirdV bd (a,c) 1 1
addDartLongE bd (RD(a,b,_)) = (x, makeFace, bd) where
  makeFace v = LD(a,v,b)
  x = findThirdV bd (b,a) 1 1

addDartLongE bd (LK(a,b,_)) = (x, makeFace, bd) where
  makeFace v = RD(b,a,v)
  x = findThirdV bd (b,a) 1 1
addDartLongE bd (RK(a,_,c)) = (x, makeFace, bd) where
  makeFace v = LD(c,v,a)
  x = findThirdV bd (a,c) 1 1
--addDartLongE bd _ = error "addDartLongE: applied to kite"

-- king vertices with 2 of the 3 darts (a kite wing and 4 dart origins present)
missingThirdDarts :: Boundary -> [TileFace]  
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



-- queen vertices (with 4 kite wings) -- add any missing half dart on a boundary kite long edge
queenDartUpdates :: Boundary -> [Update] 
queenDartUpdates bd = fmap (addDartLongE bd) (queenMissingDarts bd)

-- queen vertices (with 4 kite wings) and a boundary kite long edge
queenMissingDarts :: Boundary -> [TileFace]  
queenMissingDarts = boundaryFilter pred where
    pred bd (a,b) fc = (isLK fc && longE fc == (b,a) && length (kiteWingsAt a) ==4) ||
                       (isRK fc && longE fc == (b,a) && length (kiteWingsAt b) ==4)
                        where
                          kiteWingsAt x = filter ((==x) . wingV) $ filter isKite (facesAtBV bd x)




-- queen vertices with 3 kite wings -- add missing fourth half kite on a boundary kite short edge
queenKiteUpdates :: Boundary -> [Update] 
queenKiteUpdates bd = fmap (addKiteShortE bd) (queenMissingKite bd)

-- queen vertices with only 3 kite wings
queenMissingKite :: Boundary -> [TileFace]  
queenMissingKite = boundaryFilter pred where
    pred bd (a,b) fc = (isLK fc && shortE fc == (b,a) && length (kiteWingsAt b) ==3) ||
                       (isRK fc && shortE fc == (b,a) && length (kiteWingsAt a) ==3)
                        where
                          kiteWingsAt x = filter ((==x) . wingV) $ filter isKite (facesAtBV bd x)


{-
------------------  END OF FORCING CASES  ----------------------------
-}





















{-------------------------------------------
************
ADDING FACES
************

The difficulty is determining if any edges of a new face already exist.
This goes beyond a simple graph operation and requires use of the geometry of the faces.
However, we do not need to go to a full conversion to vectors (which would have equality test problems anyway).
Instead we introduce a representation of relative directions of edges at a vertex with an equality test.
All directions are integer multiples of 1/10th turn (mod 10) so we use these integers for comparing directions.
IntAngle n where n is 0..9

searchThirdV example
                       
e.g adding the corresponding rdart to LD(a,b,c) - we are adding to the right of the directed edge (b,a), so
find all faces involving b, assign direction IntAngle 0 to (b,a), use this to assign directions to all edges from b (based on faces)
going anticlockwise round b, then see if there is an edge in direction IntAngle 7
Then assign direction IntAngle 0 to (a,b), use this to assign direction to all edges from a (based on faces)
going clockwise round a, then see if there is an edge in direction IntAngle 1
If both are false, add the new vertex and 2 edges to create rdart
If the first is false but second is true with edge (a,d), add edge (b,d) and face RD(a,d,b)
If the first is true with edge (b,d) but second is false, add edge (a,d) and face RD(a,d,b)
If both are true (and d node matches) we have a hole to be filled with RD(a,d,b)
        
Note that findThirdV just simplifies the interface for searchThirdV
by using the 2 internal tt angles of the face being added which must both be integers 1,2 or 3
It converts these to the appropriate IntAngle (the first is subtracted from 10 to start anticlockwise search)


No crossing boundary property:
Going round a vertex starting from a boundary edge and starting towards the interior, 
there will be a boundary and gap after the last face encountered.
If there is an extra gap - i.e. faces left over when no face is found attached to the last found edge, this
indicates a crossing boundary so an error is reported as it is unsafe to assume the sought edge does not already exist.

Possible Touching Vertices.
When searchThirdV / findThirdV return Nothing, this means a new vertex needs to be created.
This will need to have its position checked against other (boundary) vertices to avoid
creating touching vertices/crossing boundary. (Taken care of in tryUpdate)
---------------------------------}


-- | IntAngles are Ints mod 10
newtype IntAngle = IntAngle Int deriving(Eq,Show)

intAngle :: Int -> IntAngle
intAngle n = IntAngle (n `mod` 10)
-- | from IntAngle n, turn clockwise/anticlockwise by IntAngle m to get a new IntAngle
anti,clock:: IntAngle -> IntAngle -> IntAngle
anti  (IntAngle n) (IntAngle m) = intAngle (n+m)
clock (IntAngle n) (IntAngle m) = intAngle (n-m)


-- | findThirdV is a simpler interface to searchThirdV for finding the third vertex for a new face.
-- findThirdV bd (a,b) n m
-- (bd is a Boundary)
-- the two integer arguments n and m are the internal angles for the new face on the boundary edge (a,b)
-- for a and b respectively and must both be either 1,2, or 3.
-- It converts n and m to IntAngles
-- but subtracts n from 10 to get the antiClockwise (external) angle on the first vertex,
-- before calling searchThirdV to return a Maybe Vertex
findThirdV:: Boundary -> (Vertex,Vertex) -> Int -> Int -> Maybe Vertex
findThirdV bd (a,b) n m = checkAngleNumbers $ searchThirdV bd (a,b) (IntAngle (10-n)) (IntAngle m)
  where checkAngleNumbers x = if n `elem` [1,2,3] && m `elem` [1,2,3]
                              then x
                              else error ("findThirdV angles should be 1,2 or 3 but found "++ show(n,m))


{-  | searchThirdV             
The main function for constructing a new face is searchThirdV
searchThirdV bd (a,b) nAnti nClock will find the third vertex of a face depending on edges found
to form a face on the >>> RIGHT HAND SIDE <<<< of the directed edge (a,b) - which must be 
a boundary directed edge.
(To add to the LHS of (a,b) then the boundary direction must be (b,a) so add to the right of (b,a))

nAnti is the angle number searched for at vertex a going anticlockwise (starting with (a,b) as angle 0).
nAnti should be 7, 8, or 9 for the face to be on RHS

nClock is the angle number searched for at vertex b going clockwise (starting with (b,a) as 0).
nClock should be 1,2, or 3 for the face to be on RHS

If existing edges are found in the specified directions
they are used and the associated vertex is returned (Just v). 
If nothing is found, Nothing is returned - indicating the a new vertex is needed

Unsafe: touching vertex problem
There is a pathalogical case where the vertex already exists but neither of the edges is found. 
In this case it will lead to an erroneous new vertex.

A gap in faces around a and b can result in a *crossing boundary* error
when it is unsafe to conclude the edges do not exist. (see also findAnti and findClock)
-} 
searchThirdV:: Boundary -> (Vertex,Vertex) -> IntAngle -> IntAngle -> Maybe Vertex
searchThirdV bd (a,b) nAnti nClock  = 
    case (findAnti nAnti (a,b) (facesAtBV bd a), findClock nClock (b,a) (facesAtBV bd b)) of
         (Just c, Just d ) -> if c==d then Just c else error "searchThirdV: non-matching end-points"
         (Just c, Nothing) -> Just c
         (Nothing, Just c) -> Just c
         (Nothing, Nothing)-> Nothing
  

{- | findClock, findAnti
For a boundary directed edge (a,b) 
findAnti n (a,b) fcs will look at directed edges from a of fcs going anti-clockwise round a with their direction
starting with (a,b) in direction 0. fcs must be the faces at a. It then returns Just c if (a,c) is found in direction n.
It will return Nothing if no such edge (a,c) is found BUT an error if there are faces left over
when a match fails - this indicates a *crossing boundary*).
It also produces an error if the last angle found is beyond the one searched for.
Similarly findClock in the clockwise direction but fcs must be faces at b.
-}
findClock, findAnti :: IntAngle -> (Vertex, Vertex) -> [TileFace] -> Maybe Vertex
findClock n (a,b) fcs = checkClockFor n $ allAnglesClock [(intAngle 0,b)] $ fcs
findAnti  n (a,b) fcs = checkAntiFor n $ allAnglesAnti [(intAngle 0,b)] $ fcs

-- | checkClockFor n and checkAntiFor n instead of lookup n, because the search for n should find it in the front pair on the resulting list or not at all.
-- However if the first angle is smaller than n for checkClockFor or bigger than n for checkAntiFor,
-- there is something wrong - so error called.
checkAntiFor,checkClockFor::IntAngle -> [(IntAngle,Vertex)] -> Maybe Vertex
checkAntiFor n [] = Nothing
checkAntiFor n ms@((m,a):_) = if m==n then Just a  else
                              if m `smallerAngle` n  then Nothing else
                              error ("checkAntiFor angle "++ show n ++" but found "++ show ms)
checkClockFor n [] = Nothing
checkClockFor n ms@((m,a):_) = if m==n then Just a  else
                          if n `smallerAngle` m  then Nothing else
                          error ("checkClockFor angle "++ show n ++" but found "++ show ms)

(IntAngle m) `smallerAngle`  (IntAngle n) = m<n-- only valid up to n==9

-- isAtV v fc asks if a face fc has v as a vertex
isAtV:: Vertex -> TileFace -> Bool           
isAtV v face  =  v `elem` faceVList face

{- | allAnglesClock and allAnglesAnti are used by findClock and findAnti
The second argument is the unprocessed list of faces attached to a common vertex a
The first argument is the accumulated list of directions found so far with last one at the front of the list
e.g (n,last) means the last edge found was (a,last) at angle n.
It looks for another face sharing vertex last (and therefore edge (a,last)) and uses this
to get the next angle and edge. The found face is removed from the list of faces to be processed in the recursive call.
If there is no such face, it will return the whole (angle,vertex) list found so far provided
there are no faces left over.
Faces left over indicate a crossing boundary at the vertex and therefore an error should be reported
(because the angle list is incomplete and unsafe to use).
-}
allAnglesClock:: [(IntAngle,Vertex)] -> [TileFace] -> [(IntAngle,Vertex)]
allAnglesClock la@((n,last):_) fcs = case filter (isAtV last) fcs of  
    [fc] ->  allAnglesClock ((clock n (intAngleAt (prevV last fc) fc), nextV last fc) :la)  $ fcs\\[fc]
    []   -> if fcs==[] then la else error ("allAnglesClock: crossing boundaries?\nGap after "++ show last ++ " before " ++ show fcs)
    other    -> error ("allAnglesClock: Conflicting faces found: " ++ show other)

allAnglesAnti:: [(IntAngle,Vertex)] -> [TileFace] -> [(IntAngle,Vertex)]
allAnglesAnti la@((n,last):_) fcs = case filter (isAtV last) fcs of  
    [fc] ->  allAnglesAnti ((anti n (intAngleAt (nextV last fc) fc), prevV last fc) :la)  $ fcs\\[fc]
    []    -> if fcs==[] then la else error ("allAnglesAnti: crossing boundaries?\nGap after "++ show last ++ " before " ++ show fcs)
    other    -> error ("allAnglesAnti: Conflicting faces found: " ++ show other)

-- | intAngleAt v fc gives the internal angle of the face fc at vertex v (which must be a vertex of the face)
-- returning an IntAngle (1,2,or 3)
intAngleAt :: Vertex -> TileFace -> IntAngle
intAngleAt v fc = faceAngles fc !! indexV v fc

-- | faceAngles returns a list of the three internal angles of a face (clockwise from originV)
-- represented as IntAngles - always 1 or 2 for kites and 1 or 3 for darts
faceAngles :: TileFace -> [IntAngle]
faceAngles (LD _) = fmap intAngle [1,3,1]
faceAngles (RD _) = fmap intAngle [1,1,3]
faceAngles _      = fmap intAngle [1,2,2] -- LK and RK


-- | mustbeLDB (large dart base / k3d2) is true of a boundary vertex if
-- it is the wing of two darts not sharing a long edge or
-- it is a wing of a dart and also a kite origin
-- (false means it is either undetermined or is a large kite centre)
mustbeLDB :: Boundary -> Vertex -> Bool
mustbeLDB bd v =
  (length dWings == 2 && not (hasMatchingE (fmap longE dWings))) ||
  (length dWings == 1 && isKiteOrigin) 
  where fcs = facesAtBV bd v
        dWings = filter ((==v) . wingV) $ filter isDart fcs
        isKiteOrigin = v `elem` fmap originV (filter isKite fcs)

-- | hasMatching asks if a directed edge list has any two matching (=opposing) directed edges.
hasMatchingE :: [(Vertex,Vertex)] -> Bool
hasMatchingE ((x,y):more) = (y,x) `elem` more || hasMatchingE more
hasMatchingE [] = False
                      





{----------------------------
********************************************
EMPLACEMENTS
********************************************
------------------------------}

-- | emplace does maximal composing with force and composeG, 
-- then applies decomposeG and force repeatedly back to the starting level.
-- It produces the 'emplacement' of influence of the argument graph.   
emplace:: Tgraph -> Tgraph
emplace g = if nullGraph g'
            then fg 
            else (force . decomposeG . emplace) g'
    where fg = force g
          g' = composeG fg 
            
nullGraph:: Tgraph -> Bool
nullGraph g = null (faces g)

-- emplaceSimple - version of emplace which does not force when composing, only when decomposing
-- only safe to use on multi-decomposed maximal graphs.
emplaceSimple :: Tgraph -> Tgraph
emplaceSimple g = if nullGraph g'
                  then force g 
                  else (force . decomposeG . emplaceSimple) g'
    where g' = composeG g

-- emplacements are best supplied with a maximally composed or near maximally composed graph
-- It produces an infinite list of emplacements of the starting graph and its decompositions.
emplacements :: Tgraph -> [Tgraph]
emplacements = (iterate (force . decomposeG)) . emplace -- was .force

-- countEmplace g finds a maximally composed graph (maxg) for g and counts the number (n) of compsitions
-- needed.  It retutns a triple of maxg, the nth emplacement of maxg, and n)
countEmplace :: Tgraph -> (Tgraph,Tgraph,Int)
countEmplace g = (maxg, emplacements maxg !! n, n) where (maxg,n) = maxFCompose g

{-------------------------------------------------------------------------
******************************************** *****************************              
Experimental: makeChoices, emplaceChoices
***************************************************************************

------------------------------------------------------------------------------}

-- | a version of emplace using makeChoices at the top level.
-- after makeChoices we use emplace to attempt further compositions but with no further choices
emplaceChoices:: Tgraph -> [Tgraph]
emplaceChoices g = 
       let fg = force g
           g' = composeG fg 
       in
           if nullGraph g'
           then fmap emplace $ makeChoices g
           else fmap (force . decomposeG) (emplaceChoices g')
                                 
{- | makeChoices is a temporary tool which does not attempt to analyse choices for correctness.
It can thus create some choices which will be incorrect.
The unknowns returned from classifyDartWings can become largeKiteCentres or largeDartBases.
This produces 2^n choices where n is the number of unknowns (excluding lone dart wing tips with valency 2).
-}
makeChoices :: Tgraph -> [Tgraph]
makeChoices g = choices unks [g] where
    unks = filter ((>2).valency g) (unknowns (classifyDartWings g))
    choices [] gs = gs
    choices (v:more) gs = choices more (fmap (forceLKC v) gs ++ fmap (forceLDB v) gs)              

-- | doUpdate: do a single update (safe or unsafe)
doUpdate:: Update -> Boundary
doUpdate u = if safeUpdate u then doSafeUpdate u
             else case tryUpdate u of
               Just bd -> bd
               Nothing -> error "doUpdate: crossing boundary (touching vertices)"

-- | For an unclassifiable dart tip v, force it to become a large dart base (largeDartBase) by
-- adding a second half dart face (sharing the kite below the existing half dart face at v)
-- This assumes exactly one dart wing tip is at v, and that half dart has a full kite below it.
forceLDB :: Vertex -> Tgraph -> Tgraph
forceLDB v g = recoverGraph $ doUpdate (addDartShortE bd k) where
    bd = makeBoundary g
    vFaces = facesAtBV bd v
    d = mustFind ((v==) . wingV) (filter isDart vFaces) $
           error ("forceLDB: no dart wing at " ++ show v)
    ks = filter ((==v) . oppV) $ filter isKite vFaces
    k = mustFind ((/= oppV d) . wingV) ks $
           error ("forceLDB: incomplete kite below dart " ++ show d)

-- | forceLKC adds 3 pieces of a larger kite at an unknown vertex. That is,
-- For an unclassifiable dart tip v, force it to become a large kite centre (largeKiteCentres) by adding
-- 3 faces - a second half dart face sharing the long edge of the existing half dart face at v,
-- and then completing the kite on the new half dart short edge.
-- This assumes exactly one dart wing tip is at v.
-- It is safe to add the 3 parts because v being unknown ensures the
-- existing dart has a boundary long edge and 
-- the new farK does not already exist (attached to existing dart farK),
-- provided the existing dart half has no kite or a full kite below.
-- If it has only a half kite below, but the new farK exists, then v will already have crossing boundaries.
forceLKC :: Vertex -> Tgraph -> Tgraph
forceLKC v g = recoverGraph bd3 where
    bd0 = makeBoundary g
    vFaces0 = facesAtBV bd0 v
    d = mustFind ((v==) . wingV) (filter isDart vFaces0) $
           error ("forceLKC: no dart wing at " ++ show v)
    bd1 = doUpdate (addDartLongE bd0 d)
    vFaces1 = facesAtBV bd1 v
    newd = head (vFaces1 \\ vFaces0)
    bd2 = doUpdate (addKiteShortE bd1 newd)
    vFaces2 = facesAtBV bd2 v
    newk = head (vFaces2 \\ vFaces1)
    bd3 = doUpdate (completeHalf bd2 newk)

{-------------
 Experimental
--------------}


-- allFComps g produces a list of all forced compositions starting from g up to but excluding the empty graph
allFComps:: Tgraph -> [Tgraph]
allFComps g = takeWhile (not . nullGraph) $ iterate (composeG . force) g

-- | allComps g produces a list of all compositions starting from g up to but excluding the empty graph
-- This is not safe in general
allComps:: Tgraph -> [Tgraph]
allComps g = takeWhile (not . nullGraph) $ iterate composeG g


-- maxCompose and maxFCompose count the number of compositions to get to a maximal graph.
-- they return a pair of the maximal graph and the count
maxCompose, maxFCompose:: Tgraph -> (Tgraph,Int)
maxCompose g = (last comps, length comps - 1) where comps = allComps g
maxFCompose g = (last comps, length comps - 1) where comps = allFComps g


{----------------
 Testing of Force
-----------------}


-- | stepForce and stepForceAll are used for testing
-- they produce intermediate Boundaries after a given number of steps (face additions)
stepForce :: Int -> Tgraph -> Boundary
stepForce n g = stepForceAll updatesBD n (makeBoundary g)

-- | used by stepForce
stepForceAll :: (Boundary -> [Update]) -> Int -> Boundary -> Boundary
stepForceAll updateGen = count where
    count 0 bd = bd
    count n bd =  case find safeUpdate updates of
                    Just u -> count (n-1) (doUpdate u)     -- first safe update then recurse
                    _  -> case tryUnsafes updates of
                            Just bd' -> count (n-1) bd' 
                            Nothing  -> bd           -- no more updates
                  where   updates = updateGen bd     -- list of generated updates for bd


