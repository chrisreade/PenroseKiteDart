module Tgraphs where

import Data.List ((\\), lookup, intersect, nub, elemIndex, find)
import Data.Maybe (mapMaybe)

import HalfTile

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

{-----------------------------------
Two operations for producing Tgraphs
------------------------------------}
                 
-- | provided a list of faces makes sense and are face-edge-connected
-- makeTgraph will create a Tgraph from the faces by calculating vertices
makeTgraph:: [TileFace] -> Tgraph
makeTgraph fcs =
    Tgraph { vertices = nub $ concatMap faceVList fcs
           , faces = fcs
           }

-- | insertFace checks there is no conflict before adding a face to a graph
-- However other code now uses insertVFace instead       
insertFace :: TileFace -> Tgraph -> Tgraph
insertFace fc g =  if noConflict fc g
                   then makeTgraph (fc:faces g)
                   else error ("insertFace: Conflicting face: " ++ show fc ++ " to add to " ++ show g)

-- | insertVFace is used to add a new face to a graph and checks there is no conflict before adding.
-- It assumes findThirdV has been used to look for the third vertex of the face, so it takes as
-- arguments: a Maybe Vertex result from findThirdV,
-- a function to create the face given the third vertex, and the tgraph.
-- If the Maybe Vertex is Nothing then a new vertex is created, 
-- otherwise the vertex is part of the existing graph.
-- Elsewhere we rely on the last face added using insertVFace is at the front of the list
-- in order to chain a sequence of additions
insertVFace :: Maybe Vertex -> (Vertex -> TileFace) -> Tgraph -> Tgraph
insertVFace Nothing makeF g = 
    let v = makeNewV (vertices g) -- create new vertex
        fc = makeF v
    in  if noConflict fc g
        then Tgraph { faces = fc:faces g
                    , vertices = v:vertices g
                    } 
        else error ("insertVFace: Conflicting face: " ++ show fc ++ " to add to " ++ show g)
insertVFace (Just v) makeF g = 
    let fc = makeF v
    in  if noConflict fc g
        then Tgraph { faces = fc:faces g
                    , vertices = vertices g
                    } 
        else error ("insertVFace: Conflicting face: " ++ show fc ++ " to add to " ++ show g)

-- | there is no conflict if none of the new directed face edges are already directed edges of g
-- (in the same direction) and the edge length types (phi/nonPhi) do not conflict
noConflict :: TileFace -> Tgraph -> Bool
noConflict fc g = null (faceDedges fc `intersect` graphDedges g) &&
                  null (faceNonPhiEdges fc `intersect` phiEdges g) &&
                  null (facePhiEdges fc `intersect` nonPhiEdges g)

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

-- | There are multi-gaps if vertices occur more than once at the start of all boundary directed edges
-- or more than once at the end of all boundary directed edges.
-- multiGapVs g returns a list of vertices with multi-gaps.
-- (which should be empty)                 
multiGapVs :: Tgraph -> [Vertex]
multiGapVs g = nub $ (fmap fst bDedges \\ vertices g) ++ (fmap snd bDedges \\ vertices g)
     where bDedges = boundaryDedges g

multiGaps :: Tgraph -> Bool
multiGaps g = not $ null $ multiGapVs g

connected :: Tgraph -> Bool
connected g = if emptyGraph g 
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

-- | checkTgraph creates a graph from faces but checks for edge conflicts and multi-gaps and connectedness
-- No multi-gaps and connected => face-edge-connected
checkTgraph:: [TileFace] -> Tgraph
checkTgraph fcs = 
    let g = makeTgraph fcs in
    if not (connected g)    then error ("checkTgraph: \nGraph not connected " ++ show g) 
    else if edgeConflicts g then error ("checkTgraph: \nConflicting face edges: " ++ show (conflictingDedges g) ++
                                        "\nConflicting length edges: " ++ show (conflictingLengthEdges g) ++
                                        "\nwith " ++ show g
                                       )
    else if multiGaps g then error ("checkTgraph: multi-gaps found at " ++ show (multiGapVs g) ++
                                    "\n in " ++ show g
                                   )
    else g

-- | select or remove faces but check resulting graph for connected and no multi-gaps
selectFaces, removeFaces  :: [TileFace] -> Tgraph -> Tgraph
selectFaces fcs g = checkTgraph (faces g `intersect` fcs)
removeFaces fcs g = checkTgraph (faces g \\ fcs)

       
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
-- Used by noMultiGaps
boundaryDedges :: Tgraph -> [(Vertex, Vertex)]
boundaryDedges g = bothDir des \\ des where 
    des = graphDedges g

{-
The following 2 edge ops are not currently used
-}

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

{- | mustFind is used frequently to search lists
It returns the first item in the list satisfying predicate p and returns
err arg when none found       
-}
mustFind :: Foldable t => (p -> Bool) -> t p -> p -> p
mustFind p ls err = case find p ls of
                     Just a  -> a
                     Nothing -> err



{----------------------------
********************************************
Emplace
********************************************
------------------------------}

-- | emplace does maximal composing with graphCompose, then forces, then applies graphDecompose and force
-- repeatedly back to the starting level. It produces the 'emplacement' of influence of the argument graph.            
emplace:: Tgraph -> Tgraph
emplace g = let g' = graphCompose g in
            if emptyGraph g'
            then force g 
            else force . graphDecompose $ emplace g'
            
emptyGraph g = null (faces g)


{-------------------------------------------------------------------------
******************************************** *****************************              
COMPOSING graphCompose and partCompose 
***************************************************************************
---------------------------------------------------------------------------}

-- | The main deterministic function for composing is graphCompose
-- which is essentially partCompose after unused faces are ignored.
graphCompose:: Tgraph -> Tgraph
graphCompose = snd . partCompose 

-- | partCompose produces a graph by composing faces which uniquely compose,
-- returning a pair consisting of unused faces of the original graph along with the composed graph
partCompose:: Tgraph -> ([TileFace],Tgraph)
partCompose g = (remainder,makeTgraph newFaces)
  where
    dwClass = classifyDartWings g
-- ignore unknowns
    newFaces = newRDs ++ newLDs ++ newRKs ++ newLKs
    remainder = faces g \\ concat (groupRDs ++ groupLDs ++ groupRKs ++ groupLKs)
{- Finding new ldarts and rdarts
find lkites/rkites whose oppV is in nodesDB
for such a half-kite find the half dart attached to the short edge
-}
    preRDs = filter ((`elem` (nodesDB dwClass)) . oppV) $ lkites g
    newRDs = fmap makeRD groupRDs 
    groupRDs = mapMaybe groupRD preRDs
    makeRD [rd,lk] = RD(originV lk, originV rd, oppV lk) 
    groupRD lk = case find (matchingShortE lk) (rdarts g) of
                  Just rd -> Just [rd,lk]
                  Nothing -> Nothing
    
    preLDs = filter ((`elem` (nodesDB dwClass)) . oppV) $ rkites g 
    newLDs = fmap makeLD groupLDs
    groupLDs = mapMaybe groupLD preLDs 
    makeLD [ld,rk] = LD(originV rk, oppV rk, originV ld)
    groupLD rk = case find (matchingShortE rk) (ldarts g) of
                  Just ld -> Just [ld,rk]
                  Nothing -> Nothing
--  glueNodes are at the bases of current level darts where the dart join is to be glued to a kite edge to form a newPhiEdge
    glueNodes = nub $ (fmap oppV (ldarts g) `intersect` fmap wingV (rkites g)) ++
                      (fmap oppV (rdarts g) `intersect` fmap wingV (lkites g))
{- Finding new rkites and lkites
Find rkites whose wing vertex is not in glueNodes BUT oppV is in nodesKC,
for each one, find mirror lkite and rdart attached to this lkite
Similar symmetric version for lkites            
-}
    preRKs = filter ((`notElem` glueNodes) . wingV) $ 
             filter ((`elem` (nodesKC dwClass)) . oppV) $ rkites g
    newRKs = fmap makeRK groupRKs 
    groupRKs = mapMaybe groupRK preRKs 
    makeRK [rd,lk,rk] = RK(originV rd, wingV rk, originV rk)
    groupRK rk = case find (matchingJoinE rk) (lkites g) of
                   Just lk -> case find (matchingShortE lk) (rdarts g) of
                                Just rd -> Just [rd,lk,rk]
                                Nothing -> Nothing
                   Nothing -> Nothing

    preLKs = filter ((`notElem` glueNodes) . wingV) $ 
             filter ((`elem` (nodesKC dwClass)) . oppV) $ lkites g
    newLKs = fmap makeLK groupLKs 
    groupLKs = mapMaybe groupLK preLKs 
    makeLK [ld,rk,lk] = LK(originV ld, originV lk, wingV lk)
    groupLK lk = case find (matchingJoinE lk) (rkites g) of
                   Just rk -> case find (matchingShortE rk) (ldarts g) of
                                Just ld -> Just [ld,rk,lk]
                                Nothing -> Nothing
                   Nothing -> Nothing

-- DWClass is a record type for the result of classifying dart wings
data DWClass = DWClass {nodesKC  :: [Vertex]
                       ,nodesDB  :: [Vertex]
                       ,unknowns :: [Vertex]
                       } deriving Show
                       
-- | classifyDartWings classifies all dart wing tips
-- the result is a DWClass record of of nodesKC, nodesDB, unknowns where
-- nodesKC are new kite centres, nodesDB are new dart bases
classifyDartWings :: Tgraph -> DWClass
classifyDartWings g = DWClass {nodesKC = kcs, nodesDB = dbs, unknowns = unks} where
   (kcs,dbs,unks) = foldl (processD g) ([],[],[]) (rdarts g ++ ldarts g)

-- kcs = kite centres of larger kites, dbs = dart bases of larger darts, unks = unclassified dart wing tips
processD g (kcs,dbs,unks) rd@(RD(orig,w,_)) -- classify wing tip w
  = if valency g w ==2 then (kcs,dbs,w:unks) else -- lone dart wing => unknown
    if w `elem` kcs || w `elem` dbs then (kcs,dbs,unks) else -- already classified
    if w `elem` fmap originV (rkites g ++ lkites g) then (kcs,w:dbs,unks) else -- wing is a half kite origin => nodeDB
    if (w,orig) `elem` fmap longE (ldarts g) then (w:kcs,dbs,unks) else -- long edge rd shared with an ld => nodeKC
    case findFarK rd g of
    Nothing -> (kcs,dbs,w:unks) -- unknown if incomplete kite attached to short edge of rd
    Just rk@(RK _)  ->  
      case find (matchingShortE rk) (faces g) of
      Just (LK _) -> (w:kcs,dbs,unks) -- short edge rk shared with an lk => nodesKC
      Just (LD _) -> (kcs,w:dbs,unks) -- short edge rk shared with an ld => nodesDB
      _ -> case find (matchingLongE rk) (faces g) of -- short edge rk has nothing attached
           Nothing -> (kcs,dbs,w:unks)  -- long edge of rk has nothing attached => unknown
           Just (LD _) -> (w:kcs,dbs,unks) -- long edge rk shared with ld => nodesKC
           Just lk@(LK _) ->               -- long edge rk shared with lk
             case find (matchingShortE lk) (faces g) of
             Just (RK _) -> (w:kcs,dbs,unks) -- short edge of this lk shared with another rk => nodesKC
             Just (RD _) -> (kcs,w:dbs,unks) -- short edge of this lk shared with rd => nodesDB
             _ -> (kcs,dbs,w:unks) -- short edge of this lk has nothing attached => unknown

processD g (kcs,dbs,unks) ld@(LD(orig,_,w)) -- classify wing tip w
  = if valency g w ==2 then (kcs,dbs,w:unks) else -- lone dart wing => unknown
    if w `elem` kcs || w `elem` dbs then (kcs,dbs,unks) else -- already classified
    if w `elem` fmap originV (rkites g ++ lkites g) then (kcs,w:dbs,unks) else -- wing is a half kite origin => nodeDB
    if (w,orig) `elem` fmap longE (rdarts g) then (w:kcs,dbs,unks) else -- long edge ld shared with an rd => nodeKC
    case findFarK ld g of
    Nothing -> (kcs,dbs,w:unks) -- unknown if incomplete kite attached to short edge of ld
    Just lk@(LK _)  ->  
      case find (matchingShortE lk) (faces g) of
      Just (RK _) -> (w:kcs,dbs,unks) -- short edge lk shared with an rk => nodesKC
      Just (RD _) -> (kcs,w:dbs,unks) -- short edge lk shared with an rd => nodesDB
      _ -> case find (matchingLongE lk) (faces g) of -- short edge lk has nothing attached
           Nothing -> (kcs,dbs,w:unks)  -- long edge of lk has nothing attached => unknown
           Just (RD _) -> (w:kcs,dbs,unks) -- long edge lk shared with rd => nodesKC
           Just rk@(RK _) ->               -- long edge lk is shared with an rk
             case find (matchingShortE rk) (faces g) of
             Just (LK _) -> (w:kcs,dbs,unks) -- short edge of this rk shared with another lk => nodesKC
             Just (LD _) -> (kcs,w:dbs,unks) -- short edge of this rk shared with ld => nodesDB
             _ -> (kcs,dbs,w:unks) -- short edge of this rk has nothing attached => unknown

-- | find the two kite halves below a dart half, return the half kite furthest away (not attached to dart).
-- Returns a Maybe.   rd produces an rk (or Nothing) ld produces an lk (or Nothing)
findFarK :: TileFace -> Tgraph -> Maybe TileFace
findFarK rd@(RD _) g = case find (matchingShortE rd) (lkites g) of
                         Nothing -> Nothing
                         Just lk -> find (matchingJoinE lk) (rkites g)
findFarK ld@(LD _) g = case find (matchingShortE ld) (rkites g) of
                         Nothing -> Nothing
                         Just rk -> find (matchingJoinE rk) (lkites g)
findFarK _ _ = error "findFarK: applied to non-dart face"

{------------------------------- 
**************************************
DECOMPOSING - graphDecompose
**************************************
----------------------------------}

-- \ graphDecompose is deterministic and should never fail with a correct Tgraph
graphDecompose :: Tgraph -> Tgraph
graphDecompose g = makeTgraph newFaces where
    allPhi = phiEdges g
    findV e = lookup e (buildAssocs allPhi newVs [])
    newVs = makeNewVs (length allPhi `div` 2) (vertices g)
    newFaces = foldr (processWith findV) [] $ faces g

-- | given existing vertices vs, create n new vertices
makeNewVs :: Int -> [Vertex] -> [Vertex]
makeNewVs n vs = [k+1..k+n] where k = maximum vs
-- | return one new vertex
makeNewV :: [Vertex] -> Vertex
makeNewV vs = 1+maximum vs -- head (makeNewVs 1 vs)

-- | build a map associating a unique vertex from vs (list of new vertices) to each edge in the list
-- assumes vs is long enough, and both (a,b) and (b,a) get the same v   
buildAssocs [] vs assoc = assoc
buildAssocs ((a,b):more) vs assoc = case lookup (a,b) assoc  of
   Just _  -> buildAssocs more vs assoc
   Nothing -> buildAssocs more (tail vs) ([((a,b),v),((b,a),v)] ++ assoc) where v = head vs

-- | main function to process a face in decomposition
-- producing new edges and faces (added to those passed in). 
-- It uses argument findV - a function to get the unique vertex assigned to each phi edge
processWith findV fc faces = case fc of
    RK(a,b,c) -> RK(c,x,b): LK(c,y,x): RD(a,x,y) :faces
      where Just x = findV (a,b)
            Just y = findV (c,a)
    LK(a,b,c) -> LK(b,c,y): RK(b,y,x): LD(a,x,y) :faces
      where Just x = findV (a,b)
            Just y = findV (c,a)       
    RD(a,b,c) -> LK(a,x,c): RD(b,c,x) :faces
      where Just x = findV (a,b)
    LD(a,b,c) -> RK(a,b,x): LD(c,x,b) :faces
      where Just x = findV (a,c)

-- infinite list of decompositions of a graph     
graphDecompositions :: Tgraph -> [Tgraph]
graphDecompositions = iterate graphDecompose


{---------------------------
***************************************************************************   
FORCING (adding missing faces which are fully determined by existing faces)
***************************************************************************

This includes:
(nonKDarts) any half dart with a missing half kite on its short edge
    then (addDK) add the appropriate half kite,
(incompletes) faces with missing mirror halves
    then (complete) add the missing mirror half,
(noTouchingDarts) darts whose wingV is a nodesDB but no second dart wing at that vertex
    then (addTouchingDart) add the missing second dart half,
(kiteGaps) kite halves with no dart on the short edge, but the mirror kite half shares its short edge with another kite
    then (addDartTop) add dart faces in the 'gap',
(almostSunStar) when at least 8 of the 10 dart faces already share an origin,
             or when at least 6 of the 10 kite faces already share an origin 
   then (completeSunStar) adding another half dart/half kite),
(noKTop)  darts whose wing is a nodesDB with no kite half on the long edge
   then (addKTop) add a missing kite half on the long edge

********  Note: That last kite half will need another kite half on its short edge  ***** TO DO
-}    
force :: Tgraph -> Tgraph
force g = case updates g of
          (g': _) -> force g'
          _  -> g        
  where updates g = fmap (addDK g) (nonKDarts g) ++
                    fmap (complete g) (incompletes g) ++
                    fmap (addTouchingDart g) (noTouchingDarts g) ++
                    fmap (addDartTop g) (kiteGaps g) ++
                    fmap (completeSunStar g) (almostSunStar g) ++
                    fmap (addKTop g) (noKTop g)


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
           
e.g adding the corresponding rdart to LD(a,b,c) - we are adding to the right of the directed edge (b,a), so
find all faces involving b, assign direction IntAngle 0 to (b,a), use this to assign directions to all edges from b (based on faces)
going anticlockwise round b, then see if there is an edge in direction IntAngle 7
Then assign direction IntAngle 0 to (a,b), use this to assign direction to all edges from a (based on faces)
going clockwise round a, then see if there is an edge in direction IntAngle 1 
If both are false, add the new vertex and 2 edges to create rdart
If the first is false but second is true with edge (a,d), add edge (b,d) and face RD(a,d,b)
If the first is true with edge (b,d) but second is false, add edge (a,d) and face RD(a,d,b)
If both are true (and d node matches) we have a hole to be filled with RD(a,d,b)

No multi-gaps property:
Going round a vertex starting from a boundary edge and starting towards the interior, 
there will be a gap after the last face encountered.
If there is an extra gap - i.e. faces left over when no face is found attached to the last found edge,
this reports an error as it is unsafe to assume the sought edge does not already exist.
---------------------------------}

-- | IntAngles are Ints mod 10
newtype IntAngle = IntAngle Int deriving(Eq,Show)

intAngle :: Int -> IntAngle
intAngle n = IntAngle (n `mod` 10)
-- | from IntAngle n, turn clockwise/anticlockwise by IntAngle m to get a new IntAngle
anti,clock:: IntAngle -> IntAngle -> IntAngle
anti  (IntAngle n) (IntAngle m) = intAngle (n+m)
clock (IntAngle n) (IntAngle m) = intAngle (n-m)


{-  | findThirdV             
The main function for constructing a new face is findThirdV
findThirdV g (a,b) nAnti nClock will find the third vertex of a face depending on edges found
to form a face on the >>>RIGHT HAND SIDE <<<< of the directed edge (a,b) - which must be a boundary edge.
(To add to the LHS of (a,b) just use the reverse directed edge (b,a) and add to the RHS)

nAnti is the angle number searched for at vertex a going anticlockwise (starting with (a,b) as angle 0).
nAnti should be 6..9 for the face to be on RHS

nClock is the angle number searched for at vertex b going clockwise (starting with (b,a) as 0).
nClock should be 1..4 for the face to be on RHS

If existing edges are found in the specified directions
they are used and the associated vertex is returned (Just v). 
If nothing is found, Nothing is returned - indicating the a new vertex is needed

Unsafe:
There is a pathalogical case where the vertex already exists but neither of the edges is found. 
In this case it will lead to an erroneous new vertex.

A gap in faces around a and b can result in a 'no multi-gaps' error
when it is unsafe to conclude the edges do not exist. (*no multi-gaps* property - see findAnti and findClock)
-}
findThirdV:: Tgraph -> (Vertex,Vertex) -> IntAngle -> IntAngle -> Maybe Vertex
findThirdV g (a,b) nAnti nClock  =
    case (findAnti nAnti (a,b) (faces g), findClock nClock (b,a) (faces g)) of
        (Just c, Just d ) -> if c==d then Just c else error "findThirdV: non-matching end-points"
        (Just c, Nothing) -> Just c
        (Nothing, Just c) -> Just c
        (Nothing, Nothing)-> Nothing

{- PREVIOUSLY findThirdV was findCreateV which created a new vertex when needed.
findCreateV:: Tgraph -> (Vertex,Vertex) -> IntAngle -> IntAngle -> Vertex
findCreateV g (a,b) nAnti nClock  =
    case (findAnti nAnti (a,b) (faces g), findClock nClock (b,a) (faces g)) of
        (Just c, Just d ) -> if c==d then c else error "findCreateV: non-matching end-points"
        (Just c, Nothing) -> c
        (Nothing, Just c) -> c
        (Nothing, Nothing)-> makeNewV (vertices g)
-}

{- | findAnti, findClock
For an edge (a,b) of a single face (i.e. must not be a shared edge of 2 faces) 
findAnti n will look at directed edges from a of faces going anti-clockwise round a with their direction
starting with (a,b) in direction 0. It then returns Just c if (a,c) is found in direction n
It will return Nothing if no such edge (a,c) is found BUT an error if there are faces left over
when a match fails - this indicates a gap. (*no multi-gaps* property required of Tgraphs for this to be avoided).
It also produces an error if the last angle found is beyond the one searched for.
Similarly findClock in the clockwise direction.
-}
findClock, findAnti :: IntAngle -> (Vertex, Vertex) -> [TileFace] -> Maybe Vertex
{-
findClock n (a,b) fcs = lookup n $ allAnglesClock [(intAngle 0,b)] $ filter (isAtV a) fcs
findAnti  n (a,b) fcs = lookup n $ allAnglesAnti  [(intAngle 0,b)] $ filter (isAtV a) fcs
-}

-- | checkFor n instead of lookup n, because the search for n should find it in the front pair or not at all.
-- However if the first angle is bigger than n, there is something wrong - so error called.
findClock n (a,b) fcs = checkClockFor n $ allAnglesClock [(intAngle 0,b)] $ filter (isAtV a) fcs
findAnti  n (a,b) fcs = checkAntiFor n $ allAnglesAnti  [(intAngle 0,b)] $ filter (isAtV a) fcs

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


isAtV:: Vertex -> TileFace -> Bool           
isAtV v face  =  v `elem` faceVList face
--isAtV a face  =  a==x || a==y || a==z where (x,y,z) = faceVs face

{- | allAnglesClock and allAnglesAnti are used by findClock and findAnti
The second argument is the unprocessed list of faces attached to a common vertex a
The first argument is the accumulated list of directions found so far with last one at the front of the list
e.g (n,last) means the last edge found was (a,last) at angle n.
It looks for another face sharing vertex last (and therefore edge (a,last)) and uses this
to get the next angle and edge. The found face is removed from the list of faces to be processed in the recursive call.
If there is no such face, it will return the whole (angle,vertex) list found so far provided
there are no faces left over.
Faces left over indicate a multi-gap in the faces and therefore an error should be reported
(because the angle list is incomplete and unsafe to use).
-}
allAnglesClock:: [(IntAngle,Vertex)] -> [TileFace] -> [(IntAngle,Vertex)]
allAnglesClock la@((n,last):_) fcs = case filter (isAtV last) fcs of  
    [fc] ->  allAnglesClock ((clock n (intAngleAt (prevV last fc) fc), nextV last fc) :la)  $ fcs\\[fc]
    []   -> if fcs==[] then la else error ("allAnglesClock: gap after "++ show last ++ " before " ++ show fcs)
    other    -> error ("allAnglesClock: Conflicting faces found: " ++ show other)

allAnglesAnti:: [(IntAngle,Vertex)] -> [TileFace] -> [(IntAngle,Vertex)]
allAnglesAnti la@((n,last):_) fcs = case filter (isAtV last) fcs of  
    [fc] ->  allAnglesAnti ((anti n (intAngleAt (nextV last fc) fc), prevV last fc) :la)  $ fcs\\[fc]
    []    -> if fcs==[] then la else error ("allAnglesAnti: gap after "++ show last ++ " before " ++ show fcs)
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
    



-- | incompletes g returns faces with missing opposite face (findMirror) - kites before darts  
incompletes :: Tgraph -> [TileFace]    
incompletes g = filter (noMirror g) $ concat [rkites g,lkites g,rdarts g,ldarts g] -- kites before darts

{- |
complete will add a symmetric (findMirror) face for a given face at the join edge.
It should only be used when there is no existing partner on the join edge.
It uses findThirdV to check if either edge of the new face already exist (apart from the join).

Unsafe Case: see findThirdV
-}
complete :: Tgraph -> TileFace -> Tgraph        
complete g (LD(a,b,_)) = insertVFace x newFace g where
    newFace v = RD(a,v,b)
    x = findThirdV g (b,a) (intAngle 7) (intAngle 1) 
complete g (RD(a,_,b)) = insertVFace x newFace g where
    newFace v = LD(a,b,v)
    x = findThirdV g (a,b) (intAngle 9) (intAngle 3)
complete g (LK(a,_,b)) = insertVFace x newFace g where
    newFace v = RK(a,b,v)
    x = findThirdV g (a,b) (intAngle 9) (intAngle 2)
complete g (RK(a,b,_)) = insertVFace x newFace g where
    newFace v = LK(a,v,b)
    x = findThirdV g (b,a) (intAngle 8) (intAngle 1)
                

-- | add a (missing) half kite at the base (short edge) of a half dart
addDK::  Tgraph -> TileFace -> Tgraph   
addDK g (RD(_,b,c)) = insertVFace x newFace g where
    newFace v = LK(v,c,b)
    x = findThirdV g (c,b) (intAngle 8) (intAngle 2)
addDK g (LD(_,b,c)) = insertVFace x newFace g where
    newFace v = RK(v,c,b)
    x = findThirdV g (c,b) (intAngle 8) (intAngle 2)
addDK g _ = error "addDK applied to non-dart face"

-- | find the joined partner face of a face if it exists (returns Maybe face)
findMirror :: TileFace -> Tgraph ->  Maybe TileFace
findMirror fc g = find (matchingJoinE fc) (faces g)       

-- | noMirror g fc is True if fc has no mirror face joined in g
noMirror :: Tgraph -> TileFace -> Bool
noMirror g fc = findMirror fc g == Nothing

-- half darts with no face (necessarily a kite half) on the short edge
nonKDarts::Tgraph -> [TileFace]
nonKDarts g = filter (noK g) (ldarts g ++ rdarts g)

-- | does a dart face have a missing kite face at its base?
noK :: Tgraph -> TileFace -> Bool
noK g rd@(RD _) = find (matchingShortE rd) (lkites g) == Nothing
noK g ld@(LD _) = find (matchingShortE ld) (rkites g) == Nothing 
noK g _         = error "noK: cannot be applied to kites"

-- | noTouchingDarts finds dart halves with a wing tip at a nodesDB where
-- there is no second dart half sharing the wing tip
noTouchingDarts :: Tgraph -> [TileFace]
noTouchingDarts g = [ head ds | v <- nodesDB (classifyDartWings g)
                              , let ds = dartsAt v, length ds  == 1] where 
    dartsAt v = filter (isAtV v) (ldarts g ++ rdarts g)

-- | add a touching dart half (wing vertices touching) to argument dart half returned by 
-- noTouchingDarts. The whole shared kite at their bases must already exist.
-- also used by forceDBnode
addTouchingDart :: Tgraph -> TileFace -> Tgraph
addTouchingDart g drt = case findFarK drt g of
                         Nothing -> error ("addTouchingDart to dart without complete kite " ++ show drt)
                         Just kt -> addDartTop g kt

-- | add a half dart top to the short edge of a half kite.
-- used by addTouchingDart and therefore forceDBnode to add a dart face BUT also used in force for kiteGaps
addDartTop :: Tgraph -> TileFace -> Tgraph
addDartTop g (RK(_,b,c)) = insertVFace x newFace g where
        newFace v = LD(v,c,b)
        x = findThirdV g (c,b) (intAngle 7) (intAngle 1)
addDartTop g (LK(_,b,c)) = insertVFace x newFace g where
        newFace v = RD(v,c,b)
        x = findThirdV g (c,b) (intAngle 9) (intAngle 3)
addDartTop g _ = error "addDartTop applied to non-kite face"


noKTop :: Tgraph -> [TileFace]
noKTop g = [ d | v <- nodesDB (classifyDartWings g), d <- ldarts g ++ rdarts g
               , wingV d == v
               , find (matchingLongE d) (rkites g++lkites g) == Nothing]
                   
addKTop :: Tgraph -> TileFace -> Tgraph
addKTop g (LD(a,_,c)) = insertVFace x newFace g where
    newFace v = RK(c,v,a)
    x = findThirdV g (a,c) (intAngle 8) (intAngle 1)
addKTop g (RD(a,b,_)) = insertVFace x newFace g where
    newFace v = LK(b,a,v)
    x = findThirdV g (b,a) (intAngle 9) (intAngle 2)
    
{- |
When two kite halves meet on their short edge, 
the other two kite halves should have darts against their short edge (in the gap).
kiteGaps finds such cases and returns the kite halves to which the darts are to be short-edge attached.
addDartTop adds a missing dart half to the supplied kite half.
-}
kiteGaps :: Tgraph -> [TileFace]
kiteGaps g = [ k | lk <- lkites g
                 , rk <- rkites g
                 , shortE lk == reverseE (shortE rk)
                 , Just k <- [findMirror lk g, findMirror rk g]
                 , noD g k
             ]

-- | noD g k g asks if there is no dart attached to the short edge of a kite half k in g
noD :: Tgraph  -> TileFace-> Bool
noD g rk@(RK _) = find (matchingShortE rk) (ldarts g) == Nothing
noD g lk@(LK _) = find (matchingShortE lk) (rdarts g) == Nothing 
noD g _         = error "noD: cannot be applied to dart"


{- | almostSunStar finds vertices which are either
the origins of 8 faces (which are necessarily 8 half kites or 8 half darts), or
the origins of 6 half kites. 
It returns those faces at such centres which don't share a long edge
with one of the other faces at that centre.
completeSunStar will add a new face of the same type (dart/kite) 
sharing the long edge.
[Note that unmatched join edges will be taken care of elsewhere with complete]
-}
almostSunStar :: Tgraph -> [TileFace]    
almostSunStar g = [ f | centre <- occurs 8 (fmap originV (faces g)) ++
                                  occurs 6 (fmap originV (lkites g ++ rkites g)) -- NEW
                      , let fcs = filter ((==centre) . originV) (faces g)
                      , f <- fcs
                      , reverseE (longE f) `notElem` fmap longE fcs
                  ]
-- | occurs n vs returns a list of items in vs that occur exactly n times
occurs :: Eq a => Int -> [a] -> [a]
occurs n vs = nub [v | v <- vs,  length (filter (==v) vs) == n]

{- | For a face f returned by almostSunStar, completeSunStar f g will add a matching half tile to f
but back to back on the long edge.
This will extend an incomplete sun or star, relying on complete to add a matching half tile.
So eventually 4 darts will complete to a star, and 3 or 4 kites will complete to a sun.
-}
completeSunStar :: Tgraph -> TileFace -> Tgraph   
completeSunStar g (RK(a,b,c)) = insertVFace x newFace g where
  newFace v = LK(a,c,v)
  x = findThirdV g (a,c) (intAngle 9) (intAngle 2)
completeSunStar g (LK(a,b,c)) = insertVFace x newFace g where
  newFace v = RK(a,v,b)
  x = findThirdV g (b,a) (intAngle 8) (intAngle 1)
completeSunStar g (RD(a,b,c)) = insertVFace x newFace g where
  newFace v = LD(a,v,b)
  x = findThirdV g (b,a) (intAngle 9) (intAngle 1)
completeSunStar g (LD(a,b,c)) = insertVFace x newFace g where
  newFace v = RD(a,c,v)
  x = findThirdV g (a,c) (intAngle 9) (intAngle 1)


{-------------------------------------------------------------------------
******************************************** *****************************              
Experimental: makeChoices, multiEmplace
***************************************************************************

There is a now both forceDBnode AND forceKCnode so composeUasKC has been replaced by graphCompose
However, forceDBnode needs to create kite below dart when it is missing - not yet done
Must also consider what to do for isolated dart wing case (currently excluded)
------------------------------------------------------------------------------}

-- | a version of emplace using makeChoices at the top level.
-- after makeChoices we use emplace to attempt further compositions but with no further choices
multiEmplace:: Tgraph -> [Tgraph]
multiEmplace g = let g' = graphCompose g in
            if emptyGraph g'
            then fmap emplace $ makeChoices g
            else fmap (force . graphDecompose) $ multiEmplace g'
                                 
{- | makeChoices is a temporary tool which does not attempt to analyse choices for correctness.
It can thus create some choices which will be incorrect.
The unknowns returned from classifyDartWings can become nodesKC or nodesDB.
This produces 2^n choices where n is the number of unknowns (excluding lone dart wing tips with valency 2).
-}
makeChoices :: Tgraph -> [Tgraph]
makeChoices g = choices unks [g] where
    unks = filter ((>2).valency g) (unknowns (classifyDartWings g))
    choices [] gs = gs
    choices (v:more) gs = choices more (fmap (forceKCnode v) gs ++ fmap (forceDBnode v) gs)              

-- | For an unclassifiable dart tip v, force it to become a large dart base (nodesDB) by
-- adding a second half dart face (sharing the kite below the existing half dart face at v)
-- This assumes exactly one dart wing tip is at v, and that half dart has a full kite below it.
forceDBnode :: Vertex -> Tgraph -> Tgraph
forceDBnode v g = addTouchingDart g d where
    d = mustFind ((v==) . wingV) (rdarts g) $
        mustFind ((v==) . wingV) (ldarts g) $
           error ("forceDBnode: no dart wing at " ++ show v)

-- | forceKCnode adds 3 pieces of a larger kite at an unknown vertex. That is,
-- For an unclassifiable dart tip v, force it to become a large kite centre (nodesKC) by adding
-- 3 faces - a second half dart face sharing the long edge of the existing half dart face at v,
-- and then completing the kite on the new half dart short edge.
-- This assumes exactly one dart wing tip is at v.
-- It is safe to add the 3 parts because v being unknown ensures the
-- existing dart has a boundary long edge and 
-- the new farK does not already exist (attached to existing dart farK),
-- provided the existing dart half has no kite or a full kite below.
-- If it has only a half kite below, but the new farK exists, then v will already have multi-gaps.
forceKCnode :: Vertex -> Tgraph -> Tgraph
forceKCnode v g = addLargeK g d where
    d = mustFind ((v==) . wingV) (rdarts g) $
        mustFind ((v==) . wingV) (ldarts g) $
           error ("forceKCnode: no dart wing at " ++ show v)

-- | add all 3 parts of a large kite half to a dart long edge
--I.e. add a dart with matching long edge, then add the complete kite on the new short edge.
addLargeK :: Tgraph -> TileFace -> Tgraph
addLargeK g (LD(a,_,c)) = addDK2 g' (head $ faces g') where
    g' = insertVFace x newFace g
    newFace v = RD(a,c,v)
    x = findThirdV g (a,c) (intAngle 9) (intAngle 1)
addLargeK g (RD(a,b,_)) = addDK2 g' (head $ faces g') where
    g' = insertVFace x newFace g
    newFace v = LD(a,v,b)
    x = findThirdV g (b,a) (intAngle 9) (intAngle 1)

-- | add a (missing) half kite at the base (short edge) of a half dart and then complete as a whole kite
addDK2 ::  Tgraph -> TileFace -> Tgraph   
addDK2 g (RD(_,b,c)) = complete g' (head $ faces g') where
    g' = insertVFace x newFace g
    newFace v = LK(v,c,b)
    x = findThirdV g (c,b) (intAngle 8) (intAngle 2)
addDK2 g (LD(_,b,c)) = complete g' (head $ faces g') where
    g' = insertVFace x newFace g
    newFace v = RK(v,c,b)
    x = findThirdV g (c,b) (intAngle 8) (intAngle 2)
addDK2 g _ = error "addDK2 applied to non-dart face"

{- |
For testing  allAnglesAnti and allAnglesClock on a boundary edge
The RHS of directed edge (a,b) should be the exterior side.
-}
testAngles g (a,b) = (allAnglesAnti  [(intAngle 0,b)] $ filter (isAtV a) (faces g),
                      allAnglesClock [(intAngle 0,a)] $ filter (isAtV b) (faces g))


