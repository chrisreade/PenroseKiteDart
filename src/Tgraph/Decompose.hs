module Tgraph.Decompose where

import qualified Data.Map as Map (Map, lookup, insert, empty, (!))

import Tgraph.Prelude



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
    (newVs , newVFor) = newVPhiMap g
    newFaces = concatMap (decompFace newVFor) (faces g)

-- | newVPhiMap g produces newVs - a list of new vertices (one for each phi edge of v)
--   and a function mapping each phi edge to its assigned vertex in newVs
--   The map is built using buildMap and both (a,b) and (b,a) get the same v   
newVPhiMap :: Tgraph -> ([Vertex], (Vertex, Vertex) -> Vertex)
newVPhiMap g = (newVs, (Map.!) $ buildMap allPhi newVs Map.empty) where
  allPhi = phiEdges g
  newVs = makeNewVs (length allPhi `div` 2) (vertices g)
  buildMap [] vs m = m
  buildMap ((a,b):more) vs m = case Map.lookup (a,b) m  of
    Just _  -> buildMap more vs m
    Nothing -> buildMap more (tail vs) (Map.insert (a,b) v (Map.insert (b,a) v m))
               where v = head vs

  -- | decompFace to process a face in decomposition
  -- producing new faces. 
  -- It uses a newVFor - a function to get the unique vertex assigned to each phi edge
decompFace:: ((Vertex,Vertex)->Vertex) -> TileFace -> [TileFace]
decompFace newVFor fc = case fc of
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
     

            

-- infinite list of decompositions of a graph     
decompositionsG :: Tgraph -> [Tgraph]
decompositionsG = iterate decomposeG

-- | decompose a SubTgraph - applies decomposition to all tracked subsets as well as the full graph
decomposeSub :: SubTgraph -> SubTgraph
decomposeSub (SubTgraph{ fullGraph = g, trackedSubsets = tlist}) = makeSubTgraph g' tlist' where
   g' = Tgraph{ vertices = newVs++vertices g
              , faces = newFaces
              }
   (newVs , newVFor) = newVPhiMap g
   newFaces = concatMap (decompFace newVFor) (faces g)
   tlist' = fmap (concatMap (decompFace newVFor)) tlist

