import Tgraphs
import TgraphExamples
import Debug.Trace (traceMarkerIO)
-- import TileLib (draw)
-- import Diagrams.Prelude

main :: IO ()
main = 
  do _ <- traceMarkerIO "starting decompositions" 
     putStrLn $ "Number of faces of a " ++ sn ++ " times decomposed King is " 
                       ++ show (length (faces kD))
     _ <- traceMarkerIO "starting force" 
     putStrLn $ "Number of faces of force (" ++ sn ++ " times decomposed King) is " 
                            ++ show (length (faces fkD))
     putStrLn $ "Max vertex of force (" ++ sn ++ " times decomposed King) is " 
                            ++ show (maxV fkD)

     _ <- traceMarkerIO "starting composing" 
     putStrLn $ "Number of faces of recomposed force (" ++ sn ++ " times decomposed King) is " 
                            ++ show (length (faces cfkD))

{-
     putStrLn $ "Number of faces of reforced force (" ++ sn ++ " times decomposed King) is " 
                            ++ show (length (faces rcfkD))
-}

  where
       sn = show n
       n = 4
       kD = {-# SCC "decomposing" #-} decompositions kingGraph !! n
       fkD = {-# SCC "forcingKD" #-} force kD
       cfkD = {-# SCC "composing" #-} last $ takeWhile (not . nullGraph) $ iterate compose fkD

{-
       fig = draw fkD
       w = width fig
-}


