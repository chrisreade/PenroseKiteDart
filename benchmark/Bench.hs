import PKD
import TgraphExamples
import Debug.Trace (traceMarkerIO)
import Control.Concurrent (threadDelay)
-- import TileLib (draw)
-- import Diagrams.Prelude

main :: IO ()
main = 
  do let wait = threadDelay 100000
     _ <- traceMarkerIO "starting decompositions" 
     wait
     let kD = {-# SCC "decomposing" #-} decompositions kingGraph !! n
     putStrLn $ "Number of faces of a " ++ sn ++ " times decomposed King is " 
                       ++ show (length (faces kD))
     putStrLn $ "Max vertex of a (" ++ sn ++ " times decomposed King) is " 
                            ++ show (maxV kD)
     _ <- traceMarkerIO "finished decomposing" 
     wait
     _ <- traceMarkerIO "starting force" 
     let fkD = {-# SCC "forcingKD" #-} forceF kD
     putStrLn $ "Number of faces of force (" ++ sn ++ " times decomposed King) is " 
                            ++ show (length $ faces $ forgetF fkD)
     putStrLn $ "Max vertex of force (" ++ sn ++ " times decomposed King) is " 
                            ++ show (maxV $ forgetF fkD)
     _ <- traceMarkerIO "finished force" 
     wait
     _ <- traceMarkerIO "starting (unchecked) composing" 
     let cfkD = {-# SCC "composing" #-} forgetF $ last $ takeWhile (not . nullGraph . forgetF) $ iterate composeF fkD
     -- let cfkD = {-# SCC "composing" #-} last $ takeWhile (not . nullGraph) $ iterate uncheckedCompose fkD
     putStrLn $ "Number of faces of recomposed force (" ++ sn ++ " times decomposed King) is " 
                            ++ show (length (faces cfkD))
     putStrLn $ "Max vertex of recomposed force (" ++ sn ++ " times decomposed King) is " 
                            ++ show (maxV cfkD)
     _  <- traceMarkerIO "finished (unchecked) composing" 
     return ()
{-
     putStrLn $ "Number of faces of reforced force (" ++ sn ++ " times decomposed King) is " 
                            ++ show (length (faces rcfkD))
-}

  where
       sn = show n
       n = 5

{-
       fig = draw fkD
       w = width fig
-}


