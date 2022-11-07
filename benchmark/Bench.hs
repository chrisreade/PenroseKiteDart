import Tgraphs
import GraphFigExamples
import Diagrams.Prelude

main :: IO ()
main = 
  do putStrLn $ "Number of faces of a " ++ sn ++ " times decomposed King is " 
                       ++ show (length (faces kD))
     putStrLn $ "Number of faces of force (" ++ sn ++ " times decomposed King) is " 
                            ++ show (length (faces fkD))

     putStrLn $ "Width of figure for force (" ++ sn ++ " times decomposed King) is " 
                            ++ show w

     putStrLn $ "Number of faces of recomposed force (" ++ sn ++ " times decomposed King) is " 
                            ++ show (length (faces cfkD))

{-
     putStrLn $ "Number of faces of reforced force (" ++ sn ++ " times decomposed King) is " 
                            ++ show (length (faces rcfkD))
-}

  where
       sn = show n
       n = 4
       kD = {-# SCC "decomposing" #-} decompositionsG kingGraph !! n
       fkD ={-# SCC "forcing" #-} force kD
       cfkD = {-# SCC "composing" #-} last $ takeWhile (not . nullGraph) $ iterate composeG fkD

       fig = {-# SCC "drawing" #-} drawGraph fkD
       w = width fig


