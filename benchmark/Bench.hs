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

  where
       sn = show n
       n = 4
       kD = decompositionsG kingGraph !! n
       fkD = force kD
       cfkD = last $ allComps fkD

       fig = drawGraph fkD
       w = width fig


