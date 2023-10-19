{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
    
import Diagrams.Prelude
import ChosenBackend (B, mainWith)

import TgraphExamples
import TestIllustrate

-- imported for testing / debugging only
import TileLib
import Tgraphs

import ArtWork
               


{-

-- To produce executable which can output multiple diagrams, use
figs::[(String,Diagram B)]
figs = trial2

main = mainWith figs

-}



-- normal executable generating single figure
fig::Diagram B
fig = newFillSun6

main :: IO ()
main = mainWith fig


{-
-- For profiling - non diagram output

main = putStrLn $ "Number of Faces of force (dartDs!!6) is: " ++ show (length $ faces $ force (dartDs!!6))
-}


