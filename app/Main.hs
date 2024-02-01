{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
    
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
{-
-- for svg output
import Diagrams.Backend.SVG.CmdLine as Backend

-- for eps output
import Diagrams.Backend.Postscript.CmdLine as Backend

-- for gif/animated gif/png/jpg/tif/bmp/pdf output NOT svg
import Diagrams.Backend.Rasterific.CmdLine as Backend
-}

import TgraphExamples

-- imported for testing / debugging only
import TileLib
import Tgraphs

import TestIllustrate
import ArtWork
               
fig::Diagram B
fig = brokenDartFig

main :: IO ()
main = mainWith fig

{-
-- normal executable generating single figure
fig::Diagram B
fig = maxExampleFig 

main :: IO ()
main = mainWith fig
-}


{-
-- For generating animated gifs with Rasterific Backend. Don't forget Use -o ...gif when generating output
figs::[(Diagram B,Int)]
figs = animGapFill

main :: IO ()
main = mainWith figs
-}


{-
-- For profiling - non diagram output
main = putStrLn $ "Number of Faces of force (dartDs!!6) is: " ++ show (length $ faces $ force (dartDs!!6))
-}


