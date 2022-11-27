{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

{-|
Module      : DecompExplain
Description : A single figure explaining (vector) decomposition of Patches
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

Exports a single figure decompExplainFig
illustrating piece decomposition with vectors (and compose choices)
-}

module DecompExplain (decompExplainFig) where
    
import Diagrams.Prelude hiding (dart)
import Diagrams.TwoD.Vector (e)

import ChosenBackend (B)  --import Diagrams.Backend.SVG.CmdLine
import TileLib

rkiteAt p = mconcat [dotAt p blue
                    ,labelEdge "(v')" p point1
                    ,labelVect "vk" p point0
                    ,point1 ~~ point0
                    ,labelP "p" p
                    ] where point1 = p .+^ (phi*^ e (ttangle 4))
                            point0 = p .+^ (phi*^ e (ttangle 5))
                            

drkiteAt p = mconcat [dotAt p blue
                     ,labelVect "vd" p point3
                     ,p ~~ point2
                     ,point1 ~~ point0
                     ,dotAt point1 blue
                     ,labelVect "vk'" point1 point2
                     ,point3 ~~ point2
                     ,point0 ~~ point2
                     ,point1 ~~ point3
                     ,labelP "p" p
                     ,labelP "p .+^ v'" (point1 .+^ (0.1 *^ (unitX ^+^ unitY)))
                     ] where point1 = p .+^ (phi*^ e (ttangle 4))
                             point0 = p .+^ (phi*^ e (ttangle 5))
                             point2 = p .+^ ((1/phi) *^ (point0 .-. p))
                             point3 = p .+^ ((1/phi^2) *^ (point1 .-. p))

rdartAt p = mconcat [dotAt p blue
                    ,labelVect "vd" p point1
                    ,labelEdge "" p point0
                    ,altlabel "(v')"  (p .+^ (0.5 *^ (point0 .-. p)))
                    ,point0 ~~ point1
                    ,labelP "p" p
                    ] where point1 = p .+^ unitX
                            point0 = p .+^ (phi*^ e (ttangle 1))
                            
drdartAt p = mconcat [dotAt p blue
                     ,labelVect "vk = vd" p point1
                     ,p ~~ point2
                     ,dotAt point0 blue
                     ,point0 ~~ point1
                     ,point1 ~~ point2
                     ,labelVect "" point0 point2        -- vd'  
                     ,altlabel "vd'"  (p .+^ (1.3 *^ (point2 .-. p)))   
                     ,labelP "p" p
                     ,labelP "p .+^ v'" point0
                     ] where point1 = p .+^ unitX
                             point0 = p .+^ (phi*^ e (ttangle 1))
                             point2 = p .+^ e (ttangle 1)

srdartAt p = mconcat [dotAt p blue
                     ,labelVect "vd" p point1
                     ,p ~~ point0
                     ,point0 ~~ point1
                     ,labelP "p" p
                     ] where point1 = p .+^ ((phi-1)*^unitX)
                             point0 = p .+^ e (ttangle 1)

spot  = circle 0.02 # lw none
dotAt p c = spot # fc c  # moveTo p

altlabel l p = text l # fontSize (local 0.1) # fc green # moveTo (p .+^ (0.2 *^ unit_X))
label l p = baselineText l # fontSize (local 0.1) # fc green # moveTo p
labelP l p = topLeftText l # fontSize (local 0.1) # fc blue # moveTo p
name l p  = baselineText l # fontSize (local 0.1) # fc red # moveTo p

labelEdge l p p' = edgeArrow p p' <> label l p'' where p'' =  p .+^ (0.5 *^ (p' .-. p))
labelVect l p p' = labelEdge l p p' # dashingG [0.03, 0.03] 0
edgeArrow = arrowBetween' (with & headLength .~ small )

line v = strokeT . fromOffsets $ [v]
{-
 [line # lw w | w <- [ultraThin, veryThin, thin, medium, thick, veryThick, ultraThick]]
-}

fig0 =  mconcat[rkiteAt    $ p2 (2.0,3.5)
               ,name "RK"  $ p2 (1.0,3.8)
               ,drkiteAt   $ p2 (2.0,2.0)
               ,name "RD"  $ p2 (1.5,2.1)
               ,name "LK"  $ p2 (1.1,2.3)
               ,name "RK"  $ p2 (0.6,2.3)
               ,rdartAt    $ p2 (3.0,3.5)
               ,name "RD"  $ p2 (3.7,3.8)
               ,drdartAt   $ p2 (3.0,2.0)
               ,name "RD"  $ p2 (4.0,2.5)
               ,name "LK"  $ p2 (3.7,2.2)
               ,srdartAt   $ p2 (2.3,0.5)
               ,name "RD"  $ p2 (2.8,0.7)
               ] # lw thin
               <> arrowV unit_Y # moveTo (p2(2.5,3.5))
               <> name "DECOMPOSITION" (p2(2.55,3.0))
               <> arrowV (0.3 *^ (unitY^+^unitX)) # moveTo (p2(2.5,1.0))
               <> arrowV (0.3 *^ (unitY^+^unit_X)) # moveTo (p2(2.5,1.0))
               <> name "INFLATE CHOICES (FOR RD)" (p2(1.8,1.5))

-- |A figure illustrating decomposition of pieces (and composition choices) with vectors.         
decompExplainFig::Diagram B
decompExplainFig = pad 1.2 $ centerXY fig0 

    

