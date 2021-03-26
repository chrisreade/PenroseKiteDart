{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
    
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import TileLib

-- | filled components and drawn edges of 4 component half-tiles in a row         
components =  [ldart, rdart, lkite, rkite]  
componentsFig = hsep 0.5 $ fmap (showOrigin . drawJComp) components 
componentsFig2 = hsep 1 $ fmap (fillDK red blue) components ++ fmap drawComp components 
   
-- | 4 decompositions in a column for each component
fourDecomps = hsep 1 $ fmap decomps components # lw thin where
        decomps cpt = vsep 1 $ fmap drawPatch $ take 5 $ decompositions [(zero,cpt)] 

-- | example of inflate in action (5 chosen inflate steps)
-- show inital and final component together on left patch,  
-- and 5 decomposition of final component in right patch
fiveInflate = hsep 1 $ fmap drawPatch [[ld,lk'], multiDecomp 5 [lk']] where -- two seperate patches
       ld  = (zero,ldart)
       lk  = inflate ld  !!1
       rk  = inflate lk  !!1
       rk' = inflate rk  !!2
       ld' = inflate rk' !!0
       lk' = inflate ld' !!1

-- | example of first  5 alternatives of 4-fold inflations of vc (vector,component)
inflatefig vc = hsep 1 $ fmap drawPatch $ fmap (:[vc]) $ take 5 $ inflations 4 vc
fiveAlternatives = inflatefig (zero,rdart)

-- | Decomposed suns
suns = decompositions sun
sun6 = suns!!6
sun5 = suns!!5 

sun6Fig = drawPatch sun6 # lw thin
-- | overlaying sun5 in red atop sun6
sun5Over6Fig = (drawPatch sun5 # lc red # dashingN [0.003,0.003] 0 <> drawPatch sun6) # lw thin
-- | colour-filled examples
filledSun6 = patchWith (fillDK red blue) sun6 # lw ultraThin
filledSun8 = patchWith (fillDK red blue) (suns!!8) # lw ultraThin
-- | Using experiment on sun6 clearly illustrates the embedded sun5
experimentFig = patchWith experiment sun6 # lw thin

markedTiles = hsep 1  
        [ drawPatch [(zero, lkite),(zero,rkite)] # showOrigin 
        , drawPatch [(zero, ldart),(zero,rdart)] # showOrigin 
        , drawPatch [(zero, lkite),(zero,rkite)] <> (pL ~~ pR # lc red # lw thick) 
        , drawPatch [(zero, ldart),(zero,rdart)] <> (origin ~~ p2(1,0) # lc red # lw thick)
        ] where pL = origin .+^ phi*^rotate (ttangle 1) unitX
                pR = origin .+^ phi*^rotate (ttangle 9) unitX


test1 = patchWith (fillDK' fuchsia cornflowerblue) sun6 # lw ultraThin # lc blue

fig::Diagram B
fig = test1

main = mainWith fig


