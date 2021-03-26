{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

 {- |
 Penrose Darts and Kites - infinite non-periodic tilings
 Main Operations of Inflate and Decompose
 using Half Tiles represented with a single vector 
 (the vector along the internal join where half tiles come together)
 -}
module TileLib where
    
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
    
{- | Component types for tiles: Left Dart, Right Dart, Left Kite, Right Kite
Half-tile components have a vector from their origin along the join edge where
origin for a dart is the tip, origin for a kite is the acute angle tip.
-}
data Component = LD (V2 Double)
               | RD (V2 Double)
               | LK (V2 Double)
               | RK (V2 Double)

-- | These are the only 4 components (oriented along x axis)
ldart,rdart,lkite,rkite:: Component
ldart = LD unitX
rdart = RD unitX
lkite = LK (phi*^unitX)
rkite = RK (phi*^unitX)

-- | All edges are powers of the golden section phi
-- We also have the interesting property of the golden section that phi^2 == phi + 1 and so 1/phi = phi-1
-- also phi^3 = 2phi +1 and 1/phi^2 = 2-phi
phi::Double
phi = (1.0 + sqrt 5.0) / 2.0

-- | All angles used are multiples of tt -  tau/10 rad = 1/10 turn = 36 deg
-- angles are from positive x axis anticlockwise
ttangle:: Int -> Angle Double
ttangle n = (fromIntegral (n `mod` 10))*^tt
             where tt = 1/10 @@ turn

{- |  The two outer tile edges of a component half-tile.
Perhaps confusingly we regard left and right of a dart differently from left and right of a kite.
This is in line with common sense view but darts are reversed from origin point of view
Going clockwise round the origin the Right Dart comes before Left Dart, but 
the Left Kite comes before Right Kite
This is manifest in only compEdges, tileEdges, compose and decompC
-}
compEdges:: Component -> [V2 Double]
compEdges (LD v) = [v',v ^-^ v'] where v' = phi*^rotate (ttangle 9) v
compEdges (RD v) = [v',v ^-^ v'] where v' = phi*^rotate (ttangle 1) v
compEdges (RK v) = [v',v ^-^ v'] where v' = rotate (ttangle 9) v
compEdges (LK v) = [v',v ^-^ v'] where v' = rotate (ttangle 1) v

-- |  the 4 tile edges of a completed half-tile component (needed for colour fill)
-- These are clockwise from the origin of the tile
tileEdges:: Component -> [V2 Double]
tileEdges (LD v) = compEdges (RD v) ++ map (zero ^-^) (reverse (compEdges (LD v)))
tileEdges (RD v) = tileEdges (LD v)
tileEdges (LK v) = compEdges (LK v) ++ map (zero ^-^) (reverse (compEdges (RK v)))
tileEdges (RK v) = tileEdges (LK v)

-- | drawing lines for the 2 outer edges of a component
drawComp:: Component -> Diagram B
drawComp = strokeLine . fromOffsets . compEdges

-- | fill whole tiles, darts with dcol and kites with kcol (colours)
-- Uses only left components to identify whole tile, ignoring right components
fillDK:: Colour Double -> Colour Double -> Component -> Diagram B
fillDK dcol kcol c =
     case c of (LD _) -> (strokeLoop $ glueLine $ fromOffsets $ tileEdges c)  # fc dcol
               (LK _) -> (strokeLoop $ glueLine $ fromOffsets $ tileEdges c)  # fc kcol
               _      -> mempty

-- | drawComp with added join edge (also fillable as a loop)
drawJComp:: Component -> Diagram B
drawJComp = strokeLoop . closeLine . fromOffsets . compEdges

-- | similar to fillDK except using drawJComp
--  so that half tiles are not completed and both left and right are filled
fillDK':: Colour Double -> Colour Double -> Component -> Diagram B
fillDK' dcol kcol c = drawComp c <> (drawJComp c # fc col # lc col) where
    col = case c of (LD _) -> dcol
                    (RD _) -> dcol
                    (LK _) -> kcol
                    (RK _) -> kcol

           
-- | experiment uses a different rule for drawing half tiles.  This clearly displays the larger kites and darts.
-- Half darts diplay the join edge in red but supress the other 2 edges as dashed lines. while
-- Half kites display only the long tile edge, supressing the short edge as dashed lines,
-- and nothing for the kite join edge.
experiment:: Component -> Diagram B
experiment c = emph c <> (drawComp c # dashingN [0.002,0.002] 0 # lw ultraThin)
  where emph c = case c of
          (LD v) -> (strokeLine . fromOffsets) [v] # lc red   -- emphasise join edge of darts in red
          (RD v) -> (strokeLine . fromOffsets) [v] # lc red 
          (LK v) -> (strokeLine . fromOffsets) [rotate (ttangle 1) v] -- emphasise long edge for kites
          (RK v) -> (strokeLine . fromOffsets) [rotate (ttangle 9) v]

-- | A patch is a list of components each with an offset represented by a vector
type Patch = [(V2 Double, Component)]

-- | turn a patch into a diagram using cd for drawing components      
patchWith cd patch = position $ fmap offset patch
    where offset (v,c) = (origin .+^ v, cd c)
    
-- | special case - turn patches to diagrams with drawComp as default
drawPatch = patchWith drawComp

-- | scale a patch by a real number
scalePatch :: Double -> Patch -> Patch
scalePatch r = fmap (\(v,c) -> (scale r v, scaleComp r c))

-- | rotate a patch by an angle
rotatePatch :: Angle Double -> Patch -> Patch
rotatePatch a = fmap (\(v,c) -> (rotate a v, rotateComp a c))

-- | move a patch by a vector
translatePatch:: V2 Double -> Patch -> Patch
translatePatch v0 = fmap (\(v,c) -> (v0 ^+^ v, c))

-- | apply a function to the vector of a component
compMap f (LD v) = LD (f v)
compMap f (RD v) = RD (f v)
compMap f (LK v) = LK (f v)
compMap f (RK v) = RK (f v)

-- | scale a component
scaleComp:: Double -> Component -> Component
scaleComp = compMap . scale 

-- | rotate a component
rotateComp :: Angle Double -> Component -> Component
rotateComp = compMap . rotate 

{- |
Decomposing splits each component in a patch into a list of smaller components to create a refined patch
Decomposition is unique.
-}
decompose :: Patch -> Patch
decompose = concatMap decompC

-- | Decomposing a component at v
decompC (v, RD vd) = [(v,        LK vd  )
                     ,(v ^+^ v', RD vd' )
                     ]  where v'  = phi*^rotate (ttangle 1) vd
                              vd' = (2-phi) *^ (zero ^-^ v')    -- (2-phi) = 1/phi^2

decompC (v, LD vd) = [(v,        RK vd  )
                     ,(v ^+^ v', LD vd' )
                     ]  where v'  = phi*^rotate (ttangle 9) vd
                              vd' = (2-phi) *^ (zero ^-^ v')    -- (2-phi) = 1/phi^2

decompC (v, RK vk) = [(v,        RD vd' )
                     ,(v ^+^ v', LK vk' )
                     ,(v ^+^ v', RK vk' )
                     ] where v'  = rotate (ttangle 9) vk
                             vd' = (2-phi) *^ v'                 -- v'/phi^2
                             vk' = ((phi-1) *^ vk) ^-^ v'        -- (phi-1) = 1/phi

decompC (v, LK vk) = [(v,        LD vd' )
                     ,(v ^+^ v', RK vk' )
                     ,(v ^+^ v', LK vk' )
                     ] where v'  = rotate (ttangle 1) vk
                             vd' = (2-phi) *^ v'                 -- v'/phi^2
                             vk' = ((phi-1) *^ vk) ^-^ v'        -- (phi-1) = 1/phi

-- | Create an infinite list of increasing decompositions of a patch
decompositions:: Patch -> [Patch]
decompositions p = inf where inf = p:fmap decompose inf

-- | get the n-fold decomposition of a patch
multiDecomp :: Int -> Patch -> Patch
multiDecomp n p = decompositions p !! n

{- |
Inflating produces a list of choices NOT a Patch
Inflating a single component at v - produces a list of alternatives
Each is a larger scale single component with new offset vector 
(to ensure the larger component contains the original in its original position in a decomposition)
-}
inflate :: (V2 Double, Component) -> [(V2 Double, Component)]
inflate (v, RD vd) = [(v ^+^ v', RD vd')
                     ,(v,        RK vk )
                     ] where v'  = (phi+1) *^ vd                  -- vd*phi^2
                             vd' = rotate (ttangle 9) (vd ^-^ v')
                             vk  = rotate (ttangle 1) v'

inflate (v, LD vd) = [(v ^+^ v', LD vd')
                     ,(v,        LK vk )
                     ] where v'  = (phi+1) *^ vd                  -- vd*phi^2
                             vd' = rotate (ttangle 1) (vd ^-^ v')
                             vk  = rotate (ttangle 9) v'

inflate (v, RK vk) = [(v,         LD vk  )
                     ,(v ^+^ lv', LK lvk') 
                     ,(v ^+^ rv', RK rvk')
                     ] where lv'  = phi*^rotate (ttangle 9) vk
                             rv'  = phi*^rotate (ttangle 1) vk
                             rvk' = phi*^rotate (ttangle 7) vk
                             lvk' = phi*^rotate (ttangle 3) vk

inflate (v, LK vk) = [(v,         RD vk  )
                     ,(v ^+^ rv', RK rvk')
                     ,(v ^+^ lv', LK lvk')
                     ] where v0 = rotate (ttangle 1) vk
                             lv'  = phi*^rotate (ttangle 9) vk
                             rv'  = phi*^rotate (ttangle 1) vk
                             rvk' = phi*^rotate (ttangle 7) vk
                             lvk' = phi*^rotate (ttangle 3) vk

-- | a list of all the alternatives after n inflation choices
inflations :: Int -> (V2 Double, Component) -> [(V2 Double, Component)]
inflations 0 vc = [vc]
inflations n vc = do
    vc' <- inflate vc
    inflations (n-1) vc'
                                
-- | combine 5 copies of a patch (rotated by multiples of 2*tt successively) 
-- use with care to avoid nonsense patches
penta:: Patch -> Patch
penta p = concatMap copy [0..4] 
            where copy n = rotatePatch (ttangle (2*n)) p
  
-- | sun and star patches 
sun,star::Patch         
sun =  penta [(zero, rkite), (zero, lkite)]
star = penta [(zero, rdart), (zero, ldart)]






