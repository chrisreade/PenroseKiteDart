# Revision history for PenroseKiteDart


## 0.8.0.0 -- 2024-02-24

Fewer exported functions (Tgraph.Prelude, Tgraph.Relabelling), some renaming

## 0.7.0.0 -- 2024-02-18

Export of modules specified and changed (with more hiding)

## 0.6.0.0 -- 2024-02-17

Now as a standalone library

## 0.5.2.0 -- 2024-02-14

Added PKD (overall wrapper module).

## 0.5.1.0 -- 2024-02-13

Removed Tgraph.Convert (Conversions now included in Tgraph.Prelude)

## 0.5.0.0 -- 2024-01-26

(Removed ChosenBackEnd)
Only Main now imports a Backend (in preparation for creating library only).
Types have been generalised in modules that were previously using Backend B 
e.g.

    pCompFig :: Diagram B

has become

    pCompFig :: Renderable (Path V2 Double) b => Diagram2D b


## 0.4.0.0 -- 2023-10-27

Tgraphs now defined as newType

## 0.3.0.0 -- 2023-10-19

Modules: 
    src/
      HalfTile.hs              -- (Half)Tile constructors - used by TileLib and Tgraph.Prelude
      ChosenBackend.hs         -- switch between e.g. SVG or PostScript
      TileLib.hs               -- Drawing of Pieces (and Patches)
      Tgraphs.hs               -- Main Graph Ops (imports and reexports all modules in Tgraph and adds extra ops)
      Tgraph/
         Tgraph.Prelude.hs     -- (imports and reexports HalfTile)
         Tgraph.Decompose.hs
         Tgraph.Compose.hs
         Tgraph.Force.hs
         Tgraph.Convert.hs     -- Converting Tgraphs to VPatches (and drawing both)
      TgraphExamples.hs

## 0.2.0.0 -- 2022-03-31

Restructured modules

Graphs and Graph ops are collected in Tgraphs.hs
Converting functions (and VPatch definition) are in Tgraph.Convert.hs
Graph example figures are in TgraphExamples.hs

Original underlying Tile ops and tile drawing are in TileLib.hs

## 0.2.0.0 -- 2021-06-18

New Graph representations and operations on graphs (decomposeG, composeG, force, and more)
Tools to convert to Patches for drawing etc and also intermediate VPatches, to display Vertex information.

Key changes to original TileLib.hs : 

1.  New versions of tile halves using polymorphic versions of constructors in HalfTile.hs
2.  Pieces (replaces Components)
3.  Redefining Patches as a list of Located Pieces (Pieces with point position rather than an offset vector)
4.  Making Patches transformable (so that scale, rotate, translate can be used instead of specialised versions)

also fillDK became fillDK' with new version of fillDK

## 0.1.0.0 -- 2021-03-16

First version.
Vector representations and drawing tools for tile components and patches plus decompose and inflate operations.
Described in <https://readerunner.wordpress.com/2021/03/20/diagrams-for-penrose-tiles/>
