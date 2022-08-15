# Revision history for PenroseKiteDart


## 0.3.0.0 -- 2022-03-31

Restructured modules: 
    src/
      HalfTile.hs              -- used by TileLib and Tgraph.Prelude
      ChosenBackend.hs         -- switch between e.g. SVG or PostScript
      TileLib.hs               -- Drawing of Patches
      TileFigExamples.hs
      Tgraphs.hs               -- Main Graph Ops (imports and reexports all modules in Tgraph except Tgraph.Convert
      Tgraph/
         Tgraph.Prelude.hs     -- (imports and reexports HalfTile)
         Tgraph.Decompose.hs
         Tgraph.Compose.hs
         Tgraph.Force.hs
         Tgraph.Convert.hs     -- Converting Tgraphs to Patches and VPatches
      GraphFigExamples.hs

Graphs and Graph ops are collected in Tgraphs.hs
Converting functions (and VPatch definition) are in Tgraph.Convert.hs
Graph example figures are in GraphFigExamples.hs
Tile Patch drawing figures are in TileFigExamples.hs

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
