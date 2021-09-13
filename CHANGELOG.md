# Revision history for PenroseKiteDart


## 0.2.0.0 -- 2021-06-18

New Graph representations and operations on graphs (graphDecompose, graphCompose, force, and more)
Tools to convert to Patches for drawing etc and also intermediate VPatches, to display Vertex information.

Graphs and Graph ops are in Tgraphs.hs
Converting functions (and VPatch definition) are in GraphConvert.hs
Graph example figures are in GraphFigExamples.hs
Tile Drawing figures, swatches, etc are in TileFigExamples.hs

Original underlying Tile ops and tile drawing are in TileLib.hs (now using HalfTile.hs)

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
