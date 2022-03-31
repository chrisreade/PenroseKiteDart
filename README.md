# PenroseKiteDart

Author: Chris Reade

March 2021

See LICENSE file

## Penrose Kites and Darts - infinite non-periodic tilings

For just displaying tilings (Patches) plus inflate/decompose operations you need modules TileLib, HalfTile,
and possibly TileFigExamples, and a version of Main that only imports what you need.
TileLib contains the main drawing tools for Patches.

For documentation/description of TileLib see 
https://readerunner.wordpress.com/2021/03/20/diagrams-for-penrose-tiles/

For more advanced use there are Tile Graphs (Tgraphs):

Tile Graphs are an experimental addition with a graph representation.
For this you also need modules Tgraphs, and Tgraph.Convert, and possibly GraphFigExamples.
Tgraphs has operations decomposeG (a Tgraph version of decompose)
plus composeG and force.
Tgraph.Convert has tools for turning Tgraphs into Patches (and VPatches).

For more info on Tgraphs see
https://readerunner.wordpress.com/2022/01/06/graphs-kites-and-darts/



