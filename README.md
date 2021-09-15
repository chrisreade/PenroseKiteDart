# PenroseKiteDart

Author: Chris Reade

March 2021

See LICENSE file

## Penrose Kites and Darts - infinite non-periodic tilings

Main Operations are decompose and inflate in TileLib along with drawing tools.
Now requires HalfTile module with half tile representation generalised
Drawing uses half tiles (Pieces) represented with vectors.

For documentation/description of TileLib see 
https://readerunner.wordpress.com/2021/03/20/diagrams-for-penrose-tiles/

For just displaying tilings plus inflate/decompose you need TileLib, HalfTile
(and possibly TileFigExamples)
and a version of Main that only imports what you need.

Version 0.2 adds experimental Tgraphs

Tgraphs are for graph representations
In HalfTile constructors are polymorphic (with previous vector based components a special case)
GraphDecompose implemented as a Tgraph version of (vector) decompose
Plus experimental Tgraph ops to compose/force

GraphConvert for turning Tgraphs into vector representations for drawing.

