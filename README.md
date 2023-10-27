# PenroseKiteDart

Author: Chris Reade

March 2021

See LICENSE file

## Penrose Kites and Darts

Penrose\'s kite and dart tiles have the property that they can tile the entire plane non-periodicly.
There are rules to ensure legal tilings with the kites and darts.
Tilings can still get stuck (so cannot be continued to cover the entire plane) - these are called incorrect.

This package is a library of tools to build and draw finite tilings with kites and darts.

## Tgraphs to Describe Finite Tilings

Tile Graphs (Tgraphs):

Tgraphs use a simple planar graph representation for patches of tiles.
Module Tgraphs includes and exports contents of several other Tgraph modules, namely 
Tgraph.Compose, Tgraph.Convert, Tgraph.Decompose, Tgraph.Force, Tgraph.Prelude.
Tgraph.Convert has tools for turning Tgraphs into VPatches (vertex patches) and for drawing.

For more information see
https://readerunner.wordpress.com/2022/01/06/graphs-kites-and-darts/
https://readerunner.wordpress.com/2023/04/26/graphs-kites-and-darts-empires-and-superforce/
https://readerunner.wordpress.com/2023/09/12/graphs-kites-and-darts-and-theorems/


TileLib and HalfTile modules contain the underlying drawing tools for tiles (and Patches).

For a description of TileLib see 
https://readerunner.wordpress.com/2021/03/20/diagrams-for-penrose-tiles/



