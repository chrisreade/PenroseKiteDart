# PenroseKiteDart

Author: Chris Reade

March 2021

See LICENSE file

## Drawing Tilings of Penrose Kites and Darts

A library for drawing and experimenting with finite tilings of Penrose\'s kite and dart tiles.
(These are parts of potentially infinite non-periodic tilings).

For just displaying tilings (Patches) plus decompose/compChoice operations you only need modules TileLib, HalfTile, ChosenBackend and possibly TileFigExamples, and a version of Main that only imports what you need.
TileLib contains the main drawing tools for Patches.

For a description of TileLib see 
https://readerunner.wordpress.com/2021/03/20/diagrams-for-penrose-tiles/

## Tgraphs to Describe Tilings

For experimenting and more advanced use there are Tile Graphs (Tgraphs):

Tgraphs use a simple planar graph representation for patches of tiles.
To use these, you also need module Tgraphs and possibly GraphFigExamples.
Module Tgraphs includes and exports contents of several other Tgraph modules, namely 
Tgraph.Compose, Tgraph.Convert, Tgraph.Decompose, Tgraph.Force, Tgraph.Prelude.
Tgraph.Convert has tools for turning Tgraphs into Patches (and VPinned).

For more information on Tgraphs see
https://readerunner.wordpress.com/2022/01/06/graphs-kites-and-darts/



