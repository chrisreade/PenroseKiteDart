# PenroseKiteDart

Author: Chris Reade

March 2021

See LICENSE file

## Penrose Kites and Darts

Penrose\'s kite and dart tiles have the property that they can tile the entire plane non-periodicly.
There are rules to ensure legal tilings with the kites and darts.
Tilings can still get stuck (so cannot be continued to cover the entire plane) - these are called incorrect.

This package is a Haskell library of tools to build, draw and explore finite tilings with kites and darts, making use of the 
Haskell Diagrams package.

## Using the Package

You will need the Haskell Diagrams package
to be installed as well as this package. (See [Haskell Diagrams](https://diagrams.github.io)).
A Main module to produce diagrams should import a chosen backend for Diagrams such as the default (SVG)
along with the Diagrams prelude

    import Diagrams.Backend.SVG.CmdLine
    import Diagrams.Prelude

plus (for tilings)

    import TileLib
    import Tgraphs

and optionally

    import TgraphExamples

Then to ouput someExample figure

    fig::Diagram B
    fig = someExample

    main :: IO ()
    main = mainWith fig

When the code is executed it will generate an SVG file.
(See the Diagrams package for more details on producing diagrams.)

## Tgraphs to Describe Finite Tilings

Tile Graphs (Tgraphs) use a simple planar graph representation for patches of tiles.
We build tilings using half tiles, each of which is a triangle using constructors
LD (left dart), RD (right dart), LK (left kite), RK (right kite).
For example a fool consists of two kites and a dart (= 4 half kites and 2 half darts):

    fool :: Tgraph
    fool = makeTgraph [RD (1,2,3),LD (1,3,4)   -- right and left dart
                      ,LK (5,3,2),RK (5,2,7)   -- left and right kite
                      ,RK (5,4,3),LK (5,6,4)   -- right and left kite
                      ]

    foolFigure :: Diagram B
    foolFigure = labelled draw fool
    
## Modules

Module `Tgraphs` includes and exports the contents of the other Tgraph modules, namely 
`Tgraph.Compose`, `Tgraph.Convert`, `Tgraph.Decompose`, `Tgraph.Force`, `Tgraph.Relabelling`, `Tgraph.Prelude`.

Module `TileLib` (which re-exports module `HalfTile`) contains underlying drawing tools for tiles (and Patches) and the constructors `LD`,`RD`,`LK`,`R` are introduced in module `HalfTile`.

For more information see

- [Graphs, Kites and Darts](https://readerunner.wordpress.com/2022/01/06/graphs-kites-and-darts/)
- [Empires and SuperForce](https://readerunner.wordpress.com/2023/04/26/graphs-kites-and-darts-empires-and-superforce/)
- [Graphs,Kites and Darts and Theorems](https://readerunner.wordpress.com/2023/09/12/graphs-kites-and-darts-and-theorems/) 

For a description of TileLib see

- [Diagrams for Penrose Tiles](https://readerunner.wordpress.com/2021/03/20/diagrams-for-penrose-tiles/)



