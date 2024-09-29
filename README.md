# PenroseKiteDart

Author: Chris Reade

March 2021 - 2024

See LICENSE file

## Penrose Kites and Darts

Penrose\'s kite and dart tiles have the property that they can tile the entire plane aperiodicly.
There are rules to ensure legal tilings with the kites and darts.
Legal tilings can still get stuck (so cannot be continued to cover the entire plane) - these are called incorrect.

This package is a Haskell library of tools to build, draw and explore finite tilings with kites and darts, making use of the 
Haskell Diagrams package.

## Using the Package

You will need the Haskell Diagrams package
to be installed as well as this package (PenroseKiteDart). (See [Haskell Diagrams](https://diagrams.github.io)).
Once installed, a Main.hs module to produce diagrams should import a chosen backend for Diagrams such as the default (SVG)
along with Diagrams.Prelude

    module Main (main) where
    
    import Diagrams.Backend.SVG.CmdLine
    import Diagrams.Prelude

plus (for Penrose Kite and Dart tilings)

    import PKD

and optionally

    import TgraphExamples

Then to ouput someExample figure

    fig::Diagram B
    fig = someExample

    main :: IO ()
    main = mainWith fig

When the code is executed it will generate an SVG file.
(See the Haskell Diagrams package for more details on producing diagrams.)

## Tgraphs to Describe Finite Tilings

Tile Graphs (`Tgraph`s) use a simple planar graph representation for finite patches of tiles.
A `Tgraph` is made from a list of faces with type `TileFace` each of which is a half-dart or a half-kite.
Each `TileFace` is thus a triangle with three positive Int vertices and a constructor
`LD` (left dart), `RD` (right dart), `LK` (left kite), `RK` (right kite).

For example a fool consists of two kites and a dart (= 4 half kites and 2 half darts):

    fool :: Tgraph
    fool = makeTgraph [RD (1,2,3),LD (1,3,4)   -- right and left dart
                      ,LK (5,3,2),RK (5,2,7)   -- left and right kite
                      ,RK (5,4,3),LK (5,6,4)   -- right and left kite
                      ]

The function

    makeTgraph :: [TileFace] -> Tgraph

performs checks to make sure the tiling is legal, raising an error if there is a problem.
To produce a diagram, we simply draw the `Tgraph`

    foolFigure :: Diagram B
    foolFigure = labelled draw fool


## Modules

Module `PKD` is the main module which imports and re-exports `Tgraphs` and `TileLib`.
`Tgraphs` imports and re-exports the contents of the other Tgraph modules, namely 
`Tgraph.Compose`, `Tgraph.Decompose`, `Tgraph.Force`, `Tgraph.Relabelling`, `Tgraph.Prelude`.
`TileLib` contains underlying drawing tools for tiles.
`Try` is imported and re-exported by `Tgraph.Prelude` - used for results of partial functions.
`HalfTile` is imported and re-exported by `Tgraph.Prelude` -  (with the constructors `LD`,`RD`,`LK`,`RK`).
`CheckBackend` is imported by `TileLib` which rexports class `OKBackend`.
(The constraint `OKBackend b =>` is used extensively in the library to abstract types from any particular Backend).
`TgraphExamples` contains example Tgraphs and Diagrams.

## Further Information

A more detailed User Guide for the PenroseKiteDart package can be found at

- [PenroseKiteDart User Guide](https://readerunner.wordpress.com/2024/04/08/penrosekitedart-user-guides/)


