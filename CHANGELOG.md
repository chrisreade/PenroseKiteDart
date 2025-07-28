# Revision history for PenroseKiteDart

Since 1.4.4 
added drawjP3 (renaming dashjP3)
new decomposeFaces
generalised phiVMap

exposed P3_Patch
exposed bothDir
exposed missingRevs

## version 1.4.4  -- 2025-8-9

Fixed bug in remainder faces for new partComposeF

Added new module `TileLibP3` for drawing P3 tilings.
This is now included in `PKD` export list
with example `testRhombus` in module `TgraphExamples`.

## version 1.4.3  -- 2025-7-1

Fixed bug in `partComposing`.
(partCompFacesFrom was assuming forced to get remainders.)
This is now incorporated in partComposeFaces and partComposeFacesF
and partCompFacesFrom is now removed.

## version 1.4.2  -- 2025-7-1

DartWingInfo has an extra field (unMapped)

Improved composing performance (now a Strict module)
New: partCompFacesFrom (used instead of composeFaceGroups in composing)

## version 1.4.1  -- 2025-6-26
Force module is now Strict (significantly improves space usage)
(HalfTile, Decompose, Relabelling also made Strict
and Prelude StrictData)

ForceState now an instance of class HasFaces

New: boundaryEdgeFilter
  (used instead of boundaryFilter to simplify UFinders)


## version 1.4 -- 2025-6-12

Changed module Tgraphs to module Tgraph.Extras

### New HasFaces class

Introduced to define common functions for 
[TileFace], Tgraph, VPatch, BoundaryState, Forced

Breaking Name Changes for
    nullGraph -> nullFaces
    graphBoundary -> boundary
    facesBoundary -> boundary
    graphDedges -> dedges
    facesDedges  -> dedges
    facesVSet -> vertexSet
    facesMaxV -> maxV
    graphBoundaryVs -> boundaryVs
    boundaryVertexSet -> vertexSet
    facesEdges -> completeEdges
    graphEdges -> completeEdges
    boundaryLoopsG -> boundaryLoops
    graphEFMap -> buildEFMap

    Also changed selector in BoundaryState to boundaryDedges (but note boundary generalised)

Generalised (to use HasFaces)
    boundary
    faces
    maxV
    vertexSet
    boundaryVs
    internalEdges
    phiEdges
    nonPhiEdges
    vertexFacesMap
    dedgesFacesMap
    buildEFMap
    extractLowestJoin
    lowestJoin
    locateVertices
    locateVerticesGen
    touchingVertices
    boundaryEdgeSet
    commonBdry
    boundaryVertexSet
    internalVertexSet
    boundaryLoops
    evalFaces
    ldarts,rdarts,lkites,rkites, kites, darts
    crossingBVs
    crossingBoundaries
    connected
    defaultAlignment
    findEdgeLoops
    hasEdgeLoops
    conflictingDedges

Breaking: No longer exported:
findLoops, axisJoin, drawEdge (use drawLocatedEdge), drawEdges (use drawLocatedEdges),
crossingVertices, tryFindThirdV, makeGenerator (use newUpdateGenerator)

### Try changed
Changed the type for Try to use ShowS instead of String
(ShowS = String -> String)

New: failReport, failReports, tryAtLeastOne

Breaking:
   Occurrences of 
        Left s :: Try a
    need to be replaced by 
        failReport s 
    or
        Left (s<>)

An instance of Show(ShowS) is provided in order to show Try results

### Changes to Forced

Forced is no longer a Functor.

Instead, 4 specific safe cases for changing a Forced Forcible
(New)
recoverGraphF,boundaryStateF,makeBoundaryStateF,initFSF

Data constructor Forced is no longer exported but
(New)
labelAsForced :: a -> Forced a (for use only when the argument is known to be forced)

(New)
tryDartAndKiteF - version of tryDartAndKiteForced with explicitly forced results.
tryCheckCasesDKF, checkCasesDKF - special case to report any counter example found
when trying to add a dart and kite to a boundary edge of an explicitly Forced forcible.
 
Removed warning pragma for makeUncheckedTgraph

## version 1.3 -- 2025-5-19

(New)
Introduced newtype operator Forced
to enable restricting functions which require a forced argument.
Forced a is an explicitly forced version of a.

Breaking changes:

Removed: 
uncheckedCompose (use new composeF with explicitly Forced Tgraph)
uncheckedPartCompose (use new partComposeF with explicitly Forced Tgraph)

Changed types (to make use of Forced) for:
compForce
allCompForce
maxCompForce
boundaryVCovering
boundaryECovering
singleChoiceEdges

Removed deprecated: 
noFails (use runTry . concatFails)
colourMaybeDKG (use colourDKG with transparent)
fillMaybeDK (use fillDK with transparent)
fillMaybePieceDK (use fillPieceDK with transparent)

Renamed:
tryOneStepF is now tryOneStepForce

Other changes:

(New)
forgetF :: Forced a -> a (to unwrap explicitly Forced)
tryForceF (to create explicitly Forced)
forceF (to create explicitly Forced)
composeF :: Forced Tgraph -> Forced Tgraph
partComposeF :: Forced Tgraph -> ([TileFace], Forced Tgraph)

Added warning in PKD for makeUncheckedTgraph


## version 1.2.1 -- 2025-4-2

Added: drawBoundaryJoins, joinDashing

Renamed: drawEdge, drawEdges as drawLocatedEdge, drawLocatedEdges
Depracating: drawEdge, drawEdges

Generalised: colourDKG, fillDK, fillKD, fillPieceDK, fillOnlyPiece
to work with AlphaColours as well as Colours

Deprecating: colourMaybeDKG, fillMaybeDK, fillMaybePieceDK

Added (strict) makeRD, makeLD, makeRK, makeLK to Tgraph.Prelude


## version 1.2 -- 2024-12-1

Release candidate:
Introduced getDartInfoForced and improved performance of uncheckedPartCompose and uncheckedCompose
removed: composedFaces = snd . partComposeFaces  (all in Tgraph.Compose)

Significant improvement on space usage (fixing leaks)
adding StrictData to modules Tgraph.HalfTile, Tgraph.Compose, Tgraph.Force.
makeUncheckedTgraph now strictly evaluates its argument list of faces.

Made UpdateGenerator a newtype in Tgraph.Force

## 1.1.1 -- 2024-11-15

Exposed combineUpdateGenerators in Tgraph.Force

Reordered lists of faces in some basic example Tgraphs
(to ensure tails of the list are also valid as Tgraphs)

## 1.1.0 -- 2024-09-28

Release candidate: 

Added module CheckBackend with class OKBackend. This is really a class synonym for the constraints on a suitable Backend
for drawing tilings. Most types involving a backend b now have a constraint OKBackend b => ...

Removed type synonym: type Diagram2D b = QDiagram b V2 Double Any (no longer needed with the above constraint).

No longer exporting: differing, changeVFMap, forcedDecomp (= force . decompose).

Moved makeTgraph to Tgraph.Prelude.
Moved emplaceChoices to TgraphExamples and added example.
Moved module Tgraph.Try out of Tgraph (so now module Try).

tryStepForceWith now raises an error for negative number of steps.

Added graphBoundaryVs to Tgraph.Prelude.
Added tryBoundaryFaceGraph to Tgraphs

Made ForceState an instance of Show.

Improved haddock comments in Tgraph.Force.

Changed dash sizes for join edges (in dashjOnly).

## 1.0.0 -- 2024-04-08

Release candidate: 

Added upper bounds on dependencies

Added new drawEmpire and changed drawEmpire1, drawEmpire2 to showEmpire1, showEmpire2

## 0.10.0.0 -- 2024-04-1

Removed some examples in TgraphExamples and export of some auxiliary functions in Tgraph.Relabelling

## 0.9.1.0 -- 2024-03-12

Tgraph.Try as a separate module (instead of part of Tgraph.Prelude)
Added labelColourSize in DrawableLabelled with labelSize as special case
Changes to labelSize and line widths in some diagrams and drawing functions.
Removed labelSmall, labelLarge.
Added drawTrackedTgraphAligned.
Both restrictVP and relevantVP now check for missing locations.

## 0.8.0.2 -- 2024-02-25

Documentation changes only.

## 0.8.0.1 -- 2024-02-24

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
