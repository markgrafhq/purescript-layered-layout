-- Copyright (c) 2012, 2020 Kiel University and others.
-- SPDX-License-Identifier: EPL-2.0
--
-- Functional translation of ELK's LabelDummyInserter, LabelDummySwitcher
-- (MEDIAN_LAYER), LabelSideSelector (SMART_DOWN), and LabelDummyRemover:
-- https://github.com/eclipse-elk/elk/tree/c831ba4613dfd6b0055851193956560351d2f907/plugins/org.eclipse.elk.alg.layered/src/org/eclipse/elk/alg/layered/intermediate
--
-- The public model supplies one measured, non-inline CENTER label per edge.
-- Labels use coarse input sizes; routes and final labels use fine units.
-- Frame conversion lives in dummySize, dummyPortOffset, and placements.
module LayeredLayout.EdgeLabels
  ( LabelState
  , LabelDummy
  , LabelSide(..)
  , insert
  , switchDummies
  , selectSides
  , portOffsets
  , placements
  , restore
  ) where

import Prelude

import Data.Array (concatMap, drop, filter, find, head, index, last, length, mapMaybe, null, reverse, snoc, take, takeWhile, uncons, zipWith)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import LayeredLayout.DummyNodes (DummyResult, isDummy)
import LayeredLayout.EdgeRouting (scaleFactor)
import LayeredLayout.Graph (Edge, EdgeId(..), Graph, Node, NodeId(..), Shape(..), Side(..))
import LayeredLayout.Grid (GridPos(..), GridSize(..), gridX, gridY, sizeH, sizeW)
import LayeredLayout.PortDistribution (EdgePortOffsets)
import LayeredLayout.Result (Direction(..), EdgeLabelPlacement, EdgePath, NodePlacement)

-- Sides are named in ELK's normalized RIGHT frame. In DOWN, Above is
-- right of the edge and Below is left; the order within a layer reverses.
data LabelSide = Above | Below

type LabelDummy =
  { node :: NodeId
  , edge :: Edge
  , tail :: EdgeId
  , size :: GridSize
  , side :: LabelSide
  }

type LabelState =
  { sizes :: Map EdgeId GridSize
  , dummies :: Array LabelDummy
  , nodes :: Array Node
  , edges :: Array Edge
  }

-- LabelDummyInserter.process: split each measured non-loop edge and retain
-- its original endpoints, identity and measured label on the new dummy.
insert :: Map EdgeId GridSize -> Graph -> Array Edge -> LabelState
insert sizes _ edges | Map.isEmpty sizes = { sizes, dummies: [], nodes: [], edges }
insert sizes graph edges = (foldl add initial edges).labels
  where
  initial =
    { labels: { sizes, dummies: [], nodes: [], edges: [] }
    , usedNodes: Set.fromFoldable (graph.nodes <#> _.id)
    , usedEdges: Set.fromFoldable (graph.edges <#> _.id)
    }
  freshNode occupied text =
    if Set.member (NodeId text) occupied then freshNode occupied (text <> "'") else NodeId text
  freshEdge occupied text =
    if Set.member (EdgeId text) occupied then freshEdge occupied (text <> "'") else EdgeId text
  add acc edge = case Map.lookup edge.id sizes of
    Just size | edge.from.node /= edge.to.node -> do
      let node = freshNode acc.usedNodes ("$label:" <> un EdgeId edge.id)
      let tail = freshEdge acc.usedEdges ("$label-tail:" <> un EdgeId edge.id)
      let dummy = { node, edge, tail, size, side: Below }
      let n = { id: node, size: dummySize size, ports: [], label: Nothing, shape: Rectangle }
      acc
        { usedNodes = Set.insert node acc.usedNodes
        , usedEdges = Set.insert tail acc.usedEdges
        , labels = acc.labels
            { dummies = snoc acc.labels.dummies dummy
            , nodes = snoc acc.labels.nodes n
            , edges = acc.labels.edges <>
                [ edge { to = { node, port: Nothing }, label = Nothing }
                , edge { id = tail, from = { node, port: Nothing }, label = Nothing }
                ]
            }
        }
    _ -> acc { labels = acc.labels { edges = snoc acc.labels.edges edge } }

-- Existing layout defaults, in fine units. With one label there is no
-- label-label spacing to retain from LabelDummyInserter's stacked extent.
edgeLabelSpacing :: Number
edgeLabelSpacing = 2.0

edgeThickness :: Number
edgeThickness = 1.0

fineScale :: Number
fineScale = toNumber scaleFactor

dummySize :: GridSize -> GridSize
dummySize size = GridSize ((sizeW size + (edgeLabelSpacing + edgeThickness) / fineScale) /\ sizeH size)

-- LabelDummySwitcher.findMedianLayerTargetId / swapNodes. The chain includes
-- both real endpoints, so its lower interior median is (length - 1) / 2.
-- Swapping identities preserves the layer slots established by crossing min.
-- Our chain adapter rebuilds segment IDs after rewiring; its two independent
-- halves also encode ELK's LONG_EDGE_BEFORE_LABEL_DUMMY boundary.
switchDummies :: LabelState -> DummyResult -> { labels :: LabelState, dummies :: DummyResult }
switchDummies labels dummies = foldl switchOne { labels, dummies } labels.dummies
  where
  switchOne acc label = case find (\c -> c.edgeId == label.edge.id) acc.dummies.chains, find (\c -> c.edgeId == label.tail) acc.dummies.chains of
    Just firstChain, Just lastChain -> do
      let chain = firstChain.nodes <> drop 1 lastChain.nodes
      let middle = (length chain - 1) `div` 2
      case index chain middle of
        Just target | target /= label.node -> do
          let
            swap n
              | n == label.node = target
              | n == target = label.node
              | otherwise = n
          let nodes = map swap chain
          let left = take (middle + 1) nodes
          let right = drop middle nodes
          let
            chains = acc.dummies.chains <#> \c ->
              if c.edgeId == label.edge.id then c { nodes = left }
              else if c.edgeId == label.tail then c { nodes = right }
              else c
          let replaced = Set.fromFoldable (firstChain.nodes <> lastChain.nodes)
          let retained = filter (\e -> not (Set.member e.from.node replaced && Set.member e.to.node replaced && (isDummy e.from.node || isDummy e.to.node || e.from.node == label.node || e.to.node == label.node))) acc.dummies.edges
          let halfHead = label.edge { to = { node: label.node, port: Nothing }, label = Nothing }
          let halfTail = label.edge { id = label.tail, from = { node: label.node, port: Nothing }, label = Nothing }
          acc
            { dummies = acc.dummies
                { layers = map (map swap) acc.dummies.layers
                , chains = chains
                , edges = retained <> chainEdges halfHead left <> chainEdges halfTail right
                }
            }
        _ -> acc
    _, _ -> acc

chainEdges :: Edge -> Array NodeId -> Array Edge
chainEdges edge nodes = zipWith make nodes (drop 1 nodes)
  where
  make a b = edge
    { id = if length nodes == 2 then edge.id else EdgeId (un EdgeId edge.id <> ":" <> un NodeId a <> "->" <> un NodeId b)
    , from = { node: a, port: if Just a == head nodes then edge.from.port else Nothing }
    , to = { node: b, port: if Just b == last nodes then edge.to.port else Nothing }
    }

-- LabelSideSelector.smart / smartForConsecutiveDummyNodeRun /
-- applyForDummyNodeRunWithSimpleLoops. Only CENTER labels are represented,
-- so smartForRegularNode's end-label decisions have no work in this model.
selectSides :: Array (Array NodeId) -> DummyResult -> LabelState -> LabelState
selectSides _ _ labels | null labels.dummies = labels
selectSides layers dummies labels = labels
  { dummies = labels.dummies <#> \d -> d { side = if Set.member d.node aboveNodes then Above else Below } }
  where
  labelNodes = Set.fromFoldable (labels.dummies <#> _.node)
  isLabel n = Set.member n labelNodes
  isVirtual n = isDummy n || isLabel n
  -- LongEdgeSplitter.setDummyProperties propagates the original endpoints
  -- through both halves, rather than treating the label as a real endpoint.
  endpoints = Map.fromFoldable $ concatMap
    ( \c ->
        let
          ends = case find (\d -> d.edge.id == c.edgeId || d.tail == c.edgeId) labels.dummies of
            Just d -> Just d.edge.from.node /\ Just d.edge.to.node
            Nothing -> head c.nodes /\ last c.nodes
        in
          c.nodes <#> \n -> n /\ ends
    )
    dummies.chains
  aboveNodes = foldl layerSides Set.empty layers
  layerSides selected layer = scan selected true (reverse layer)
  scan selected top rest = case uncons rest of
    Nothing -> selected
    Just { head: first, tail: remaining } | not (isVirtual first) -> scan selected false remaining
    _ -> do
      let run = takeWhile isVirtual rest
      let remaining = drop (length run) rest
      let bottom = null remaining
      let count = length (filter isLabel run)
      let
        selected' =
          if top && (not bottom || length run > 1) && count == 1 && fromMaybe false (isLabel <$> head run) then
            selectAbove (head run) selected
          else if bottom && (not top || length run > 1) && count == 1 && fromMaybe false (isLabel <$> last run) then selected
          else if length run == 2 then selectAbove (head run) selected
          else simpleRuns selected run
      scan selected' false remaining
  selectAbove Nothing selected = selected
  selectAbove (Just n) selected = if isLabel n then Set.insert n selected else selected
  simpleRuns selected rest = case uncons rest of
    Nothing -> selected
    Just { head: first } -> do
      let run = takeWhile (\n -> Map.lookup n endpoints == Map.lookup first endpoints) rest
      let selected' = if length run == 2 then selectAbove (head run) selected else selected
      simpleRuns selected' (drop (length run) rest)

-- LabelDummyInserter.createLabelDummy starts ports at floor(thickness/2).
-- LabelSideSelector.applyLabelSide moves ABOVE ports to extent-ceil(t/2).
-- Transforming their one-unit extent into DOWN yields these local offsets.
dummyPortOffset :: LabelDummy -> Number
dummyPortOffset d = case d.side of
  Above -> 0.0
  Below -> sizeW d.size * fineScale + edgeLabelSpacing

portOffsets :: LabelState -> Array Edge -> EdgePortOffsets -> EdgePortOffsets
portOffsets labels _ offsets | null labels.dummies = offsets
portOffsets labels edges offsets = foldl add offsets edges
  where
  byNode = Map.fromFoldable (labels.dummies <#> \d -> d.node /\ d)
  add acc edge = set edge.id North edge.to.node (set edge.id South edge.from.node acc)
  set eid side node acc = case Map.lookup node byNode of
    Nothing -> acc
    Just d -> Map.insert (eid /\ side) (dummyPortOffset d) acc

-- LabelDummyRemover.placeLabelsForVerticalLayout, transformed into DOWN.
-- A single label fills its longitudinal space; only its side offset remains.
placements :: LabelState -> Array NodePlacement -> Array EdgeLabelPlacement
placements labels _ | null labels.dummies = []
placements labels nodes = mapMaybe place labels.dummies
  where
  byNode = Map.fromFoldable (nodes <#> \n -> n.node /\ n)
  place d = Map.lookup d.node byNode <#> \n ->
    let
      sideOffset = case d.side of
        Above -> edgeThickness + edgeLabelSpacing
        Below -> 0.0
    in
      { edge: d.edge.id
      , position: GridPos ((gridX n.position * fineScale + sideOffset) /\ (gridY n.position * fineScale))
      , size: GridSize (sizeW d.size * fineScale /\ sizeH d.size * fineScale)
      }

-- LabelDummyRemover / LongEdgeJoiner.joinAt: join at the fixed ports through
-- the dummy, never at an edge midpoint. Final routing removes collinear bends.
restore :: LabelState -> Array EdgePath -> Array EdgePath
restore labels paths | null labels.dummies = paths
restore labels paths = mapMaybe (\p -> Map.lookup p.edge joined) paths
  where
  joined = foldl join (Map.fromFoldable (paths <#> \p -> p.edge /\ p)) labels.dummies
  join acc d = case Map.lookup d.edge.id acc, Map.lookup d.tail acc of
    Just firstPath, Just lastPath -> do
      let
        bridge = case last firstPath.segments, head lastPath.segments of
          Just a, Just b | a.end /= b.start -> [ { start: a.end, end: b.start, direction: V } ]
          _, _ -> []
      let segments = firstPath.segments <> bridge <> lastPath.segments
      Map.insert d.edge.id (firstPath { segments = segments }) (Map.delete d.tail acc)
    _, _ -> acc
