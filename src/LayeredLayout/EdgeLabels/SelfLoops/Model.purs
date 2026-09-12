-- Copyright (c) 2018, 2019 Kiel University and others.
-- SPDX-License-Identifier: EPL-2.0
-- This program is made available under the Eclipse Public License 2.0:
-- https://www.eclipse.org/legal/epl-2.0
--
-- Functional translation of ELK c831ba4613dfd6b0055851193956560351d2f907:
-- SelfLoopHolder.initialize/initializeHyperLoop, SelfLoopPreProcessor.hidePorts,
-- PortSideAssigner.assignToNorthSide, PortRestorer (STACKED), and GraphTransformer.transpose.
-- All internal geometry is fine-grid, normalized RIGHT. Only the public adapter
-- transposes back to DOWN. Named ports are FIXED_POS, automatic ports FREE on
-- nodes without named ports. No merging of automatically generated ports.
-- The existing oracle puts selfLoopDistribution on the root. That option targets
-- nodes and is not inherited, so the effective source setting is default NORTH.
-- Keeping NORTH here preserves the existing observable contract; no public
-- per-node distribution option is introduced by this translation.
module LayeredLayout.EdgeLabels.SelfLoops.Model where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import LayeredLayout.Graph (Edge, EdgeId, Node, PortId, Side(..))
import LayeredLayout.Grid (GridSize, sizeH, sizeW)

type Point = { x :: Number, y :: Number }
type Size = { width :: Number, height :: Number }
type Margin = { left :: Number, right :: Number, top :: Number, bottom :: Number }

data PortKey = Named PortId | Automatic EdgeId Boolean

derive instance Eq PortKey
derive instance Ord PortKey

type LoopPort =
  { key :: PortKey
  , side :: Side
  , position :: Point
  , hidden :: Boolean
  , connected :: Boolean
  , flow :: Int
  , index :: Int
  }

type LoopEdge = { edge :: EdgeId, source :: PortKey, target :: PortKey, size :: Maybe Size }
type HyperLoop = { id :: Int, edges :: Array LoopEdge, ports :: Array LoopPort }
type Holder = { size :: Size, ports :: Array LoopPort, loops :: Array HyperLoop }

sides :: Array Side
sides = [ North, East, South, West ]

sideIndex :: Side -> Int
sideIndex = case _ of
  North -> 0
  East -> 1
  South -> 2
  West -> 3

right :: Side -> Side
right = case _ of
  North -> East
  East -> South
  South -> West
  West -> North

left :: Side -> Side
left = right <<< right <<< right

transposeSide :: Side -> Side
transposeSide = case _ of
  North -> West
  East -> South
  South -> East
  West -> North

normalizedSize :: GridSize -> Size
normalizedSize size = { width: sizeH size * 4.0, height: sizeW size * 4.0 }

zeroMargin :: Margin
zeroMargin = { left: 0.0, right: 0.0, top: 0.0, bottom: 0.0 }

includePoint :: Size -> Margin -> Point -> Margin
includePoint size margin p =
  { left: max margin.left (-p.x)
  , right: max margin.right (p.x - size.width)
  , top: max margin.top (-p.y)
  , bottom: max margin.bottom (p.y - size.height)
  }

portKey :: Edge -> Boolean -> PortKey
portKey edge source = case (if source then edge.from else edge.to).port of
  Just id -> Named id
  Nothing -> Automatic edge.id source

-- SelfLoopHolder's BFS visits outgoing edges before incoming edges and retains
-- first encounter order. Shared named ports therefore make one hyperloop, not
-- independent concentric edges; automatic endpoints each have their own key.
makeHolder :: Map EdgeId GridSize -> Node -> Array Edge -> Holder
makeHolder sizes node edges = { size, ports: positioned, loops: restoredLoops }
  where
  size = normalizedSize node.size
  free = A.null node.ports
  incident = A.filter (\e -> e.from.node == node.id || e.to.node == node.id) edges
  rawLoopEdges = A.filter (\e -> e.from.node == node.id && e.to.node == node.id) incident <#> \e ->
    { edge: e.id, source: portKey e true, target: portKey e false, size: normalizedSize <$> M.lookup e.id sizes }
  named = node.ports <#> \p ->
    let
      offset = Int.toNumber p.offset * 4.0
      side = transposeSide p.side
      position = anchor size side offset
    in
      { key: Named p.id, side, position, hidden: false, connected: false, flow: 0, index: 0 }
  automatic = A.concatMap
    ( \e -> (if e.from.node == node.id && e.from.port == Nothing then [ auto e true ] else []) <>
        (if e.to.node == node.id && e.to.port == Nothing then [ auto e false ] else [])
    )
    incident
  auto e source =
    { key: Automatic e.id source
    , side: if source then East else West
    , position: anchor size (if source then East else West) 0.0
    , hidden: free && e.from.node == e.to.node
    , connected: e.from.node /= e.to.node
    , flow: if source then -1 else 1
    , index: 0
    }
  allPorts = (named <> automatic) <#> \p -> p
    { flow = foldl (\n e -> n + (if e.target == p.key then 1 else 0) - (if e.source == p.key then 1 else 0)) 0 rawLoopEdges
    , connected = A.any
        ( \e -> e.from.node /= e.to.node &&
            ((e.from.node == node.id && portKey e true == p.key) || (e.to.node == node.id && portKey e false == p.key))
        )
        incident
    }
  -- LNode.getOutgoingEdges iterates source ports, then each port's edge list.
  loopEdges = A.concatMap (\p -> A.filter (\e -> e.source == p.key) rawLoopEdges) allPorts
  groups = collectGroups allPorts loopEdges
  assigned = if free then assignSides groups else groups
  assignedPorts = M.fromFoldable (A.concatMap (\g -> g.ports <#> \p -> p.key /\ p) assigned)
  portsWithSides = allPorts <#> \p -> fromMaybe p (M.lookup p.key assignedPorts)
  ordered = if free then restorePortOrder portsWithSides assigned else A.sortBy comparePort portsWithSides
  positioned = A.mapWithIndex (\i p -> p { index = i, position = if free then freePosition ordered p else p.position }) ordered
  byKey = M.fromFoldable (positioned <#> \p -> p.key /\ p)
  restoredLoops = assigned <#> \g -> g
    { ports = A.sortBy (\a b -> compare a.index b.index)
        (g.ports <#> \p -> fromMaybe p (M.lookup p.key byKey))
    }
  freePosition orderedPorts p =
    let
      onSide = A.filter (\q -> q.side == p.side) orderedPorts
      index = fromMaybe 0 (A.findIndex (\q -> q.key == p.key) onSide)
      extent = if p.side == North || p.side == South then size.width else size.height
      distance = extent * Int.toNumber (index + 1) / Int.toNumber (A.length onSide + 1)
      offset = if p.side == South || p.side == West then extent - distance else distance
    in
      anchor size p.side offset

anchor :: Size -> Side -> Number -> Point
anchor size side offset = case side of
  North -> { x: offset, y: 0.0 }
  East -> { x: size.width, y: offset }
  South -> { x: offset, y: size.height }
  West -> { x: 0.0, y: offset }

comparePort :: LoopPort -> LoopPort -> Ordering
comparePort a b = compare (sideIndex a.side) (sideIndex b.side) <> compare (clockwiseOffset a) (clockwiseOffset b)
  where
  clockwiseOffset p = case p.side of
    North -> p.position.x
    East -> p.position.y
    South -> -p.position.x
    West -> -p.position.y

collectGroups :: Array LoopPort -> Array LoopEdge -> Array HyperLoop
collectGroups ports = go []
  where
  go acc remaining = case A.uncons remaining of
    Nothing -> acc
    Just { head: first } ->
      let
        reached = bfs [] [] [ first.source ] remaining
        keys = A.nub (A.concatMap (\e -> [ e.source, e.target ]) reached)
        groupPorts = A.mapMaybe (\key -> A.find (\p -> p.key == key) ports) keys
        rest = A.filter (\e -> not (A.any (\r -> r.edge == e.edge) reached)) remaining
      in
        go (A.snoc acc { id: A.length acc, edges: reached, ports: groupPorts }) rest
  bfs visited found queue remaining = case A.uncons queue of
    Nothing -> found
    Just { head: key, tail } | A.elem key visited -> bfs visited found tail remaining
    Just { head: key, tail } ->
      let
        adjacent = A.filter (\e -> e.source == key) remaining <> A.filter (\e -> e.target == key) remaining
        fresh = A.filter (\e -> not (A.any (\r -> r.edge == e.edge) found)) (A.nubByEq (\a b -> a.edge == b.edge) adjacent)
        next = adjacent <#> \e -> if e.source == key then e.target else e.source
      in
        bfs (A.snoc visited key) (found <> fresh) (tail <> next) remaining

assignSides :: Array HyperLoop -> Array HyperLoop
assignSides = map (\g -> g { ports = g.ports <#> \p -> if p.hidden then p { side = North } else p })

loopSides :: HyperLoop -> Array Side
loopSides g = A.filter (\side -> A.any (\p -> p.side == side) g.ports) sides

twoSides :: Side -> Side -> Array Side
twoSides North West = [ West, North ]
twoSides a b = [ a, b ]

-- PortRestorer.processOneSideLoops(STACKED)/addToTargetArea. With automatic
-- ports and NORTH, each component has exactly one source and target; all ports
-- are hidden. Prepending the source half and appending the target half makes
-- the first loop innermost. FIXED_POS bypasses restoration altogether.
restorePortOrder :: Array LoopPort -> Array HyperLoop -> Array LoopPort
restorePortOrder ports groups = middle <> A.concatMap regular sides
  where
  middle = foldl stack [] groups
  stack placed g =
    let
      sorted = A.sortBy (\a b -> compare a.flow b.flow) g.ports
      split = A.length sorted `div` 2
    in
      A.reverse (A.take split sorted) <> placed <> A.reverse (A.drop split sorted)
  regular side = A.filter (\p -> p.side == side && not p.hidden) ports
