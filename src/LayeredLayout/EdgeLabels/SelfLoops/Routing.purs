-- Copyright (c) 2018, 2019, 2020 Kiel University and others.
-- SPDX-License-Identifier: EPL-2.0
-- This program is made available under the Eclipse Public License 2.0:
-- https://www.eclipse.org/legal/epl-2.0
--
-- ELK c831ba4613dfd6b0055851193956560351d2f907, loops/routing:
-- RoutingDirector, LabelPlacer, RoutingSlotAssigner, OrthogonalSelfLoopRouter;
-- p5edges/orthogonal/HyperEdgeCycleDetector and breakNonCriticalCycles.
-- Non-inline CENTER labels, STACKED ordering, zero-size ports, no label manager.
-- Coordinates remain normalized RIGHT throughout this module. DOWN labels are
-- transposed on import and aggregated horizontally, as SelfHyperLoopLabels does
-- when the graph's original direction is vertical. Label-label spacing is zero;
-- edge-edge/node-self-loop spacing is 10, edge-label spacing is 2 (Core.melk).
module LayeredLayout.EdgeLabels.SelfLoops.Routing (Geometry, compute) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl, sum)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import LayeredLayout.EdgeLabels.SelfLoops.Model (Holder, HyperLoop, LoopPort, Margin, Point, Size, includePoint, left, loopSides, right, sides, twoSides, zeroMargin)
import LayeredLayout.Graph (EdgeId, Side(..))
import LayeredLayout.JavaRandom (Random)
import LayeredLayout.EdgeRouting.HyperEdgeCycleDetector (Dep, DepKind(..))
import LayeredLayout.EdgeRouting.HyperEdgeCycleDetector as CycleDetector

type Label = { edge :: EdgeId, size :: Size }
data Alignment = Center | AlignLeft | AlignRight | Top

type LabelBox =
  { labels :: Array Label, size :: Size, position :: Point, side :: Side, alignment :: Alignment }

type DirectedLoop =
  { loop :: HyperLoop
  , first :: LoopPort
  , last :: LoopPort
  , occupied :: Array Side
  , label :: Maybe LabelBox
  , slots :: Map Side Int
  }

type Geometry =
  { paths :: Array { edge :: EdgeId, points :: Array Point }
  , labels :: Array { edge :: EdgeId, position :: Point, size :: Size }
  , margin :: Margin
  }

compute :: Random -> Holder -> Geometry /\ Random
compute random holder = geometry /\ nextRandom
  where
  directed = A.mapMaybe (direct holder) holder.loops
  dependencies = createDependencies directed
  acyclic /\ nextRandom = breakCycles random directed dependencies
  raw = rawSlots directed acyclic
  slotted = compactSlots holder raw (directed <#> \g -> g { slots = M.fromFoldable (g.occupied <#> \s -> s /\ value g.loop.id raw) })
  innerMargin = foldl (includePoint holder.size) zeroMargin (holder.ports <#> _.position)
  positions = slotPositions holder.size innerMargin slotted
  paths = A.concatMap (routeLoop positions) slotted
  labels = A.concatMap (placeLabels positions) slotted
  points = A.concatMap _.points paths <> A.concatMap (\l -> [ l.position, { x: l.position.x + l.size.width, y: l.position.y + l.size.height } ]) labels
  geometry = { paths, labels, margin: foldl (includePoint holder.size) innerMargin points }

value :: forall k. Ord k => k -> Map k Int -> Int
value key = fromMaybe 0 <<< M.lookup key

-- RoutingDirector: all ports are already indexed clockwise. A one-side loop
-- never wraps; opposing sides minimize crossed regular-port penalties; four
-- sides break at the greatest penalty (including the WEST/NORTH seam).
direct :: Holder -> HyperLoop -> Maybe DirectedLoop
direct holder g = do
  firstPort <- A.head g.ports
  lastPort <- A.last g.ports
  let
    lowest side = fromMaybe firstPort (A.find (\p -> p.side == side) g.ports)
    highest side = fromMaybe lastPort (A.find (\p -> p.side == side) (A.reverse g.ports))
    endpoints a b = lowest a /\ highest b
    penalty a b = sum (holder.ports <#> \p -> if withinPortInterval false a.index b.index p.index then (if p.connected then 3 else 1) else 0)
    first /\ last = case loopSides g of
      [ side ] -> endpoints side side
      [ a, b ] | right (right a) == b ->
        let
          l1 /\ r1 = endpoints a b
          l2 /\ r2 = endpoints b a
        in
          if penalty l1 r1 <= penalty l2 r2 then l1 /\ r1 else l2 /\ r2
      [ a, b ] -> case twoSides a b of
        [ l, r ] -> endpoints l r
        _ -> firstPort /\ lastPort
      ss | A.length ss == 3 -> case A.find (\side -> not (A.elem side ss)) sides of
        Just missing -> endpoints (right missing) (left missing)
        Nothing -> firstPort /\ lastPort
      _ ->
        let
          pairs = [ lastPort /\ firstPort ] <> A.zip g.ports (A.drop 1 g.ports)
          worst = foldl (\(a /\ b) (c /\ d) -> if penalty c d > penalty a b then c /\ d else a /\ b) (lastPort /\ firstPort) pairs
          l /\ r = worst
        in
          r /\ l
    -- Upstream defect, reproduced with fixed ports and an unused-port gap:
    -- RoutingDirector.determineFourSideLoopRoutes(236–263) can choose two split
    -- endpoints on one side. computeOccupiedPortSides(103–112) then terminates
    -- immediately, leaving the other side-slot arrays empty; getBaseVector
    -- (OrthogonalSelfLoopRouter421–433) produces NaNs in elkjs. Four-sided
    -- occupancy must remain four-sided regardless of that split. This is an
    -- intentional source bug correction, not literal crash/NaN parity.
    occupied = if A.length (loopSides g) == 4 then sides else sideWalk right first.side last.side
    directed = { loop: g, first, last, occupied, label: Nothing, slots: M.empty }
  pure (directed { label = labelBox holder.size directed })

withinPortInterval :: Boolean -> Int -> Int -> Int -> Boolean
withinPortInterval inclusive a b p
  | a <= b = if inclusive then p >= a && p <= b else p > a && p < b
  | otherwise = if inclusive then p >= a || p <= b else p > a || p < b

sideWalk :: (Side -> Side) -> Side -> Side -> Array Side
sideWalk advance first last = if first == last then [ first ] else [ first ] <> sideWalk advance (advance first) last

-- LabelPlacer.assignSideAndAlignment/computeCoordinates, STACKED branch.
-- The original four-side code reads leftmost twice; retain that source behavior.
labelBox :: Size -> DirectedLoop -> Maybe LabelBox
labelBox node g = if A.null labels then Nothing else Just { labels, size, position, side, alignment }
  where
  labels = A.mapMaybe (\e -> e.size <#> \measuredSize -> { edge: e.edge, size: measuredSize }) g.loop.edges
  size = foldl (\s l -> { width: s.width + l.size.width, height: max s.height l.size.height }) { width: 0.0, height: 0.0 } labels
  fs = g.first.side
  ls = g.last.side
  topmost = if g.last.position.y < g.first.position.y then g.last else g.first
  side /\ alignment /\ reference = case loopSides g.loop of
    [ s ] | s == East || s == West -> s /\ Top /\ topmost
    [ s ] -> s /\ Center /\ g.first
    [ a, b ] | right (right a) /= b ->
      if fs == North then North /\ AlignLeft /\ g.first
      else if ls == North then North /\ AlignRight /\ g.last
      else if fs == South then South /\ AlignRight /\ g.first
      else South /\ AlignLeft /\ g.last
    ss | A.length ss == 4 -> (if fs == North then South else North) /\ Center /\ g.first
    _ ->
      if not (A.elem North g.occupied) then South /\ Center /\ g.first
      else if not (A.elem South g.occupied) then North /\ Center /\ g.first
      else if not (A.elem West g.occupied) then North /\ AlignLeft /\ g.first
      else North /\ AlignRight /\ g.last
  position = case alignment of
    Center -> { x: (node.width - size.width) / 2.0, y: 0.0 }
    AlignLeft -> { x: reference.position.x, y: 0.0 }
    AlignRight -> { x: reference.position.x - size.width, y: 0.0 }
    Top -> { x: 0.0, y: reference.position.y }

labelsOverlap :: DirectedLoop -> DirectedLoop -> Boolean
labelsOverlap a b = case a.label, b.label of
  Just x, Just y -> x.side == y.side && (x.side == North || x.side == South)
    && x.position.x <= y.position.x + y.size.width
    && x.position.x + x.size.width >= y.position.x
  _, _ -> false

active :: DirectedLoop -> LoopPort -> Boolean
active g p = withinPortInterval true g.first.index g.last.index p.index

createDependencies :: Array DirectedLoop -> Array Dep
createDependencies loops = A.concat (A.mapWithIndex pair loops)
  where
  count upper lower = A.length (A.filter (active lower) upper.loop.ports)
  pair i a = A.concatMap
    ( \b ->
        let
          ab = count a b
          ba = count b a
          dep x y weight = { src: x.loop.id, tgt: y.loop.id, weight, kind: Regular }
        in
          if ab < ba then [ dep a b (ba - ab) ]
          else if ba < ab then [ dep b a (ab - ba) ]
          else if ab /= 0 || labelsOverlap a b then [ dep a b 0, dep b a 0 ]
          else []
    )
    (A.drop (i + 1) loops)

-- Self-loop dependencies are regular; the channel router also uses critical ones.
breakCycles :: Random -> Array DirectedLoop -> Array Dep -> Array Dep /\ Random
breakCycles random loops dependencies = A.mapMaybe orient dependencies /\ final.random
  where
  final = CycleDetector.detect false random (loops <#> _.loop >>> _.id) dependencies
  orient d
    | value d.src final.marks <= value d.tgt final.marks = Just d
    | d.weight == 0 = Nothing
    | otherwise = Just (d { src = d.tgt, tgt = d.src })

rawSlots :: Array DirectedLoop -> Array Dep -> Map Int Int
rawSlots loops deps = go sinks initial M.empty
  where
  ids = loops <#> _.loop >>> _.id
  initial = M.fromFoldable (ids <#> \id -> id /\ A.length (A.filter (\d -> d.src == id) deps))
  sinks = A.filter (\id -> value id initial == 0) ids
  go queue remaining slots = case A.uncons queue of
    Nothing -> slots
    Just { head: id, tail } ->
      let
        nextSlot = value id slots + 1
        step acc d =
          let
            n = value d.src acc.remaining - 1
          in
            { queue: if n == 0 then A.snoc acc.queue d.src else acc.queue
            , remaining: M.insert d.src n acc.remaining
            , slots: M.insert d.src (max nextSlot (value d.src acc.slots)) acc.slots
            }
        updated = foldl step { queue: tail, remaining, slots } (A.filter (\d -> d.tgt == id) deps)
      in
        go updated.queue updated.remaining updated.slots

compactSlots :: Holder -> Map Int Int -> Array DirectedLoop -> Array DirectedLoop
compactSlots holder raw loops = foldl onSide loops sides
  where
  onSide current side = current <#> \g -> case M.lookup g.loop.id assigned of
    Just slot -> g { slots = M.insert side slot g.slots }
    Nothing -> g
    where
    sidePorts = A.filter (\p -> p.side == side) holder.ports
    ordered = A.sortBy (\a b -> compare (value a.loop.id raw) (value b.loop.id raw)) (A.filter (\g -> A.elem side g.occupied) current)
    assigned =
      if A.null sidePorts then M.fromFoldable (A.mapWithIndex (\i g -> g.loop.id /\ i) ordered)
      else (foldl place { ports: M.empty, slots: M.empty, placed: [] } ordered).slots
    place acc g =
      let
        spanned = A.filter (active g) sidePorts
        lowest = foldl (\n p -> max n (value p.index acc.ports)) 0 spanned
        conflicts = A.filter (labelsOverlap g) acc.placed <#> \other -> value other.loop.id acc.slots
        available n = if A.elem n conflicts then available (n + 1) else n
        slot = available lowest
      in
        { ports: foldl (\m p -> M.insert p.index (slot + 1) m) acc.ports spanned
        , slots: M.insert g.loop.id slot acc.slots
        , placed: A.snoc acc.placed g
        }

type Positions = Map (Side /\ Int) Number

slotPositions :: Size -> Margin -> Array DirectedLoop -> Positions
slotPositions size margin loops = foldl perSide M.empty sides
  where
  perSide acc side = (foldl place { positions: acc, position: baseline side } indices).positions
    where
    onSide = A.filter (\g -> A.elem side g.occupied) loops
    count = foldl (\n g -> max n (value side g.slots + 1)) 0 onSide
    indices = if count == 0 then [] else A.range 0 (count - 1)
    place state slot =
      let
        height =
          if side == North || side == South then foldl
            ( \h g -> case g.label of
                Just label | label.side == side && value side g.slots == slot -> max h label.size.height
                _ -> h
            )
            0.0
            onSide
          else 0.0
        increment = 10.0 + if height > 0.0 then height + 2.0 else 0.0
        factor = if side == North || side == West then -1.0 else 1.0
      in
        { positions: M.insert (side /\ slot) state.position state.positions, position: state.position + factor * increment }
  baseline = case _ of
    North -> -margin.top - 10.0
    East -> size.width + margin.right + 10.0
    South -> size.height + margin.bottom + 10.0
    West -> -margin.left - 10.0

slotPosition :: Positions -> DirectedLoop -> Side -> Number
slotPosition positions g side = fromMaybe 0.0 (M.lookup (side /\ value side g.slots) positions)

routeLoop :: Positions -> DirectedLoop -> Array { edge :: EdgeId, points :: Array Point }
routeLoop positions g = A.mapMaybe routeEdge g.loop.edges
  where
  base side =
    let
      pos = slotPosition positions g side
    in
      if side == North || side == South then { x: 0.0, y: pos } else { x: pos, y: 0.0 }
  outer port =
    let
      p = base port.side
    in
      if port.side == North || port.side == South then p { x = port.position.x } else p { y = port.position.y }
  corner a b =
    let
      pa = base a
      pb = base b
    in
      { x: pa.x + pb.x, y: pa.y + pb.y }
  routeEdge edge = do
    source <- A.find (\p -> p.key == edge.source) g.loop.ports
    target <- A.find (\p -> p.key == edge.target) g.loop.ports
    let
      clockwise =
        if source.side == target.side then source.index < target.index
        else if right source.side == target.side then true
        else if left source.side == target.side then false
        else A.elem (right source.side) g.occupied
      walk = sideWalk (if clockwise then right else left) source.side target.side
      corners = A.zipWith corner walk (A.drop 1 walk)
    pure { edge: edge.edge, points: [ source.position, outer source ] <> corners <> [ outer target, target.position ] }

-- OrthogonalSelfLoopRouter.placeLabels followed by
-- SelfHyperLoopLabels.applyPlacementVerticalForVerticalLayout (before transpose).
placeLabels :: Positions -> DirectedLoop -> Array { edge :: EdgeId, position :: Point, size :: Size }
placeLabels positions g = case g.label of
  Nothing -> []
  Just box -> (foldl place { x: origin.x, labels: [] } box.labels).labels
    where
    baseline = slotPosition positions g box.side
    origin = case box.side of
      North -> box.position { y = baseline - 2.0 - box.size.height }
      South -> box.position { y = baseline + 2.0 }
      West -> box.position { x = baseline - 2.0 - box.size.width }
      East -> box.position { x = baseline + 2.0 }
    place acc label =
      let
        y = origin.y + if box.side == North then box.size.height - label.size.height else 0.0
      in
        { x: acc.x + label.size.width
        , labels: A.snoc acc.labels { edge: label.edge, position: { x: acc.x, y }, size: label.size }
        }
