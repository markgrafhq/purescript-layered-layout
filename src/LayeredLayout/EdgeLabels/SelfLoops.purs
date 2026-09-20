-- Copyright (c) 2018, 2019, 2020 Kiel University and others.
-- SPDX-License-Identifier: EPL-2.0
-- This program is made available under the Eclipse Public License 2.0:
-- https://www.eclipse.org/legal/epl-2.0
--
-- Source: Eclipse ELK c831ba4613dfd6b0055851193956560351d2f907.
-- SelfLoopPreProcessor, SelfLoopPortRestorer, SelfLoopRouter and
-- SelfLoopPostProcessor are fused into an immutable node-local calculation.
-- Their ordering/routing algorithms live in Model and Routing below this
-- namespace, without introducing Java graph mutation into the layout pipeline.
--
-- Frame boundary: graph nodes and measured labels arrive in coarse DOWN units.
-- Model transposes x/y and multiplies by four to obtain ELK's internal RIGHT
-- frame. Routing works only in that frame. This adapter transposes points,
-- boxes, and margins back; only node reservations are divided by four.
--
-- Node reservations encode ELK LMargin in this engine's rectangular node-size
-- model. Loop-label hitboxes stay independent obstacles: endpoint owner
-- filtering must not permit another route through a painted loop label.
-- They duplicate exact label boxes, never enlarge or relocate loop geometry.
module LayeredLayout.EdgeLabels.SelfLoops
  ( LoopState
  , empty
  , restrict
  , afterRouting
  , prepare
  , margins
  , portOffsets
  , routingObstacles
  , route
  , placements
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Set as S
import Data.Tuple.Nested ((/\))
import LayeredLayout.EdgeLabels (EdgeLabelSpec, LabelPlacement(..))
import LayeredLayout.EdgeLabels.SelfLoops.Model (Holder, Margin, Point, PortKey(..), Size, includePoint, makeHolder, transposeSide, zeroMargin)
import LayeredLayout.EdgeLabels.SelfLoops.Routing (Geometry, compute)
import LayeredLayout.Graph (Edge, EdgeId(..), Graph, NodeId(..), Side(..))
import LayeredLayout.Grid (GridPos(..), GridSize(..), gridX, gridY, sizeH, sizeW)
import LayeredLayout.JavaRandom (Random)
import LayeredLayout.PortDistribution (EdgePortOffsets)
import LayeredLayout.Result (Direction(..), EdgeLabelPlacement, EdgePath, NodePlacement)

type Owner =
  { node :: NodeId
  , size :: GridSize
  , margin :: Margin
  , geometry :: Geometry
  , regularPorts :: Map Side (Array Number)
  }

newtype LoopState = LoopState { owners :: Array Owner, random :: Maybe Random }

instance Semigroup LoopState where
  append (LoopState left) (LoopState right) = LoopState
    { owners: left.owners <> right.owners
    , random: case right.random of
        Just random -> Just random
        Nothing -> left.random
    }

-- Component caches retain owner-local loop geometry without exposing it to the
-- pipeline adapter. Coordinates are supplied separately at routing time.
restrict :: S.Set NodeId -> LoopState -> LoopState
restrict nodes (LoopState state) = LoopState (state { owners = A.filter (\o -> S.member o.node nodes) state.owners })

empty :: LoopState
empty = LoopState { owners: [], random: Nothing }

-- ELK's component graphs share the root generator. Preserve the state consumed
-- by loop routing so the next component starts with the same random sequence.
afterRouting :: Random -> LoopState -> Random
afterRouting fallback (LoopState state) = fromMaybe fallback state.random

margins :: LoopState -> Map NodeId Margin
margins (LoopState { owners }) = M.fromFoldable (owners <#> \owner -> owner.node /\ owner.margin)

-- Prepare after crossing minimization, before coordinate assignment. Input edges
-- retain original IDs and are in acyclic layout direction. Self loops themselves
-- must be excluded from the regular dummy/routing pipeline, even when unlabelled.
-- The supplied JavaRandom is the crossing minimizer's final state; it is threaded
-- across owners exactly as SelfLoopRouter processes the graph's node sequence.
prepare :: Random -> Map EdgeId EdgeLabelSpec -> Graph -> LoopState
prepare random specs graph =
  let
    state = foldl add { owners: [], random } graph.nodes
  in
    LoopState { owners: state.owners, random: Just state.random }
  where
  -- Self-loop geometry owns CENTER label sectors. Explicit TAIL labels are
  -- finalized from their authored route instead.
  sizes = M.mapMaybeWithKey
    ( \_ spec -> case spec.placement of
        Center -> Just spec.size
        Tail _ -> Nothing
    )
    specs
  incident = foldl
    ( \m e ->
        let
          m' = M.insertWith (<>) e.from.node [ e ] m
        in
          if e.from.node == e.to.node then m' else M.insertWith (<>) e.to.node [ e ] m'
    )
    M.empty
    graph.edges
  loopNodes = S.fromFoldable (A.filter (\e -> e.from.node == e.to.node) graph.edges <#> _.from >>> _.node)
  add state node | not (S.member node.id loopNodes) = state
  add state node =
    let
      holder = makeHolder sizes node (fromMaybe [] (M.lookup node.id incident))
      rawGeometry /\ random' = compute state.random holder
      geometry = repairGeometry holder rawGeometry
      m = geometry.margin
      margin = { left: m.top, right: m.bottom, top: m.left, bottom: m.right }
      regularPorts = foldl collect M.empty holder.ports <#> A.sort
      collect ports p = case p.key of
        Automatic _ _ | p.connected ->
          let
            side = transposeSide p.side
            offset = if p.side == North || p.side == South then p.position.x else p.position.y
          in
            M.insertWith (<>) side [ offset ] ports
        _ -> ports
      owner = { node: node.id, size: node.size, margin, geometry, regularPorts }
    in
      { owners: A.snoc state.owners owner, random: random' }

-- PortRestorer's loop sectors also move ordinary automatic ports on the same
-- side. Preserve their crossing-minimized order from the supplied offset map,
-- but distribute them into the restored side's regular-port slots. The caller
-- translates those physical offsets into any reserved assignment frame.
portOffsets :: LoopState -> Array Edge -> EdgePortOffsets -> EdgePortOffsets
portOffsets (LoopState { owners }) edges offsets = foldl perOwner offsets owners
  where
  perOwner acc o = foldl (onSide o) acc [ North, South, East, West ]
  onSide o acc side = foldl assign acc (A.mapWithIndex (/\) sorted)
    where
    isSource = side == South
    endpoint e = if isSource then e.from else e.to
    -- The regular layered pipeline uses DOWN South exits and North entries.
    -- Explicit endpoints are routed from their named port records instead.
    relevant =
      if side == North || side == South then A.filter
        ( \e ->
            e.from.node /= e.to.node && (endpoint e).node == o.node && (endpoint e).port == Nothing
        )
        edges
      else []
    sorted = A.sortBy (\a b -> compare (M.lookup (a.id /\ side) offsets) (M.lookup (b.id /\ side) offsets)) relevant
    slots = fromMaybe [] (M.lookup side o.regularPorts)
    assign m (i /\ e) = case A.index slots i of
      Just offset -> M.insert (e.id /\ side) offset m
      Nothing -> M.update (\offset -> Just offset) (e.id /\ side) m

routingObstacles :: LoopState -> Array NodePlacement -> Array NodePlacement -> Array NodePlacement
routingObstacles (LoopState { owners: [] }) reserved _ = reserved
routingObstacles (LoopState { owners }) reserved restored = reserved <> (foldl addOwner initial owners).extra
  where
  initial = { used: S.fromFoldable (reserved <#> _.node), extra: [] }
  byNode = M.fromFoldable (restored <#> \n -> n.node /\ n)
  addOwner acc owner = case M.lookup owner.node byNode of
    Nothing -> acc
    Just node -> foldl (add node) acc owner.geometry.labels
  fresh used text = if S.member (NodeId text) used then fresh used (text <> "'") else NodeId text
  add owner acc localLabel =
    let
      label = placeLabel owner localLabel
      node = fresh acc.used ("$loop-label:" <> un EdgeId label.edge)
      placement =
        { node
        , position: GridPos (gridX label.position / 4.0 /\ gridY label.position / 4.0)
        , size: GridSize (sizeW label.size / 4.0 /\ sizeH label.size / 4.0)
        , layer: owner.layer
        , order: owner.order
        }
    in
      { used: S.insert node acc.used, extra: A.snoc acc.extra placement }

-- Route before compaction so loop bends participate in the same separation
-- constraints as ordinary edges. Labels are projected after owners move.
route :: LoopState -> Array NodePlacement -> Array EdgePath
route (LoopState { owners: [] }) _ = []
route (LoopState { owners }) nodes = A.concatMap add owners
  where
  byNode = M.fromFoldable (nodes <#> \n -> n.node /\ n)
  add owner = case M.lookup owner.node byNode of
    Nothing -> []
    Just node ->
      let
        point p = GridPos ((gridX node.position * 4.0 + p.y) /\ (gridY node.position * 4.0 + p.x))
      in
        owner.geometry.paths <#> \path ->
          let
            points = foldl (\ps p -> if A.last ps == Just p then ps else A.snoc ps p) [] (path.points <#> point)
            segments = A.zipWith (\start end -> { start, end, direction: if gridX start == gridX end then V else H }) points (A.drop 1 points)
            bends = A.dropEnd 1 (A.drop 1 points)
          in
            { edge: path.edge, segments, bends, bendType: [], jumps: [], reversed: false }

placements :: LoopState -> Array NodePlacement -> Array EdgeLabelPlacement
placements (LoopState { owners: [] }) _ = []
placements (LoopState { owners }) nodes = A.concatMap place owners
  where
  byNode = M.fromFoldable (nodes <#> \node -> node.node /\ node)
  place owner = case M.lookup owner.node byNode of
    Nothing -> []
    Just node -> owner.geometry.labels <#> placeLabel node

-- | Four-sided fixed-port hyperloops can contain labels whose shared ELK label
-- | sector is on a different side from their own route. Repair only labels
-- | that do not run beside a positive-length projection of their own route.

repairGeometry :: Holder -> Geometry -> Geometry
repairGeometry holder geometry =
  let
    labels = repairLabels holder.size geometry.paths geometry.labels
    points =
      (holder.ports <#> _.position)
        <> A.concatMap _.points geometry.paths
        <> A.concatMap (\label -> [ label.position, { x: label.position.x + label.size.width, y: label.position.y + label.size.height } ]) labels
  in
    geometry { labels = labels, margin = foldl (includePoint holder.size) zeroMargin points }

repairLabels
  :: Size
  -> Array { edge :: EdgeId, points :: Array Point }
  -> Array { edge :: EdgeId, position :: Point, size :: Size }
  -> Array { edge :: EdgeId, position :: Point, size :: Size }
repairLabels node paths labels = _.placed (foldl repair { placed: [] } labels)
  where
  pathByEdge = M.fromFoldable (paths <#> \path -> path.edge /\ path.points)
  fixed = A.filter
    ( \label -> case M.lookup label.edge pathByEdge of
        Just points -> associated label points
        Nothing -> true
    )
    labels
  repair state label =
    let
      repaired = case M.lookup label.edge pathByEdge of
        Just points | not (associated label points) ->
          fromMaybe label (A.find (clear label state.placed) (candidates label points))
        _ -> label
    in
      state { placed = A.snoc state.placed repaired }
  clear original placed label =
    clearNode node label
      && not (A.any (overlaps label) (placed <> A.filter (\fixedLabel -> fixedLabel.edge /= original.edge) fixed))
      && not (A.any (\path -> path.edge /= original.edge && crosses label path.points) paths)

labelGap :: Number
labelGap = 2.0

associated :: forall r. { position :: Point, size :: Size | r } -> Array Point -> Boolean
associated label points = A.any near (A.zip points (A.drop 1 points))
  where
  near (start /\ end)
    | start.x == end.x =
        rangeDistance start.x label.position.x (label.position.x + label.size.width) <= labelGap
          && positiveOverlap (min start.y end.y) (max start.y end.y) label.position.y (label.position.y + label.size.height)
    | otherwise =
        rangeDistance start.y label.position.y (label.position.y + label.size.height) <= labelGap
          && positiveOverlap (min start.x end.x) (max start.x end.x) label.position.x (label.position.x + label.size.width)

candidates
  :: { edge :: EdgeId, position :: Point, size :: Size }
  -> Array Point
  -> Array { edge :: EdgeId, position :: Point, size :: Size }
candidates label points = A.concatMap around (A.zip points (A.drop 1 points))
  where
  around (start /\ end)
    | start.x == end.x =
        let
          low = min start.y end.y
          high = max start.y end.y
          y = low + (high - low - label.size.height) / 2.0
        in
          if high - low < label.size.height then []
          else
            [ label { position = { x: start.x - labelGap - label.size.width, y } }
            , label { position = { x: start.x + labelGap, y } }
            ]
    | otherwise =
        let
          low = min start.x end.x
          high = max start.x end.x
          x = low + (high - low - label.size.width) / 2.0
        in
          if high - low < label.size.width then []
          else
            [ label { position = { x, y: start.y - labelGap - label.size.height } }
            , label { position = { x, y: start.y + labelGap } }
            ]

crosses :: forall r. { position :: Point, size :: Size | r } -> Array Point -> Boolean
crosses label points = A.any crossesSegment (A.zip points (A.drop 1 points))
  where
  crossesSegment (start /\ end)
    | start.x == end.x =
        start.x > label.position.x
          && start.x < label.position.x + label.size.width
          && positiveOverlap (min start.y end.y) (max start.y end.y) label.position.y (label.position.y + label.size.height)
    | otherwise =
        start.y > label.position.y
          && start.y < label.position.y + label.size.height
          && positiveOverlap (min start.x end.x) (max start.x end.x) label.position.x (label.position.x + label.size.width)

clearNode :: forall r. Size -> { position :: Point, size :: Size | r } -> Boolean
clearNode node label =
  label.position.x + label.size.width <= 0.0
    || label.position.x >= node.width
    || label.position.y + label.size.height <= 0.0
    || label.position.y >= node.height

overlaps
  :: forall left right
   . { position :: Point, size :: Size | left }
  -> { position :: Point, size :: Size | right }
  -> Boolean
overlaps a b =
  a.position.x < b.position.x + b.size.width
    && a.position.x + a.size.width > b.position.x
    && a.position.y < b.position.y + b.size.height
    && a.position.y + a.size.height > b.position.y

rangeDistance :: Number -> Number -> Number -> Number
rangeDistance value low high
  | value < low = low - value
  | value > high = value - high
  | otherwise = 0.0

positiveOverlap :: Number -> Number -> Number -> Number -> Boolean
positiveOverlap low high otherLow otherHigh = max low otherLow < min high otherHigh

placeLabel :: NodePlacement -> { edge :: EdgeId, position :: Point, size :: Size } -> EdgeLabelPlacement
placeLabel node label =
  { edge: label.edge
  , position: GridPos ((gridX node.position * 4.0 + label.position.y) /\ (gridY node.position * 4.0 + label.position.x))
  , size: GridSize (label.size.height /\ label.size.width)
  }
