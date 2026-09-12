-- Copyright (c) 2011, 2015 Kiel University and others.
-- SPDX-License-Identifier: EPL-2.0
--
-- NorthSouthPortPreprocessor / NorthSouthPortPostprocessor, in DOWN coordinates.
-- These are physical-port proxies, not long-edge nodes or artificial owner edges.
module LayeredLayout.PortDummies
  ( State
  , PortDummy
  , empty
  , isPortDummy
  , prepare
  , expand
  , unprepare
  , rewire
  , restore
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Set as S
import Data.String as Str
import Data.Tuple.Nested ((/\))
import LayeredLayout.DummyNodes (DummyResult)
import LayeredLayout.Graph (Edge, Node, NodeId(..), Port, PortId(..), Shape(..), Side(..))
import LayeredLayout.Grid (GridPos(..), GridSize(..), gridX, gridY, sizeW)
import LayeredLayout.Result (Direction(..), EdgePath, NodePlacement)

type PortDummy =
  { node :: Node
  , owner :: NodeId
  , port :: Port
  , incoming :: Boolean
  , outgoing :: Boolean
  }

type State = { dummies :: Array PortDummy }

empty :: State
empty = { dummies: [] }

isPortDummy :: NodeId -> Boolean
isPortDummy node = Str.take 6 (un NodeId node) == "$port:"

prepare :: Map NodeId (Array Port) -> DummyResult -> { state :: State, dummies :: DummyResult }
prepare declared original = { state, dummies: rewire state (original { layers = expand state original.layers }) }
  where
  occupied = S.fromFoldable (A.concat original.layers)
  incidence = foldl indexEdge M.empty original.edges
  indexEdge acc edge = indexEnd false edge.to (indexEnd true edge.from acc)
  indexEnd outgoing endpoint acc = case endpoint.port of
    Nothing -> acc
    Just port -> M.alter
      (Just <<< (\old -> if outgoing then old { outgoing = true } else old { incoming = true }) <<< fromMaybe { incoming: false, outgoing: false })
      (endpoint.node /\ port)
      acc
  created = foldl owner { dummies: Nil, used: occupied } (A.concat original.layers)
  state = { dummies: A.reverse (A.fromFoldable created.dummies) }
  owner acc node = foldl (side node) acc [ West, East ]
  side node acc direction =
    let
      ports = A.sortBy (comparing _.offset) (A.filter (\p -> p.side == direction) (fromMaybe [] (M.lookup node declared)))
      attached p = fromMaybe { incoming: false, outgoing: false } (M.lookup (node /\ p.id) incidence)
      incoming = _.incoming <<< attached
      outgoing = _.outgoing <<< attached
      -- The model-order processor appends through a reversed list view,
      -- yielding reversed outputs followed by inputs in the underlying list.
      ordered = A.reverse (A.filter (not <<< incoming) ports) <> A.filter incoming ports
      inputs = A.filter (\p -> incoming p && not (outgoing p)) ordered
      outputs = A.filter (\p -> outgoing p && not (incoming p)) ordered
      both = A.filter (\p -> incoming p && outgoing p) ordered
      add current port =
        let
          id = fresh current.used ("$port:" <> un NodeId node <> ":" <> un PortId port.id)
          make s = { id: PortId (un NodeId id <> ":" <> show s), side: s, offset: 0, label: Nothing }
          dummy =
            { node: { id, size: GridSize (0.0 /\ 0.0), ports: (if incoming port then [ make North ] else []) <> (if outgoing port then [ make South ] else []), label: Nothing, shape: Rectangle }
            , owner: node
            , port
            , incoming: incoming port
            , outgoing: outgoing port
            }
        in
          { dummies: Cons dummy current.dummies, used: S.insert id current.used }
    in
      foldl add acc (inputs <> outputs <> both)
  fresh used name = if S.member (NodeId name) used then fresh used (name <> "'") else NodeId name

expand :: State -> Array (Array NodeId) -> Array (Array NodeId)
expand state layers
  | A.null state.dummies = layers
  | otherwise = map (A.concatMap insert) layers
      where
      byOwner = foldl
        (\acc d -> M.insertWith (<>) (d.owner /\ d.port.side) (Cons d.node.id Nil) acc)
        M.empty
        state.dummies
      insert node =
        let
          members side = A.fromFoldable (fromMaybe Nil (M.lookup (node /\ side) byOwner))
        in
          members West <> [ node ] <> A.reverse (members East)

-- SortByInputModelProcessor precedes NorthSouthPortPreprocessor. Crossing min
-- reconstructs that input before model sorting, then expands the same proxies.
unprepare :: State -> DummyResult -> DummyResult
unprepare state original
  | A.null state.dummies = original
  | otherwise =
      original
        { layers = map (A.filter (not <<< flip M.member byNode)) original.layers
        , edges = map (\edge -> edge { from = endpoint edge.from, to = endpoint edge.to }) original.edges
        }
      where
      byNode = M.fromFoldable (map (\d -> d.node.id /\ d) state.dummies)
      endpoint current = case M.lookup current.node byNode of
        Nothing -> current
        Just d -> { node: d.owner, port: Just d.port.id }

-- Keep chain endpoints and segment identities in their original namespace.
-- LabelDummySwitcher may rebuild segments; applying this again is idempotent.
rewire :: State -> DummyResult -> DummyResult
rewire state original = original { edges = map rewrite original.edges }
  where
  byPort = M.fromFoldable (map (\d -> (d.owner /\ d.port.id) /\ d) state.dummies)
  endpoint direction current = fromMaybe current do
    port <- current.port
    dummy <- M.lookup (current.node /\ port) byPort
    proxy <- A.find (\p -> p.side == direction) dummy.node.ports
    pure { node: dummy.node.id, port: Just proxy.id }
  rewrite edge =
    let
      from = endpoint South edge.from
      to = endpoint North edge.to
    in
      if from == edge.from && to == edge.to then edge else edge { from = from, to = to }

-- Routed segment endpoints are not bend points in ELK. Drop the temporary
-- endpoint, retain the router's bends, and insert the owner anchor and its
-- perpendicular turn through the physical-port proxy's cross-axis coordinate.
restore :: State -> Array NodePlacement -> Array Edge -> Array EdgePath -> Array EdgePath
restore state placements edges paths
  | A.null state.dummies = paths
  | otherwise = map restorePath paths
      where
      nodes = M.fromFoldable (map (\n -> n.node /\ n) placements)
      proxies = M.fromFoldable (map (\d -> d.node.id /\ d) state.dummies)
      byEdge = M.fromFoldable (map (\e -> e.id /\ e) edges)
      turn node = do
        dummy <- M.lookup node proxies
        owner <- M.lookup dummy.owner nodes
        placed <- M.lookup node nodes
        let x = 4.0 * (gridX owner.position + if dummy.port.side == East then sizeW owner.size else 0.0)
        let y = 4.0 * (gridY owner.position + toNumber dummy.port.offset)
        pure { anchor: GridPos (x /\ y), bend: GridPos ((4.0 * gridX placed.position) /\ y) }
      restorePath path = fromMaybe path do
        edge <- M.lookup path.edge byEdge
        if not (M.member edge.from.node proxies || M.member edge.to.node proxies) then Nothing else Just unit
        first <- A.head path.segments
        let original = [ first.start ] <> map _.end path.segments
        let source = turn edge.from.node
        let target = turn edge.to.node
        let
          start = case source of
            Nothing -> original
            Just p -> [ p.anchor, p.bend ] <> A.drop 1 original
        let
          points = case target of
            Nothing -> start
            Just p -> A.take (A.length start - 1) start <> [ p.bend, p.anchor ]
        let segments = A.mapMaybe segment (A.zip points (A.drop 1 points))
        pure (path { segments = segments, bends = map _.end (A.take (A.length segments - 1) segments) })
      segment (start /\ end)
        | start == end = Nothing
        | otherwise = Just { start, end, direction: if gridX start == gridX end then V else H }
