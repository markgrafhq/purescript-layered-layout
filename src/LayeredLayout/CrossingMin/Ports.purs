-- Copyright (c) 2010, 2012, 2015, 2016, 2020 Kiel University and others.
-- SPDX-License-Identifier: EPL-2.0
-- Port of ELK's model-order and barycenter port distributors for the
-- downward, proper layered graph. Arrays are physical left-to-right order;
-- ELK's clockwise WEST order is their reverse.
module LayeredLayout.CrossingMin.Ports
  ( Ports
  , PhysicalPort
  , build
  , rewire
  , groups
  , ranks
  , distribute
  , toOrder
  , reorder
  , fixed
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl, sum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import LayeredLayout.Graph (Edge, EdgeId, NodeId, Port, PortId, Side(..))
import LayeredLayout.PortDistribution (PortOrder)

type PhysicalPort = { id :: Maybe PortId, edges :: Array Edge }
type Ports = Map (NodeId /\ Side) (Array PhysicalPort)

build :: Map NodeId (Array Port) -> Array Edge -> Ports
build declared edges = mapWithIndex orderFixed (foldl add M.empty edges)
  where
  orderFixed (node /\ side) ps =
    if fixed declared node then
      A.sortBy (if side == South then comparing (clockwise node) else flip (comparing (clockwise node))) ps
    else ps
  clockwise node p = fromMaybe (0 /\ 0) do
    pid <- p.id
    port <- A.find (\q -> q.id == pid) (fromMaybe [] (M.lookup node declared))
    pure case port.side of
      West -> 0 /\ port.offset
      South -> 1 /\ port.offset
      East -> 2 /\ negate port.offset
      North -> 3 /\ negate port.offset
  add acc edge
    | edge.from.node == edge.to.node = acc
    | otherwise = addEnd North edge.to.node edge.to.port edge (addEnd South edge.from.node edge.from.port edge acc)
  addEnd side node pid edge acc = M.alter (Just <<< insert <<< fromMaybe []) (node /\ side) acc
    where
    insert ps = case pid >>= \p -> A.findIndex (\port -> port.id == Just p) ps of
      Just i -> fromMaybe ps (A.modifyAt i (\port -> port { edges = A.snoc port.edges edge }) ps)
      Nothing -> A.snoc ps { id: pid, edges: [ edge ] }

-- Preserve sorted physical ports when the NS preprocessor moves an edge to a
-- proxy. Only new proxy nodes need their fresh fixed port arrays.
rewire :: Map NodeId (Array Port) -> Array Edge -> Ports -> Ports
rewire declared edges ports = M.union (mapWithIndex retain ports) (build declared edges)
  where
  byId = M.fromFoldable (map (\e -> e.id /\ e) edges)
  retain (node /\ side) = A.mapMaybe \p ->
    let
      remaining = A.mapMaybe
        (\old -> M.lookup old.id byId >>= \e -> if (if side == South then e.from.node else e.to.node) == node then Just e else Nothing)
        p.edges
    in
      if A.null remaining then Nothing else Just (p { edges = remaining })

groups :: NodeId -> Side -> Ports -> Array PhysicalPort
groups node side = fromMaybe [] <<< M.lookup (node /\ side)

fixed :: Map NodeId (Array Port) -> NodeId -> Boolean
fixed declared node = not (A.null (fromMaybe [] (M.lookup node declared)))

reorder :: NodeId -> Side -> Array PhysicalPort -> Ports -> Ports
reorder node side = M.insert (node /\ side)

-- Layer-total and node-relative ranks count physical ports, not edges.
ranks :: Boolean -> Array NodeId -> Side -> Ports -> Map EdgeId Number
ranks relative layer side ports = (foldl node { consumed: 0.0, result: M.empty } layer).result
  where
  node acc nid =
    let
      ps = groups nid side ports
      count = A.length ps
      increment = if relative then 1.0 / toNumber (count + 1) else 1.0
      input = side == North
      position =
        if input then acc.consumed + (if relative then 1.0 - increment else toNumber count)
        else acc.consumed + increment
      assigned = foldl
        ( \state p ->
            { result: foldl (\out e -> M.insert e.id state.position out) state.result p.edges
            , position: state.position + if input then -increment else increment
            }
        )
        { result: acc.result, position }
        (if input then A.reverse ps else ps)
    in
      { result: assigned.result, consumed: acc.consumed + if relative then 1.0 else toNumber count }

distribute :: Map NodeId (Array Port) -> Array NodeId -> Side -> Map EdgeId Number -> Ports -> Ports
distribute declared layer side reference = foldlNode
  where
  foldlNode ports = foldl node ports layer
  node ports nid
    | fixed declared nid = ports
    | otherwise = reorder nid side (A.sortBy (comparing barycenter) (groups nid side ports)) ports
  barycenter port =
    let
      values = A.mapMaybe (\e -> M.lookup e.id reference) port.edges
    in
      if A.null values then 0.0 else sum values / toNumber (A.length values)

toOrder :: Ports -> PortOrder
toOrder ports = foldl node M.empty (M.toUnfoldable ports :: Array ((NodeId /\ Side) /\ Array PhysicalPort))
  where
  node acc ((_ /\ side) /\ ps) = foldl (\m (i /\ p) -> foldl (\out e -> M.insert (e.id /\ side) i out) m p.edges)
    acc
    (A.mapWithIndex (/\) ps)
