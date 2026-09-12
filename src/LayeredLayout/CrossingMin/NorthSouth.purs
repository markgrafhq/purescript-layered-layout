-- Copyright (c) 2011, 2020 Kiel University and others.
-- SPDX-License-Identifier: EPL-2.0
-- North/south-port layout units and CrossingsCounter's in-layer arc traversal.
module LayeredLayout.CrossingMin.NorthSouth (Metadata, metadata, associates, constraints, crossings, neighboringCrossings, preventsSwitch) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl, sum)
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\), type (/\))
import LayeredLayout.CrossingMin.Ports (Ports)
import LayeredLayout.CrossingMin.Ports as P
import LayeredLayout.DummyNodes (isDummy, isLabelDummy)
import LayeredLayout.Graph (NodeId, Port, PortId, Side(..))
import LayeredLayout.PortDummies (State, PortDummy)

type Metadata =
  { dummies :: Map NodeId PortDummy
  , owners :: Map NodeId (Array PortDummy)
  , units :: Map NodeId NodeId
  , successors :: Array { before :: NodeId, after :: NodeId }
  , portPositions :: Map (NodeId /\ PortId) Int
  }

metadata :: State -> Map NodeId (Array Port) -> Metadata
metadata state declared =
  { dummies: M.fromFoldable (map (\d -> d.node.id /\ d) state.dummies)
  , owners: map (A.reverse <<< A.fromFoldable) ownerLists
  , units: foldl (\acc d -> M.insert d.node.id d.owner acc) fixedOwners state.dummies
  , successors: map (\d -> if d.port.side == West then { before: d.node.id, after: d.owner } else { before: d.owner, after: d.node.id }) state.dummies
  , portPositions: M.fromFoldable (A.concatMap positionPorts declarations)
  }
  where
  ownerLists = foldl (\acc d -> M.insertWith (<>) d.owner (Cons d Nil) acc) M.empty state.dummies
  declarations = M.toUnfoldable declared :: Array (NodeId /\ Array Port)
  fixedOwners = M.fromFoldable (A.mapMaybe (\(node /\ ports) -> if A.null ports then Nothing else Just (node /\ node)) declarations)
  positionPorts (node /\ ports) = A.concatMap
    (\side -> A.mapWithIndex (\i p -> (node /\ p.id) /\ i) (A.sortBy (comparing _.offset) (A.filter (\p -> p.side == side) ports)))
    [ West, East ]

associates :: Metadata -> NodeId -> Array NodeId
associates info node = map (_.node.id) (fromMaybe [] (M.lookup node info.owners))

constraints :: Metadata -> Array NodeId -> Array { before :: NodeId, after :: NodeId }
constraints info nodes = info.successors <> separated
  where
  normal = A.filter (\n -> not (isDummy n || isLabelDummy n || M.member n info.dummies)) nodes
  members owner = if M.member owner info.units then [ owner ] <> associates info owner else []
  separated = A.concatMap (\(a /\ b) -> A.concatMap (\before -> map (\after -> { before, after }) (members b)) (members a)) (A.zip normal (A.drop 1 normal))

preventsSwitch :: Metadata -> NodeId -> NodeId -> Boolean
preventsSwitch info a b
  | M.isEmpty info.dummies || isDummy a || isDummy b = false
  | otherwise = normalAndProxy || differentUnits || northern || southern
      where
      normal n = not (isDummy n || isLabelDummy n || M.member n info.dummies)
      normalAndProxy = (normal a && M.member b info.dummies) || (normal b && M.member a info.dummies)
      multi n = case M.lookup n info.units of
        Just owner -> owner /= n
        Nothing -> false
      side n direction = A.any (\d -> d.port.side == direction) (fromMaybe [] (M.lookup n info.owners))
      northern = side a West
      southern = side b East
      differentUnits = (multi a || multi b || side a East || side b West) && M.lookup a info.units /= M.lookup b info.units

-- GreedySwitch's neighboring counter deliberately differs from the all-arc
-- trial score: crossing a long-edge neighbor costs one, even for fanout.
neighboringCrossings :: Metadata -> Ports -> NodeId -> NodeId -> { before :: Int, after :: Int }
neighboringCrossings info ports upper lower = case M.lookup upper info.dummies /\ M.lookup lower info.dummies of
  Just a /\ Just b | a.owner == b.owner ->
    let
      farther /\ closer = if a.port.side == West then a /\ b else b /\ a
      position d = fromMaybe 0 (M.lookup (d.owner /\ d.port.id) info.portPositions)
    in
      if position farther > position closer then
        { before: degree closer South, after: degree farther North }
      else { before: degree closer North, after: degree farther South }
  Just a /\ _ | isDummy lower -> if a.port.side == West then { before: 1, after: 0 } else { before: 0, after: 1 }
  _ /\ Just b | isDummy upper -> if b.port.side == West then { before: 0, after: 1 } else { before: 1, after: 0 }
  _ | normal upper && isDummy lower -> { before: count upper East, after: count upper West }
  _ | isDummy upper && normal lower -> { before: count lower West, after: count lower East }
  _ -> { before: 0, after: 0 }
  where
  degree dummy side = sum (map (A.length <<< _.edges) (P.groups dummy.node.id side ports))
  count owner side = A.length (A.filter (\d -> d.port.side == side) (fromMaybe [] (M.lookup owner info.owners)))
  normal node = not (isDummy node || isLabelDummy node || M.member node info.dummies)

crossings :: Metadata -> Ports -> Array (Array NodeId) -> Int
crossings info ports layers
  | M.isEmpty info.dummies = 0
  | otherwise = sum (map layerCrossings layers)
      where
      degree node side = sum (map (A.length <<< _.edges) (P.groups node side ports))
      layerCrossings nodes
        | not (A.any (flip M.member info.dummies) nodes) = 0
        | otherwise = (foldl visit { active: [], score: 0 } (A.mapWithIndex (/\) ordered)).score
            where
            flush acc = acc { ordered = acc.ordered <> map (\n -> n /\ South) acc.stack, stack = [] }
            append side acc node = acc { ordered = A.snoc acc.ordered (node /\ side) }
            step acc node =
              let
                currentUnit = M.lookup node info.units
                changed = case acc.lastUnit of
                  Just previous -> currentUnit /= Nothing && currentUnit /= Just previous && node /= previous
                  Nothing -> false
                cleared = if changed then flush acc else acc
                current = cleared { lastUnit = if currentUnit == Nothing then cleared.lastUnit else currentUnit }
                physical direction = A.sortBy (comparing (_.port.offset)) (A.filter (\d -> d.port.side == direction) (fromMaybe [] (M.lookup node info.owners)))
                ownerPorts ds x = x { ordered = x.ordered <> map (\d -> d.node.id /\ East) ds }
              in
                if M.member node info.dummies || isDummy node then
                  let
                    indexed = if degree node North > 0 then append North current node else current
                  in
                    if degree node South > 0 then indexed { stack = [ node ] <> indexed.stack } else indexed
                else if isLabelDummy node then current
                else ownerPorts (A.reverse (physical East)) (flush (ownerPorts (physical West) current))
            ordered = (flush (foldl step { ordered: [], stack: [], lastUnit: Nothing } nodes)).ordered
            positions = M.fromFoldable (A.mapWithIndex (\i key -> key /\ i) ordered)
            targets (node /\ side)
              | side == East = case M.lookup node info.dummies of
                  Nothing -> []
                  Just d -> (if d.incoming then [ { key: node /\ North, degree: degree node North } ] else []) <> (if d.outgoing then [ { key: node /\ South, degree: degree node South } ] else [])
              | M.member node info.dummies = [ { key: node /\ East, degree: degree node side } ]
              | otherwise = let opposite = if side == North then South else North in [ { key: node /\ opposite, degree: degree node opposite } ]
            visit acc (index /\ key) =
              let
                active = A.filter (_ /= index) acc.active
                forward = A.mapMaybe (\target -> M.lookup target.key positions >>= \end -> if end > index then Just { end, degree: target.degree } else Nothing) (targets key)
                score = sum (map (\target -> A.length (A.filter (_ < target.end) active) * target.degree) forward)
              in
                { active: active <> map _.end forward, score: acc.score + score }
