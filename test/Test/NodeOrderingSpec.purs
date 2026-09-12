module Test.NodeOrderingSpec (nodeOrderingSpec) where

import Prelude

import Data.Array as A
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Set as S
import Data.Tuple.Nested ((/\))
import LayeredLayout as LL
import LayeredLayout.CrossingMin as CrossingMin
import LayeredLayout.CrossingMin.NorthSouth as NorthSouth
import LayeredLayout.CrossingMin.Ports as PhysicalPorts
import LayeredLayout.DummyNodes (insertDummies, isDummy)
import LayeredLayout.EdgeRouting.HyperEdges as HyperEdges
import LayeredLayout.Graph (Constraints(..), Edge, EdgeId(..), NodeId(..), PortId(..), Shape(..), Side(..))
import LayeredLayout.Grid (GridPos(..), GridSize(..), gridY)
import LayeredLayout.JavaRandom as JR
import LayeredLayout.PortDistribution (distributePorts)
import LayeredLayout.PortDummies as PortDummies
import LayeredLayout.Result (Direction(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

nodeOrderingSpec :: Spec Unit
nodeOrderingSpec = describe "Physical port and node ordering" do
  it "uncrosses the physical ports even when the node-only score is zero" do
    let
      edges = [ edge "e0" "a" "d", edge "e1" "a" "b", edge "e2" "c" "d" ]
      result = CrossingMin.minimize
        { iterations: 8, constraints: [], modelOrder: model [ "a", "b", "c", "d" ], ports: M.empty, chains: [], random: JR.mkRandom 1.0, reversed: S.empty, portDummies: PortDummies.empty }
        (map (map NodeId) [ [ "a", "c" ], [ "b", "d" ] ])
        edges
      offsets = distributePorts result.portOrder result.layout edges
        (M.fromFoldable (map (\n -> NodeId n /\ GridSize (40.0 /\ 20.0)) [ "a", "b", "c", "d" ]))
    result.layout `shouldEqual` map (map NodeId) [ [ "c", "a" ], [ "d", "b" ] ]
    M.lookup (EdgeId "e0" /\ South) offsets `shouldEqual` Just (40.0 / 3.0)
    M.lookup (EdgeId "e1" /\ South) offsets `shouldEqual` Just (80.0 / 3.0)
    M.lookup (EdgeId "e2" /\ North) offsets `shouldEqual` Just (40.0 / 3.0)

  it "orders a long-edge dummy before a source with no preceding-layer connection" do
    let
      edges = [ edge "span" "a" "e", edge "ab" "a" "b", edge "bd" "b" "d", edge "de" "d" "e", edge "ce" "c" "e" ]
      layers = map (map NodeId) [ [ "a" ], [ "b" ], [ "c", "d" ], [ "e" ] ]
      indices = M.fromFoldable (A.concat (A.mapWithIndex (\i ns -> map (\n -> n /\ i) ns) layers))
      split = insertDummies indices edges layers
      result = CrossingMin.minimize
        { iterations: 8, constraints: [], modelOrder: model [ "a", "b", "c", "d", "e" ], ports: M.empty, chains: split.chains, random: JR.mkRandom 1.0, reversed: S.empty, portDummies: PortDummies.empty }
        split.layers
        split.edges
    result.layout `shouldEqual` map (map NodeId)
      [ [ "a" ], [ "$d:span:1", "b" ], [ "$d:span:2", "c", "d" ], [ "e" ] ]

  it "escapes the initial long-edge arrangement on randomized subsequent trials" do
    let
      edges =
        [ edge "ab" "a" "b"
        , edge "ac" "a" "c"
        , edge "bd" "b" "d"
        , edge "be" "b" "e"
        , edge "cf" "c" "f"
        , edge "cg" "c" "g"
        , edge "dh" "d" "h"
        , edge "ah" "a" "h"
        , edge "eg" "e" "g"
        , edge "fh" "f" "h"
        , edge "parallel-dh" "d" "h"
        ]
      layers = map (map NodeId) [ [ "a" ], [ "b", "c" ], [ "d", "e", "f" ], [ "g", "h" ] ]
      indices = M.fromFoldable (A.concat (A.mapWithIndex (\i ns -> map (\n -> n /\ i) ns) layers))
      split = insertDummies indices edges layers
      result = CrossingMin.minimize
        { iterations: 8
        , constraints: []
        , modelOrder: model [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        , ports: M.empty
        , chains: split.chains
        , random: JR.mkRandom 1.0
        , reversed: S.empty
        , portDummies: PortDummies.empty
        }
        split.layers
        split.edges
    result.layout `shouldEqual` map (map NodeId)
      [ [ "a" ], [ "$d:ah:1", "b", "c" ], [ "$d:ah:2", "d", "e", "f", "$d:cg:1" ], [ "h", "g" ] ]

  it "groups reversed long edges by their forward companion's model order" do
    let
      edges = [ edge "rev" "c" "b", edge "cd" "c" "d", edge "cb" "c" "b", edge "ab" "a" "b" ]
      layers = map (map NodeId) [ [ "c" ], [ "a", "d" ], [ "b" ] ]
      indices = M.fromFoldable (A.concat (A.mapWithIndex (\i ns -> map (\n -> n /\ i) ns) layers))
      split = insertDummies indices edges layers
      result = CrossingMin.minimize
        { iterations: 8
        , constraints: []
        , modelOrder: model [ "a", "b", "c", "d" ]
        , ports: M.empty
        , chains: split.chains
        , random: JR.mkRandom 1.0
        , reversed: S.singleton (EdgeId "rev")
        , portDummies: PortDummies.empty
        }
        split.layers
        split.edges
    result.layout `shouldEqual` map (map NodeId) [ [ "c" ], [ "a", "d", "$d:rev:1", "$d:cb:1" ], [ "b" ] ]

  it "keeps edges sharing a fixed physical port at one downstream offset" do
    let
      first = edge "ab" "a" "b"
      second = edge "ac" "a" "c"
      edges = [ first { from { port = Just (PortId "shared") } }, second { from { port = Just (PortId "shared") } } ]
      ports = M.singleton (NodeId "a") [ { id: PortId "shared", side: South, offset: 0, label: Nothing } ]
      result = CrossingMin.minimize
        { iterations: 8, constraints: [], modelOrder: model [ "a", "b", "c" ], ports, chains: [], random: JR.mkRandom 1.0, reversed: S.empty, portDummies: PortDummies.empty }
        (map (map NodeId) [ [ "a" ], [ "b", "c" ] ])
        edges
      offsets = distributePorts result.portOrder result.layout edges (M.singleton (NodeId "a") (GridSize (40.0 /\ 20.0)))
    M.lookup (EdgeId "ab" /\ South) offsets `shouldEqual` Just 20.0
    M.lookup (EdgeId "ac" /\ South) offsets `shouldEqual` Just 20.0

  it "keeps side-port layout units ordered through branches and a long edge" do
    let
      names = [ "gateway", "auth", "catalog", "risk", "payment", "audit", "notification", "archive" ]
      port name side = { id: PortId (name <> show side), side, offset: 2, label: Nothing }
      declared = M.fromFoldable (map (\name -> NodeId name /\ map (port name) [ North, South, East, West ]) names)
      connection a sourceSide b targetSide =
        { id: EdgeId (a <> "-" <> b)
        , from: { node: NodeId a, port: Just (port a sourceSide).id }
        , to: { node: NodeId b, port: Just (port b targetSide).id }
        , label: Nothing
        }
      edges =
        [ connection "gateway" South "auth" North
        , connection "gateway" East "catalog" West
        , connection "auth" East "risk" West
        , connection "auth" South "payment" North
        , connection "catalog" South "payment" East
        , connection "risk" South "audit" North
        , connection "payment" South "notification" North
        , connection "audit" East "archive" West
        , connection "notification" South "archive" North
        , connection "gateway" West "archive" East
        ]
      layers = map (map NodeId) [ [ "gateway" ], [ "auth", "catalog" ], [ "risk", "payment" ], [ "audit", "notification" ], [ "archive" ] ]
      indices = M.fromFoldable (A.concat (A.mapWithIndex (\i ns -> map (\n -> n /\ i) ns) layers))
      prepared = PortDummies.prepare declared (insertDummies indices edges layers)
      ports = M.union declared (M.fromFoldable (map (\d -> d.node.id /\ d.node.ports) prepared.state.dummies))
      result = CrossingMin.minimize
        { iterations: 8
        , constraints: []
        , modelOrder: model names
        , ports
        , chains: prepared.dummies.chains
        , random: JR.mkRandom 1.0
        , reversed: S.empty
        , portDummies: prepared.state
        }
        prepared.dummies.layers
        prepared.dummies.edges
      identify node = case A.find (\d -> d.node.id == node) prepared.state.dummies of
        Just d -> un NodeId d.owner <> ":" <> show d.port.side
        Nothing -> if isDummy node then "long" else un NodeId node
    map (map identify) result.layout `shouldEqual`
      [ [ "gateway:West", "gateway", "gateway:East" ]
      , [ "auth", "auth:East", "catalog:West", "catalog", "long" ]
      , [ "risk:West", "risk", "payment", "payment:East", "long" ]
      , [ "audit", "audit:East", "notification", "long" ]
      , [ "archive:West", "archive", "archive:East" ]
      ]

  it "orders whole side-port units when a hard constraint reverses their owners" do
    let
      port name = { id: PortId name, side: West, offset: 2, label: Nothing }
      declared = M.fromFoldable [ NodeId "a" /\ [ port "pa" ], NodeId "b" /\ [ port "pb" ] ]
      first = edge "ac" "a" "c"
      second = edge "bc" "b" "c"
      edges = [ first { from { port = Just (PortId "pa") } }, second { from { port = Just (PortId "pb") } } ]
      prepared = PortDummies.prepare declared
        { layers: map (map NodeId) [ [ "a", "b" ], [ "c" ] ], edges, chains: [] }
      ports = M.union declared (M.fromFoldable (map (\d -> d.node.id /\ d.node.ports) prepared.state.dummies))
      result = CrossingMin.minimize
        { iterations: 8
        , constraints: [ OrderConstraint { before: NodeId "b", after: NodeId "a" } ]
        , modelOrder: model [ "a", "b", "c" ]
        , ports
        , chains: []
        , random: JR.mkRandom 1.0
        , reversed: S.empty
        , portDummies: prepared.state
        }
        prepared.dummies.layers
        prepared.dummies.edges
      identify node = case A.find (\d -> d.node.id == node) prepared.state.dummies of
        Just d -> un NodeId d.owner <> ":West"
        Nothing -> un NodeId node
    map (map identify) result.layout `shouldEqual` [ [ "b:West", "b", "a:West", "a" ], [ "c" ] ]

  it "counts a shared side-port fanout once against a long-edge neighbor" do
    let
      source = { id: PortId "shared", side: West, offset: 2, label: Nothing }
      edges = map (\e -> e { from { port = Just source.id } })
        [ edge "ab0" "a" "b", edge "ab1" "a" "b", edge "ab2" "a" "b" ]
      declared = M.singleton (NodeId "a") [ source ]
      prepared = PortDummies.prepare declared
        { layers: map (map NodeId) [ [ "x" ], [ "a", "$d:span:1" ], [ "b" ] ]
        , edges: edges <> [ edge "span0" "x" "$d:span:1", edge "span1" "$d:span:1" "b" ]
        , chains: []
        }
      proxy = fromMaybe (NodeId "missing") (map (_.node.id) (A.head prepared.state.dummies))
      physical = PhysicalPorts.build declared prepared.dummies.edges
      info = NorthSouth.metadata prepared.state declared
    NorthSouth.neighboringCrossings info physical proxy (NodeId "$d:span:1")
      `shouldEqual` { before: 1, after: 0 }

  it "restores a shared side port without retaining its temporary endpoint" do
    let
      source = { id: PortId "shared", side: East, offset: 2, label: Nothing }
      edges = map (\e -> e { from { port = Just source.id } }) [ edge "ab" "a" "b", edge "ac" "a" "c" ]
      layers = map (map NodeId) [ [ "a" ], [ "b", "c" ] ]
      split = insertDummies (M.fromFoldable [ NodeId "a" /\ 0, NodeId "b" /\ 1, NodeId "c" /\ 1 ]) edges layers
      prepared = PortDummies.prepare (M.singleton (NodeId "a") [ source ]) split
      dummy = fromMaybe (NodeId "missing") (map (_.node.id) (A.head prepared.state.dummies))
      placements =
        [ { node: NodeId "a", position: GridPos (10.0 /\ 5.0), size: GridSize (10.0 /\ 6.0), layer: 0, order: 0 }
        , { node: dummy, position: GridPos (24.0 /\ 5.0), size: GridSize (0.0 /\ 0.0), layer: 0, order: 1 }
        ]
      path =
        { edge: EdgeId "ab"
        , reversed: false
        , jumps: []
        , bends: []
        , bendType: []
        , segments: [ { start: GridPos (96.0 /\ 20.0), end: GridPos (96.0 /\ 80.0), direction: V } ]
        }
      restored = PortDummies.restore prepared.state placements prepared.dummies.edges [ path ]
    map _.segments restored `shouldEqual`
      [ [ { start: GridPos (80.0 /\ 28.0), end: GridPos (96.0 /\ 28.0), direction: H }
        , { start: GridPos (96.0 /\ 28.0), end: GridPos (96.0 /\ 80.0), direction: V }
        ]
      ]

  it "splits critical routing cycles using distances within each connection side" do
    let
      connections =
        [ { name: "span", source: "s0", target: "t0", incoming: 0.0, outgoing: 133.5 }
        , { name: "left", source: "s1", target: "t1", incoming: 58.0, outgoing: -58.0 }
        , { name: "right", source: "s2", target: "t2", incoming: 125.5, outgoing: 9.0 }
        ]
      assignments = connections <#> \c ->
        { edge: edge c.name c.source c.target
        , fromPos: c.incoming /\ 0.0
        , toPos: c.outgoing /\ 40.0
        , fromSide: South
        , toSide: North
        }
      placements = A.concat
        ( A.mapWithIndex
            ( \order c ->
                [ { node: NodeId c.source, position: GridPos (0.0 /\ 0.0), size: GridSize (0.0 /\ 0.0), layer: 0, order }
                , { node: NodeId c.target, position: GridPos (0.0 /\ 10.0), size: GridSize (0.0 /\ 0.0), layer: 1, order }
                ]
            )
            connections
        )
      plan = HyperEdges.assignSlots (JR.mkRandom 4096.0) assignments placements
    map (\info -> { channels: info.slotCount, split: map _.splitX info.partner })
      (M.lookup (EdgeId "span") plan.slots)
      `shouldEqual` Just { channels: 4, split: Just 91.75 }

  it "preserves routing depth at fractional port boundaries without compaction" do
    let
      name i = "generated-" <> show i
      sizes =
        [ 17.0 /\ 14.0
        , 8.0 /\ 13.0
        , 6.0 /\ 14.0
        , 22.0 /\ 10.0
        , 6.0 /\ 7.0
        , 12.0 /\ 6.0
        , 22.0 /\ 13.0
        , 17.0 /\ 14.0
        , 21.0 /\ 14.0
        , 18.0 /\ 9.0
        , 10.0 /\ 11.0
        , 21.0 /\ 8.0
        , 10.0 /\ 13.0
        , 13.0 /\ 9.0
        , 9.0 /\ 7.0
        , 11.0 /\ 9.0
        , 21.0 /\ 5.0
        , 21.0 /\ 10.0
        , 20.0 /\ 5.0
        , 14.0 /\ 8.0
        , 10.0 /\ 12.0
        , 21.0 /\ 9.0
        , 8.0 /\ 11.0
        , 21.0 /\ 7.0
        , 18.0 /\ 8.0
        ]
      nodes = A.mapWithIndex
        ( \i size ->
            { id: NodeId (name i), size: GridSize size, ports: [], shape: Rectangle, label: Nothing }
        )
        sizes
      backbone = A.filter (\i -> i /= 13 && i /= 20) (A.range 1 24) <#> \i ->
        edge ("backbone-" <> show i) (name ((i - 1) / 2)) (name i)
      connections =
        [ "cross-span" /\ 0 /\ 24
        , "cross-2" /\ 2 /\ 24
        , "cross-4" /\ 4 /\ 8
        , "cross-5" /\ 5 /\ 21
        , "cross-6" /\ 6 /\ 12
        , "cross-10" /\ 10 /\ 16
        , "cross-11" /\ 11 /\ 24
        , "cross-14" /\ 14 /\ 21
        , "cross-15" /\ 15 /\ 24
        , "cross-16" /\ 16 /\ 24
        , "cross-17" /\ 17 /\ 23
        , "cross-18" /\ 18 /\ 23
        , "cross-19" /\ 19 /\ 24
        , "cross-20" /\ 20 /\ 22
        , "feedback-10" /\ 10 /\ 4
        , "feedback-17" /\ 17 /\ 15
        , "parallel-1" /\ 0 /\ 1
        , "parallel-9" /\ 4 /\ 9
        , "parallel-13" /\ 6 /\ 13
        , "parallel-20" /\ 9 /\ 20
        , "parallel-24" /\ 11 /\ 24
        ]
      edges = backbone <> map (\(id /\ source /\ target) -> edge id (name source) (name target)) connections
      result = LL.full (LL.defaultConfig { compactPostRouting = false }) { nodes, edges, constraints: [] }
    -- Reassociating BK shifts changes a boundary port by ~1e-13, dropping
    -- one 10-fine routing channel. Post-routing compaction masks the error.
    map (gridY <<< _.position) (A.find (\n -> n.node == NodeId (name 24)) result.result.nodes)
      `shouldEqual` Just 129.5

model :: Array String -> M.Map NodeId Int
model = M.fromFoldable <<< A.mapWithIndex (\i n -> NodeId n /\ i)

edge :: String -> String -> String -> Edge
edge name source target =
  { id: EdgeId name
  , from: { node: NodeId source, port: Nothing }
  , to: { node: NodeId target, port: Nothing }
  , label: Nothing
  }
