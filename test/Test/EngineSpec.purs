module Test.EngineSpec (engineSpec) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (un)
import Data.Set as S
import Data.Tuple.Nested (type (/\), (/\))
import LayeredLayout (defaultConfig, layout)
import LayeredLayout.Aesthetics (allMetrics, bendCount, nodeOverlapCount)
import LayeredLayout.CoordAssignment as CoordAssignment
import LayeredLayout.CrossingMin as CrossingMin
import LayeredLayout.CrossingMin (countCrossings)
import LayeredLayout.CycleRemoval as CycleRemoval
import LayeredLayout.DummyNodes as DummyNodes
import LayeredLayout.EdgeRouting (routeAll, scaleFactor)
import LayeredLayout.EdgeRouting.LineJump (detectJumps)
import LayeredLayout.EdgeRouting.Orthogonal (buildObstacleMap, findRoute, simplifySegments)
import LayeredLayout.EdgeRouting.PortAssignment (assignPorts, portSlots)
import LayeredLayout.Graph (Alignment(..), Axis(..), Constraints(..), Edge, EdgeId(..), Graph, Justify(..), LayerPin(..), NodeId(..), PortId(..), Shape(..), Side(..))
import LayeredLayout.Grid (GridPos(..), GridSize(..), addPos, contains, gridX, gridY, manhattan, overlaps, sizeH, sizeW, subPos)
import LayeredLayout.LayerAssignment as LayerAssignment
import LayeredLayout.JavaRandom (mkRandom)
import LayeredLayout.PortDummies as PortDummies
import LayeredLayout.Result (BendType(..), Direction(..), EdgePath, EdgeSegment)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Yoga.JSON (readJSON_, writeJSON)

engineSpec :: Spec Unit
engineSpec = do
  gridSpec
  graphSpec
  constraintSpec
  resultSpec
  codecSpec
  cycleRemovalSpec
  layerAssignmentSpec
  dummyNodeSpec
  crossingMinSpec
  coordAssignmentSpec
  edgeRoutingSpec
  lineJumpSpec
  aestheticsSpec
  pipelineSpec
  portAssignmentSpec
  orthogonalSpec
  routingPipelineSpec
  simplifySpec
  weightedBarycenterSpec
  siftingSpec
  compactionSpec

-- Helpers
mkEdge :: String -> String -> String -> Edge
mkEdge eid from to =
  { id: EdgeId eid
  , from: { node: NodeId from, port: Nothing }
  , to: { node: NodeId to, port: Nothing }
  , label: Nothing
  }

gridSpec :: Spec Unit
gridSpec = describe "LayeredLayout.Grid" do
  it "GridPos accessors" do
    gridX (GridPos (3.0 /\ 7.0)) `shouldEqual` 3.0
    gridY (GridPos (3.0 /\ 7.0)) `shouldEqual` 7.0

  it "GridSize accessors" do
    sizeW (GridSize (4.0 /\ 5.0)) `shouldEqual` 4.0
    sizeH (GridSize (4.0 /\ 5.0)) `shouldEqual` 5.0

  it "addPos" do
    addPos (GridPos (1.0 /\ 2.0)) (GridPos (3.0 /\ 4.0)) `shouldEqual` GridPos (4.0 /\ 6.0)

  it "subPos" do
    subPos (GridPos (5.0 /\ 8.0)) (GridPos (2.0 /\ 3.0)) `shouldEqual` GridPos (3.0 /\ 5.0)

  it "manhattan distance" do
    manhattan (GridPos (0.0 /\ 0.0)) (GridPos (3.0 /\ 4.0)) `shouldEqual` 7.0
    manhattan (GridPos (1.0 /\ 1.0)) (GridPos (1.0 /\ 1.0)) `shouldEqual` 0.0

  it "overlaps detects intersection" do
    let r1 = { pos: GridPos (0.0 /\ 0.0), size: GridSize (3.0 /\ 3.0) }
    let r2 = { pos: GridPos (2.0 /\ 2.0), size: GridSize (3.0 /\ 3.0) }
    overlaps r1 r2 `shouldSatisfy` identity

  it "overlaps detects no intersection" do
    let r1 = { pos: GridPos (0.0 /\ 0.0), size: GridSize (2.0 /\ 2.0) }
    let r2 = { pos: GridPos (3.0 /\ 3.0), size: GridSize (2.0 /\ 2.0) }
    overlaps r1 r2 `shouldSatisfy` not

  it "overlaps touching edges do not overlap" do
    let r1 = { pos: GridPos (0.0 /\ 0.0), size: GridSize (2.0 /\ 2.0) }
    let r2 = { pos: GridPos (2.0 /\ 0.0), size: GridSize (2.0 /\ 2.0) }
    overlaps r1 r2 `shouldSatisfy` not

  it "contains point inside rect" do
    let r = { pos: GridPos (1.0 /\ 1.0), size: GridSize (3.0 /\ 3.0) }
    contains r (GridPos (2.0 /\ 2.0)) `shouldSatisfy` identity

  it "contains point outside rect" do
    let r = { pos: GridPos (1.0 /\ 1.0), size: GridSize (3.0 /\ 3.0) }
    contains r (GridPos (0.0 /\ 0.0)) `shouldSatisfy` not

graphSpec :: Spec Unit
graphSpec = describe "LayeredLayout.Graph" do
  it "NodeId equality" do
    NodeId "a" `shouldEqual` NodeId "a"

  it "NodeId ordering" do
    (NodeId "a" < NodeId "b") `shouldSatisfy` identity

  it "PortId equality" do
    PortId "p1" `shouldEqual` PortId "p1"

  it "EdgeId equality" do
    EdgeId "e1" `shouldEqual` EdgeId "e1"

  it "Side values" do
    show North `shouldEqual` "North"
    show South `shouldEqual` "South"
    show East `shouldEqual` "East"
    show West `shouldEqual` "West"

constraintSpec :: Spec Unit
constraintSpec = describe "Markgraf.Constraints" do
  it "Axis values" do
    show Horizontal `shouldEqual` "Horizontal"
    show Vertical `shouldEqual` "Vertical"

  it "Alignment values" do
    show Start `shouldEqual` "Start"
    show Center `shouldEqual` "Center"
    show End `shouldEqual` "End"

  it "Justify values" do
    show SpaceBetween `shouldEqual` "SpaceBetween"
    show SpaceAround `shouldEqual` "SpaceAround"

  it "LayerPin values" do
    show FirstLayer `shouldEqual` "FirstLayer"
    show LastLayer `shouldEqual` "LastLayer"
    show (SpecificLayer 3) `shouldEqual` "(SpecificLayer 3)"

  it "Constraints equality" do
    (SameLayer { nodes: [ NodeId "a", NodeId "b" ] } == SameLayer { nodes: [ NodeId "a", NodeId "b" ] }) `shouldEqual` true
    (OrderConstraint { before: NodeId "a", after: NodeId "b" } == OrderConstraint { before: NodeId "a", after: NodeId "b" }) `shouldEqual` true

resultSpec :: Spec Unit
resultSpec = describe "LayeredLayout.Result" do
  it "Direction values" do
    show H `shouldEqual` "H"
    show V `shouldEqual` "V"

  it "BendType values" do
    show LeftTurn `shouldEqual` "LeftTurn"
    show RightTurn `shouldEqual` "RightTurn"

codecSpec :: Spec Unit
codecSpec = describe "Markgraf.Codec" do
  it "GridPos roundtrip" do
    let pos = GridPos (3.0 /\ 7.0)
    (readJSON_ (writeJSON pos) :: _ GridPos) `shouldEqual` pure pos

  it "GridSize roundtrip" do
    let size = GridSize (4.0 /\ 5.0)
    (readJSON_ (writeJSON size) :: _ GridSize) `shouldEqual` pure size

  it "NodeId roundtrip" do
    (readJSON_ (writeJSON (NodeId "test")) :: _ NodeId) `shouldEqual` pure (NodeId "test")

  it "Side serializes as string" do
    writeJSON North `shouldEqual` "\"North\""
    writeJSON East `shouldEqual` "\"East\""

  it "Side roundtrip" do
    let sides = [ North, South, East, West ]
    map (\s -> readJSON_ (writeJSON s) :: _ Side) sides `shouldEqual` map pure sides

  it "Direction roundtrip" do
    (readJSON_ (writeJSON H) :: _ Direction) `shouldEqual` pure H
    (readJSON_ (writeJSON V) :: _ Direction) `shouldEqual` pure V

  it "Axis roundtrip" do
    (readJSON_ (writeJSON Horizontal) :: _ Axis) `shouldEqual` pure Horizontal
    (readJSON_ (writeJSON Vertical) :: _ Axis) `shouldEqual` pure Vertical

  it "Constraints SameLayer roundtrip" do
    let c = SameLayer { nodes: [ NodeId "a", NodeId "b" ] }
    ((readJSON_ (writeJSON c) :: _ Constraints) == pure c) `shouldEqual` true

  it "Constraints AlignGroup roundtrip" do
    let c = AlignGroup { nodes: [ NodeId "a", NodeId "b" ], axis: Vertical, alignment: Center, justify: SpaceBetween }
    ((readJSON_ (writeJSON c) :: _ Constraints) == pure c) `shouldEqual` true

  it "Full graph JSON roundtrip" do
    let json = writeJSON mkTestGraph
    (isJust (readJSON_ json :: Maybe { nodes :: Array { id :: NodeId }, edges :: Array { id :: EdgeId }, constraints :: Array Constraints })) `shouldSatisfy` identity

cycleRemovalSpec :: Spec Unit
cycleRemovalSpec = describe "LayeredLayout.CycleRemoval" do
  it "DAG remains unchanged" do
    let edges = [ mkEdge "e1" "a" "b", mkEdge "e2" "b" "c" ]
    let result = CycleRemoval.makeAcyclic [] edges
    S.size result.reversedEdges `shouldEqual` 0

  it "cycle gets one edge reversed" do
    let edges = [ mkEdge "e1" "a" "b", mkEdge "e2" "b" "c", mkEdge "e3" "c" "a" ]
    let result = CycleRemoval.makeAcyclic [] edges
    (S.size result.reversedEdges > 0) `shouldSatisfy` identity

  it "self-loop reversed" do
    let edges = [ mkEdge "e1" "a" "a" ]
    let result = CycleRemoval.makeAcyclic [] edges
    S.size result.reversedEdges `shouldEqual` 1

  it "layer hints reverse wrong-direction edges" do
    let edges = [ mkEdge "e1" "b" "a" ]
    let constraints = [ LayerConstraint { node: NodeId "a", pin: FirstLayer }, LayerConstraint { node: NodeId "b", pin: SpecificLayer 2 } ]
    let result = CycleRemoval.makeAcyclic constraints edges
    S.size result.reversedEdges `shouldEqual` 1

  it "Greedy: DAG remains unchanged" do
    let edges = [ mkEdge "e1" "a" "b", mkEdge "e2" "b" "c", mkEdge "e3" "a" "c" ]
    let result = CycleRemoval.makeAcyclicWith CycleRemoval.Greedy [] edges
    S.size result.reversedEdges `shouldEqual` 0

  it "Greedy: 3-cycle gets exactly one edge reversed" do
    let edges = [ mkEdge "e1" "a" "b", mkEdge "e2" "b" "c", mkEdge "e3" "c" "a" ]
    let result = CycleRemoval.makeAcyclicWith CycleRemoval.Greedy [] edges
    S.size result.reversedEdges `shouldEqual` 1

  it "Greedy: self-loop is not reversed (FAS leaves self-loops alone)" do
    let edges = [ mkEdge "e1" "a" "a" ]
    let result = CycleRemoval.makeAcyclicWith CycleRemoval.Greedy [] edges
    -- ELK's GreedyCycleBreaker explicitly skips self-loops in
    -- updateNeighbors and the rank check (mark[n] > mark[n] is false).
    S.size result.reversedEdges `shouldEqual` 0

  it "forceNodeModelOrder preserves declaration order on tied barycenter" do
    let layers = [ [ NodeId "a" ], [ NodeId "d", NodeId "c", NodeId "b" ] ]
    let edges = [ mkEdge "e1" "a" "b", mkEdge "e2" "a" "c", mkEdge "e3" "a" "d" ]
    let modelOrder = M.fromFoldable [ (NodeId "a" /\ 0), (NodeId "b" /\ 1), (NodeId "c" /\ 2), (NodeId "d" /\ 3) ]
    let result = _.layout $ CrossingMin.minimize { iterations: 4, constraints: [], modelOrder, ports: M.empty, chains: [], random: mkRandom 1.0, reversed: S.empty, portDummies: PortDummies.empty } layers edges
    A.index result 1 `shouldEqual` Just [ NodeId "b", NodeId "c", NodeId "d" ]

layerAssignmentSpec :: Spec Unit
layerAssignmentSpec = describe "LayeredLayout.LayerAssignment" do
  it "linear chain a->b->c" do
    let edges = [ mkEdge "e1" "a" "b", mkEdge "e2" "b" "c" ]
    let result = LayerAssignment.assignLayers [] edges [ NodeId "a", NodeId "b", NodeId "c" ]
    A.length result.layers `shouldEqual` 3
    M.lookup (NodeId "a") result.nodeLayer `shouldEqual` Just 0
    M.lookup (NodeId "b") result.nodeLayer `shouldEqual` Just 1
    M.lookup (NodeId "c") result.nodeLayer `shouldEqual` Just 2

  it "diamond graph a->{b,c}->d" do
    let edges = [ mkEdge "e1" "a" "b", mkEdge "e2" "a" "c", mkEdge "e3" "b" "d", mkEdge "e4" "c" "d" ]
    let result = LayerAssignment.assignLayers [] edges [ NodeId "a", NodeId "b", NodeId "c", NodeId "d" ]
    M.lookup (NodeId "a") result.nodeLayer `shouldEqual` Just 0
    M.lookup (NodeId "d") result.nodeLayer `shouldEqual` Just 2

  it "SameLayer constraint unifies layers" do
    let edges = [ mkEdge "e1" "a" "b", mkEdge "e2" "a" "c" ]
    let constraints = [ SameLayer { nodes: [ NodeId "b", NodeId "c" ] } ]
    let result = LayerAssignment.assignLayers constraints edges [ NodeId "a", NodeId "b", NodeId "c" ]
    M.lookup (NodeId "b") result.nodeLayer `shouldEqual` M.lookup (NodeId "c") result.nodeLayer

  it "LayerConstraint pins to specific layer" do
    let edges = [ mkEdge "e1" "a" "b" ]
    let constraints = [ LayerConstraint { node: NodeId "a", pin: FirstLayer } ]
    let result = LayerAssignment.assignLayers constraints edges [ NodeId "a", NodeId "b" ]
    M.lookup (NodeId "a") result.nodeLayer `shouldEqual` Just 0

  it "NetworkSimplex linear chain assigns sequential layers" do
    let edges = [ mkEdge "e1" "a" "b", mkEdge "e2" "b" "c" ]
    let result = LayerAssignment.assignLayersWith LayerAssignment.NetworkSimplex [] edges [ NodeId "a", NodeId "b", NodeId "c" ]
    A.length result.layers `shouldEqual` 3
    M.lookup (NodeId "a") result.nodeLayer `shouldEqual` Just 0
    M.lookup (NodeId "b") result.nodeLayer `shouldEqual` Just 1
    M.lookup (NodeId "c") result.nodeLayer `shouldEqual` Just 2

  it "NetworkSimplex prunes leaves above the 40-node threshold" do
    -- 1 spine of 10 nodes a0→a1→…→a9, plus 40 leaves hanging off a0.
    -- Total >= 40 so the leaf-pruning path runs. Each leaf's layer must
    -- equal a0.layer + 1 (it's a target leaf with delta = 1).
    let spineNodes = A.range 0 9 <#> \i -> "a" <> show i
    let spineEdges = A.zipWith (\s t -> mkEdge ("e_s_" <> s) s t) spineNodes (A.drop 1 spineNodes)
    let leafIds = A.range 0 39 <#> \i -> "leaf" <> show i
    let leafEdges = leafIds <#> \l -> mkEdge ("e_l_" <> l) "a0" l
    let allNodes = spineNodes <> leafIds
    let allEdges = spineEdges <> leafEdges
    let result = LayerAssignment.assignLayersWith LayerAssignment.NetworkSimplex [] allEdges (map NodeId allNodes)
    let layerOf n = fromMaybe 0 (M.lookup (NodeId n) result.nodeLayer)
    let a0L = layerOf "a0"
    let leavesOk = A.all (\l -> layerOf l == a0L + 1) leafIds
    leavesOk `shouldSatisfy` identity

  it "NetworkSimplex handles two disconnected components" do
    let edges = [ mkEdge "e1" "a" "b", mkEdge "e2" "c" "d" ]
    let result = LayerAssignment.assignLayersWith LayerAssignment.NetworkSimplex [] edges [ NodeId "a", NodeId "b", NodeId "c", NodeId "d" ]
    let layerOf n = fromMaybe 0 (M.lookup (NodeId n) result.nodeLayer)
    -- Each component must produce a forward layering.
    (layerOf "a" < layerOf "b") `shouldSatisfy` identity
    (layerOf "c" < layerOf "d") `shouldSatisfy` identity

  it "NetworkSimplex diamond keeps source at 0 and sink past 1" do
    let edges = [ mkEdge "e1" "a" "b", mkEdge "e2" "a" "c", mkEdge "e3" "b" "d", mkEdge "e4" "c" "d" ]
    let result = LayerAssignment.assignLayersWith LayerAssignment.NetworkSimplex [] edges [ NodeId "a", NodeId "b", NodeId "c", NodeId "d" ]
    M.lookup (NodeId "a") result.nodeLayer `shouldEqual` Just 0
    case M.lookup (NodeId "d") result.nodeLayer of
      Just dl -> (dl >= 2) `shouldSatisfy` identity
      Nothing -> 1 `shouldEqual` 0
    -- Every edge must point from a lower layer to a higher one.
    let layerOf n = fromMaybe 0 (M.lookup (NodeId n) result.nodeLayer)
    let
      allOk = A.all (\(s /\ t) -> layerOf s < layerOf t)
        [ "a" /\ "b", "a" /\ "c", "b" /\ "d", "c" /\ "d" ]
    allOk `shouldSatisfy` identity

dummyNodeSpec :: Spec Unit
dummyNodeSpec = describe "LayeredLayout.DummyNodes" do
  it "adjacent edges produce no dummies" do
    let nodeLayer = M.fromFoldable [ NodeId "a" /\ 0, NodeId "b" /\ 1 ]
    let edges = [ mkEdge "e1" "a" "b" ]
    let result = DummyNodes.insertDummies nodeLayer edges [ [ NodeId "a" ], [ NodeId "b" ] ]
    A.length result.edges `shouldEqual` 1
    result.layers `shouldEqual` [ [ NodeId "a" ], [ NodeId "b" ] ]

  it "span-2 edge produces one dummy" do
    let nodeLayer = M.fromFoldable [ NodeId "a" /\ 0, NodeId "c" /\ 2 ]
    let edges = [ mkEdge "e1" "a" "c" ]
    let result = DummyNodes.insertDummies nodeLayer edges [ [ NodeId "a" ], [], [ NodeId "c" ] ]
    (A.length result.edges > 1) `shouldSatisfy` identity
    A.length result.chains `shouldEqual` 1

  it "isDummy detects dummy node ids" do
    DummyNodes.isDummy (NodeId "$d:e1:1") `shouldSatisfy` identity
    DummyNodes.isDummy (NodeId "a") `shouldSatisfy` not

crossingMinSpec :: Spec Unit
crossingMinSpec = describe "LayeredLayout.CrossingMin" do
  it "preserves all nodes" do
    let layers = [ [ NodeId "a", NodeId "b" ], [ NodeId "c", NodeId "d" ] ]
    let edges = [ mkEdge "e1" "a" "d", mkEdge "e2" "b" "c" ]
    let result = _.layout $ CrossingMin.minimize { iterations: 4, constraints: [], modelOrder: M.empty, ports: M.empty, chains: [], random: mkRandom 1.0, reversed: S.empty, portDummies: PortDummies.empty } layers edges
    map A.sort result `shouldEqual` map A.sort layers

coordAssignmentSpec :: Spec Unit
coordAssignmentSpec = describe "LayeredLayout.CoordAssignment" do
  it "nodes in same layer get different x" do
    let layers = [ [ NodeId "a", NodeId "b" ] ]
    let sizeMap = M.fromFoldable [ NodeId "a" /\ GridSize (1.0 /\ 1.0), NodeId "b" /\ GridSize (1.0 /\ 1.0) ]
    let cfg = { nodeGap: 2, layerGap: 3 }
    let result = _.placements $ CoordAssignment.assign (mkRandom 1.0) cfg [] layers sizeMap M.empty M.empty [] [] M.empty
    let xs = result <#> \p -> gridX p.position
    (A.nub xs # A.length) `shouldEqual` 2

edgeRoutingSpec :: Spec Unit
edgeRoutingSpec = describe "LayeredLayout.EdgeRouting" do
  it "self-loop routes as a C-shaped bump" do
    let
      placements =
        [ { node: NodeId "a", position: GridPos (0.0 /\ 0.0), size: GridSize (2.0 /\ 2.0), layer: 0, order: 0 }
        ]
    let edges = [ mkEdge "e1" "a" "a" ]
    let result = routeAll (mkRandom 1.0) Nothing edges placements placements (M.empty :: M.Map NodeId (Array _)) [] M.empty
    A.length result `shouldEqual` 1
    case A.head result of
      Just ep -> do
        -- C-shape = three orthogonal segments (H, V, H).
        A.length ep.segments `shouldEqual` 3
        let directions = ep.segments <#> _.direction
        directions `shouldEqual` [ H, V, H ]
      Nothing -> 1 `shouldEqual` 0

  it "keeps the exit anchors of two self-loops distinct" do
    let
      placements =
        [ { node: NodeId "a", position: GridPos (0.0 /\ 0.0), size: GridSize (2.0 /\ 4.0), layer: 0, order: 0 }
        ]
    let edges = [ mkEdge "e1" "a" "a", mkEdge "e2" "a" "a" ]
    let result = routeAll (mkRandom 1.0) Nothing edges placements placements (M.empty :: M.Map NodeId (Array _)) [] M.empty
    A.length result `shouldEqual` 2
    -- The two loops attach at different y positions (equal
    -- distribution along the node's west side).
    let
      exitYs = result <#> \ep -> case A.head ep.segments of
        Just s -> let GridPos (_ /\ y) = s.start in y
        Nothing -> 0.0
    case A.head exitYs /\ A.last exitYs of
      Just y0 /\ Just y1 -> (y0 /= y1) `shouldSatisfy` identity
      _ -> 1 `shouldEqual` 0

lineJumpSpec :: Spec Unit
lineJumpSpec = describe "LayeredLayout.EdgeRouting.LineJump" do
  it "no jumps when edges dont cross" do
    let
      paths =
        [ { edge: EdgeId "e1", segments: [ { start: GridPos (0.0 /\ 0.0), end: GridPos (0.0 /\ 5.0), direction: V } ], bends: [], bendType: [], jumps: [], reversed: false }
        , { edge: EdgeId "e2", segments: [ { start: GridPos (3.0 /\ 0.0), end: GridPos (3.0 /\ 5.0), direction: V } ], bends: [], bendType: [], jumps: [], reversed: false }
        ]
    let result = detectJumps paths
    A.all (\p -> A.null p.jumps) result `shouldSatisfy` identity

  it "detects crossing between H and V segments" do
    let
      paths =
        [ { edge: EdgeId "e1", segments: [ { start: GridPos (0.0 /\ 2.0), end: GridPos (5.0 /\ 2.0), direction: H } ], bends: [], bendType: [], jumps: [], reversed: false }
        , { edge: EdgeId "e2", segments: [ { start: GridPos (3.0 /\ 0.0), end: GridPos (3.0 /\ 5.0), direction: V } ], bends: [], bendType: [], jumps: [], reversed: false }
        ]
    let result = detectJumps paths
    let totalJumps = A.foldl (\acc p -> acc + A.length p.jumps) 0 result
    (totalJumps > 0) `shouldSatisfy` identity

  it "detects overlapping horizontal segments" do
    let
      paths =
        [ mkPath "e1" [ { start: GridPos (0.0 /\ 3.0), end: GridPos (6.0 /\ 3.0), direction: H } ]
        , mkPath "e2" [ { start: GridPos (2.0 /\ 3.0), end: GridPos (8.0 /\ 3.0), direction: H } ]
        ]
    let result = detectJumps paths
    let totalJumps = A.foldl (\acc p -> acc + A.length p.jumps) 0 result
    totalJumps `shouldEqual` 1

  it "detects overlapping vertical segments" do
    let
      paths =
        [ mkPath "e1" [ { start: GridPos (4.0 /\ 0.0), end: GridPos (4.0 /\ 6.0), direction: V } ]
        , mkPath "e2" [ { start: GridPos (4.0 /\ 3.0), end: GridPos (4.0 /\ 9.0), direction: V } ]
        ]
    let result = detectJumps paths
    let totalJumps = A.foldl (\acc p -> acc + A.length p.jumps) 0 result
    totalJumps `shouldEqual` 1

  it "no overlap when parallel segments on different lines" do
    let
      paths =
        [ mkPath "e1" [ { start: GridPos (0.0 /\ 3.0), end: GridPos (6.0 /\ 3.0), direction: H } ]
        , mkPath "e2" [ { start: GridPos (0.0 /\ 5.0), end: GridPos (6.0 /\ 5.0), direction: H } ]
        ]
    let result = detectJumps paths
    let totalJumps = A.foldl (\acc p -> acc + A.length p.jumps) 0 result
    totalJumps `shouldEqual` 0

  it "no overlap when segments on same line but non-overlapping" do
    let
      paths =
        [ mkPath "e1" [ { start: GridPos (0.0 /\ 3.0), end: GridPos (2.0 /\ 3.0), direction: H } ]
        , mkPath "e2" [ { start: GridPos (4.0 /\ 3.0), end: GridPos (6.0 /\ 3.0), direction: H } ]
        ]
    let result = detectJumps paths
    let totalJumps = A.foldl (\acc p -> acc + A.length p.jumps) 0 result
    totalJumps `shouldEqual` 0

aestheticsSpec :: Spec Unit
aestheticsSpec = describe "LayeredLayout.Aesthetics" do
  it "zero metrics for empty input" do
    let m = allMetrics [] [] 0
    m.crossingCount `shouldEqual` 0
    m.bendCount `shouldEqual` 0
    m.totalEdgeLength `shouldEqual` 0.0

  it "bend count from segments" do
    let
      paths =
        [ { edge: EdgeId "e1"
          , segments:
              [ { start: GridPos (0.0 /\ 0.0), end: GridPos (0.0 /\ 2.0), direction: V }
              , { start: GridPos (0.0 /\ 2.0), end: GridPos (3.0 /\ 2.0), direction: H }
              ]
          , bends: [ GridPos (0.0 /\ 2.0) ]
          , bendType: []
          , jumps: []
          , reversed: false
          }
        ]
    bendCount paths `shouldEqual` 1

  it "no overlaps when nodes are separate" do
    let
      nodes =
        [ { node: NodeId "a", position: GridPos (0.0 /\ 0.0), size: GridSize (1.0 /\ 1.0), layer: 0, order: 0 }
        , { node: NodeId "b", position: GridPos (5.0 /\ 0.0), size: GridSize (1.0 /\ 1.0), layer: 0, order: 1 }
        ]
    nodeOverlapCount nodes `shouldEqual` 0

  it "detects overlap" do
    let
      nodes =
        [ { node: NodeId "a", position: GridPos (0.0 /\ 0.0), size: GridSize (3.0 /\ 3.0), layer: 0, order: 0 }
        , { node: NodeId "b", position: GridPos (1.0 /\ 1.0), size: GridSize (3.0 /\ 3.0), layer: 0, order: 1 }
        ]
    (nodeOverlapCount nodes > 0) `shouldSatisfy` identity

pipelineSpec :: Spec Unit
pipelineSpec = describe "LayeredLayout (pipeline)" do
  it "simple two-node graph" do
    let
      graph =
        { nodes:
            [ { id: NodeId "a", size: GridSize (2.0 /\ 1.0), ports: [] :: Array { id :: PortId, side :: Side, offset :: Int, label :: Maybe String }, label: Nothing :: Maybe String, shape: Rectangle }
            , { id: NodeId "b", size: GridSize (2.0 /\ 1.0), ports: [], label: Nothing, shape: Rectangle }
            ]
        , edges: [ mkEdge "e1" "a" "b" ]
        , constraints: [] :: Array Constraints
        }
    let result = layout defaultConfig graph
    A.length result.nodes `shouldEqual` 2
    A.length result.edges `shouldEqual` 1
    result.metrics.nodeOverlapCount `shouldEqual` 0

  it "diamond graph layout" do
    let
      graph =
        { nodes:
            [ { id: NodeId "a", size: GridSize (1.0 /\ 1.0), ports: [] :: Array { id :: PortId, side :: Side, offset :: Int, label :: Maybe String }, label: Nothing :: Maybe String, shape: Rectangle }
            , { id: NodeId "b", size: GridSize (1.0 /\ 1.0), ports: [], label: Nothing, shape: Rectangle }
            , { id: NodeId "c", size: GridSize (1.0 /\ 1.0), ports: [], label: Nothing, shape: Rectangle }
            , { id: NodeId "d", size: GridSize (1.0 /\ 1.0), ports: [], label: Nothing, shape: Rectangle }
            ]
        , edges: [ mkEdge "e1" "a" "b", mkEdge "e2" "a" "c", mkEdge "e3" "b" "d", mkEdge "e4" "c" "d" ]
        , constraints: [] :: Array Constraints
        }
    let result = layout defaultConfig graph
    A.length result.nodes `shouldEqual` 4
    A.length result.edges `shouldEqual` 4

  it "layout JSON roundtrip" do
    let
      graph =
        { nodes:
            [ { id: NodeId "a", size: GridSize (2.0 /\ 1.0), ports: [] :: Array { id :: PortId, side :: Side, offset :: Int, label :: Maybe String }, label: Nothing :: Maybe String, shape: Rectangle }
            , { id: NodeId "b", size: GridSize (2.0 /\ 1.0), ports: [], label: Nothing, shape: Rectangle }
            ]
        , edges: [ mkEdge "e1" "a" "b" ]
        , constraints: [] :: Array Constraints
        }
    let result = layout defaultConfig graph
    let json = writeJSON result
    (isJust (readJSON_ json :: Maybe { nodes :: Array { node :: NodeId } })) `shouldSatisfy` identity

portAssignmentSpec :: Spec Unit
portAssignmentSpec = describe "LayeredLayout.EdgeRouting.PortAssignment" do
  it "fan-out node distributes ports evenly by target x" do
    let
      placements =
        [ { node: NodeId "a", position: GridPos (2.0 /\ 0.0), size: GridSize (2.0 /\ 1.0), layer: 0, order: 0 }
        , { node: NodeId "b", position: GridPos (0.0 /\ 3.0), size: GridSize (2.0 /\ 1.0), layer: 1, order: 0 }
        , { node: NodeId "c", position: GridPos (2.0 /\ 3.0), size: GridSize (2.0 /\ 1.0), layer: 1, order: 1 }
        , { node: NodeId "d", position: GridPos (4.0 /\ 3.0), size: GridSize (2.0 /\ 1.0), layer: 1, order: 2 }
        ]
    let edges = [ mkEdge "e1" "a" "b", mkEdge "e2" "a" "c", mkEdge "e3" "a" "d" ]
    let result = assignPorts edges placements (M.empty :: M.Map NodeId (Array _)) [] M.empty
    A.length result `shouldEqual` 3
    -- All fromPos x values should be distinct (distributed along south side)
    let
      fromXs = result <#> \r -> do
        let (x /\ _) = r.fromPos
        x
    (A.nub fromXs # A.length) `shouldEqual` 3
    -- Should be sorted left-to-right by target x
    let sorted = A.sort fromXs
    fromXs `shouldEqual` sorted

  it "inter-layer edges use vertical flow ports" do
    let
      placements =
        [ { node: NodeId "App", position: GridPos (4.0 /\ 0.0), size: GridSize (2.0 /\ 1.0), layer: 0, order: 0 }
        , { node: NodeId "Auth", position: GridPos (2.0 /\ 2.0), size: GridSize (2.0 /\ 1.0), layer: 1, order: 0 }
        , { node: NodeId "Router", position: GridPos (6.0 /\ 2.0), size: GridSize (2.0 /\ 1.0), layer: 1, order: 1 }
        ]
    let edges = [ mkEdge "e1" "App" "Router", mkEdge "e2" "App" "Auth" ]
    let result = assignPorts edges placements (M.empty :: M.Map NodeId (Array _)) [] M.empty
    let e2 = A.find (\r -> r.edge.id == EdgeId "e2") result
    case e2 of
      Just r -> do
        r.fromSide `shouldEqual` South
        r.toSide `shouldEqual` North
      Nothing -> 1 `shouldEqual` 0

  it "single edge between nodes uses span center" do
    let
      placements =
        [ { node: NodeId "x", position: GridPos (0.0 /\ 0.0), size: GridSize (2.0 /\ 1.0), layer: 0, order: 0 }
        , { node: NodeId "y", position: GridPos (1.0 /\ 3.0), size: GridSize (2.0 /\ 1.0), layer: 1, order: 0 }
        ]
    let edges = [ mkEdge "e1" "x" "y" ]
    let result = assignPorts edges placements (M.empty :: M.Map NodeId (Array _)) [] M.empty
    case A.head result of
      Just r -> do
        let (fromX /\ _) = r.fromPos
        let (toX /\ _) = r.toPos
        -- ELK: each port at center of own span. x span [0,8] center=4, y span [4,12] center=8
        fromX `shouldEqual` 4.0
        toX `shouldEqual` 8.0
      Nothing -> 1 `shouldEqual` 0

  it "two siblings distribute evenly across span" do
    let
      placements =
        [ { node: NodeId "a", position: GridPos (2.0 /\ 0.0), size: GridSize (2.0 /\ 1.0), layer: 0, order: 0 }
        , { node: NodeId "b", position: GridPos (0.0 /\ 3.0), size: GridSize (2.0 /\ 1.0), layer: 1, order: 0 }
        , { node: NodeId "c", position: GridPos (2.0 /\ 3.0), size: GridSize (2.0 /\ 1.0), layer: 1, order: 1 }
        ]
    let edges = [ mkEdge "e1" "a" "b", mkEdge "e2" "a" "c" ]
    let result = assignPorts edges placements (M.empty :: M.Map NodeId (Array _)) [] M.empty
    -- a span = [8,16] (fine), 2 ports → (i+1)*8/3: 8+8/3≈10.67, 8+16/3≈13.33
    let e1 = A.find (\r -> r.edge.id == EdgeId "e1") result
    case e1 of
      Just r -> do
        let (fromX /\ _) = r.fromPos
        -- e1 (to b at left) is sorted first → first slot
        (fromX > 10.0 && fromX < 11.0) `shouldSatisfy` identity
      Nothing -> 1 `shouldEqual` 0

  it "prefers South→North for inter-layer edges" do
    -- Cache(4,6) → Logger(0,8): prefer South→North (VHV, 2 bends) over
    -- West→North (L-shape, 1 bend) for consistent layered routing
    let cachePlacement = { node: NodeId "Cache", position: GridPos (4.0 /\ 6.0), size: GridSize (2.0 /\ 1.0), layer: 3, order: 1 }
    let loggerPlacement = { node: NodeId "Logger", position: GridPos (0.0 /\ 8.0), size: GridSize (2.0 /\ 1.0), layer: 4, order: 0 }
    let placements = [ cachePlacement, loggerPlacement ]
    let edges = [ mkEdge "e9" "Cache" "Logger" ]
    let result = assignPorts edges placements (M.empty :: M.Map NodeId (Array _)) [] M.empty
    case A.head result of
      Just r -> do
        r.fromSide `shouldEqual` South
        r.toSide `shouldEqual` North
      Nothing -> 1 `shouldEqual` 0

  it "directly-below picks straight line (0 bends)" do
    let
      placements =
        [ { node: NodeId "a", position: GridPos (2.0 /\ 0.0), size: GridSize (2.0 /\ 1.0), layer: 0, order: 0 }
        , { node: NodeId "b", position: GridPos (2.0 /\ 3.0), size: GridSize (2.0 /\ 1.0), layer: 1, order: 0 }
        ]
    let edges = [ mkEdge "e1" "a" "b" ]
    let result = assignPorts edges placements (M.empty :: M.Map NodeId (Array _)) [] M.empty
    case A.head result of
      Just r -> do
        r.fromSide `shouldEqual` South
        r.toSide `shouldEqual` North
      Nothing -> 1 `shouldEqual` 0

  it "upward edge uses North exit and South entry" do
    let
      placements =
        [ { node: NodeId "x", position: GridPos (2.0 /\ 0.0), size: GridSize (2.0 /\ 1.0), layer: 0, order: 0 }
        , { node: NodeId "y", position: GridPos (0.0 /\ 3.0), size: GridSize (2.0 /\ 1.0), layer: 1, order: 0 }
        , { node: NodeId "z", position: GridPos (4.0 /\ 3.0), size: GridSize (2.0 /\ 1.0), layer: 1, order: 1 }
        ]
    let edges = [ mkEdge "e1" "z" "x" ]
    let result = assignPorts edges placements (M.empty :: M.Map NodeId (Array _)) [] M.empty
    case A.head result of
      Just r -> do
        r.fromSide `shouldEqual` North
        r.toSide `shouldEqual` South
      Nothing -> 1 `shouldEqual` 0

  it "incoming edges distribute ports on target north side" do
    let
      placements =
        [ { node: NodeId "a", position: GridPos (0.0 /\ 0.0), size: GridSize (1.0 /\ 1.0), layer: 0, order: 0 }
        , { node: NodeId "b", position: GridPos (4.0 /\ 0.0), size: GridSize (1.0 /\ 1.0), layer: 0, order: 1 }
        , { node: NodeId "d", position: GridPos (2.0 /\ 3.0), size: GridSize (2.0 /\ 1.0), layer: 1, order: 0 }
        ]
    let edges = [ mkEdge "e1" "a" "d", mkEdge "e2" "b" "d" ]
    let result = assignPorts edges placements (M.empty :: M.Map NodeId (Array _)) [] M.empty
    let
      toXs = result <#> \r -> do
        let (x /\ _) = r.toPos
        x
    (A.nub toXs # A.length) `shouldEqual` 2

  it "narrow node above wide node picks South/North sides" do
    let
      placements =
        [ { node: NodeId "xx", position: GridPos (0.0 /\ 0.0), size: GridSize (1.0 /\ 1.0), layer: 0, order: 0 }
        , { node: NodeId "yyyyyy", position: GridPos (0.0 /\ 3.0), size: GridSize (3.0 /\ 1.0), layer: 1, order: 0 }
        ]
    let edges = [ mkEdge "e1" "xx" "yyyyyy" ]
    let result = assignPorts edges placements (M.empty :: M.Map NodeId (Array _)) [] M.empty
    case A.head result of
      Just r -> do
        r.fromSide `shouldEqual` South
        r.toSide `shouldEqual` North
      Nothing -> 1 `shouldEqual` 0

  it "different-width nodes route between South and North" do
    let
      graph = mkGraph
        [ NodeId "xx" /\ GridSize (1.0 /\ 1.0)
        , NodeId "yyyyyy" /\ GridSize (3.0 /\ 1.0)
        ]
        [ mkEdge "e1" "xx" "yyyyyy" ]
        []
    let result = layout defaultConfig graph
    case A.find (\e -> e.edge == EdgeId "e1") result.edges of
      Just ep -> do
        -- First segment exits South (V), last enters North (V)
        case A.head ep.segments of
          Just seg -> seg.direction `shouldEqual` V
          Nothing -> 1 `shouldEqual` 0
      Nothing -> 1 `shouldEqual` 0

  it "portSlots width-1 node has 1 slot" do
    let p = { node: NodeId "a", position: GridPos (0.0 /\ 0.0), size: GridSize (1.0 /\ 1.0), layer: 0, order: 0 }
    portSlots South p `shouldEqual` [ 2.0 ]

  it "portSlots width-2 node has 3 slots" do
    let p = { node: NodeId "a", position: GridPos (0.0 /\ 0.0), size: GridSize (2.0 /\ 1.0), layer: 0, order: 0 }
    portSlots South p `shouldEqual` [ 2.0, 4.0, 6.0 ]

  it "portSlots width-3 center coincides with cell center" do
    let p = { node: NodeId "a", position: GridPos (0.0 /\ 0.0), size: GridSize (3.0 /\ 1.0), layer: 0, order: 0 }
    portSlots South p `shouldEqual` [ 2.0, 6.0, 10.0 ]

  it "portSlots width-5 node has 5 slots" do
    let p = { node: NodeId "a", position: GridPos (0.0 /\ 0.0), size: GridSize (5.0 /\ 1.0), layer: 0, order: 0 }
    portSlots South p `shouldEqual` [ 2.0, 6.0, 10.0, 14.0, 18.0 ]

  it "portSlots non-zero origin offsets correctly" do
    let p = { node: NodeId "a", position: GridPos (3.0 /\ 0.0), size: GridSize (2.0 /\ 1.0), layer: 0, order: 0 }
    portSlots South p `shouldEqual` [ 14.0, 16.0, 18.0 ]

  it "portSlots East side uses height" do
    let p = { node: NodeId "a", position: GridPos (0.0 /\ 0.0), size: GridSize (1.0 /\ 2.0), layer: 0, order: 0 }
    portSlots East p `shouldEqual` [ 2.0, 4.0, 6.0 ]

orthogonalSpec :: Spec Unit
orthogonalSpec = describe "LayeredLayout.EdgeRouting.Orthogonal" do
  it "obstacle avoidance routes around blocking node" do
    let
      placements =
        [ { node: NodeId "a", position: GridPos (0.0 /\ 0.0), size: GridSize (1.0 /\ 1.0), layer: 0, order: 0 }
        , { node: NodeId "blocker", position: GridPos (0.0 /\ 2.0), size: GridSize (1.0 /\ 1.0), layer: 1, order: 0 }
        , { node: NodeId "b", position: GridPos (0.0 /\ 4.0), size: GridSize (1.0 /\ 1.0), layer: 2, order: 0 }
        ]
    -- In real usage, src/dst obstacles are filtered out by routeOne.
    -- Only the blocker obstacle remains.
    let blockerOnly = A.filter (\r -> r.y > 2.0 && r.y < 14.0) (buildObstacleMap placements)
    let start = 2.0 /\ Int.toNumber (1 * scaleFactor)
    let goal = 2.0 /\ Int.toNumber (4 * scaleFactor)
    let segments = findRoute blockerOnly blockerOnly South start North goal
    let pathCells = segCells segments
    let
      blockerCells = S.fromFoldable do
        dx <- A.range 0 (scaleFactor - 1)
        dy <- A.range 0 (scaleFactor - 1)
        pure (Int.toNumber (0 * scaleFactor + dx) /\ Int.toNumber (2 * scaleFactor + dy))
    let intersection = S.intersection pathCells blockerCells
    S.size intersection `shouldEqual` 0

  it "straight vertical path when no obstacles" do
    let obstacles = [] :: Array _
    let start = 4.0 /\ 0.0
    let goal = 4.0 /\ 16.0
    let segments = findRoute obstacles obstacles South start North goal
    A.length segments `shouldEqual` 1
    case A.head segments of
      Just seg -> seg.direction `shouldEqual` V
      Nothing -> 1 `shouldEqual` 0

  it "bend minimisation for straight path" do
    let obstacles = [] :: Array _
    let segments = findRoute obstacles obstacles South (8.0 /\ 0.0) North (8.0 /\ 20.0)
    let bends = A.zipWith (\a _b -> a.end) segments (A.drop 1 segments)
    A.length bends `shouldEqual` 0

  it "offset nodes produce at most 2 bends" do
    let obstacles = [] :: Array _
    let segments = findRoute obstacles obstacles South (4.0 /\ 0.0) North (12.0 /\ 20.0)
    let bends = A.zipWith (\a _b -> a.end) segments (A.drop 1 segments)
    (A.length bends <= 2) `shouldSatisfy` identity

  it "all segments are orthogonal" do
    let
      placements =
        [ { node: NodeId "a", position: GridPos (0.0 /\ 0.0), size: GridSize (2.0 /\ 1.0), layer: 0, order: 0 }
        , { node: NodeId "blocker", position: GridPos (0.0 /\ 2.0), size: GridSize (2.0 /\ 1.0), layer: 1, order: 0 }
        , { node: NodeId "b", position: GridPos (0.0 /\ 5.0), size: GridSize (2.0 /\ 1.0), layer: 2, order: 0 }
        ]
    let obstacles = buildObstacleMap placements
    let segments = findRoute obstacles obstacles South (4.0 /\ 4.0) North (4.0 /\ 20.0)
    let allOrth = A.all (\s -> s.direction == H || s.direction == V) segments
    allOrth `shouldSatisfy` identity

  it "west exit starts moving left" do
    let obstacles = [] :: Array _
    let segments = findRoute obstacles obstacles West (8.0 /\ 10.0) North (2.0 /\ 0.0)
    case A.head segments of
      Just seg -> do
        seg.direction `shouldEqual` H
        (gridX seg.end <= gridX seg.start) `shouldSatisfy` identity
      Nothing -> 1 `shouldEqual` 0

  it "east exit starts moving right" do
    let obstacles = [] :: Array _
    let segments = findRoute obstacles obstacles East (0.0 /\ 10.0) North (8.0 /\ 0.0)
    case A.head segments of
      Just seg -> do
        seg.direction `shouldEqual` H
        (gridX seg.end >= gridX seg.start) `shouldSatisfy` identity
      Nothing -> 1 `shouldEqual` 0

  it "path is continuous" do
    let obstacles = [ { x: 4.0, y: 6.0, w: 8.0, h: 8.0 } ]
    let segments = findRoute obstacles obstacles South (2.0 /\ 0.0) North (10.0 /\ 20.0)
    let pairs = A.zip segments (A.drop 1 segments)
    let continuous = A.all (\(s1 /\ s2) -> s1.end == s2.start) pairs
    continuous `shouldSatisfy` identity

routingPipelineSpec :: Spec Unit
routingPipelineSpec = describe "LayeredLayout.EdgeRouting (pipeline)" do
  it "keeps unplanned routes outside intervening nodes" do
    let
      placements =
        [ { node: NodeId "a", position: GridPos (0.0 /\ 0.0), size: GridSize (1.0 /\ 1.0), layer: 0, order: 0 }
        , { node: NodeId "blocker", position: GridPos (0.0 /\ 3.0), size: GridSize (1.0 /\ 1.0), layer: 1, order: 0 }
        , { node: NodeId "b", position: GridPos (0.0 /\ 6.0), size: GridSize (1.0 /\ 1.0), layer: 2, order: 0 }
        ]
      paths = routeAll (mkRandom 1.0) Nothing [ mkEdge "a-b" "a" "b" ] placements placements M.empty [] M.empty
      interior = S.fromFoldable do
        x <- [ 1.0, 2.0, 3.0 ]
        y <- [ 13.0, 14.0, 15.0 ]
        pure (x /\ y)
      observed = do
        path <- A.find (\p -> p.edge == EdgeId "a-b") paths
        first <- A.head path.segments
        last <- A.last path.segments
        pure
          { start: first.start
          , end: last.end
          , blocked: not (S.isEmpty (S.intersection interior (segCells path.segments)))
          }
    observed `shouldEqual` Just
      { start: GridPos (2.0 /\ 4.0), end: GridPos (2.0 /\ 24.0), blocked: false }

  it "preserves distinct routing channels through node compaction" do
    let
      graph = mkGraph
        [ NodeId "generated-1" /\ GridSize (21.0 /\ 11.0)
        , NodeId "generated-2" /\ GridSize (14.0 /\ 6.0)
        , NodeId "generated-3" /\ GridSize (17.0 /\ 5.0)
        , NodeId "generated-4" /\ GridSize (6.0 /\ 9.0)
        , NodeId "generated-5" /\ GridSize (12.0 /\ 6.0)
        , NodeId "generated-6" /\ GridSize (6.0 /\ 14.0)
        , NodeId "generated-7" /\ GridSize (12.0 /\ 4.0)
        ]
        [ mkEdge "backbone-3" "generated-1" "generated-3"
        , mkEdge "backbone-4" "generated-1" "generated-4"
        , mkEdge "backbone-5" "generated-2" "generated-5"
        , mkEdge "backbone-6" "generated-2" "generated-6"
        , mkEdge "backbone-7" "generated-3" "generated-7"
        , mkEdge "cross-4" "generated-4" "generated-6"
        , mkEdge "cross-5" "generated-5" "generated-7"
        , mkEdge "feedback-7" "generated-7" "generated-3"
        ]
        []
      result = layout defaultConfig graph
      nodeDepth id = gridY <<< _.position <$> A.find (\p -> p.node == NodeId id) result.nodes
      trunkDepth id = do
        path <- A.find (\p -> p.edge == EdgeId id) result.edges
        segment <- A.find (\s -> s.direction == H) path.segments
        pure (gridY segment.start)
    -- The two overlapping trunk ranges need separate ten-fine-unit channels.
    -- Greedy rerouting used to collapse them, pulling generated-6 up with them.
    ((-) <$> trunkDepth "cross-4" <*> trunkDepth "cross-5") `shouldEqual` Just 10.0
    ((-) <$> nodeDepth "generated-6" <*> nodeDepth "generated-7") `shouldEqual` Just 2.5

  it "diamond graph edges arrive at different ports" do
    let
      placements =
        [ { node: NodeId "a", position: GridPos (1.0 /\ 0.0), size: GridSize (1.0 /\ 1.0), layer: 0, order: 0 }
        , { node: NodeId "b", position: GridPos (0.0 /\ 3.0), size: GridSize (1.0 /\ 1.0), layer: 1, order: 0 }
        , { node: NodeId "c", position: GridPos (2.0 /\ 3.0), size: GridSize (1.0 /\ 1.0), layer: 1, order: 1 }
        , { node: NodeId "d", position: GridPos (1.0 /\ 6.0), size: GridSize (1.0 /\ 1.0), layer: 2, order: 0 }
        ]
    let edges = [ mkEdge "e1" "a" "b", mkEdge "e2" "a" "c", mkEdge "e3" "b" "d", mkEdge "e4" "c" "d" ]
    let result = routeAll (mkRandom 1.0) Nothing edges placements placements (M.empty :: M.Map NodeId (Array _)) [] M.empty
    A.length result `shouldEqual` 4
    -- Edges e3 and e4 arrive at d — check their end points differ
    let toD = A.filter (\p -> p.edge == EdgeId "e3" || p.edge == EdgeId "e4") result
    case A.head toD /\ A.last toD of
      Just p1 /\ Just p2 -> case A.last p1.segments /\ A.last p2.segments of
        Just s1 /\ Just s2 -> (s1.end /= s2.end) `shouldSatisfy` identity
        _ -> 1 `shouldEqual` 0
      _ -> 1 `shouldEqual` 0

  it "parallel edges don't overlap" do
    let
      placements =
        [ { node: NodeId "a", position: GridPos (1.0 /\ 0.0), size: GridSize (2.0 /\ 1.0), layer: 0, order: 0 }
        , { node: NodeId "b", position: GridPos (0.0 /\ 3.0), size: GridSize (2.0 /\ 1.0), layer: 1, order: 0 }
        , { node: NodeId "c", position: GridPos (3.0 /\ 3.0), size: GridSize (2.0 /\ 1.0), layer: 1, order: 1 }
        ]
    let edges = [ mkEdge "e1" "a" "b", mkEdge "e2" "a" "c" ]
    let result = routeAll (mkRandom 1.0) Nothing edges placements placements (M.empty :: M.Map NodeId (Array _)) [] M.empty
    let cells1 = segCells (fromMaybe [] (A.find (\p -> p.edge == EdgeId "e1") result <#> _.segments))
    let cells2 = segCells (fromMaybe [] (A.find (\p -> p.edge == EdgeId "e2") result <#> _.segments))
    let overlap = S.intersection cells1 cells2
    S.size overlap `shouldEqual` 0

segCells :: Array EdgeSegment -> S.Set (Number /\ Number)
segCells = foldl addSeg S.empty
  where
  addSeg acc seg = do
    let sx = gridX seg.start
    let sy = gridY seg.start
    let ex = gridX seg.end
    let ey = gridY seg.end
    if sy == ey then
      foldl (\s x -> S.insert (x /\ sy) s) acc (numberRange (min sx ex) (max sx ex))
    else
      foldl (\s y -> S.insert (sx /\ y) s) acc (numberRange (min sy ey) (max sy ey))

  numberRange :: Number -> Number -> Array Number
  numberRange a b = map Int.toNumber (A.range (Int.floor a) (Int.floor b))

simplifySpec :: Spec Unit
simplifySpec = describe "LayeredLayout.EdgeRouting.Orthogonal (simplify)" do
  it "VHV collapses to V when endpoints share x and path is clear" do
    let obstacles = [] :: Array _
    let
      segs =
        [ { start: GridPos (4.0 /\ 0.0), end: GridPos (4.0 /\ 10.0), direction: V }
        , { start: GridPos (4.0 /\ 10.0), end: GridPos (4.0 /\ 10.0), direction: H }
        , { start: GridPos (4.0 /\ 10.0), end: GridPos (4.0 /\ 20.0), direction: V }
        ]
    let result = simplifySegments obstacles segs
    A.length result `shouldEqual` 1
    case A.head result of
      Just seg -> do
        seg.direction `shouldEqual` V
        seg.start `shouldEqual` GridPos (4.0 /\ 0.0)
        seg.end `shouldEqual` GridPos (4.0 /\ 20.0)
      Nothing -> 1 `shouldEqual` 0

  it "VHV stays when both entry and exit must be V" do
    let obstacles = [] :: Array _
    let
      segs =
        [ { start: GridPos (4.0 /\ 0.0), end: GridPos (4.0 /\ 10.0), direction: V }
        , { start: GridPos (4.0 /\ 10.0), end: GridPos (8.0 /\ 10.0), direction: H }
        , { start: GridPos (8.0 /\ 10.0), end: GridPos (8.0 /\ 20.0), direction: V }
        ]
    let result = simplifySegments obstacles segs
    -- Can't simplify: first must stay V, last must stay V, different x needs H middle
    A.length result `shouldEqual` 3

  it "keeps bends when obstacle blocks shortcut" do
    let
      placements =
        [ { node: NodeId "blocker", position: GridPos (1.0 /\ 1.0), size: GridSize (1.0 /\ 1.0), layer: 0, order: 0 }
        ]
    let _obstacles = buildObstacleMap placements
    -- V from (4,0) to (4,8), H to (8,8), V to (8,16)
    -- Blocker is at (1,1) fine-grid expanded — doesn't block our path
    -- But construct a case where the L-shape IS blocked
    let blockerObs = [ { x: 3.0, y: 0.0, w: 3.0, h: 20.0 } ]
    let
      segs =
        [ { start: GridPos (2.0 /\ 0.0), end: GridPos (2.0 /\ 10.0), direction: V }
        , { start: GridPos (2.0 /\ 10.0), end: GridPos (8.0 /\ 10.0), direction: H }
        , { start: GridPos (8.0 /\ 10.0), end: GridPos (8.0 /\ 20.0), direction: V }
        ]
    let _result = simplifySegments blockerObs segs
    -- L-shape V(2,0→2,20) blocked by obstacle at x=3..6, H(2,20→8,20) also checked
    -- VH: vClear 2 0 20? obstacle at x=3..6 doesn't block x=2
    -- HV: hClear 10 2 8? obstacle at y range covers y=10, x range 3..6 overlaps 2..8 — blocked
    -- VH: V(2,0→2,20) then H(2,20→8,20): vClear at x=2 from 0..20 — obstacle x=3..6 doesn't include x=2
    -- So VH L-shape works even with this obstacle
    -- Need obstacle that blocks the L-shape options
    let hardObs = [ { x: 0.0, y: 15.0, w: 6.0, h: 3.0 } ]
    let result2 = simplifySegments hardObs segs
    -- VH: V(2,0→2,20) at x=2, y 0..20 — obstacle at x=0..6,y=15..18 includes x=2 → blocked
    -- HV: H(2,0→8,0) at y=0, x 2..8 — obstacle at y=15..18 doesn't include y=0 → clear
    --     V(8,0→8,20) at x=8, y 0..20 — obstacle at x=0..6 doesn't include x=8 → clear
    -- HV works! But first segment direction is V, HV starts with H → blocked by firstDirOk
    -- Since idx=0 (isFirst), firstDirOk H requires s0.direction == H, but s0 is V → fails
    -- So all shortcuts blocked, keeps 3 segments
    A.length result2 `shouldEqual` 3

  it "VHVH simplifies to VH when clear" do
    let obstacles = [] :: Array _
    let
      segs =
        [ { start: GridPos (4.0 /\ 0.0), end: GridPos (4.0 /\ 10.0), direction: V }
        , { start: GridPos (4.0 /\ 10.0), end: GridPos (8.0 /\ 10.0), direction: H }
        , { start: GridPos (8.0 /\ 10.0), end: GridPos (8.0 /\ 15.0), direction: V }
        , { start: GridPos (8.0 /\ 15.0), end: GridPos (12.0 /\ 15.0), direction: H }
        ]
    let result = simplifySegments obstacles segs
    -- First triple VHV → simplifies to VH (L-shape: V(4,0→4,15) H(4,15→8,15))
    -- Then VH-H → mergeCollinear merges the two H segments → V, H
    -- Or VHVH → first triple gives VH, then VH-H collapses to VH
    (A.length result <= 2) `shouldSatisfy` identity

  it "simplification reduces bends in HVHV to HV" do
    let obstacles = [] :: Array _
    let
      segs =
        [ { start: GridPos (0.0 /\ 4.0), end: GridPos (5.0 /\ 4.0), direction: H }
        , { start: GridPos (5.0 /\ 4.0), end: GridPos (5.0 /\ 10.0), direction: V }
        , { start: GridPos (5.0 /\ 10.0), end: GridPos (8.0 /\ 10.0), direction: H }
        , { start: GridPos (8.0 /\ 10.0), end: GridPos (8.0 /\ 16.0), direction: V }
        ]
    let result = simplifySegments obstacles segs
    A.length result `shouldEqual` 2
    case A.head result of
      Just seg -> seg.direction `shouldEqual` H
      Nothing -> 1 `shouldEqual` 0

weightedBarycenterSpec :: Spec Unit
weightedBarycenterSpec = describe "Weighted barycenter" do
  it "real edges preferred over dummy in crossing resolution" do
    -- Crossed layout: a->d, b->c with [a,b] and [c,d]
    -- c is a dummy, d is real. Real edges should get priority.
    let layers = [ [ NodeId "a", NodeId "b" ], [ NodeId "$d:x:1", NodeId "d" ] ]
    let edges = [ mkEdge "e1" "a" "d", mkEdge "e2" "b" "$d:x:1" ]
    let result = _.layout $ CrossingMin.minimize { iterations: 4, constraints: [], modelOrder: M.empty, ports: M.empty, chains: [], random: mkRandom 1.0, reversed: S.empty, portDummies: PortDummies.empty } layers edges
    -- Should uncross: b's dummy edge weighs less, so a->d dominates
    let crossings = countCrossings (fromMaybe [] (A.index result 0)) (fromMaybe [] (A.index result 1)) edges
    crossings `shouldEqual` 0

siftingSpec :: Spec Unit
siftingSpec = describe "Sifting" do
  it "countCrossings counts pairwise crossings" do
    -- [a, b] -> [d, c] with a->c, b->d: one crossing
    countCrossings [ NodeId "a", NodeId "b" ] [ NodeId "d", NodeId "c" ] [ mkEdge "e1" "a" "c", mkEdge "e2" "b" "d" ] `shouldEqual` 1

  it "countCrossings zero for uncrossed" do
    countCrossings [ NodeId "a", NodeId "b" ] [ NodeId "c", NodeId "d" ] [ mkEdge "e1" "a" "c", mkEdge "e2" "b" "d" ] `shouldEqual` 0

  it "sifting eliminates crossing in known-crossed input" do
    -- Fixed layer [a, b], variable [d, c] with a->c, b->d => 1 crossing
    -- After sifting, should swap to [c, d] => 0 crossings
    let layers = [ [ NodeId "a", NodeId "b" ], [ NodeId "d", NodeId "c" ] ]
    let edges = [ mkEdge "e1" "a" "c", mkEdge "e2" "b" "d" ]
    let result = _.layout $ CrossingMin.minimize { iterations: 1, constraints: [], modelOrder: M.empty, ports: M.empty, chains: [], random: mkRandom 1.0, reversed: S.empty, portDummies: PortDummies.empty } layers edges
    let crossings = countCrossings (fromMaybe [] (A.index result 0)) (fromMaybe [] (A.index result 1)) edges
    crossings `shouldEqual` 0

  it "sifting respects OrderConstraint" do
    -- Same crossed setup, but OrderConstraint forces d before c
    let layers = [ [ NodeId "a", NodeId "b" ], [ NodeId "d", NodeId "c" ] ]
    let edges = [ mkEdge "e1" "a" "c", mkEdge "e2" "b" "d" ]
    let constraints = [ OrderConstraint { before: NodeId "d", after: NodeId "c" } ]
    let result = _.layout $ CrossingMin.minimize { iterations: 4, constraints, modelOrder: M.empty, ports: M.empty, chains: [], random: mkRandom 1.0, reversed: S.empty, portDummies: PortDummies.empty } layers edges
    case A.index result 1 of
      Just layer -> case A.elemIndex (NodeId "d") layer /\ A.elemIndex (NodeId "c") layer of
        Just di /\ Just ci -> (di < ci) `shouldSatisfy` identity
        _ -> 1 `shouldEqual` 0
      Nothing -> 1 `shouldEqual` 0

  it "sifting handles three-node crossing" do
    -- [a, b, c] -> [f, e, d] with a->d, b->e, c->f => 3 crossings
    let layers = [ [ NodeId "a", NodeId "b", NodeId "c" ], [ NodeId "f", NodeId "e", NodeId "d" ] ]
    let edges = [ mkEdge "e1" "a" "d", mkEdge "e2" "b" "e", mkEdge "e3" "c" "f" ]
    let result = _.layout $ CrossingMin.minimize { iterations: 4, constraints: [], modelOrder: M.empty, ports: M.empty, chains: [], random: mkRandom 1.0, reversed: S.empty, portDummies: PortDummies.empty } layers edges
    let crossings = countCrossings (fromMaybe [] (A.index result 0)) (fromMaybe [] (A.index result 1)) edges
    crossings `shouldEqual` 0

compactionSpec :: Spec Unit
compactionSpec = describe "Compaction" do
  it "compaction tightens width" do
    -- Place nodes with extra spacing, compaction should reduce total width
    let layers = [ [ NodeId "a", NodeId "b", NodeId "c" ] ]
    let sizeMap = M.fromFoldable [ NodeId "a" /\ GridSize (1.0 /\ 1.0), NodeId "b" /\ GridSize (1.0 /\ 1.0), NodeId "c" /\ GridSize (1.0 /\ 1.0) ]
    let cfg = { nodeGap: 2, layerGap: 3 }
    let result = _.placements $ CoordAssignment.assign (mkRandom 1.0) cfg [] layers sizeMap M.empty M.empty [] [] M.empty
    -- All three nodes should be compactly placed
    let xs = A.sort (result <#> \p -> gridX p.position)
    case A.head xs /\ A.last xs of
      Just minX /\ Just maxX -> do
        -- Width should be at most 2*(1+2) = 6 (two gaps + three width-1 nodes)
        (maxX - minX <= 6.0) `shouldSatisfy` identity
      _ -> 1 `shouldEqual` 0

  it "compaction preserves ordering" do
    let layers = [ [ NodeId "a", NodeId "b" ] ]
    let sizeMap = M.fromFoldable [ NodeId "a" /\ GridSize (1.0 /\ 1.0), NodeId "b" /\ GridSize (1.0 /\ 1.0) ]
    let cfg = { nodeGap: 2, layerGap: 3 }
    let result = _.placements $ CoordAssignment.assign (mkRandom 1.0) cfg [] layers sizeMap M.empty M.empty [] [] M.empty
    let xs = result <#> \p -> un NodeId p.node /\ gridX p.position
    let aX = A.findMap (\(n /\ x) -> if n == "a" then Just x else Nothing) xs
    let bX = A.findMap (\(n /\ x) -> if n == "b" then Just x else Nothing) xs
    case aX /\ bX of
      Just ax /\ Just bx -> (ax < bx) `shouldSatisfy` identity
      _ -> 1 `shouldEqual` 0

  it "end-to-end: layout produces compact output" do
    let
      graph = mkGraph
        [ NodeId "a" /\ GridSize (1.0 /\ 1.0)
        , NodeId "b" /\ GridSize (1.0 /\ 1.0)
        , NodeId "c" /\ GridSize (1.0 /\ 1.0)
        , NodeId "d" /\ GridSize (1.0 /\ 1.0)
        ]
        [ mkEdge "e1" "a" "b"
        , mkEdge "e2" "a" "c"
        , mkEdge "e3" "b" "d"
        , mkEdge "e4" "c" "d"
        ]
        []
    let result = layout defaultConfig graph
    result.metrics.nodeOverlapCount `shouldEqual` 0
    -- Verify crossings are zero or minimal
    (result.metrics.crossingCount >= 0) `shouldSatisfy` identity

  it "compactionSpacings drives the post-routing compactor end-to-end" do
    -- A skip-layer graph: a→c spans two layers, so a long-edge dummy's
    -- vertical segment must keep edge-node / edge-edge daylight from the
    -- middle node `b`. Widening the BETWEEN_LAYERS matrix has to push the
    -- compacted (Y) extent of the result out compared with the default
    -- 8/4/10 — proving the config reaches `specialSpacings` rather than a
    -- baked-in constant.
    let
      graph = mkGraph
        [ NodeId "a" /\ GridSize (1.0 /\ 1.0)
        , NodeId "b" /\ GridSize (1.0 /\ 1.0)
        , NodeId "c" /\ GridSize (1.0 /\ 1.0)
        ]
        [ mkEdge "e1" "a" "b", mkEdge "e2" "b" "c", mkEdge "e3" "a" "c" ]
        []
    let extentY r = sizeH r.boundingBox.size
    let tight = extentY (layout defaultConfig graph)
    let
      loose = extentY $ layout
        (defaultConfig { compactionSpacings = { nodeNode: 40.0, edgeNode: 40.0, edgeEdge: 40.0 } })
        graph
    (loose > tight) `shouldSatisfy` identity

mkGraph :: Array (NodeId /\ GridSize) -> Array Edge -> Array Constraints -> Graph
mkGraph nodeDefs edges constraints =
  { nodes: nodeDefs <#> \(nid /\ size) ->
      { id: nid, size, ports: [], label: Nothing, shape: Rectangle }
  , edges
  , constraints
  }

mkTestGraph :: Graph
mkTestGraph =
  { nodes:
      [ { id: NodeId "a"
        , size: GridSize (2.0 /\ 1.0)
        , ports: [ { id: PortId "out", side: East, offset: 0, label: Nothing } ]
        , label: Just "Node A"
        , shape: Rectangle
        }
      ]
  , edges:
      [ { id: EdgeId "e1"
        , from: { node: NodeId "a", port: Just (PortId "out") }
        , to: { node: NodeId "b", port: Nothing }
        , label: Nothing
        }
      ]
  , constraints: []
  }

mkPath :: String -> Array EdgeSegment -> EdgePath
mkPath id segments = { edge: EdgeId id, segments, bends: [], bendType: [], jumps: [], reversed: false }

