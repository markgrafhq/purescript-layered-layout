module Test.NodePlacementSpec (nodePlacementSpec) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Set as S
import Data.Tuple.Nested (type (/\), (/\))
import LayeredLayout (defaultConfig, layout)
import LayeredLayout.Compaction.EdgeAwareScanlineConstraints (scanlineConstraints)
import LayeredLayout.Compaction.HorizontalGraphCompactor as Compaction
import LayeredLayout.Compaction.OneD as OneD
import LayeredLayout.CoordAssignment as CoordAssignment
import LayeredLayout.Graph (Edge, EdgeId(..), Label(..), NodeId(..), PortId(..), Shape(..), Side(..))
import LayeredLayout.Grid (GridPos(..), GridSize(..), gridX, gridY, sizeH, sizeW)
import LayeredLayout.EdgeRouting.Orthogonal as Orthogonal
import LayeredLayout.Result as Result
import LayeredLayout.JavaRandom (mkRandom)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

nodePlacementSpec :: Spec Unit
nodePlacementSpec = describe "BK node placement" do
  it "aligns parallel edges using the current endpoint's physical port order" do
    let
      layers = [ [ NodeId "a" ], [ NodeId "b", NodeId "c" ] ]
      sizes = M.fromFoldable $ [ "a", "b", "c" ] <#> \n -> NodeId n /\ GridSize (4.0 /\ 5.0)
      edges = [ edge "ab" "a" "b", edge "ac1" "a" "c", edge "ac2" "a" "c" ]
      offsets = M.fromFoldable
        [ (EdgeId "ab" /\ South) /\ 4.0
        , (EdgeId "ab" /\ North) /\ 8.0
        , (EdgeId "ac1" /\ South) /\ 8.0
        , (EdgeId "ac1" /\ North) /\ (16.0 / 3.0)
        , (EdgeId "ac2" /\ South) /\ 12.0
        , (EdgeId "ac2" /\ North) /\ (32.0 / 3.0)
        ]
      positions = CoordAssignment.assignFine { nodeGap: 3, layerGap: 2 } layers sizes M.empty M.empty edges offsets
    -- Upstream BK: b-a = -26, c-a = 2. Choosing the first global
    -- edge in both directions instead gives -25 1/3 and 2 2/3.
    relativePositions positions "a" [ "b" /\ (-26.0), "c" /\ 2.0 ] `shouldSatisfy` identity

  it "retains incident-edge order when parallel edges share a fixed port" do
    let
      layers = [ [ NodeId "a" ], [ NodeId "b", NodeId "c" ] ]
      sizes = M.fromFoldable $ [ "a", "b", "c" ] <#> \n -> NodeId n /\ GridSize (4.0 /\ 5.0)
      shared = { node: NodeId "c", port: Just (PortId "shared") }
      edges =
        [ edge "ab" "a" "b"
        , (edge "ac1" "a" "c") { to = shared }
        , (edge "ac2" "a" "c") { to = shared }
        ]
      ports = M.singleton (NodeId "c")
        [ { id: PortId "shared", side: North, offset: 2, label: Nothing } ]
      offsets = M.fromFoldable
        [ (EdgeId "ab" /\ South) /\ 4.0
        , (EdgeId "ab" /\ North) /\ 8.0
        , (EdgeId "ac1" /\ South) /\ 8.0
        , (EdgeId "ac2" /\ South) /\ 12.0
        ]
      positions = CoordAssignment.assignFine { nodeGap: 3, layerGap: 2 } layers sizes M.empty ports edges offsets
    relativePositions positions "a" [ "b" /\ (-28.0), "c" /\ 0.0 ] `shouldSatisfy` identity

  it "keeps noncrossing blocks aligned on both sides of a long-edge segment" do
    let
      layers = map (map NodeId)
        [ [ "a" ]
        , [ "b", "$d:span:1", "c" ]
        , [ "d", "$d:span:2", "e" ]
        , [ "f", "g" ]
        ]
      sizes = M.fromFoldable $
        [ "a" /\ 4.0
        , "b" /\ 22.0
        , "c" /\ 4.0
        , "d" /\ 4.0
        , "e" /\ 4.0
        , "f" /\ 16.0
        , "g" /\ 4.0
        ] <#> \(n /\ width) -> NodeId n /\ GridSize (width /\ 5.0)
      connections =
        [ { id: "ab", from: "a", to: "b", source: 4.0, target: 44.0 }
        , { id: "span0", from: "a", to: "$d:span:1", source: 8.0, target: 0.0 }
        , { id: "ac", from: "a", to: "c", source: 12.0, target: 8.0 }
        , { id: "bd", from: "b", to: "d", source: 44.0, target: 8.0 }
        , { id: "span1", from: "$d:span:1", to: "$d:span:2", source: 0.0, target: 0.0 }
        , { id: "ce", from: "c", to: "e", source: 8.0, target: 8.0 }
        , { id: "df", from: "d", to: "f", source: 16.0 / 3.0, target: 32.0 }
        , { id: "dg", from: "d", to: "g", source: 32.0 / 3.0, target: 4.0 }
        , { id: "span2", from: "$d:span:2", to: "g", source: 0.0, target: 8.0 }
        , { id: "eg", from: "e", to: "g", source: 8.0, target: 12.0 }
        ]
      edges = connections <#> \e -> edge e.id e.from e.to
      offsets = M.fromFoldable $ A.concatMap
        (\e -> [ (EdgeId e.id /\ South) /\ e.source, (EdgeId e.id /\ North) /\ e.target ])
        connections
      positions = CoordAssignment.assignFine { nodeGap: 3, layerGap: 2 } layers sizes M.empty M.empty edges offsets
    -- The left block b-d-f must not be reconsidered against the right
    -- interval's boundary. Upstream BK gives these unrounded offsets.
    relativePositions positions "b"
      [ "a" /\ 90.0
      , "c" /\ 109.0
      , "d" /\ 36.0
      , "e" /\ 109.0
      , "f" /\ (28.0 / 3.0)
      , "g" /\ 90.0
      ] `shouldSatisfy` identity

  it "selects the balanced reference using node extents rather than origins" do
    let
      positions = fixturePositions M.empty
        [ [ "a" ], [ "b", "$d:span:1" ], [ "c", "d", "$d:span:2" ], [ "e", "f" ] ]
        [ "a" /\ 84.0, "b" /\ 88.0, "c" /\ 72.0, "d" /\ 28.0, "e" /\ 64.0, "f" /\ 44.0 ]
        [ connection "a" "b" 50.4 (176.0 / 3.0)
        , connection "a" "$d:span:1" 67.2 0.0
        , connection "b" "d" 66.0 (56.0 / 3.0)
        , connection "$d:span:1" "$d:span:2" 0.0 0.0
        , connection "c" "e" 18.0 32.0
        , connection "c" "f" 36.0 8.8
        , connection "c" "f" 54.0 17.6
        , connection "d" "f" 14.0 26.4
        , connection "$d:span:2" "f" 0.0 35.2
        ]
    relativePositions positions "e"
      [ "a" /\ (197.0 / 3.0)
      , "b" /\ (1283.0 / 30.0)
      , "c" /\ 6.1
      , "d" /\ 90.1
      , "f" /\ 80.8
      ] `shouldSatisfy` identity

  it "straightens late blocks using incoming physical-port order" do
    let
      positions = fixturePositions M.empty
        [ [ "a" ]
        , [ "$d:left:1", "b" ]
        , [ "$d:left:2", "c", "d", "$d:right:1" ]
        , [ "e", "f", "g" ]
        ]
        [ "a" /\ 48.0
        , "b" /\ 60.0
        , "c" /\ 80.0
        , "d" /\ 28.0
        , "e" /\ 56.0
        , "f" /\ 24.0
        , "g" /\ 28.0
        ]
        [ connection "a" "$d:left:1" 12.0 0.0
        , connection "$d:left:1" "$d:left:2" 0.0 0.0
        , connection "b" "d" 15.0 14.0
        , connection "b" "$d:right:1" 45.0 0.0
        , connection "$d:left:2" "f" 0.0 6.0
        , connection "c" "e" (80.0 / 3.0) 28.0
        , connection "c" "f" (160.0 / 3.0) 12.0
        , connection "d" "f" 14.0 18.0
        , connection "$d:right:1" "g" 0.0 21.0
        ]
    relativePositions positions "a"
      [ "b" /\ 114.5
      , "c" /\ 23.0
      , "d" /\ 115.0
      , "e" /\ (19.0 / 3.0)
      , "f" /\ 85.0
      , "g" /\ 142.5
      ] `shouldSatisfy` identity

  it "balances physical node bounds without discarding reserved margins" do
    let
      margins = M.singleton (NodeId "a") { left: 10.0, right: 0.0, top: 0.0, bottom: 0.0 }
      positions = fixturePositions margins
        [ [ "a" ], [ "b", "c" ], [ "d" ] ]
        [ "a" /\ 74.0, "b" /\ 16.0, "c" /\ 8.0, "d" /\ 88.0 ]
        [ connection "a" "b" (94.0 / 3.0) 8.0
        , connection "a" "c" (158.0 / 3.0) 4.0
        , connection "b" "d" 8.0 (88.0 / 3.0)
        , connection "c" "d" 4.0 (176.0 / 3.0)
        ]
    relativePositions positions "d"
      [ "a" /\ 2.0, "b" /\ (64.0 / 3.0), "c" /\ (164.0 / 3.0) ] `shouldSatisfy` identity

  it "places zero-width side-port dummies with edge-to-node clearance" do
    let
      positions = fixturePositions M.empty
        [ [ "a", "$port:a:out" ], [ "b" ] ]
        [ "a" /\ 16.0, "$port:a:out" /\ 0.0, "b" /\ 16.0 ]
        [ connection "$port:a:out" "b" 0.0 8.0 ]
    -- A fixed EAST port on a DOWN graph produces this NS dummy.
    -- Upstream puts b 18 fine units right of a; treating the dummy
    -- as a normal node gives 20.
    relativePositions positions "a"
      [ "$port:a:out" /\ 26.0, "b" /\ 18.0 ] `shouldSatisfy` identity

  it "aligns unequal node depths by physical ports rather than parallel-edge counts" do
    let
      layers = map (map NodeId) [ [ "a" ], [ "b", "c" ], [ "d" ] ]
      sizes = M.fromFoldable $ [ "a", "b", "c", "d" ] <#> \n ->
        NodeId n /\ GridSize (4.0 /\ if n == "c" then 10.0 else 5.0)
      shared = { node: NodeId "b", port: Just (PortId "shared") }
      edges =
        [ edge "ab" "a" "b"
        , edge "ac" "a" "c"
        , (edge "bd1" "b" "d") { from = shared }
        , (edge "bd2" "b" "d") { from = shared }
        , edge "cd" "c" "d"
        ]
      ports = M.singleton (NodeId "b")
        [ { id: PortId "shared", side: South, offset: 2, label: Nothing } ]
      placed = _.placements $ CoordAssignment.assign (mkRandom 1.0) { nodeGap: 3, layerGap: 2 }
        []
        layers
        sizes
        M.empty
        ports
        edges
        []
        M.empty
      depths = M.fromFoldable $ placed <#> \n -> n.node /\ (4.0 * gridY n.position)
    -- Upstream places b ten fine units below c. Top alignment gives
    -- zero; counting both edges on the shared port gives 13 1/3.
    relativePositions depths "c" [ "b" /\ 10.0 ] `shouldSatisfy` identity

  it "keeps canonical feedback endpoints attached when owners compact differently" do
    let
      placement node y layer =
        { node: NodeId node, position: GridPos (0.0 /\ y), size: GridSize (5.0 /\ 5.0), layer, order: 0 }
      feedback =
        { edge: EdgeId "feedback"
        , segments: [ { start: GridPos (10.0 /\ 100.0), end: GridPos (10.0 /\ 20.0), direction: Result.V } ]
        , bends: []
        , bendType: []
        , jumps: []
        , reversed: true
        }
      compacted = Compaction.compactPostRouting Compaction.EdgeLength
        { nodeNode: 12.0, edgeEdge: 10.0 }
        Compaction.defaultBetweenLayersSpacings
        { nodes: [ placement "a" 0.0 0, placement "b" 25.0 1 ]
        , edges: [ edge "feedback" "b" "a" ]
        , paths: [ feedback ]
        , ports: M.empty
        }
      endpoints = do
        path <- A.head compacted.edges
        segment <- A.head path.segments
        pure (gridY segment.start /\ gridY segment.end)
    -- b moves from y100 to y28 while a stays at y0. `reversed`
    -- records cycle breaking, not a reversal of the canonical path.
    endpoints `shouldEqual` Just (28.0 /\ 20.0)

  it "does not create compaction barriers from empty transverse intervals" do
    let
      hitboxes =
        [ { x: 0.0, y: 0.0, width: 10.0, height: 10.0 }
        , { x: 20.0, y: -10.0, width: 0.0, height: 0.0 }
        , { x: 40.0, y: 20.0, width: 10.0, height: 10.0 }
        ]
      add acc hitbox =
        let
          added = OneD.addCNode { origin: Nothing :: Maybe Unit, kind: Nothing, hitbox } acc
        in
          (OneD.addCGroup { master: Just added.id, nodes: [ added.id ] } added.graph).graph
      graph = foldl add (OneD.newCGraph S.empty) hitboxes
      constrained = OneD.runConstraintAlgorithm scanlineConstraints (OneD.newOneD graph)
    -- High-before-low tie breaking would remove the empty interval
    -- before inserting it, leaving a phantom obstacle in the scanline.
    A.concatMap _.constraints (OneD.allCNodes constrained) `shouldEqual` []

  it "preserves both master-frame and stricter loop-segment clearances" do
    -- A contained loop segment must not hide its owner's reserved
    -- label frame; nor may the frame replace stronger edge-edge spacing.
    map (\(frameGap /\ _) -> frameGap >= 4.0) (groupedLoopClearances (52.0 / 3.0))
      `shouldEqual` Just true
    map (\(_ /\ loopGap) -> loopGap >= 10.0) (groupedLoopClearances 2.0)
      `shouldEqual` Just true

  it "keeps a measured loop label clear of unrelated routes after full layout" do
    let
      name id = "generated-" <> show id
      node id w h =
        { id: NodeId (name id)
        , size: GridSize (w / 4.0 /\ h / 4.0)
        , ports: []
        , label: Nothing
        , shape: Rectangle
        }
      measured id w h = EdgeId id /\ GridSize (w / 4.0 /\ h / 4.0)
      sizes = M.fromFoldable
        [ measured "backbone-5" 52.0 48.0
        , measured "backbone-9" 60.0 24.0
        , measured "cross-1" 80.0 48.0
        , measured "cross-4" 52.0 20.0
        , measured "cross-9" 36.0 4.0
        , measured "feedback-5" 88.0 12.0
        , measured "loop-8" 112.0 44.0
        ]
      link id from to =
        let
          e = edge id (name from) (name to)
        in
          if M.member e.id sizes then e { label = Just (Label ("CENTER " <> id)) } else e
      graph =
        { nodes:
            [ node 0 48.0 52.0
            , node 1 88.0 40.0
            , node 2 52.0 52.0
            , node 3 32.0 56.0
            , node 4 76.0 52.0
            , node 5 72.0 24.0
            , node 6 40.0 40.0
            , node 8 64.0 52.0
            , node 9 56.0 32.0
            , node 10 48.0 36.0
            , node 11 64.0 28.0
            ]
        , edges:
            [ link "backbone-5" 3 5
            , link "backbone-9" 8 9
            , link "cross-0" 0 4
            , link "cross-1" 1 3
            , link "cross-2" 2 9
            , link "cross-3" 3 9
            , link "cross-4" 4 6
            , link "cross-9" 9 11
            , link "feedback-5" 5 2
            , link "feedback-10" 10 0
            , link "parallel-6" 2 6
            , link "loop-8" 8 8
            ]
        , constraints: []
        }
      result = layout (defaultConfig { edgeLabelSizes = sizes }) graph
      clear = do
        label <- A.find (\box -> box.edge == EdgeId "loop-8") result.edgeLabels
        route <- A.find (\path -> path.edge == EdgeId "cross-3") result.edges
        _ <- A.head route.segments
        pure $ Orthogonal.isRouteClear
          [ { x: gridX label.position, y: gridY label.position, w: sizeW label.size, h: sizeH label.size } ]
          route.segments
    clear `shouldEqual` Just true

  it "includes implicit self-loop segments when compacting incident branches" do
    let
      compacted = Compaction.compactPostRouting Compaction.EdgeLength
        { nodeNode: 12.0, edgeEdge: 10.0 }
        Compaction.defaultBetweenLayersSpacings
        { nodes:
            [ { node: NodeId "a", position: GridPos (8.0 /\ 0.0), size: GridSize (22.5 /\ 4.0), layer: 0, order: 0 }
            , { node: NodeId "b", position: GridPos (0.0 /\ 8.5), size: GridSize (8.0 /\ 8.0), layer: 1, order: 0 }
            ]
        , edges: [ edge "loop" "a" "a", edge "ab" "a" "b" ]
        , paths:
            [ orthogonalPath "ab" [ 58.0 /\ 16.0, 58.0 /\ 20.0, 16.0 /\ 20.0, 16.0 /\ 34.0 ]
            , orthogonalPath "loop"
                [ 42.0 /\ (16.0 / 3.0)
                , 32.0 /\ (16.0 / 3.0)
                , 32.0 /\ (32.0 / 3.0)
                , 42.0 /\ (32.0 / 3.0)
                ]
            ]
        , ports: M.empty
        }
      depths = M.fromFoldable $ compacted.nodes <#> \n -> n.node /\ (4.0 * gridY n.position)
    -- The fractional loop port reserves one additional fine unit.
    -- Omitting loop segments or assuming SOUTH/NORTH ports gives 24.
    relativePositions depths "a" [ "b" /\ 25.0 ] `shouldSatisfy` identity

orthogonalPath :: String -> Array (Number /\ Number) -> Result.EdgePath
orthogonalPath id coordinates =
  let
    points = map GridPos coordinates
  in
    { edge: EdgeId id
    , segments: A.zipWith
        (\start end -> { start, end, direction: if gridY start == gridY end then Result.H else Result.V })
        points
        (A.drop 1 points)
    , bends: A.dropEnd 1 (A.drop 1 points)
    , bendType: []
    , jumps: []
    , reversed: false
    }

groupedLoopClearances :: Number -> Maybe (Number /\ Number)
groupedLoopClearances offset = do
  owner <- A.find (\n -> n.node == NodeId "b") compacted.nodes
  cross <- A.find (\path -> path.edge == EdgeId "cross") compacted.edges >>= \path -> A.head path.bends
  loop <- A.find (\path -> path.edge == EdgeId "loop") compacted.edges >>= \path -> A.head path.bends
  pure ((4.0 * gridY owner.position - gridY cross) /\ (gridY loop - gridY cross))
  where
  node id x y w h layer =
    { node: NodeId id
    , position: GridPos (x / 4.0 /\ y / 4.0)
    , size: GridSize (w / 4.0 /\ h / 4.0)
    , layer
    , order: 0
    }
  compacted = Compaction.compactPostRouting Compaction.EdgeLength
    { nodeNode: 12.0, edgeEdge: 10.0 }
    Compaction.defaultBetweenLayersSpacings
    { nodes:
        [ node "a" 200.0 (-10.0) 20.0 10.0 0
        , node "b" 0.0 20.0 188.0 52.0 1
        , node "c" 20.0 120.0 40.0 10.0 2
        ]
    , edges: [ edge "cross" "a" "c", edge "loop" "b" "b" ]
    , paths:
        [ orthogonalPath "cross"
            [ 210.0 /\ 0.0
            , 210.0 /\ 10.0
            , (-11.0) /\ 10.0
            , (-11.0) /\ 100.0
            , (281.0 / 6.0) /\ 100.0
            , (281.0 / 6.0) /\ 120.0
            ]
        , orthogonalPath "loop"
            [ 124.0 /\ (20.0 + offset)
            , 114.0 /\ (20.0 + offset)
            , 114.0 /\ (72.0 - offset)
            , 124.0 /\ (72.0 - offset)
            ]
        ]
    , ports: M.empty
    }

type Connection = { from :: String, to :: String, source :: Number, target :: Number }

connection :: String -> String -> Number -> Number -> Connection
connection from to source target = { from, to, source, target }

fixturePositions
  :: CoordAssignment.NodeMargins
  -> Array (Array String)
  -> Array (String /\ Number)
  -> Array Connection
  -> M.Map NodeId Number
fixturePositions margins layers widths connections =
  CoordAssignment.assignFine { nodeGap: 3, layerGap: 2 }
    (map (map NodeId) layers)
    sizes
    margins
    M.empty
    edges
    offsets
  where
  sizes = M.fromFoldable $ widths <#> \(n /\ width) -> NodeId n /\ GridSize (width / 4.0 /\ 5.0)
  edges = A.mapWithIndex (\i e -> edge (show i) e.from e.to) connections
  offsets = M.fromFoldable $ A.concat $ A.mapWithIndex
    (\i e -> [ (EdgeId (show i) /\ South) /\ e.source, (EdgeId (show i) /\ North) /\ e.target ])
    connections

edge :: String -> String -> String -> Edge
edge id from to =
  { id: EdgeId id
  , from: { node: NodeId from, port: Nothing }
  , to: { node: NodeId to, port: Nothing }
  , label: Nothing
  }

relativePositions :: M.Map NodeId Number -> String -> Array (String /\ Number) -> Boolean
relativePositions positions anchor expected = case M.lookup (NodeId anchor) positions of
  Nothing -> false
  Just origin -> A.all
    ( \(node /\ x) -> case M.lookup (NodeId node) positions of
        Nothing -> false
        Just actual -> abs (actual - origin - x) < 0.000001
    )
    expected
