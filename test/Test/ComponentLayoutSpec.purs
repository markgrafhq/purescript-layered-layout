module Test.ComponentLayoutSpec (componentLayoutSpec) where

import Prelude

import Data.Array as A
import Data.Foldable (traverse_)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import LayeredLayout (defaultConfig, fromCoords, fromCrossMin, fromDummies, fromRouting, full)
import LayeredLayout.Components as Components
import LayeredLayout.EdgeLabels as EdgeLabels
import LayeredLayout.Graph (Constraints(..), Edge, EdgeId(..), Graph, NodeId(..), PortId(..), Shape(..), Side(..))
import LayeredLayout.Grid (GridPos(..), GridSize(..), gridX, gridY, sizeW)
import LayeredLayout.EdgeRouting.Orthogonal as Orthogonal
import LayeredLayout.Result (Direction(..), EdgePath)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

componentLayoutSpec :: Spec Unit
componentLayoutSpec = describe "Independent component layout" do
  it "packs two chains with component spacing rather than joining their layers" do
    let result = (full defaultConfig chains).result
    let y id = gridY <<< _.position <$> A.find (\n -> n.node == NodeId id) result.nodes
    ((-) <$> y "b" <*> y "a") `shouldEqual` Just 7.0
    ((-) <$> y "c" <*> y "a") `shouldEqual` Just 17.0
    ((-) <$> y "d" <*> y "a") `shouldEqual` Just 24.0

  it "accepts either exact inflated-obstacle boundary while rejecting its interior" do
    let obstacle = [ { x: 2.0, y: 2.0, w: 4.0, h: 4.0 } ]
    let vertical x = [ { start: GridPos (x /\ 0.0), end: GridPos (x /\ 8.0), direction: V } ]
    let horizontal y = [ { start: GridPos (0.0 /\ y), end: GridPos (8.0 /\ y), direction: H } ]
    Orthogonal.isRouteClear obstacle (vertical 2.0) `shouldEqual` true
    Orthogonal.isRouteClear obstacle (vertical 6.0) `shouldEqual` true
    Orthogonal.isRouteClear obstacle (vertical 3.0) `shouldEqual` false
    Orthogonal.isRouteClear obstacle (horizontal 2.0) `shouldEqual` true
    Orthogonal.isRouteClear obstacle (horizontal 6.0) `shouldEqual` true
    Orthogonal.isRouteClear obstacle (horizontal 3.0) `shouldEqual` false

  it "keeps labels and routes attached through cached component reruns" do
    let config = defaultConfig { edgeLabels = M.fromFoldable [ EdgeId "ab" /\ { size: GridSize (7.0 /\ 3.0), placement: EdgeLabels.Center }, EdgeId "cd" /\ { size: GridSize (5.0 /\ 2.0), placement: EdgeLabels.Center } ] }
    let initial = full config chains
    let reruns = [ (fromDummies config chains initial.pipeline).result, (fromCrossMin config chains initial.pipeline).result, (fromCoords config chains initial.pipeline).result, fromRouting config chains initial.pipeline ]
    traverse_
      ( \result -> do
          result.nodes `shouldEqual` initial.result.nodes
          result.edges `shouldEqual` initial.result.edges
          result.edgeLabels `shouldEqual` initial.result.edgeLabels
          result.boundingBox `shouldEqual` initial.result.boundingBox
      )
      reruns
  it "centers a source-owned tail label in its final bent terminal run" do
    let
      graph = chains { nodes = A.take 2 chains.nodes, edges = [ edge "ab" "a" "b" ] }
      specs = M.singleton (EdgeId "ab")
        { size: GridSize (4.0 /\ 2.0)
        , placement: EdgeLabels.Tail EdgeLabels.CenterTerminalRun
        }
      labels = EdgeLabels.insert specs graph graph.edges
      nodes =
        [ { node: NodeId "a", position: GridPos (0.0 /\ 0.0), size: GridSize (10.0 /\ 5.0), layer: 0, order: 0 }
        , { node: NodeId "b", position: GridPos (20.0 /\ 20.0), size: GridSize (10.0 /\ 5.0), layer: 1, order: 0 }
        ]

      path :: EdgePath
      path =
        { edge: EdgeId "ab"
        , segments:
            [ { start: GridPos (20.0 /\ 20.0), end: GridPos (20.0 /\ 60.0), direction: V }
            , { start: GridPos (20.0 /\ 60.0), end: GridPos (80.0 /\ 60.0), direction: H }
            ]
        , bends: [ GridPos (20.0 /\ 60.0) ]
        , bendType: []
        , jumps: []
        , reversed: false
        }
      visual = M.singleton (NodeId "a") { left: 0.0, right: 2.0, top: 0.0, bottom: 5.0 }
      placed = EdgeLabels.tailPlacements visual graph labels nodes [ path ] M.empty []
    ((_.position <$> A.head placed)) `shouldEqual` Just (GridPos (22.0 /\ 38.5))

  it "centers a source-owned tail label in a short terminal run with positive clearance" do
    let
      graph = chains { nodes = A.take 2 chains.nodes, edges = [ edge "ab" "a" "b" ] }
      specs = M.singleton (EdgeId "ab")
        { size: GridSize (4.0 /\ 2.0)
        , placement: EdgeLabels.Tail EdgeLabels.CenterTerminalRun
        }
      labels = EdgeLabels.insert specs graph graph.edges
      nodes =
        [ { node: NodeId "a", position: GridPos (0.0 /\ 0.0), size: GridSize (10.0 /\ 5.0), layer: 0, order: 0 }
        , { node: NodeId "b", position: GridPos (20.0 /\ 20.0), size: GridSize (10.0 /\ 5.0), layer: 1, order: 0 }
        ]

      path :: EdgePath
      path =
        { edge: EdgeId "ab"
        , segments:
            [ { start: GridPos (20.0 /\ 20.0), end: GridPos (20.0 /\ 30.0), direction: V }
            , { start: GridPos (20.0 /\ 30.0), end: GridPos (80.0 /\ 30.0), direction: H }
            ]
        , bends: [ GridPos (20.0 /\ 30.0) ]
        , bendType: []
        , jumps: []
        , reversed: false
        }
      placed = EdgeLabels.tailPlacements M.empty graph labels nodes [ path ] M.empty []
    ((_.position <$> A.head placed)) `shouldEqual` Just (GridPos (22.0 /\ 21.0))

  it "keeps exact small feedback routes forward with source-owned tail labels" do
    let
      graph =
        { nodes: [ "a", "b", "c", "d" ] <#> \id ->
            { id: NodeId id, size: GridSize (1.0 /\ 1.0), ports: [], label: Nothing, shape: Rectangle }
        , edges:
            [ edge "a->b" "a" "b"
            , edge "a->c" "a" "c"
            , edge "b->d" "b" "d"
            , edge "c->d" "c" "d"
            , edge "d->a" "d" "a"
            ]
        , constraints: []
        }
      labelSpec = { size: GridSize (1.40625 /\ 0.53125), placement: EdgeLabels.Tail EdgeLabels.CenterTerminalRun }
      config = defaultConfig
        { edgeLabels = M.fromFoldable (graph.edges <#> \connection -> connection.id /\ labelSpec)
        }
      result = (full config graph).result
      position id = _.position <$> A.find (\node -> node.node == NodeId id) result.nodes
      route id = A.find (\path -> path.edge == EdgeId id) result.edges
      firstVerticalIsForward id = route id <#>
        \path -> case A.find (\segment -> segment.direction == V) path.segments of
          Just segment -> gridY segment.end > gridY segment.start
          Nothing -> false
    traverse_
      (\(left /\ right) -> ((/=) <$> position left <*> position right) `shouldEqual` Just true)
      [ "a" /\ "b", "a" /\ "c", "a" /\ "d", "b" /\ "c", "b" /\ "d", "c" /\ "d" ]
    traverse_ (\id -> firstVerticalIsForward id `shouldEqual` Just true) [ "a->b", "a->c", "b->d", "c->d" ]
    A.length result.edgeLabels `shouldEqual` A.length graph.edges

  it "invalidates cached coordinates when a visual margin changes" do
    let
      config = defaultConfig
        { edgeLabels = M.singleton (EdgeId "ab") { size: GridSize (4.0 /\ 2.0), placement: EdgeLabels.Tail EdgeLabels.Adjacent }
        }
      initial = full config chains
      changed = config { nodeVisualMargins = M.singleton (NodeId "a") { left: 0.0, right: 2.0, top: 0.0, bottom: 5.0 } }
    (fromCoords changed chains initial.pipeline).result `shouldEqual` (full changed chains).result

  it "retains the authored source for a restored tail route" do
    let
      graph = chains { nodes = A.take 2 chains.nodes, edges = [ edge "ba" "b" "a" ] }
      specs = M.singleton (EdgeId "ba")
        { size: GridSize (4.0 /\ 2.0)
        , placement: EdgeLabels.Tail EdgeLabels.Adjacent
        }
      labels = EdgeLabels.insert specs graph graph.edges
      nodes =
        [ { node: NodeId "a", position: GridPos (0.0 /\ 20.0), size: GridSize (10.0 /\ 5.0), layer: 1, order: 0 }
        , { node: NodeId "b", position: GridPos (20.0 /\ 0.0), size: GridSize (10.0 /\ 5.0), layer: 0, order: 0 }
        ]

      path :: EdgePath
      path =
        { edge: EdgeId "ba"
        , segments:
            [ { start: GridPos (120.0 /\ 20.0), end: GridPos (120.0 /\ 60.0), direction: V }
            , { start: GridPos (120.0 /\ 60.0), end: GridPos (20.0 /\ 60.0), direction: H }
            ]
        , bends: [ GridPos (120.0 /\ 60.0) ]
        , bendType: []
        , jumps: []
        , reversed: true
        }
      placed = EdgeLabels.tailPlacements M.empty graph labels nodes [ path ] M.empty []
    ((_.position <$> A.head placed)) `shouldEqual` Just (GridPos (122.0 /\ 22.0))

  it "centers a fixed-side tail label along a horizontal terminal run" do
    let
      graph = chains { nodes = A.take 2 chains.nodes, edges = [ edge "ab" "a" "b" ] }
      specs = M.singleton (EdgeId "ab")
        { size: GridSize (4.0 /\ 2.0)
        , placement: EdgeLabels.Tail EdgeLabels.CenterTerminalRun
        }
      labels = EdgeLabels.insert specs graph graph.edges
      nodes =
        [ { node: NodeId "a", position: GridPos (0.0 /\ 0.0), size: GridSize (10.0 /\ 5.0), layer: 0, order: 0 }
        , { node: NodeId "b", position: GridPos (20.0 /\ 20.0), size: GridSize (10.0 /\ 5.0), layer: 1, order: 0 }
        ]

      path :: EdgePath
      path =
        { edge: EdgeId "ab"
        , segments:
            [ { start: GridPos (40.0 /\ 10.0), end: GridPos (100.0 /\ 10.0), direction: H }
            , { start: GridPos (100.0 /\ 10.0), end: GridPos (100.0 /\ 80.0), direction: V }
            ]
        , bends: [ GridPos (100.0 /\ 10.0) ]
        , bendType: []
        , jumps: []
        , reversed: false
        }
      placed = EdgeLabels.tailPlacements M.empty graph labels nodes [ path ] M.empty []
    ((_.position <$> A.head placed)) `shouldEqual` Just (GridPos (62.0 /\ 12.0))

  it "uses the compacted reserved tail hitbox when route candidates are blocked" do
    let
      graph = chains { nodes = A.take 2 chains.nodes, edges = [ edge "ab" "a" "b" ] }
      specs = M.singleton (EdgeId "ab")
        { size: GridSize (4.0 /\ 2.0)
        , placement: EdgeLabels.Tail EdgeLabels.CenterTerminalRun
        }
      labels = EdgeLabels.insert specs graph graph.edges
      nodes =
        [ { node: NodeId "a", position: GridPos (0.0 /\ 0.0), size: GridSize (10.0 /\ 5.0), layer: 0, order: 0 }
        , { node: NodeId "b", position: GridPos (20.0 /\ 20.0), size: GridSize (10.0 /\ 5.0), layer: 1, order: 0 }
        ]

      path :: EdgePath
      path =
        { edge: EdgeId "ab"
        , segments:
            [ { start: GridPos (20.0 /\ 20.0), end: GridPos (20.0 /\ 60.0), direction: V }
            , { start: GridPos (20.0 /\ 60.0), end: GridPos (80.0 /\ 60.0), direction: H }
            ]
        , bends: [ GridPos (20.0 /\ 60.0) ]
        , bendType: []
        , jumps: []
        , reversed: false
        }

      blocker :: EdgePath
      blocker =
        { edge: EdgeId "blocker"
        , segments: [ { start: GridPos (0.0 /\ 42.0), end: GridPos (50.0 /\ 42.0), direction: H } ]
        , bends: []
        , bendType: []
        , jumps: []
        , reversed: false
        }
      fallback = M.singleton (EdgeId "ab")
        { edge: EdgeId "ab"
        , position: GridPos (60.0 /\ 38.0)
        , size: GridSize (16.0 /\ 8.0)
        }
      placed = EdgeLabels.tailPlacements M.empty graph labels nodes [ path, blocker ] fallback []
    ((_.position <$> A.head placed)) `shouldEqual` Just (GridPos (60.0 /\ 38.0))

  it "restores fixed side anchors through labelled component reruns" do
    let
      graph = chains
        { nodes = chains.nodes <#> \node -> node
            { ports =
                [ { id: PortId "out", side: East, offset: 2, label: Nothing }
                , { id: PortId "in", side: West, offset: 3, label: Nothing }
                ]
            }
        , edges = chains.edges <#> \connection -> connection
            { from = connection.from { port = Just (PortId "out") }
            , to = connection.to { port = Just (PortId "in") }
            }
        }
      config = defaultConfig { edgeLabels = M.fromFoldable [ EdgeId "ab" /\ { size: GridSize (7.0 /\ 3.0), placement: EdgeLabels.Center }, EdgeId "cd" /\ { size: GridSize (5.0 /\ 2.0), placement: EdgeLabels.Center } ] }
      initial = full config graph
      results =
        [ initial.result
        , (fromDummies config graph initial.pipeline).result
        , (fromCrossMin config graph initial.pipeline).result
        , (fromCoords config graph initial.pipeline).result
        , fromRouting config graph initial.pipeline
        ]
    traverse_
      ( \result -> do
          A.sort (result.nodes <#> _.node) `shouldEqual` A.sort (graph.nodes <#> _.id)
          result.nodes `shouldEqual` initial.result.nodes
          result.edges `shouldEqual` initial.result.edges
          result.edgeLabels `shouldEqual` initial.result.edgeLabels
          traverse_
            ( \connection -> do
                let
                  source = A.find (\node -> node.node == connection.from.node) result.nodes
                  target = A.find (\node -> node.node == connection.to.node) result.nodes
                  path = A.find (\route -> route.edge == connection.id) result.edges
                  start = _.start <$> (path >>= A.head <<< _.segments)
                  end = _.end <$> (path >>= A.last <<< _.segments)
                  sourceAnchor = source <#> \node -> GridPos
                    ((gridX node.position + sizeW node.size) * 4.0 /\ (gridY node.position + 2.0) * 4.0)
                  targetAnchor = target <#> \node -> GridPos
                    (gridX node.position * 4.0 /\ (gridY node.position + 3.0) * 4.0)
                start `shouldEqual` sourceAnchor
                end `shouldEqual` targetAnchor
            )
            graph.edges
      )
      results

  it "repackages changed component sizes without retaining previous offsets" do
    let initial = full defaultConfig chains
    let grown = chains { nodes = chains.nodes <#> \n -> if n.id == NodeId "a" then n { size = GridSize (25.0 /\ 5.0) } else n }
    let expected = (full defaultConfig grown).result
    let actual = (fromCoords defaultConfig grown initial.pipeline).result
    actual.nodes `shouldEqual` expected.nodes
    actual.edges `shouldEqual` expected.edges
    actual.boundingBox `shouldEqual` expected.boundingBox

  it "does not separate nodes joined by a relative-position constraint" do
    let graph = chains { nodes = A.filter (\n -> n.id == NodeId "a" || n.id == NodeId "c") chains.nodes, edges = [], constraints = [ RelativePosition { anchor: NodeId "a", target: NodeId "c", offset: GridPos (20.0 /\ 4.0) } ] }
    let result = (full (defaultConfig { compactPostRouting = false }) graph).result
    let position id = _.position <$> A.find (\n -> n.node == NodeId id) result.nodes
    ((\a c -> GridPos ((gridX c - gridX a) /\ (gridY c - gridY a))) <$> position "a" <*> position "c") `shouldEqual` Just (GridPos (20.0 /\ 4.0))

  it "preserves virtual frame extents when repacking translated components" do
    let
      graph = chains { nodes = A.take 1 chains.nodes <#> \node -> node { size = GridSize (1.0 /\ 10.0) }, edges = [] }
      base = (full defaultConfig graph).result
      extended = base { boundingBox = { pos: GridPos (-0.01 /\ 0.0), size: GridSize (1.01 /\ 10.0) } }
      other = base { nodes = base.nodes <#> \node -> node { node = NodeId "b" } }
      packed = Components.pack [ extended, other ] <#> _.result
      repacked = Components.pack packed <#> _.result
      separation results =
        let
          nodes = A.concatMap _.nodes results
          x id = gridX <<< _.position <$> A.find (\node -> node.node == NodeId id) nodes
        in
          (-) <$> x "a" <*> x "b"
    -- The extended layout sorts after the smaller component, and its
    -- visible node remains 0.01 grid units inside the preserved frame.
    traverse_ (\results -> separation results `shouldEqual` Just 6.01) [ packed, repacked ]

chains :: Graph
chains =
  { nodes: [ "a", "b", "c", "d" ] <#> \id -> { id: NodeId id, size: GridSize (10.0 /\ 5.0), ports: [], label: Nothing, shape: Rectangle }
  , edges: [ edge "ab" "a" "b", edge "cd" "c" "d" ]
  , constraints: []
  }

edge :: String -> String -> String -> Edge
edge id from to = { id: EdgeId id, from: { node: NodeId from, port: Nothing }, to: { node: NodeId to, port: Nothing }, label: Nothing }
