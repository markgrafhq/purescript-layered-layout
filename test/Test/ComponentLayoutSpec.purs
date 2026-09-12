module Test.ComponentLayoutSpec (componentLayoutSpec) where

import Prelude

import Data.Array as A
import Data.Foldable (traverse_)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import LayeredLayout (defaultConfig, fromCoords, fromCrossMin, fromDummies, fromRouting, full)
import LayeredLayout.Components as Components
import LayeredLayout.Graph (Constraints(..), Edge, EdgeId(..), Graph, NodeId(..), PortId(..), Shape(..), Side(..))
import LayeredLayout.Grid (GridPos(..), GridSize(..), gridX, gridY, sizeW)
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

  it "keeps labels and routes attached through cached component reruns" do
    let config = defaultConfig { edgeLabelSizes = M.fromFoldable [ EdgeId "ab" /\ GridSize (7.0 /\ 3.0), EdgeId "cd" /\ GridSize (5.0 /\ 2.0) ] }
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
      config = defaultConfig { edgeLabelSizes = M.fromFoldable [ EdgeId "ab" /\ GridSize (7.0 /\ 3.0), EdgeId "cd" /\ GridSize (5.0 /\ 2.0) ] }
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
