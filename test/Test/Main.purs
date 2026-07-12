module Test.Main where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import LayeredLayout (defaultConfig, layout)
import LayeredLayout.CycleRemoval as CycleRemoval
import LayeredLayout.Graph (Edge, EdgeId(..), Graph, Node, NodeId(..), Shape(..))
import LayeredLayout.Grid (GridPos(..), GridSize(..), gridX, gridY)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "geometry" do
    it "reads grid coordinates" do
      gridX (GridPos (3.0 /\ 7.0)) `shouldEqual` 3.0
      gridY (GridPos (3.0 /\ 7.0)) `shouldEqual` 7.0

  describe "cycle removal" do
    it "leaves a DAG unchanged" do
      let edges = [ mkEdge "e1" "a" "b", mkEdge "e2" "b" "c" ]
      (CycleRemoval.makeAcyclic [] edges).edges `shouldEqual` edges

    it "breaks a directed cycle" do
      let
        edges = [ mkEdge "e1" "a" "b", mkEdge "e2" "b" "c", mkEdge "e3" "c" "a" ]
        result = CycleRemoval.makeAcyclic [] edges
      Set.size result.reversedEdges `shouldEqual` 1

  describe "layout pipeline" do
    it "places every node and routes every edge" do
      let result = layout defaultConfig diamond
      Array.length result.nodes `shouldEqual` Array.length diamond.nodes
      Array.length result.edges `shouldEqual` Array.length diamond.edges

    it "is deterministic" do
      layout defaultConfig diamond `shouldEqual` layout defaultConfig diamond

    it "keeps a linear chain in increasing layers" do
      let
        result = layout defaultConfig linearChain
        layers = result.nodes <#> _.layer
      layers `shouldEqual` [ 0, 1, 2, 3 ]

mkNode :: String -> Node
mkNode id =
  { id: NodeId id
  , size: GridSize (1.0 /\ 1.0)
  , ports: []
  , label: Just id
  , shape: Rectangle
  }

mkEdge :: String -> String -> String -> Edge
mkEdge id from to =
  { id: EdgeId id
  , from: { node: NodeId from, port: Nothing }
  , to: { node: NodeId to, port: Nothing }
  , label: Nothing
  }

linearChain :: Graph
linearChain =
  { nodes: map mkNode [ "a", "b", "c", "d" ]
  , edges: [ mkEdge "e1" "a" "b", mkEdge "e2" "b" "c", mkEdge "e3" "c" "d" ]
  , constraints: []
  }

diamond :: Graph
diamond =
  { nodes: map mkNode [ "a", "b", "c", "d" ]
  , edges:
      [ mkEdge "e1" "a" "b"
      , mkEdge "e2" "a" "c"
      , mkEdge "e3" "b" "d"
      , mkEdge "e4" "c" "d"
      ]
  , constraints: []
  }
