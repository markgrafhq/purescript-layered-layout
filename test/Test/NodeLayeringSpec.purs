module Test.NodeLayeringSpec (nodeLayeringSpec) where

import Prelude

import Data.Array as A
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import LayeredLayout.Graph (NodeId(..))
import LayeredLayout.LayerAssignment.NetworkSimplex (networkSimplex)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

nodeLayeringSpec :: Spec Unit
nodeLayeringSpec = describe "Node layering" do
  it "layers and balances each component independently of unrelated layer filling" do
    let leftNodes = [ "a", "b", "d", "e", "f", "g", "h" ]
    let leftEdges = [ "b" /\ "e", "a" /\ "d", "g" /\ "h", "f" /\ "g", "d" /\ "f", "a" /\ "b", "e" /\ "h" ]
    let rightNodes = [ "u", "v", "w", "x", "y" ]
    let rightEdges = [ "u" /\ "v", "v" /\ "w", "v" /\ "x", "v" /\ "y" ]
    let left = layer leftNodes leftEdges
    let right = layer rightNodes rightEdges
    let combined = layer (leftNodes <> rightNodes <> [ "isolated" ]) (leftEdges <> rightEdges)
    combined `shouldEqual` M.insert (NodeId "isolated") 0 (M.union left right)
    M.lookup (NodeId "b") combined `shouldEqual` Just 2
    feasible (leftEdges <> rightEdges) combined `shouldEqual` true

  it "keeps parallel incoming edges out of the outgoing balancing span" do
    let nodes = [ "a", "b", "c", "d", "x" ]
    let edges = [ "a" /\ "b", "b" /\ "c", "c" /\ "d", "a" /\ "x", "a" /\ "x", "x" /\ "d", "x" /\ "d" ]
    let ranks = layer nodes edges
    -- The two incoming span-1 edges must not prevent x moving toward d.
    M.lookup (NodeId "x") ranks `shouldEqual` Just 2
    feasible edges ranks `shouldEqual` true

  it "uses depth-first incident-edge order to choose between optimal feasible trees" do
    let nodes = [ "a", "c", "d", "e", "f", "g", "h", "i" ]
    let edges = [ "f" /\ "g", "a" /\ "d", "a" /\ "c", "c" /\ "i", "d" /\ "h", "e" /\ "f", "g" /\ "i", "e" /\ "h" ]
    let ranks = layer nodes edges
    -- ELK's DFS visits a,d,h,e,f,g,i,c. A breadth-first tree instead
    -- translates the a branch down and the e branch up by one layer.
    M.lookup (NodeId "a") ranks `shouldEqual` Just 0
    M.lookup (NodeId "e") ranks `shouldEqual` Just 1
    M.lookup (NodeId "i") ranks `shouldEqual` Just 4
    feasible edges ranks `shouldEqual` true

  it "balances in component traversal order after the feasible tree is chosen" do
    let nodes = [ "a", "b", "d", "e", "f", "g", "h" ]
    let edges = [ "b" /\ "e", "a" /\ "d", "g" /\ "h", "f" /\ "g", "d" /\ "f", "a" /\ "b", "e" /\ "h" ]
    let ranks = layer nodes edges
    -- DFS balances e before b: e leaves layer 2, allowing b to fill it.
    -- Scanning model order instead leaves b at 1 even after e moves.
    M.lookup (NodeId "e") ranks `shouldEqual` Just 3
    M.lookup (NodeId "b") ranks `shouldEqual` Just 2
    feasible edges ranks `shouldEqual` true

layer :: Array String -> Array (String /\ String) -> Map NodeId Int
layer nodes edges = networkSimplex (map NodeId nodes)
  (edges <#> \(src /\ tgt) -> { src: NodeId src, tgt: NodeId tgt })

feasible :: Array (String /\ String) -> Map NodeId Int -> Boolean
feasible edges ranks = A.all
  ( \(src /\ tgt) -> case M.lookup (NodeId src) ranks /\ M.lookup (NodeId tgt) ranks of
      Just source /\ Just target -> target - source >= 1
      _ -> false
  )
  edges
