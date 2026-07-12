module Test.Fixtures (allCases) where

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import LayeredLayout.Graph (Edge, EdgeId(..), Graph, Node, NodeId(..), Shape(..))
import LayeredLayout.Grid (GridSize(..))

allCases :: Array { name :: String, graph :: Graph }
allCases =
  [ { name: "Two nodes", graph: twoNodes }
  , { name: "Linear chain", graph: linearChain }
  , { name: "Diamond", graph: diamond }
  , { name: "Skip layer", graph: skipLayer }
  , { name: "W-shape", graph: wShape }
  , { name: "Fan-out", graph: fanOut }
  , { name: "Fan-in", graph: fanIn }
  , { name: "Multi-width", graph: multiWidth }
  , { name: "Module deps", graph: moduleDeps }
  , { name: "Long edge", graph: longEdge }
  , { name: "Binary tree", graph: binaryTree }
  , { name: "Converging", graph: converging }
  -- New cases that exercise specific algorithm differences:
  , { name: "Cycle (triangle)", graph: triangleCycle }
  , { name: "Cycle (loop)", graph: loopCycle }
  , { name: "NS-favoured layering", graph: nsFavoured }
  , { name: "Self-loop", graph: selfLoop }
  , { name: "Two self-loops", graph: twoSelfLoops }
  , { name: "Mixed self + edges", graph: mixedSelfLoops }
  , { name: "Tied barycenter", graph: tiedBarycenter }
  , { name: "Branch-long", graph: branchLong }
  -- Exercises the post-routing compactor port (N/S synthetic VSes,
  -- inverted-port branch, same-edge VS zeroing). Compactor itself is
  -- still gated off here; visible in compactionDiffs panel.
  , { name: "Back edges", graph: backEdges }
  , { name: "Long edge wide", graph: longEdgeWide }
  ]

backEdges :: Graph
backEdges =
  { nodes: [ mkN "a", mkN "b", mkN "c", mkN "d" ]
  , edges:
      [ mkE "e1" "a" "b"
      , mkE "e2" "b" "c"
      , mkE "e3" "c" "d"
      , mkE "e4" "c" "a"
      , mkE "e5" "d" "b"
      ]
  , constraints: []
  }

longEdgeWide :: Graph
longEdgeWide =
  { nodes:
      [ mkN "entry"
      , mkN "parse"
      , mkN "check"
      , mkN "apply"
      , mkN "commit"
      , mkN "audit"
      ]
  , edges:
      [ mkE "e1" "entry" "parse"
      , mkE "e2" "parse" "check"
      , mkE "e3" "check" "apply"
      , mkE "e4" "apply" "commit"
      , mkE "e5" "entry" "audit"
      , mkE "e6" "audit" "commit"
      , mkE "e7" "parse" "audit"
      ]
  , constraints: []
  }

-- Minimal repro: branching + re-convergence + one long edge.
-- Five real nodes; the C→E edge skips a layer so a dummy lands in
-- layer 2 next to D, mirroring moduleDeps' Auth→DB pattern.
branchLong :: Graph
branchLong =
  { nodes: [ mkN "A", mkN "B", mkN "C", mkN "D", mkN "E" ]
  , edges:
      [ mkE "e1" "A" "B"
      , mkE "e2" "A" "C"
      , mkE "e3" "B" "D"
      , mkE "e4" "B" "E"
      , mkE "e5" "C" "E"
      , mkE "e6" "D" "E"
      ]
  , constraints: []
  }

mkN :: String -> Node
mkN nid = { id: NodeId nid, size: GridSize (1.0 /\ 1.0), ports: [], label: Just nid, shape: Rectangle }

mkNW :: String -> Int -> Node
mkNW nid w = { id: NodeId nid, size: GridSize (Int.toNumber w /\ 1.0), ports: [], label: Just nid, shape: Rectangle }

mkE :: String -> String -> String -> Edge
mkE eid from to = { id: EdgeId eid, from: { node: NodeId from, port: Nothing }, to: { node: NodeId to, port: Nothing }, label: Nothing }

twoNodes :: Graph
twoNodes = { nodes: [ mkN "A", mkN "B" ], edges: [ mkE "e1" "A" "B" ], constraints: [] }

diamond :: Graph
diamond = { nodes: [ mkN "a", mkN "b", mkN "c", mkN "d" ], edges: [ mkE "e1" "a" "b", mkE "e2" "a" "c", mkE "e3" "b" "d", mkE "e4" "c" "d" ], constraints: [] }

fanOut :: Graph
fanOut = { nodes: [ mkNW "A" 2, mkN "B", mkN "C", mkN "D", mkN "E" ], edges: [ mkE "e1" "A" "B", mkE "e2" "A" "C", mkE "e3" "A" "D", mkE "e4" "A" "E" ], constraints: [] }

fanIn :: Graph
fanIn = { nodes: [ mkN "A", mkN "B", mkN "C", mkN "D", mkNW "E" 2 ], edges: [ mkE "e1" "A" "E", mkE "e2" "B" "E", mkE "e3" "C" "E", mkE "e4" "D" "E" ], constraints: [] }

linearChain :: Graph
linearChain = { nodes: [ mkN "A", mkN "B", mkN "C", mkN "D" ], edges: [ mkE "e1" "A" "B", mkE "e2" "B" "C", mkE "e3" "C" "D" ], constraints: [] }

skipLayer :: Graph
skipLayer = { nodes: [ mkN "A", mkN "B", mkN "C" ], edges: [ mkE "e1" "A" "B", mkE "e2" "A" "C", mkE "e3" "B" "C" ], constraints: [] }

wShape :: Graph
wShape = { nodes: [ mkN "A", mkN "B", mkN "C", mkN "D", mkN "E" ], edges: [ mkE "e1" "A" "C", mkE "e2" "A" "D", mkE "e3" "B" "D", mkE "e4" "B" "E" ], constraints: [] }

multiWidth :: Graph
multiWidth = { nodes: [ mkN "narrow", mkNW "wide" 4, mkNW "medium" 2 ], edges: [ mkE "e1" "narrow" "wide", mkE "e2" "narrow" "medium" ], constraints: [] }

longEdge :: Graph
longEdge = { nodes: [ mkNW "A" 2, mkNW "B" 2, mkNW "C" 2, mkNW "D" 2 ], edges: [ mkE "e1" "A" "B", mkE "e2" "B" "C", mkE "e3" "C" "D", mkE "e4" "A" "D" ], constraints: [] }

binaryTree :: Graph
binaryTree =
  { nodes: [ mkNW "root" 2, mkN "L", mkN "R", mkN "LL", mkN "LR", mkN "RL", mkN "RR" ]
  , edges: [ mkE "e1" "root" "L", mkE "e2" "root" "R", mkE "e3" "L" "LL", mkE "e4" "L" "LR", mkE "e5" "R" "RL", mkE "e6" "R" "RR" ]
  , constraints: []
  }

converging :: Graph
converging = { nodes: [ mkN "A", mkN "B", mkN "C", mkN "D", mkNW "E" 2 ], edges: [ mkE "e1" "A" "C", mkE "e2" "B" "D", mkE "e3" "C" "E", mkE "e4" "D" "E" ], constraints: [] }

moduleDeps :: Graph
moduleDeps =
  { nodes:
      [ mkNW "App" 2
      , mkNW "Router" 2
      , mkNW "Auth" 2
      , mkNW "API" 2
      , mkNW "DB" 2
      , mkNW "Cache" 2
      , mkNW "Logger" 2
      , mkNW "Config" 2
      ]
  , edges:
      [ mkE "e1" "App" "Router"
      , mkE "e2" "App" "Auth"
      , mkE "e3" "Router" "API"
      , mkE "e4" "Auth" "API"
      , mkE "e5" "Auth" "DB"
      , mkE "e6" "API" "DB"
      , mkE "e7" "API" "Cache"
      , mkE "e8" "DB" "Logger"
      , mkE "e9" "Cache" "Logger"
      , mkE "e10" "Logger" "Config"
      ]
  , constraints: []
  }

-- A 3-cycle: GreedyCycleBreaker should pick the same back-edge as
-- ELK's GREEDY_MODEL_ORDER (last edge in the cycle in model order).
triangleCycle :: Graph
triangleCycle =
  { nodes: [ mkN "A", mkN "B", mkN "C" ]
  , edges: [ mkE "e1" "A" "B", mkE "e2" "B" "C", mkE "e3" "C" "A" ]
  , constraints: []
  }

-- A larger cycle to highlight that GREEDY reverses fewer edges than
-- DFS would. Specifically a 4-cycle plus a chord.
loopCycle :: Graph
loopCycle =
  { nodes: [ mkN "A", mkN "B", mkN "C", mkN "D" ]
  , edges:
      [ mkE "e1" "A" "B"
      , mkE "e2" "B" "C"
      , mkE "e3" "C" "D"
      , mkE "e4" "D" "A"
      , mkE "e5" "A" "C"
      ]
  , constraints: []
  }

-- A graph where NetworkSimplex compacts differently than
-- LongestPath. With LP, B is pushed to layer 2 because D is a
-- sink; NS keeps B at layer 1 to shorten the A→B edge.
--
--     A → B
--     A → C → D
--
-- LP: A=0, C=1, B=2, D=2  (total edge length = 2 + 1 + 1 = 4)
-- NS: A=0, B=1, C=1, D=2  (total edge length = 1 + 1 + 1 = 3)
nsFavoured :: Graph
nsFavoured =
  { nodes: [ mkN "A", mkN "B", mkN "C", mkN "D" ]
  , edges: [ mkE "e1" "A" "B", mkE "e2" "A" "C", mkE "e3" "C" "D" ]
  , constraints: []
  }

-- A node with a single self-loop — exercises the C-shape
-- self-loop router on the east side.
selfLoop :: Graph
selfLoop =
  { nodes: [ mkN "A" ]
  , edges: [ mkE "e1" "A" "A" ]
  , constraints: []
  }

-- Two self-loops on one node — tests the equal-distribution
-- spacing of entry / exit stubs along the east side.
twoSelfLoops :: Graph
twoSelfLoops =
  { nodes: [ mkN "A" ]
  , edges: [ mkE "e1" "A" "A", mkE "e2" "A" "A" ]
  , constraints: []
  }

-- A self-loop on a node that also has regular incoming and
-- outgoing edges, so the routers have to coexist.
mixedSelfLoops :: Graph
mixedSelfLoops =
  { nodes: [ mkN "A", mkN "B", mkN "C" ]
  , edges:
      [ mkE "e1" "A" "B"
      , mkE "e2" "B" "B"
      , mkE "e3" "B" "C"
      ]
  , constraints: []
  }

-- A graph where every middle-layer node has barycenter 0.5 (one
-- predecessor + one successor at symmetric positions). The model-
-- order tie-break is the only thing that decides their final
-- order; the visual layout is fully determined by node-input
-- order.
tiedBarycenter :: Graph
tiedBarycenter =
  { nodes:
      [ mkN "S"
      , mkN "M1"
      , mkN "M2"
      , mkN "M3"
      , mkN "T"
      ]
  , edges:
      [ mkE "e1" "S" "M1"
      , mkE "e2" "S" "M2"
      , mkE "e3" "S" "M3"
      , mkE "e4" "M1" "T"
      , mkE "e5" "M2" "T"
      , mkE "e6" "M3" "T"
      ]
  , constraints: []
  }
