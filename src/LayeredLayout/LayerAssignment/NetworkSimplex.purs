-- | Layer assignment via network simplex.
-- |
-- | The Gansner-Koutsofios-North-Vo core lives in `LayeredLayout.NetworkSimplex`
-- | (shared with the post-routing graph compactor). This module adds
-- | the layering-specific wrapping:
-- |
-- |   1. Partition the graph into weakly-connected components and run
-- |      the simplex on each (`connectedComponents`).
-- |   2. Stack components vertically so component i sits below the
-- |      stack of components 0..i-1 (`stackVertically`).
-- |   3. After the optimal layering is normalised, run a balancing
-- |      pass that moves nodes whose in/out-degrees match to a
-- |      less-populated layer when their feasible window allows it.
module LayeredLayout.LayerAssignment.NetworkSimplex (networkSimplex) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as S
import Data.Tuple.Nested (type (/\), (/\))
import LayeredLayout.Graph (NodeId)
import LayeredLayout.NetworkSimplex (NEdge, runNetworkSimplex)

-- | Run the network simplex layerer over the given node ids and
-- | edges. Each edge has weight 1 and delta 1 (the minimum span).
-- | The graph is split into weakly-connected components and the
-- | simplex runs independently on each one so disconnected sub-graphs
-- | all receive a feasible layering. The per-component layers are
-- | then stacked vertically and a balancing pass moves nodes with
-- | matching in/out-degrees to less-populated layers where possible.
networkSimplex :: Array NodeId -> Array { src :: NodeId, tgt :: NodeId } -> Map NodeId Int
networkSimplex nodes rawEdges = do
  let components = connectedComponents nodes rawEdges
  let perComponent = components <#> \cn -> runOnComponent cn rawEdges
  let stacked = stackVertically perComponent
  let merged = foldl M.union M.empty stacked
  let allEdges = toNEdges rawEdges
  balance nodes allEdges merged

-- | Convert plain edges into the shared core's `NEdge` shape with
-- | weight = 1.0 and delta = 1.
toNEdges :: Array { src :: NodeId, tgt :: NodeId } -> Array (NEdge NodeId)
toNEdges = A.mapWithIndex \i e -> { src: e.src, tgt: e.tgt, delta: 1, weight: 1.0, eid: i }

-- | Run the network simplex on a single weakly-connected component.
runOnComponent :: Array NodeId -> Array { src :: NodeId, tgt :: NodeId } -> Map NodeId Int
runOnComponent compNodes rawEdges = do
  let nodeSet = S.fromFoldable compNodes
  let edgesInComp = A.filter (\e -> S.member e.src nodeSet && S.member e.tgt nodeSet) rawEdges
  runNetworkSimplex compNodes (toNEdges edgesInComp)

-- | Stack components vertically: offset each component's layers so
-- | component i starts after all previous components' layers. Component 0
-- | stays at 0..max, component 1 shifts to prevMax+1..prevMax+1+max, etc.
stackVertically :: Array (Map NodeId Int) -> Array (Map NodeId Int)
stackVertically comps = _.result $ foldl shiftOne { base: 0, result: [] } comps
  where
  shiftOne acc comp = do
    let maxLayer = foldl max 0 (M.values comp)
    let height = maxLayer + 1
    let shifted = if acc.base == 0 then comp else map (_ + acc.base) comp
    { base: acc.base + height, result: acc.result <> [ shifted ] }

-- | Port of `NetworkSimplexLayerer.connectedComponents`. Walks the
-- | graph treating edges as undirected and partitions nodes into
-- | weakly-connected components, preserving the input order within
-- | each component.
connectedComponents :: Array NodeId -> Array { src :: NodeId, tgt :: NodeId } -> Array (Array NodeId)
connectedComponents nodes rawEdges = result.components
  where
  adj = foldl
    ( \m e -> do
        let m' = M.insertWith (<>) e.src [ e.tgt ] m
        M.insertWith (<>) e.tgt [ e.src ] m'
    )
    M.empty
    rawEdges

  result = foldl visit { visited: S.empty, components: [] } nodes

  visit st node
    | S.member node st.visited = st
    | otherwise = do
        let comp = expand [ node ] st.visited []
        st
          { visited = foldl (\s n -> S.insert n s) st.visited comp.nodes
          , components = st.components <> [ comp.nodes ]
          }

  expand stack visited acc = case A.uncons stack of
    Nothing -> { nodes: acc }
    Just { head: n, tail: rest } ->
      if S.member n visited then expand rest visited acc
      else do
        let visited' = S.insert n visited
        let neighbours = fromMaybe [] (M.lookup n adj)
        expand (rest <> neighbours) visited' (acc <> [ n ])

-- | Port of `NetworkSimplex.balance`. After the optimal layering is
-- | normalised, scan every node whose in-degree matches its out-degree
-- | and consider shifting it to a layer with fewer occupants. The
-- | feasible window is `[layer - minSpanIn + 1, layer + minSpanOut - 1]`
-- | where minSpanIn / minSpanOut are the minimum incident-edge spans.
balance :: Array NodeId -> Array (NEdge NodeId) -> Map NodeId Int -> Map NodeId Int
balance nodes edges layers0 = do
  let highest = foldl (\m n -> max m (fromMaybe 0 (M.lookup n layers0))) 0 nodes
  let filling0 = M.fromFoldable (A.range 0 highest <#> \i -> i /\ countAt i layers0)
  let inDegree = byNodeCount _.tgt edges
  let outDegree = byNodeCount _.src edges
  let
    result = foldl
      (\acc n -> tryMove n inDegree outDegree edges acc)
      { layers: layers0, filling: filling0 }
      nodes
  result.layers
  where
  countAt :: Int -> Map NodeId Int -> Int
  countAt l ls = foldl (\c k -> if M.lookup k ls == Just l then c + 1 else c) 0 nodes

  byNodeCount :: (NEdge NodeId -> NodeId) -> Array (NEdge NodeId) -> Map NodeId Int
  byNodeCount keyFn = foldl (\m e -> M.insertWith (+) (keyFn e) 1 m) M.empty

  tryMove
    :: NodeId
    -> Map NodeId Int
    -> Map NodeId Int
    -> Array (NEdge NodeId)
    -> { layers :: Map NodeId Int, filling :: Map Int Int }
    -> { layers :: Map NodeId Int, filling :: Map Int Int }
  tryMove n inDeg outDeg es acc = do
    let inD = fromMaybe 0 (M.lookup n inDeg)
    let outD = fromMaybe 0 (M.lookup n outDeg)
    if inD /= outD || inD == 0 then acc
    else do
      let curLayer = fromMaybe 0 (M.lookup n acc.layers)
      let mSpanIn /\ mSpanOut = minimalSpan n es acc.layers
      if mSpanIn < 0 || mSpanOut < 0 then acc
      else do
        let lo = curLayer - mSpanIn + 1
        let hi = curLayer + mSpanOut - 1
        if hi < lo then acc
        else do
          let candidates = A.range lo hi
          let
            { best, bestFill } = foldl
              ( \b i -> do
                  let f = fromMaybe 0 (M.lookup i acc.filling)
                  if f < b.bestFill then { best: i, bestFill: f } else b
              )
              { best: curLayer, bestFill: fromMaybe 0 (M.lookup curLayer acc.filling) }
              candidates
          if best == curLayer then acc
          else do
            let curFill = fromMaybe 0 (M.lookup curLayer acc.filling)
            let
              filling' = M.insert curLayer (curFill - 1)
                $ M.insert best (bestFill + 1) acc.filling
            { layers: M.insert n best acc.layers, filling: filling' }

  -- | Port of `minimalSpan`: minimum incoming-edge span and minimum
  -- | outgoing-edge span at a node, in current layer coords.
  minimalSpan :: NodeId -> Array (NEdge NodeId) -> Map NodeId Int -> Int /\ Int
  minimalSpan n es ls = do
    let layerOfL m = fromMaybe 0 (M.lookup m ls)
    let
      r = foldl
        ( \acc e ->
            if e.tgt == n then do
              let s = layerOfL n - layerOfL e.src
              acc { mIn = min acc.mIn s }
            else if e.src == n then do
              let s = layerOfL e.tgt - layerOfL n
              acc { mOut = min acc.mOut s }
            else acc
        )
        { mIn: top, mOut: top }
        es
    let mIn = if r.mIn == top then -1 else r.mIn
    let mOut = if r.mOut == top then -1 else r.mOut
    mIn /\ mOut
    where
    top = 1000000000
