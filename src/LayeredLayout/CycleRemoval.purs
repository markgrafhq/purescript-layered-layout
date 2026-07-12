module LayeredLayout.CycleRemoval
  ( makeAcyclic
  , makeAcyclicWith
  , makeAcyclicWithOrder
  , AcyclicResult
  , CycleStrategy(..)
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as S
import Data.Tuple.Nested (type (/\), (/\))
import LayeredLayout.Graph (Constraints(..), Edge, LayerPin(..), NodeId)

type AcyclicResult =
  { edges :: Array Edge
  , reversedEdges :: Set (NodeId /\ NodeId)
  }

-- | Cycle-removal strategy. `DepthFirst` is the existing
-- | DepthFirstCycleBreaker port. `Greedy` is ELK's default
-- | GreedyCycleBreaker, which drains sinks/sources iteratively and
-- | breaks ties by maximum out-flow; it generally reverses fewer
-- | edges than the DFS variant.
data CycleStrategy = DepthFirst | Greedy

derive instance Eq CycleStrategy

makeAcyclic :: Array Constraints -> Array Edge -> AcyclicResult
makeAcyclic = makeAcyclicWith DepthFirst

makeAcyclicWith :: CycleStrategy -> Array Constraints -> Array Edge -> AcyclicResult
makeAcyclicWith strat constraints edges = makeAcyclicWithOrder strat [] constraints edges

-- | Like `makeAcyclicWith` but lets the caller pass the canonical
-- | input order of nodes so the Greedy strategy can break max-outflow
-- | ties by model order (port of ELK's GREEDY_MODEL_ORDER variant).
-- | Pass `[]` when no model order should be applied.
makeAcyclicWithOrder :: CycleStrategy -> Array NodeId -> Array Constraints -> Array Edge -> AcyclicResult
makeAcyclicWithOrder strat modelOrder constraints edges = do
  let
    hints = layerHints constraints
    modelIdx = M.fromFoldable (A.mapWithIndex (\i n -> n /\ i) modelOrder)
    backEdges = case strat of
      DepthFirst -> findBackEdges hints edges
      Greedy -> findBackEdgesGreedy hints modelIdx edges
    resultEdges = edges <#> \e -> do
      let key = e.from.node /\ e.to.node
      if S.member key backEdges then reverseEdge e else e
  { edges: resultEdges, reversedEdges: backEdges }

-- | Port of `GreedyCycleBreaker.process`. Runs the Eades-Lin-Smyth
-- | feedback-arc-set heuristic over the directed graph: drain sinks
-- | and sources, then break ties by selecting the unprocessed node
-- | with the maximum out-flow (outDeg - inDeg). The mark order is
-- | sources<low to high>, max-outflow nodes, sinks<high to low>; an
-- | edge whose source mark exceeds its target mark is a back-edge.
-- | Layer hints reverse cleanly here too: a hint-reversed edge is
-- | accumulated up-front so it's never a candidate for reversal.
findBackEdgesGreedy :: Map NodeId Int -> Map NodeId Int -> Array Edge -> Set (NodeId /\ NodeId)
findBackEdgesGreedy hints modelIdx edges = do
  let allNodes = A.nub $ (edges <#> \e -> e.from.node) <> (edges <#> \e -> e.to.node)
  let nonSelfEdges = A.filter (\e -> e.from.node /= e.to.node) edges
  let inDeg0 = foldl (\m e -> M.insertWith (+) (e.to.node) 1 m) M.empty nonSelfEdges
  let outDeg0 = foldl (\m e -> M.insertWith (+) (e.from.node) 1 m) M.empty nonSelfEdges
  let sourcesInit = A.filter (\n -> fromMaybe 0 (M.lookup n inDeg0) == 0) allNodes
  let sinksInit = A.filter (\n -> fromMaybe 0 (M.lookup n outDeg0) == 0) allNodes
  let initialMarks = M.empty :: Map NodeId Int
  let
    st = drainAll
      { remaining: A.filter (\n -> not (A.elem n sourcesInit) && not (A.elem n sinksInit)) allNodes
      , marks: initialMarks
      , inDeg: inDeg0
      , outDeg: outDeg0
      , sources: sourcesInit
      , sinks: sinksInit
      , nextLeft: 1
      , nextRight: -1
      }
  let total = A.length allNodes
  let shifted = shiftNegative total st.marks allNodes
  collectBackEdges hints shifted edges
  where
  drainAll st = case A.uncons st.sinks of
    Just { head: sink, tail: rest } -> do
      let st' = st { sinks = rest, marks = M.insert sink st.nextRight st.marks, nextRight = st.nextRight - 1 }
      drainAll (updateNeighborsGreedy edges sink st')
    Nothing -> case A.uncons st.sources of
      Just { head: src, tail: rest } -> do
        let st' = st { sources = rest, marks = M.insert src st.nextLeft st.marks, nextLeft = st.nextLeft + 1 }
        drainAll (updateNeighborsGreedy edges src st')
      Nothing -> case pickMaxOutflow st of
        Nothing -> st
        Just node -> do
          let
            st' = st
              { remaining = A.filter (_ /= node) st.remaining
              , marks = M.insert node st.nextLeft st.marks
              , nextLeft = st.nextLeft + 1
              }
          drainAll (updateNeighborsGreedy edges node st')

  -- Port of ELK's GREEDY_MODEL_ORDER: when several remaining nodes
  -- share the maximum outflow, break the tie by their position in
  -- the input model order (smaller index wins). When no model order
  -- is supplied (`modelIdx` empty), the comparison falls back to the
  -- node's id, matching plain GreedyCycleBreaker but in a
  -- deterministic way.
  pickMaxOutflow st = A.head sorted
    where
    sorted = A.sortBy compareNodes st.remaining
    outflow n = fromMaybe 0 (M.lookup n st.outDeg) - fromMaybe 0 (M.lookup n st.inDeg)
    compareNodes a b = case compare (outflow b) (outflow a) of
      EQ -> compare (orderOf a) (orderOf b)
      other -> other
    orderOf n = fromMaybe 1000000 (M.lookup n modelIdx)

  shiftNegative total marks ns = foldl shift marks ns
    where
    shiftBase = total + 1
    shift m n = case M.lookup n m of
      Just k | k < 0 -> M.insert n (k + shiftBase) m
      _ -> m

  collectBackEdges hs marks es = foldl
    ( \acc e -> do
        let s = e.from.node
        let t = e.to.node
        if s == t then acc
        else if isHintReversed hs s t then S.insert (s /\ t) acc
        else case M.lookup s marks /\ M.lookup t marks of
          Just ms /\ Just mt | ms > mt -> S.insert (s /\ t) acc
          _ -> acc
    )
    S.empty
    es

-- | Decrement neighbour in/out degree to simulate node removal,
-- | promoting nodes to source/sink lists when their counters reach 0.
updateNeighborsGreedy
  :: Array Edge
  -> NodeId
  -> { remaining :: Array NodeId
     , marks :: Map NodeId Int
     , inDeg :: Map NodeId Int
     , outDeg :: Map NodeId Int
     , sources :: Array NodeId
     , sinks :: Array NodeId
     , nextLeft :: Int
     , nextRight :: Int
     }
  -> { remaining :: Array NodeId
     , marks :: Map NodeId Int
     , inDeg :: Map NodeId Int
     , outDeg :: Map NodeId Int
     , sources :: Array NodeId
     , sinks :: Array NodeId
     , nextLeft :: Int
     , nextRight :: Int
     }
updateNeighborsGreedy edges node st = do
  let st1 = st { remaining = A.filter (_ /= node) st.remaining }
  foldl visit st1 edges
  where
  visit acc e = do
    let s = e.from.node
    let t = e.to.node
    if s == t then acc
    else if s == node && not (M.member t acc.marks) then do
      let inT = (fromMaybe 0 (M.lookup t acc.inDeg)) - 1
      let inDeg' = M.insert t inT acc.inDeg
      let outT = fromMaybe 0 (M.lookup t acc.outDeg)
      if inT <= 0 && outT > 0 && not (A.elem t acc.sources) then acc { inDeg = inDeg', sources = acc.sources <> [ t ] }
      else acc { inDeg = inDeg' }
    else if t == node && not (M.member s acc.marks) then do
      let outS = (fromMaybe 0 (M.lookup s acc.outDeg)) - 1
      let outDeg' = M.insert s outS acc.outDeg
      let inS = fromMaybe 0 (M.lookup s acc.inDeg)
      if outS <= 0 && inS > 0 && not (A.elem s acc.sinks) then acc { outDeg = outDeg', sinks = acc.sinks <> [ s ] }
      else acc { outDeg = outDeg' }
    else acc

findBackEdges :: Map NodeId Int -> Array Edge -> Set (NodeId /\ NodeId)
findBackEdges hints edges = do
  let adj = buildAdj edges
  let allNodes = A.nub $ (edges <#> _.from.node) <> (edges <#> _.to.node)
  -- Port of `DepthFirstCycleBreaker.process`: DFS starts from source
  -- nodes (those with no incoming edges) first, then visits any
  -- remaining unvisited nodes. This produces deterministic, well-founded
  -- back-edge detection: cycles among non-sources have a defined entry
  -- point, while reachable nodes are visited from their natural parent.
  let hasIncoming = foldl (\s e -> S.insert e.to.node s) S.empty edges
  let sources = A.filter (\n -> not (S.member n hasIncoming)) allNodes
  let nonSources = A.filter (\n -> S.member n hasIncoming) allNodes
  let
    result = foldl (\acc node -> visitNode hints adj node acc)
      { visiting: S.empty, visited: S.empty, backEdges: S.empty }
      (sources <> nonSources)
  result.backEdges

type DFSState =
  { visiting :: Set NodeId
  , visited :: Set NodeId
  , backEdges :: Set (NodeId /\ NodeId)
  }

visitNode :: Map NodeId Int -> Map NodeId (Array NodeId) -> NodeId -> DFSState -> DFSState
visitNode hints adj node state
  | S.member node state.visited = state
  | S.member node state.visiting = state
  | otherwise = do
      let state' = state { visiting = S.insert node state.visiting }
      let children = fromMaybe [] (M.lookup node adj)
      let state'' = foldl (visitEdge hints adj node) state' children
      state'' { visiting = S.delete node state''.visiting, visited = S.insert node state''.visited }

visitEdge :: Map NodeId Int -> Map NodeId (Array NodeId) -> NodeId -> DFSState -> NodeId -> DFSState
visitEdge hints adj fromNode state toNode
  | isHintReversed hints fromNode toNode = state { backEdges = S.insert (fromNode /\ toNode) state.backEdges }
  | S.member toNode state.visiting = state { backEdges = S.insert (fromNode /\ toNode) state.backEdges }
  | S.member toNode state.visited = state
  | otherwise = visitNode hints adj toNode state

isHintReversed :: Map NodeId Int -> NodeId -> NodeId -> Boolean
isHintReversed hints fromNode toNode = case M.lookup fromNode hints /\ M.lookup toNode hints of
  Just f /\ Just t -> f > t
  _ -> false

reverseEdge :: Edge -> Edge
reverseEdge e = e { from = e.to, to = e.from }

buildAdj :: Array Edge -> Map NodeId (Array NodeId)
buildAdj = foldl (\m e -> M.insertWith (<>) e.from.node [ e.to.node ] m) M.empty

layerHints :: Array Constraints -> Map NodeId Int
layerHints = foldl go M.empty
  where
  go acc = case _ of
    LayerConstraint { node, pin: SpecificLayer n } -> M.insert node n acc
    LayerConstraint { node, pin: FirstLayer } -> M.insert node 0 acc
    LayerConstraint { node, pin: LastLayer } -> M.insert node 99999 acc
    _ -> acc
