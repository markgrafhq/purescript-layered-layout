-- | Generic Gansner-Koutsofios-North-Vo network simplex algorithm,
-- | shared between layer assignment and the post-routing graph
-- | compactor.
-- |
-- | Port of ELK's `org.eclipse.elk.alg.common.networksimplex.NetworkSimplex`.
-- |
-- | The algorithm is parameterised over the node identifier type
-- | (any `Ord n`) and accepts per-edge `delta` (minimum span) and
-- | `weight` (importance for shortness). Cutvalues are stored as
-- | `Number` (mirroring ELK's `double`) and compared against
-- | `fuzzyStZero` to track ELK's tolerance for floating-point
-- | imprecision.
-- |
-- | Connected-component splitting, layer balancing, and disconnected
-- | component stacking are left to callers — they are layering-specific.
module LayeredLayout.NetworkSimplex
  ( NEdge
  , runNetworkSimplex
  , fuzzyStZero
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as S
import Data.Tuple.Nested ((/\))

-- | Tolerance for floating-point imprecision when checking whether a
-- | cut value is negative. Port of ELK's `FUZZY_ST_ZERO = -1e-10`.
fuzzyStZero :: Number
fuzzyStZero = -1.0e-10

-- | An edge for the network simplex algorithm.
-- |
-- | * `delta`  – the minimum span (target.layer − source.layer ≥ delta).
-- | * `weight` – contribution to the optimisation objective. Network
-- |   simplex minimises the weighted sum of `weight * (layer span)`
-- |   across all edges, so heavier edges are kept shorter.
-- | * `eid`    – a per-edge integer index, unique within the input
-- |   array; the algorithm uses it as a key into cutvalue / visited
-- |   sets.
type NEdge n =
  { src :: n
  , tgt :: n
  , delta :: Int
  , weight :: Number
  , eid :: Int
  }

type NSState n =
  { layer :: Map n Int
  , treeNode :: Set n
  , treeEdge :: Set Int
  , poID :: Map n Int
  , lowestPoID :: Map n Int
  , cutvalue :: Map Int Number
  , postOrder :: Int
  , edgeVisited :: Set Int
  }

-- | Threshold below which the leaf-pruning optimisation is skipped
-- | (port of `REMOVE_SUBTREES_THRESH = 40`).
removeSubtreesThreshold :: Int
removeSubtreesThreshold = 40

-- | Sentinel used as `+infinity` for slack searches.
infInt :: Int
infInt = 1000000000

-- | Run the network simplex over the given nodes and edges.
-- |
-- | The caller is responsible for ensuring the input is weakly-
-- | connected (split into components, or inject an artificial root
-- | that dominates all sources). When the input is disconnected the
-- | algorithm still terminates, but the layering of the unreached
-- | component(s) is not optimised.
runNetworkSimplex :: forall n. Ord n => Array n -> Array (NEdge n) -> Map n Int
runNetworkSimplex nodes edges
  | A.null nodes = M.empty
  | A.length nodes < removeSubtreesThreshold = normalise nodes (runSimplexCore nodes edges)
  | otherwise = do
      let pruned = removeSubtrees nodes edges
      let coreLayer = runSimplexCore pruned.coreNodes pruned.coreEdges
      let merged = reattachSubtrees pruned.removed coreLayer
      normalise nodes merged

-- | Run the simplex on a graph without leaf-pruning and without
-- | normalising. The caller normalises after any reattach.
runSimplexCore :: forall n. Ord n => Array n -> Array (NEdge n) -> Map n Int
runSimplexCore nodes edges = do
  let st0 = initialState nodes
  let st1 = layeringTopological nodes edges st0
  if A.null edges then st1.layer
  else do
    let st2 = feasibleTree nodes edges st1
    let iterLimit = 4 * A.length nodes
    let st3 = optimiseLoop iterLimit nodes edges st2
    st3.layer

-- ── Subtree removal / reattachment ─────────────────────────────────

removeSubtrees
  :: forall n
   . Ord n
  => Array n
  -> Array (NEdge n)
  -> { coreNodes :: Array n
     , coreEdges :: Array (NEdge n)
     , removed :: Array { node :: n, neighbour :: n, viaSrc :: Boolean }
     }
removeSubtrees nodes edges = do
  let
    degree0 = foldl
      (\m e -> M.insertWith (+) e.src 1 (M.insertWith (+) e.tgt 1 m))
      M.empty
      edges
  let initialLeaves = A.filter (\n -> fromMaybe 0 (M.lookup n degree0) == 1) nodes
  let
    drained = drain
      { degree: degree0
      , removedNodes: S.empty
      , removedEdges: S.empty
      , record: ([] :: Array { node :: n, neighbour :: n, viaSrc :: Boolean })
      , queue: initialLeaves
      }
  { coreNodes: A.filter (\n -> not (S.member n drained.removedNodes)) nodes
  , coreEdges: A.filter (\e -> not (S.member e.eid drained.removedEdges)) edges
  , removed: drained.record
  }
  where
  drain st = case A.uncons st.queue of
    Nothing -> st
    Just { head: leaf, tail: rest } ->
      if S.member leaf st.removedNodes then drain (st { queue = rest })
      else case findIncidentEdge leaf st of
        Nothing -> drain (st { queue = rest })
        Just edge -> do
          let neighbour = if edge.src == leaf then edge.tgt else edge.src
          let viaSrc = edge.src == leaf
          let
            st' = st
              { degree = M.insert neighbour ((fromMaybe 0 (M.lookup neighbour st.degree)) - 1) st.degree
              , removedNodes = S.insert leaf st.removedNodes
              , removedEdges = S.insert edge.eid st.removedEdges
              , record = st.record <> [ { node: leaf, neighbour, viaSrc } ]
              , queue = rest
              }
          let nDeg = fromMaybe 0 (M.lookup neighbour st'.degree)
          if nDeg == 1 && not (S.member neighbour st'.removedNodes) then drain (st' { queue = st'.queue <> [ neighbour ] })
          else drain st'

  findIncidentEdge leaf st = A.find
    ( \e -> not (S.member e.eid st.removedEdges)
        && (e.src == leaf || e.tgt == leaf)
    )
    edges

reattachSubtrees
  :: forall n
   . Ord n
  => Array { node :: n, neighbour :: n, viaSrc :: Boolean }
  -> Map n Int
  -> Map n Int
reattachSubtrees record coreLayer = foldl step coreLayer (A.reverse record)
  where
  step layers r = do
    let neighbourLayer = fromMaybe 0 (M.lookup r.neighbour layers)
    let myLayer = if r.viaSrc then neighbourLayer - 1 else neighbourLayer + 1
    M.insert r.node myLayer layers

-- ── Initial state + topological layering ───────────────────────────

initialState :: forall n. Ord n => Array n -> NSState n
initialState nodes =
  { layer: M.fromFoldable (nodes <#> \n -> n /\ 0)
  , treeNode: S.empty
  , treeEdge: S.empty
  , poID: M.empty
  , lowestPoID: M.empty
  , cutvalue: M.empty
  , postOrder: 1
  , edgeVisited: S.empty
  }

layerOf :: forall n. Ord n => NSState n -> n -> Int
layerOf st n = fromMaybe 0 (M.lookup n st.layer)

layeringTopological :: forall n. Ord n => Array n -> Array (NEdge n) -> NSState n -> NSState n
layeringTopological nodes edges st0 = do
  let outgoing = byNode _.src edges
  let incoming = byNode _.tgt edges
  let initIncident = nodes <#> \n -> n /\ A.length (fromMaybe [] (M.lookup n incoming))
  let incidentMap0 = M.fromFoldable initIncident
  let sources = A.filter (\n -> fromMaybe 0 (M.lookup n incidentMap0) == 0) nodes
  go outgoing incidentMap0 sources st0
  where
  go outs incident queue st = case A.uncons queue of
    Nothing -> st
    Just { head: n, tail: rest } -> do
      let myOuts = fromMaybe [] (M.lookup n outs)
      let layN = layerOf st n
      let
        { st: st', incident: incident', queue: queue' } = foldl
          ( \acc e -> do
              let tgtLayer = max (layerOf acc.st e.tgt) (layN + e.delta)
              let stNext = acc.st { layer = M.insert e.tgt tgtLayer acc.st.layer }
              let nextIncident = (fromMaybe 0 (M.lookup e.tgt acc.incident)) - 1
              let incidentNext = M.insert e.tgt nextIncident acc.incident
              let queueNext = if nextIncident == 0 then acc.queue <> [ e.tgt ] else acc.queue
              { st: stNext, incident: incidentNext, queue: queueNext }
          )
          { st, incident, queue: rest }
          myOuts
      go outs incident' queue' st'

-- ── feasibleTree ───────────────────────────────────────────────────

feasibleTree :: forall n. Ord n => Array n -> Array (NEdge n) -> NSState n -> NSState n
feasibleTree nodes edges st0 = cutvalues nodes edges (postorderTraversal' nodes edges (expandTightTree nodes edges st0))

expandTightTree :: forall n. Ord n => Array n -> Array (NEdge n) -> NSState n -> NSState n
expandTightTree nodes edges st = case A.head nodes of
  Nothing -> st
  Just root -> do
    let stCleared = st { edgeVisited = S.empty, treeNode = S.empty, treeEdge = S.empty }
    let result = tightTreeDFS edges root stCleared
    if result.count >= A.length nodes then result.st
    else case minimalSlack edges result.st of
      Nothing -> result.st
      Just e -> do
        let slack = layerOf result.st e.tgt - layerOf result.st e.src - e.delta
        let actualSlack = if S.member e.tgt result.st.treeNode then -slack else slack
        let
          shifted = result.st
            { layer = foldl
                ( \m nd ->
                    if S.member nd result.st.treeNode then M.insert nd (layerOf result.st nd + actualSlack) m
                    else m
                )
                result.st.layer
                nodes
            }
        expandTightTree nodes edges shifted

tightTreeDFS :: forall n. Ord n => Array (NEdge n) -> n -> NSState n -> { count :: Int, st :: NSState n }
tightTreeDFS edges root st = do
  let st' = st { treeNode = S.insert root st.treeNode }
  let connected = A.filter (\e -> (e.src == root || e.tgt == root) && not (S.member e.eid st'.edgeVisited)) edges
  foldl visit { count: 1, st: st' } connected
  where
  visit acc e =
    if S.member e.eid acc.st.edgeVisited then acc
    else do
      let st1 = acc.st { edgeVisited = S.insert e.eid acc.st.edgeVisited }
      let
        current =
          if S.member e.src st1.treeNode && not (S.member e.tgt st1.treeNode) then e.src
          else if S.member e.tgt st1.treeNode && not (S.member e.src st1.treeNode) then e.tgt
          else e.src
      let other = if e.src == current then e.tgt else e.src
      if S.member e.eid st1.treeEdge then
        if S.member other st1.treeNode then acc { st = st1 }
        else do
          let r = tightTreeDFS edges other st1
          { count: acc.count + r.count, st: r.st }
      else if
        not (S.member other st1.treeNode)
          && e.delta == layerOf st1 e.tgt - layerOf st1 e.src then do
        let st2 = st1 { treeEdge = S.insert e.eid st1.treeEdge }
        let r = tightTreeDFS edges other st2
        { count: acc.count + r.count, st: r.st }
      else acc { st = st1 }

minimalSlack :: forall n. Ord n => Array (NEdge n) -> NSState n -> Maybe (NEdge n)
minimalSlack edges st = (foldl scan { edge: Nothing, slack: infInt } edges).edge
  where
  scan acc e = do
    let s = S.member e.src st.treeNode
    let t = S.member e.tgt st.treeNode
    if s == t then acc
    else do
      let slack = layerOf st e.tgt - layerOf st e.src - e.delta
      if slack < acc.slack then { edge: Just e, slack }
      else acc

-- ── postorderTraversal ────────────────────────────────────────────

postorderTraversal' :: forall n. Ord n => Array n -> Array (NEdge n) -> NSState n -> NSState n
postorderTraversal' nodes edges st = case A.head nodes of
  Nothing -> st
  Just root -> do
    let stCleared = st { edgeVisited = S.empty, postOrder = 1, poID = M.empty, lowestPoID = M.empty }
    (postorderDFS edges root stCleared).st

postorderDFS :: forall n. Ord n => Array (NEdge n) -> n -> NSState n -> { lowest :: Int, st :: NSState n }
postorderDFS edges node st0 = do
  let
    connected = A.filter
      ( \e -> S.member e.eid st0.treeEdge
          && (e.src == node || e.tgt == node)
          && not (S.member e.eid st0.edgeVisited)
      )
      edges
  let result = foldl visit { lowest: infInt, st: st0 } connected
  let myPo = result.st.postOrder
  let lowest' = min result.lowest myPo
  let
    st' = result.st
      { poID = M.insert node myPo result.st.poID
      , lowestPoID = M.insert node lowest' result.st.lowestPoID
      , postOrder = myPo + 1
      }
  { lowest: lowest', st: st' }
  where
  visit acc e = do
    let st1 = acc.st { edgeVisited = S.insert e.eid acc.st.edgeVisited }
    let other = if e.src == node then e.tgt else e.src
    let r = postorderDFS edges other st1
    { lowest: min acc.lowest r.lowest, st: r.st }

-- ── cutvalues ─────────────────────────────────────────────────────

cutvalues :: forall n. Ord n => Array n -> Array (NEdge n) -> NSState n -> NSState n
cutvalues nodes edges st0 = do
  let unknownInit = nodes <#> \n -> n /\ A.fromFoldable (S.fromFoldable (incidentTreeEdges edges st0 n))
  let initSt = { unknown: M.fromFoldable unknownInit, cutvalue: M.empty :: Map Int Number }
  let leafs = A.filter (\n -> A.length (fromMaybe [] (M.lookup n initSt.unknown)) == 1) nodes
  let final = foldl (drainLeaf edges) initSt leafs
  st0 { cutvalue = final.cutvalue }

incidentTreeEdges :: forall n. Ord n => Array (NEdge n) -> NSState n -> n -> Array (NEdge n)
incidentTreeEdges edges st node = A.filter
  (\e -> S.member e.eid st.treeEdge && (e.src == node || e.tgt == node))
  edges

drainLeaf
  :: forall n
   . Ord n
  => Array (NEdge n)
  -> { unknown :: Map n (Array (NEdge n)), cutvalue :: Map Int Number }
  -> n
  -> { unknown :: Map n (Array (NEdge n)), cutvalue :: Map Int Number }
drainLeaf edges acc startNode = go acc startNode
  where
  go st node = case fromMaybe [] (M.lookup node st.unknown) of
    [ toDetermine ] -> do
      let other = if toDetermine.src == node then toDetermine.tgt else toDetermine.src
      let value = computeCutvalue edges st node toDetermine
      let unknown' = removeFrom node toDetermine (removeFrom other toDetermine st.unknown)
      let cutvalue' = M.insert toDetermine.eid value st.cutvalue
      go { unknown: unknown', cutvalue: cutvalue' } other
    _ -> st

  removeFrom n e m = case M.lookup n m of
    Just xs -> M.insert n (A.filter (\x -> x.eid /= e.eid) xs) m
    Nothing -> m

computeCutvalue
  :: forall n
   . Ord n
  => Array (NEdge n)
  -> { unknown :: Map n (Array (NEdge n)), cutvalue :: Map Int Number }
  -> n
  -> NEdge n
  -> Number
computeCutvalue edges st node toDetermine = foldl accumulate toDetermine.weight connected
  where
  connected = A.filter (\e -> e.eid /= toDetermine.eid && (e.src == node || e.tgt == node)) edges
  source = toDetermine.src
  target = toDetermine.tgt

  accumulate val e = do
    let
      isTreeEdge = case M.lookup e.eid st.cutvalue of
        Just _ -> true
        Nothing -> false
    if isTreeEdge then do
      let cv = fromMaybe 0.0 (M.lookup e.eid st.cutvalue)
      if (source == e.src) || (target == e.tgt) then val - (cv - e.weight)
      else val + (cv - e.weight)
    else if node == source then
      if e.src == node then val + e.weight else val - e.weight
    else if e.src == node then val - e.weight
    else val + e.weight

-- ── optimise loop: leaveEdge / enterEdge / exchange ───────────────

optimiseLoop :: forall n. Ord n => Int -> Array n -> Array (NEdge n) -> NSState n -> NSState n
optimiseLoop iterLimit nodes edges st = go iterLimit st
  where
  go 0 s = s
  go k s = case leaveEdge edges s of
    Nothing -> s
    Just leave -> case enterEdge edges leave s of
      Nothing -> s
      Just enter -> go (k - 1) (exchange nodes edges leave enter s)

-- | A tree edge with a negative cut value (below `fuzzyStZero`) is
-- | the candidate to leave the spanning tree.
leaveEdge :: forall n. Array (NEdge n) -> NSState n -> Maybe (NEdge n)
leaveEdge edges st = A.find
  ( \e -> S.member e.eid st.treeEdge
      && fromMaybe 0.0 (M.lookup e.eid st.cutvalue) < fuzzyStZero
  )
  edges

enterEdge :: forall n. Ord n => Array (NEdge n) -> NEdge n -> NSState n -> Maybe (NEdge n)
enterEdge edges leave st = (foldl scan { edge: Nothing, slack: infInt } edges).edge
  where
  scan acc e =
    if isInHead st e.src leave && not (isInHead st e.tgt leave) then do
      let s = layerOf st e.tgt - layerOf st e.src - e.delta
      if s < acc.slack then { edge: Just e, slack: s }
      else acc
    else acc

isInHead :: forall n. Ord n => NSState n -> n -> NEdge n -> Boolean
isInHead st node leave = do
  let srcPo = lookupPo st leave.src
  let tgtPo = lookupPo st leave.tgt
  let srcLow = lookupLow st leave.src
  let tgtLow = lookupLow st leave.tgt
  let nodePo = lookupPo st node
  if
    srcLow <= nodePo && nodePo <= srcPo
      && tgtLow <= nodePo
      && nodePo <= tgtPo then srcPo >= tgtPo
  else srcPo < tgtPo

lookupPo :: forall n. Ord n => NSState n -> n -> Int
lookupPo st n = fromMaybe 0 (M.lookup n st.poID)

lookupLow :: forall n. Ord n => NSState n -> n -> Int
lookupLow st n = fromMaybe 0 (M.lookup n st.lowestPoID)

exchange
  :: forall n
   . Ord n
  => Array n
  -> Array (NEdge n)
  -> NEdge n
  -> NEdge n
  -> NSState n
  -> NSState n
exchange nodes edges leave enter st = do
  let st1 = st { treeEdge = S.insert enter.eid (S.delete leave.eid st.treeEdge) }
  let delta0 = layerOf st1 enter.tgt - layerOf st1 enter.src - enter.delta
  let delta = if isInHead st1 enter.tgt leave then delta0 else -delta0
  let
    st2 = st1
      { layer = foldl
          ( \m nd ->
              if not (isInHead st1 nd leave) then M.insert nd (layerOf st1 nd + delta) m
              else m
          )
          st1.layer
          nodes
      }
  let st3 = postorderTraversal' nodes edges st2
  cutvalues nodes edges st3

-- ── normalise ─────────────────────────────────────────────────────

-- | Shift the layering so the minimum layer is 0.
normalise :: forall n. Ord n => Array n -> Map n Int -> Map n Int
normalise nodes layers = do
  let lo = foldl (\m n -> min m (fromMaybe 0 (M.lookup n layers))) infInt nodes
  M.fromFoldable (nodes <#> \n -> n /\ ((fromMaybe 0 (M.lookup n layers)) - lo))

-- ── helpers ──────────────────────────────────────────────────────

byNode :: forall n. Ord n => (NEdge n -> n) -> Array (NEdge n) -> Map n (Array (NEdge n))
byNode keyFn = foldl (\m e -> M.insertWith (<>) (keyFn e) [ e ] m) M.empty
