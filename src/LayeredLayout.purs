module LayeredLayout
  ( layout
  , Config
  , defaultConfig
  , Pipeline
  , ComponentCache
  , LayoutOutput
  , full
  , fromDummies
  , fromCrossMin
  , fromCoords
  , fromRouting
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Set as S
import Data.Tuple.Nested (type (/\), (/\))
import LayeredLayout.Aesthetics (allMetrics)
import LayeredLayout.Compaction.HorizontalGraphCompactor (BetweenLayersSpacings, CompactionStrategy(..), compactPostRouting, defaultBetweenLayersSpacings)
import LayeredLayout.Components as Components
import LayeredLayout.EdgeRouting (routeAll)
import LayeredLayout.EdgeRouting.HyperEdges (SlotInfo)
import LayeredLayout.EdgeLabels as EdgeLabels
import LayeredLayout.EdgeLabels.SelfLoops as SelfLoops
import LayeredLayout.EdgeRouting.LineJump (detectJumps)
import LayeredLayout.EdgeRouting.Orthogonal (mergeCollinear, removeZeroLength)
import LayeredLayout.PortDistribution as PortDistribution
import LayeredLayout.PortDummies as PortDummies
import LayeredLayout.Graph (EdgeId(..), Graph, NodeId(..), Side(..))
import LayeredLayout.Grid (GridPos(..), GridRect, GridSize(..), gridX, gridY, sizeH, sizeW)
import LayeredLayout.JavaRandom (Random, mkRandom)
import LayeredLayout.CoordAssignment as CoordAssignment
import LayeredLayout.CrossingMin as CrossingMin
import LayeredLayout.CycleRemoval as CycleRemoval
import LayeredLayout.DummyNodes as DummyNodes
import LayeredLayout.LayerAssignment as LayerAssignment
import LayeredLayout.Result (EdgePath, EdgeSegment, LayoutResult, NodePlacement)

type Config =
  { nodeGap :: Int
  , layerGap :: Int
  , iterations :: Int
  , maxGapCount :: Int
  , layerer :: LayerAssignment.LayererStrategy
  , cycleBreaker :: CycleRemoval.CycleStrategy
  -- Run post-routing compaction along the layer-growth axis while
  -- preserving node, label, and edge clearances.
  , compactPostRouting :: Boolean
  -- Between-layer compaction spacings, in fine/router units. Within-layer
  -- node spacing comes from nodeGap; within-layer edge spacing is ELK's 10.
  , compactionSpacings :: BetweenLayersSpacings
  , edgeLabelSizes :: M.Map EdgeId GridSize
  }

defaultConfig :: Config
defaultConfig =
  { nodeGap: 3
  , layerGap: 2
  -- d2/ELK: THOROUGHNESS=8 controls how many randomized layer
  -- sweeps the crossing minimiser runs before keeping the best.
  , iterations: 8
  , maxGapCount: 2
  -- Match d2/ELK defaults: NetworkSimplex minimises total edge
  -- length and Greedy reverses fewer edges than DFS. Both produce
  -- noticeably more compact layouts on non-trivial graphs.
  , layerer: LayerAssignment.NetworkSimplex
  , cycleBreaker: CycleRemoval.Greedy
  , compactPostRouting: true
  , compactionSpacings: defaultBetweenLayersSpacings
  , edgeLabelSizes: M.empty
  }

-- | Cached intermediate results from each pipeline phase.
-- Holding onto this allows selective recomputation when only
-- later phases need to rerun.
-- Caches retain topology and declared ports. Use full when either changes.
type Pipeline =
  { acyclic :: CycleRemoval.AcyclicResult
  , layered :: LayerAssignment.LayeredGraph
  , withDummies :: DummyNodes.DummyResult
  , components :: Array ComponentCache
  , initialRandom :: Random
  , ordered :: Array (Array NodeId)
  , random :: Random
  , routedRandom :: Random
  , portOrder :: PortDistribution.PortOrder
  , portDummies :: PortDummies.State
  , placements :: Array NodePlacement
  , labels :: EdgeLabels.LabelState
  , loops :: SelfLoops.LoopState
  }

-- Component frames are reversible: cached placements use packed coordinates,
-- while every algorithm reruns in its original component-local frame.
type ComponentCache =
  { nodes :: S.Set NodeId
  , layerOffset :: Int
  , layerCount :: Int
  , offset :: GridPos
  , initialRandom :: Random
  , random :: Random
  , routedRandom :: Random
  }

type LayoutOutput = { pipeline :: Pipeline, result :: LayoutResult }

-- ══════════════════════════════════════════════════════════════════
--  Phased entry points
-- ══════════════════════════════════════════════════════════════════

-- | Full pipeline from scratch.
full :: Config -> Graph -> LayoutOutput
full cfg graph = case Components.partition graph of
  [] -> fullConnected (mkRandom 1.0) cfg graph
  [ _ ] -> fullConnected (mkRandom 1.0) cfg graph
  components -> combineComponents cfg graph (foldl run { random: mkRandom 1.0, outputs: [] } components).outputs
  where
  run state component =
    let
      output = fullConnected state.random cfg component
    in
      { random: output.pipeline.routedRandom, outputs: A.snoc state.outputs output }

fullConnected :: Random -> Config -> Graph -> LayoutOutput
fullConnected initialRandom cfg graph = fromDummies cfg graph pipeline
  where
  acyclic = CycleRemoval.makeAcyclicWithOrder cfg.cycleBreaker allNodeIds graph.constraints regularEdges
  labels = EdgeLabels.insert cfg.edgeLabelSizes graph acyclic.edges
  layeredIds = if A.null labels.nodes then allNodeIds else allNodeIds <> (labels.nodes <#> _.id)
  layered = LayerAssignment.assignLayersWith cfg.layerer graph.constraints labels.edges layeredIds
  pipeline = { acyclic, layered, labels, loops: SelfLoops.empty, components: [], initialRandom, random: initialRandom, routedRandom: initialRandom, portOrder: M.empty, portDummies: PortDummies.empty, withDummies: dummyPlaceholder, ordered: [], placements: [] }
  allNodeIds = graph.nodes <#> _.id
  -- SelfLoopPreProcessor removes these before cycle removal and layering.
  regularEdges = A.filter (\e -> e.from.node /= e.to.node) graph.edges

-- | Rerun from dummy node insertion (node moved to different layer).
fromDummies :: Config -> Graph -> Pipeline -> LayoutOutput
fromDummies cfg graph pipeline | cfg.edgeLabelSizes /= pipeline.labels.sizes = full cfg graph
fromDummies cfg graph pipeline | not (A.null pipeline.components) = rerunComponents fromDummies cfg graph pipeline
fromDummies cfg graph pipeline = fromCrossMin cfg graph pipeline'
  where
  withDummies = DummyNodes.insertDummies pipeline.layered.nodeLayer pipeline.labels.edges pipeline.layered.layers
  prepared = PortDummies.prepare (M.fromFoldable (graph.nodes <#> \n -> n.id /\ n.ports)) withDummies
  pipeline' = pipeline { withDummies = prepared.dummies, portDummies = prepared.state }

-- | Rerun from crossing minimization (node order changed).
fromCrossMin :: Config -> Graph -> Pipeline -> LayoutOutput
fromCrossMin cfg graph pipeline | cfg.edgeLabelSizes /= pipeline.labels.sizes = full cfg graph
fromCrossMin cfg graph pipeline | not (A.null pipeline.components) = rerunComponents fromCrossMin cfg graph pipeline
fromCrossMin cfg graph pipeline = fromCoords cfg graph pipeline'
  where
  modelOrder = M.fromFoldable (A.mapWithIndex (\i n -> n.id /\ i) graph.nodes)
  reversed = S.fromFoldable
    ( A.mapMaybe
        (\edge -> if S.member (edge.from.node /\ edge.to.node) pipeline.acyclic.reversedEdges then Just edge.id else Nothing)
        graph.edges
    )
  reversedWithLabels = foldl
    (\ids label -> if S.member label.edge.id reversed then S.insert label.tail ids else ids)
    reversed
    pipeline.labels.dummies
  minimized = CrossingMin.minimize
    { iterations: cfg.iterations
    , constraints: graph.constraints
    , modelOrder
    , ports: M.fromFoldable ((graph.nodes <> (pipeline.portDummies.dummies <#> _.node)) <#> \n -> n.id /\ n.ports)
    , chains: pipeline.withDummies.chains
    , random: pipeline.initialRandom
    , reversed: reversedWithLabels
    , portDummies: pipeline.portDummies
    }
    pipeline.withDummies.layers
    pipeline.withDummies.edges
  pipeline' = pipeline { ordered = minimized.layout, random = minimized.random, portOrder = minimized.portOrder }

-- | Rerun from coordinate assignment (node size or constraint changed).
fromCoords :: Config -> Graph -> Pipeline -> LayoutOutput
fromCoords cfg graph pipeline | cfg.edgeLabelSizes /= pipeline.labels.sizes = full cfg graph
fromCoords cfg graph pipeline | not (A.null pipeline.components) = rerunComponents fromCoords cfg graph pipeline
fromCoords cfg graph pipeline = do
  let switched = EdgeLabels.switchDummies pipeline.labels (pipeline.withDummies { layers = pipeline.ordered })
  let dummies = PortDummies.rewire pipeline.portDummies switched.dummies
  let portOrder = if A.null pipeline.labels.dummies then pipeline.portOrder else remapPortOrder pipeline.withDummies dummies pipeline.portOrder
  let labels = EdgeLabels.selectSides dummies.layers dummies switched.labels
  let
    loopEdges = A.filter (\e -> e.from.node == e.to.node) graph.edges
    loops
      | A.null loopEdges = SelfLoops.empty
      | otherwise =
          let
            byId = M.fromFoldable (graph.nodes <#> \n -> n.id /\ n)
            loopOwners = A.mapMaybe (\id -> M.lookup id byId) (A.concat dummies.layers)
          in
            SelfLoops.prepare pipeline.random cfg.edgeLabelSizes
              (graph { nodes = loopOwners, edges = pipeline.acyclic.edges <> loopEdges })
  let prepared = pipeline { labels = labels, loops = loops, withDummies = dummies, ordered = dummies.layers, portOrder = portOrder }
  let portNodes = graph.nodes <> (pipeline.portDummies.dummies <#> _.node)
  let sizedNodes = portNodes <> labels.nodes
  let sizeMap = M.fromFoldable (sizedNodes <#> \n -> n.id /\ n.size)
  let portMap = M.fromFoldable (portNodes <#> \n -> n.id /\ n.ports)
  let
    portOffsets = SelfLoops.portOffsets SelfLoops.ReservedFrame loops prepared.withDummies.edges
      $ EdgeLabels.portOffsets labels prepared.withDummies.edges
      $
        PortDistribution.distributePorts prepared.portOrder prepared.ordered prepared.withDummies.edges (toFineSize sizeMap)
  let
    assigned = CoordAssignment.assign
      (SelfLoops.afterRouting prepared.random loops)
      { nodeGap: cfg.nodeGap, layerGap: cfg.layerGap }
      graph.constraints
      prepared.ordered
      (SelfLoops.reserveNodes loops sizeMap)
      (SelfLoops.margins loops)
      portMap
      prepared.withDummies.edges
      prepared.withDummies.chains
      portOffsets
  let pipeline' = prepared { placements = assigned.placements, routedRandom = assigned.random }
  let result = finalize cfg graph pipeline' (Just assigned.slots)
  { pipeline: pipeline', result }

-- | Rerun edge routing only (node position changed, same order).
fromRouting :: Config -> Graph -> Pipeline -> LayoutResult
fromRouting cfg graph pipeline | cfg.edgeLabelSizes /= pipeline.labels.sizes = (full cfg graph).result
fromRouting cfg graph pipeline | not (A.null pipeline.components) =
  (rerunComponents (\config component cache -> { pipeline: cache, result: fromRouting config component cache }) cfg graph pipeline).result
fromRouting cfg graph pipeline = finalize cfg graph pipeline Nothing

-- ══════════════════════════════════════════════════════════════════
--  Backward-compatible monolithic entry points
-- ══════════════════════════════════════════════════════════════════

layout :: Config -> Graph -> LayoutResult
layout cfg graph = (full cfg graph).result

-- ══════════════════════════════════════════════════════════════════
--  Internal
-- ══════════════════════════════════════════════════════════════════

rerunComponents
  :: (Config -> Graph -> Pipeline -> LayoutOutput)
  -> Config
  -> Graph
  -> Pipeline
  -> LayoutOutput
rerunComponents rerun cfg graph pipeline = combineComponents cfg graph
  (pipeline.components <#> \component -> rerun cfg (Components.restrict component.nodes graph) (componentPipeline component pipeline))

componentPipeline :: ComponentCache -> Pipeline -> Pipeline
componentPipeline component pipeline = pipeline
  { components = []
  , initialRandom = component.initialRandom
  , random = component.random
  , routedRandom = component.routedRandom
  , acyclic =
      { edges: A.filter memberEdge pipeline.acyclic.edges
      , reversedEdges: S.filter (\(a /\ b) -> member a && member b) pipeline.acyclic.reversedEdges
      }
  , layered =
      { layers: slice pipeline.layered.layers
      , nodeLayer: map (_ - component.layerOffset) (M.filterKeys member pipeline.layered.nodeLayer)
      }
  , withDummies =
      { layers: dummyLayers
      , edges
      , chains: A.filter (A.all member <<< _.nodes) pipeline.withDummies.chains
      }
  , ordered = slice pipeline.ordered
  , portOrder = M.filterKeys (\(edge /\ _) -> S.member edge edgeIds) pipeline.portOrder
  , portDummies = { dummies: A.filter (\dummy -> S.member dummy.owner component.nodes) pipeline.portDummies.dummies }
  , placements = A.filter (member <<< _.node) pipeline.placements <#> \n ->
      (Components.moveNode inverse n) { layer = n.layer - component.layerOffset }
  , labels = pipeline.labels
      { dummies = A.filter (member <<< _.node) pipeline.labels.dummies
      , nodes = A.filter (member <<< _.id) pipeline.labels.nodes
      , edges = A.filter memberEdge pipeline.labels.edges
      }
  , loops = SelfLoops.restrict component.nodes pipeline.loops
  }
  where
  slice = A.slice component.layerOffset (component.layerOffset + component.layerCount)
  dummyLayers = slice pipeline.withDummies.layers
  members = S.union component.nodes (S.fromFoldable (A.concat dummyLayers))
  member = flip S.member members
  memberEdge edge = member edge.from.node && member edge.to.node
  edges = A.filter memberEdge pipeline.withDummies.edges
  edgeIds = S.fromFoldable (edges <#> _.id)
  inverse = GridPos (negate (gridX component.offset) /\ negate (gridY component.offset))

combineComponents :: Config -> Graph -> Array LayoutOutput -> LayoutOutput
combineComponents cfg graph outputs = case A.uncons framed of
  Nothing -> fullConnected (mkRandom 1.0) cfg graph
  Just { head, tail } ->
    let
      combined = foldl merge head tail
      result = combined.result
    in
      combined { result = result { metrics = allMetrics result.nodes result.edges 0 } }
  where
  packed = Components.pack (outputs <#> _.result)
  framed = _.outputs $ foldl frame { layerOffset: 0, outputs: [] } (A.zip outputs packed)
  frame state (output /\ positioned) =
    let
      p = output.pipeline
      count = A.length p.layered.layers
      offset = state.layerOffset
      component =
        { nodes: S.fromFoldable (output.result.nodes <#> _.node)
        , layerOffset: offset
        , layerCount: count
        , offset: positioned.offset
        , initialRandom: p.initialRandom
        , random: p.random
        , routedRandom: p.routedRandom
        }
      pipeline = p
        { components = [ component ]
        , layered = p.layered { nodeLayer = map (_ + offset) p.layered.nodeLayer }
        , placements = p.placements <#> \n -> (Components.moveNode positioned.offset n) { layer = n.layer + offset }
        }
      result = positioned.result { nodes = positioned.result.nodes <#> \n -> n { layer = n.layer + offset } }
    in
      { layerOffset: offset + count, outputs: A.snoc state.outputs { pipeline, result } }
  merge left right =
    let
      a = left.pipeline
      b = right.pipeline
      pipeline = a
        { components = a.components <> b.components
        , random = b.random
        , routedRandom = b.routedRandom
        , acyclic = { edges: a.acyclic.edges <> b.acyclic.edges, reversedEdges: S.union a.acyclic.reversedEdges b.acyclic.reversedEdges }
        , layered = { layers: a.layered.layers <> b.layered.layers, nodeLayer: M.union a.layered.nodeLayer b.layered.nodeLayer }
        , withDummies = { layers: a.withDummies.layers <> b.withDummies.layers, edges: a.withDummies.edges <> b.withDummies.edges, chains: a.withDummies.chains <> b.withDummies.chains }
        , ordered = a.ordered <> b.ordered
        , portOrder = M.union a.portOrder b.portOrder
        , portDummies = { dummies: a.portDummies.dummies <> b.portDummies.dummies }
        , placements = a.placements <> b.placements
        , labels = a.labels { dummies = a.labels.dummies <> b.labels.dummies, nodes = a.labels.nodes <> b.labels.nodes, edges = a.labels.edges <> b.labels.edges }
        , loops = a.loops <> b.loops
        }
      result = left.result
        { nodes = left.result.nodes <> right.result.nodes
        , edges = left.result.edges <> right.result.edges
        , edgeLabels = left.result.edgeLabels <> right.result.edgeLabels
        , boundingBox = unionBounds left.result.boundingBox right.result.boundingBox
        }
    in
      { pipeline, result }

-- LabelDummySwitcher rebuilds segment identities. Only original chain endpoints
-- carry multiple ports; every intermediate dummy has a single port per side.
-- Transfer those endpoint ranks by chain identity instead of losing them when
-- the label moves to its median layer.
remapPortOrder :: DummyNodes.DummyResult -> DummyNodes.DummyResult -> PortDistribution.PortOrder -> PortDistribution.PortOrder
remapPortOrder before after order = M.fromFoldable (A.concatMap remap entries)
  where
  entries = M.toUnfoldable order :: Array ((EdgeId /\ Side) /\ Int)
  oldChains = M.fromFoldable (before.chains <#> \chain -> chain.edgeId /\ chain)
  replacements = M.fromFoldable $ A.concatMap
    ( \chain -> case M.lookup chain.edgeId oldChains of
        Nothing -> []
        Just old ->
          let
            oldIds = segmentIds old
            newIds = segmentIds chain
          in
            A.catMaybes
              [ (\oldId newId -> (oldId /\ South) /\ newId) <$> A.head oldIds <*> A.head newIds
              , (\oldId newId -> (oldId /\ North) /\ newId) <$> A.last oldIds <*> A.last newIds
              ]
    )
    after.chains
  retained = S.fromFoldable (after.edges <#> _.id)
  remap ((edge /\ side) /\ rank) =
    case M.lookup (edge /\ side) replacements of
      Just replacement -> [ (replacement /\ side) /\ rank ]
      Nothing | S.member edge retained -> [ (edge /\ side) /\ rank ]
      Nothing -> []
  segmentIds chain =
    if A.length chain.nodes <= 2 then [ chain.edgeId ]
    else A.zipWith (\a b -> EdgeId (un EdgeId chain.edgeId <> ":" <> un NodeId a <> "->" <> un NodeId b)) chain.nodes (A.drop 1 chain.nodes)

finalize :: Config -> Graph -> Pipeline -> Maybe (M.Map EdgeId SlotInfo) -> LayoutResult
finalize cfg graph pipeline slotPlan = do
  let portNodes = graph.nodes <> (pipeline.portDummies.dummies <#> _.node)
  let portMap = M.fromFoldable (portNodes <#> \n -> n.id /\ n.ports)
  let sizedNodes = portNodes <> pipeline.labels.nodes
  let sizeMap = M.fromFoldable (sizedNodes <#> \n -> n.id /\ n.size)
  -- NodeRelativePortDistributor places regular ports alongside restored loop
  -- sectors. Routing uses actual owner coordinates, not BK's reserved frame.
  let
    portOffsets = SelfLoops.portOffsets SelfLoops.OwnerFrame pipeline.loops pipeline.withDummies.edges
      $ EdgeLabels.portOffsets pipeline.labels pipeline.withDummies.edges
      $ PortDistribution.distributePorts pipeline.portOrder pipeline.ordered pipeline.withDummies.edges (toFineSize sizeMap)
  let portDummyIds = S.fromFoldable (pipeline.portDummies.dummies <#> _.node.id)
  let realPlacements = A.filter (\p -> not (DummyNodes.isDummy p.node || S.member p.node portDummyIds)) pipeline.placements
  -- Per-segment routing: route each broken-up dummy edge against the full
  -- placements (so dummies act as obstacles for unrelated edges), then
  -- stitch the routed segments back into a single path per chain. This
  -- is the path of `BaseRoutingDirectionStrategy.getPortPositionOnHyperNode`
  -- (port-position sharing through dummies, handled inside `assignPorts`)
  -- combined with ELK's chain assembly. Reversed back-edges are stitched
  -- in reversed order with each segment's endpoints flipped so the
  -- rendered direction matches the original edge.
  let restoredPlacements = SelfLoops.restoreNodes pipeline.loops pipeline.placements
  let obstacles = SelfLoops.routingObstacles pipeline.loops pipeline.placements restoredPlacements
  let
    segmentPaths = PortDummies.restore pipeline.portDummies restoredPlacements pipeline.withDummies.edges
      (routeAll (SelfLoops.afterRouting pipeline.random pipeline.loops) slotPlan pipeline.withDummies.edges restoredPlacements obstacles portMap pipeline.withDummies.chains portOffsets)
  let reversedSet = pipeline.acyclic.reversedEdges
  let hasCenterLabels = not (A.null pipeline.labels.dummies)
  let regularRoutingEdges = if hasCenterLabels then pipeline.labels.edges else A.filter (\edge -> edge.from.node /= edge.to.node) graph.edges
  let loopEdges = A.filter (\edge -> edge.from.node == edge.to.node) graph.edges
  let routingEdges = regularRoutingEdges <> loopEdges
  let originalKeyById = M.fromFoldable (regularRoutingEdges <#> \edge -> edge.id /\ (edge.from.node /\ edge.to.node))
  let
    stitched = stitchChains pipeline.withDummies.chains originalKeyById segmentPaths
      <> SelfLoops.route pipeline.loops restoredPlacements
  let
    compacted =
      if cfg.compactPostRouting then
        compactPostRouting EdgeLength { nodeNode: 4.0 * toNumber cfg.nodeGap, edgeEdge: 10.0 } cfg.compactionSpacings
          { nodes: realPlacements
          , edges: routingEdges
          , paths: stitched
          , ports: portMap
          }
      else
        { nodes: realPlacements
        , edges: stitched
        , boundingBox: Components.bounds { nodes: realPlacements, edges: stitched, edgeLabels: [] }
        }
  let
    retainedNodes =
      if hasCenterLabels then
        let
          realIds = S.fromFoldable (graph.nodes <#> _.id)
        in
          A.filter (\p -> S.member p.node realIds) compacted.nodes
      else compacted.nodes
  let nodes = SelfLoops.restoreNodes pipeline.loops retainedNodes
  let edgeLabels = EdgeLabels.placements pipeline.labels compacted.nodes <> SelfLoops.placements pipeline.loops nodes
  let restored = EdgeLabels.restore pipeline.labels compacted.edges
  let
    oriented =
      if hasCenterLabels then
        let
          originalEdges = M.fromFoldable (graph.edges <#> \e -> e.id /\ e)
        in
          restored <#> \p -> case M.lookup p.edge originalEdges of
            Just e | S.member (e.from.node /\ e.to.node) reversedSet ->
              p { segments = reverseSegments p.segments, reversed = true }
            _ -> p
      else restored
  let
    simplified = oriented <#> \p ->
      let
        segs = mergeCollinear (removeZeroLength p.segments)
      in
        p { segments = segs, bends = A.zipWith (\s _ -> s.end) segs (A.drop 1 segs) }
  let withJumps = detectJumps simplified
  let metrics = allMetrics nodes withJumps 0
  { nodes, edges: withJumps, edgeLabels, boundingBox: compacted.boundingBox, metrics }

stitchChains
  :: Array { edgeId :: EdgeId, nodes :: Array NodeId }
  -> M.Map EdgeId (NodeId /\ NodeId)
  -> Array EdgePath
  -> Array EdgePath
stitchChains chains originalKeyById segmentPaths = chains <#> stitchOne
  where
  pathBySegId = M.fromFoldable (segmentPaths <#> \p -> un EdgeId p.edge /\ p)

  -- chain.nodes is in routing direction (acyclic edge.from → edge.to). For
  -- cycle-removed edges the routing direction is the inverse of the original
  -- edge, so we reverse to canonicalise.
  chainIsReversed chain = case M.lookup chain.edgeId originalKeyById, A.head chain.nodes of
    Just (origFrom /\ _), Just first -> first /= origFrom
    _, _ -> false

  stitchOne chain
    | A.length chain.nodes <= 2 = case M.lookup (un EdgeId chain.edgeId) pathBySegId of
        Just p -> do
          let r = chainIsReversed chain
          let oriented = if r then reverseSegments p.segments else p.segments
          let segs = mergeCollinear (removeZeroLength oriented)
          p { edge = chain.edgeId, segments = segs, bends = A.zipWith (\s _ -> s.end) segs (A.drop 1 segs), reversed = r }
        Nothing -> emptyPath chain.edgeId
    | otherwise = do
        let segIds = A.zipWith (\a b -> un EdgeId chain.edgeId <> ":" <> un NodeId a <> "->" <> un NodeId b) chain.nodes (A.drop 1 chain.nodes)
        let segPaths = A.mapMaybe (\sid -> M.lookup sid pathBySegId) segIds
        let raw = A.concatMap _.segments segPaths
        let r = chainIsReversed chain
        let oriented = if r then reverseSegments raw else raw
        let merged = mergeCollinear (removeZeroLength oriented)
        { edge: chain.edgeId
        , segments: merged
        , bends: A.zipWith (\s _ -> s.end) merged (A.drop 1 merged)
        , bendType: []
        , jumps: []
        , reversed: r
        }

emptyPath :: EdgeId -> EdgePath
emptyPath eid =
  { edge: eid
  , segments: []
  , bends: []
  , bendType: []
  , jumps: []
  , reversed: false
  }

reverseSegments :: Array EdgeSegment -> Array EdgeSegment
reverseSegments = A.reverse <<< map flipEnds
  where
  flipEnds s = { start: s.end, end: s.start, direction: s.direction }

dummyPlaceholder :: DummyNodes.DummyResult
dummyPlaceholder = { layers: [], edges: [], chains: [] }

-- | Convert a grid-unit `sizeMap` to fine units (× sf=4) so it matches
-- | what the BK pass works with internally.
toFineSize :: M.Map NodeId GridSize -> M.Map NodeId GridSize
toFineSize = map (\(GridSize (w /\ h)) -> GridSize (w * 4.0 /\ h))

unionBounds :: GridRect -> GridRect -> GridRect
unionBounds a b =
  { pos: GridPos (left /\ top)
  , size: GridSize ((right - left) /\ (bottom - top))
  }
  where
  left = min (gridX a.pos) (gridX b.pos)
  top = min (gridY a.pos) (gridY b.pos)
  right = max (gridX a.pos + sizeW a.size) (gridX b.pos + sizeW b.size)
  bottom = max (gridY a.pos + sizeH a.size) (gridY b.pos + sizeH b.size)
