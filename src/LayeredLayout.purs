module LayeredLayout
  ( layout
  , Config
  , defaultConfig
  , Pipeline
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
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Set as S
import Data.Tuple.Nested (type (/\), (/\))
import LayeredLayout.Aesthetics (allMetrics)
import LayeredLayout.Compaction.HorizontalGraphCompactor (BetweenLayersSpacings, CompactionStrategy(..), compactPostRouting, defaultBetweenLayersSpacings)
import LayeredLayout.EdgeRouting (routeAll)
import LayeredLayout.EdgeRouting.LineJump (detectJumps)
import LayeredLayout.EdgeRouting.Orthogonal (mergeCollinear, removeZeroLength)
import LayeredLayout.PortDistribution as PortDistribution
import LayeredLayout.Graph (EdgeId(..), Graph, NodeId(..))
import LayeredLayout.Grid (GridPos(..), GridSize(..), gridX, gridY, sizeH, sizeW)
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
  -- When true, run the post-routing HorizontalGraphCompactor along
  -- markgraf's Y-axis (ELK port; phases 1–5 of the compaction pipeline)
  -- to squeeze out long-edge-dummy-induced vertical bloat. Default
  -- false until the existing panel suite has been re-validated.
  , compactPostRouting :: Boolean
  -- BETWEEN_LAYERS spacing matrix the post-routing compactor hands
  -- back per node-type pair (and the edge-edge value the edge-aware
  -- scanline inflates by). In router-grid units; see
  -- `BetweenLayersSpacings`. Defaults match ELK's options for the
  -- current panel suite (8/4/10).
  , compactionSpacings :: BetweenLayersSpacings
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
  }

-- | Cached intermediate results from each pipeline phase.
-- Holding onto this allows selective recomputation when only
-- later phases need to rerun.
type Pipeline =
  { acyclic :: CycleRemoval.AcyclicResult
  , layered :: LayerAssignment.LayeredGraph
  , withDummies :: DummyNodes.DummyResult
  , ordered :: Array (Array NodeId)
  , placements :: Array NodePlacement
  }

type LayoutOutput = { pipeline :: Pipeline, result :: LayoutResult }

-- ══════════════════════════════════════════════════════════════════
--  Phased entry points
-- ══════════════════════════════════════════════════════════════════

-- | Full pipeline from scratch.
full :: Config -> Graph -> LayoutOutput
full cfg graph = fromDummies cfg graph pipeline
  where
  acyclic = CycleRemoval.makeAcyclicWithOrder cfg.cycleBreaker allNodeIds graph.constraints graph.edges
  layered = LayerAssignment.assignLayersWith cfg.layerer graph.constraints acyclic.edges allNodeIds
  pipeline = { acyclic, layered, withDummies: dummyPlaceholder, ordered: [], placements: [] }
  allNodeIds = graph.nodes <#> _.id

-- | Rerun from dummy node insertion (node moved to different layer).
fromDummies :: Config -> Graph -> Pipeline -> LayoutOutput
fromDummies cfg graph pipeline = fromCrossMin cfg graph pipeline'
  where
  withDummies = DummyNodes.insertDummies pipeline.layered.nodeLayer pipeline.acyclic.edges pipeline.layered.layers
  pipeline' = pipeline { withDummies = withDummies }

-- | Rerun from crossing minimization (node order changed).
fromCrossMin :: Config -> Graph -> Pipeline -> LayoutOutput
fromCrossMin cfg graph pipeline = fromCoords cfg graph pipeline'
  where
  modelOrder = M.fromFoldable (A.mapWithIndex (\i n -> n.id /\ i) graph.nodes)
  ordered = CrossingMin.minimize
    { iterations: cfg.iterations
    , constraints: graph.constraints
    , modelOrder
    }
    pipeline.withDummies.layers
    pipeline.withDummies.edges
  pipeline' = pipeline { ordered = ordered }

-- | Rerun from coordinate assignment (node size or constraint changed).
fromCoords :: Config -> Graph -> Pipeline -> LayoutOutput
fromCoords cfg graph pipeline = do
  let sizeMap = M.fromFoldable (graph.nodes <#> \n -> n.id /\ n.size)
  let portMap = M.fromFoldable (graph.nodes <#> \n -> n.id /\ n.ports)
  let
    portOffsets = PortDistribution.distributePorts
      pipeline.ordered
      pipeline.withDummies.edges
      (toFineSize sizeMap)
  let
    placements = CoordAssignment.assign
      { nodeGap: cfg.nodeGap, layerGap: cfg.layerGap }
      graph.constraints
      pipeline.ordered
      sizeMap
      portMap
      pipeline.withDummies.edges
      pipeline.withDummies.chains
      portOffsets
  let pipeline' = pipeline { placements = placements }
  let result = finalize cfg graph pipeline'
  { pipeline: pipeline', result }

-- | Rerun edge routing only (node position changed, same order).
fromRouting :: Config -> Graph -> Pipeline -> LayoutResult
fromRouting cfg graph pipeline = finalize cfg graph pipeline

-- ══════════════════════════════════════════════════════════════════
--  Backward-compatible monolithic entry points
-- ══════════════════════════════════════════════════════════════════

layout :: Config -> Graph -> LayoutResult
layout cfg graph = (full cfg graph).result

-- ══════════════════════════════════════════════════════════════════
--  Internal
-- ══════════════════════════════════════════════════════════════════

finalize :: Config -> Graph -> Pipeline -> LayoutResult
finalize cfg graph pipeline = do
  let portMap = M.fromFoldable (graph.nodes <#> \n -> n.id /\ n.ports)
  let sizeMap = M.fromFoldable (graph.nodes <#> \n -> n.id /\ n.size)
  -- Post-BK port distribution. ELK's `NodeRelativePortDistributor`
  -- assigns per-edge offsets on each node side, sorted by the layer
  -- order of the connected node. Edge routing uses these offsets so
  -- north/south ports on a node with multiple siblings are spread
  -- evenly. Self-loops route on east/west via `routeSelfLoops` so they
  -- must not count as north/south siblings here.
  let regularDummyEdges = A.filter (\e -> e.from.node /= e.to.node) pipeline.withDummies.edges
  let
    portOffsets = PortDistribution.distributePorts
      pipeline.ordered
      regularDummyEdges
      (toFineSize sizeMap)
  let realPlacements = A.filter (\p -> not (DummyNodes.isDummy p.node)) pipeline.placements
  -- Per-segment routing: route each broken-up dummy edge against the full
  -- placements (so dummies act as obstacles for unrelated edges), then
  -- stitch the routed segments back into a single path per chain. This
  -- is the path of `BaseRoutingDirectionStrategy.getPortPositionOnHyperNode`
  -- (port-position sharing through dummies, handled inside `assignPorts`)
  -- combined with ELK's chain assembly. Reversed back-edges are stitched
  -- in reversed order with each segment's endpoints flipped so the
  -- rendered direction matches the original edge.
  let segmentPaths = routeAll pipeline.withDummies.edges pipeline.placements portMap pipeline.withDummies.chains portOffsets
  let reversedSet = pipeline.acyclic.reversedEdges
  let originalKeyById = M.fromFoldable (graph.edges <#> \e -> e.id /\ (e.from.node /\ e.to.node))
  let stitched = stitchChains pipeline.withDummies.chains reversedSet originalKeyById segmentPaths
  let
    compacted =
      if cfg.compactPostRouting then
        compactPostRouting EdgeLength cfg.compactionSpacings
          { nodes: realPlacements
          , edges: graph.edges
          , paths: stitched
          , ports: portMap
          }
      else { nodes: realPlacements, edges: stitched }
  let
    simplified = compacted.edges <#> \p ->
      let
        segs = mergeCollinear (removeZeroLength p.segments)
      in
        p { segments = segs, bends = A.zipWith (\s _ -> s.end) segs (A.drop 1 segs) }
  let withJumps = detectJumps simplified
  let metrics = allMetrics compacted.nodes withJumps 0
  let bbox = boundingBox compacted.nodes
  { nodes: compacted.nodes, edges: withJumps, boundingBox: bbox, metrics }

stitchChains
  :: Array { edgeId :: EdgeId, nodes :: Array NodeId }
  -> S.Set (NodeId /\ NodeId)
  -> M.Map EdgeId (NodeId /\ NodeId)
  -> Array EdgePath
  -> Array EdgePath
stitchChains chains reversedSet originalKeyById segmentPaths = chains <#> stitchOne
  where
  pathBySegId = M.fromFoldable (segmentPaths <#> \p -> un EdgeId p.edge /\ p)

  isReversed eid = case M.lookup eid originalKeyById of
    Just key -> S.member key reversedSet
    Nothing -> false

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

boundingBox :: forall r. Array { position :: GridPos, size :: GridSize | r } -> { pos :: GridPos, size :: GridSize }
boundingBox placements = do
  let maxX = foldl (\mx p -> max mx (gridX p.position + sizeW p.size)) 0.0 placements
  let maxY = foldl (\my p -> max my (gridY p.position + sizeH p.size)) 0.0 placements
  { pos: GridPos (0.0 /\ 0.0), size: GridSize (maxX /\ maxY) }
