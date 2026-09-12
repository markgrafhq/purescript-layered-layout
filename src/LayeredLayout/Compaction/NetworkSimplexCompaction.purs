-- Copyright (c) 2017 Kiel University and others.
-- SPDX-License-Identifier: EPL-2.0
-- Translated from ELK c831ba4613dfd6b0055851193956560351d2f907:
-- NetworkSimplexCompaction.addSeparationConstraints, addEdgeConstraints,
-- addArtificialSourceNode, and compact.
--
-- | Phase 3 of the post-routing graph compaction port.
-- |
-- | An `ICompactionAlgorithm` implementation that drives the generic
-- | `LayeredLayout.NetworkSimplex` core. Port of ELK's
-- | `org.eclipse.elk.alg.layered.intermediate.compaction.NetworkSimplexCompaction`.
-- |
-- | The ELK version is hard-coded against `LNode` / `VerticalSegment`
-- | to add edge-length constraints and pick weights for vertical-segment
-- | / node pairs. Here that knowledge is supplied by the caller via a
-- | `CompactionHooks a` record so the algorithm stays polymorphic in
-- | the origin type. The phase 4 transformer fills in non-default
-- | hooks; callers that only need separation constraints can pass
-- | `defaultHooks`.
module LayeredLayout.Compaction.NetworkSimplexCompaction
  ( CompactionHooks
  , ExtraEdge
  , defaultHooks
  , networkSimplexCompaction
  , separationWeight
  , edgeWeight
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as M
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import LayeredLayout.Compaction.OneD
  ( CGraph
  , CGroupId
  , CNode
  , CNodeId
  , ICompactionAlgorithm(..)
  , OneDState
  , allCNodes
  , isHorizontalDir
  , lookupCNode
  , updateCNode
  )
import LayeredLayout.NetworkSimplex (NEdge, runNetworkSimplex)

-- | Constants matching ELK's `NetworkSimplexCompaction`.
separationWeight :: Number
separationWeight = 1.0

edgeWeight :: Number
edgeWeight = 100.0

-- | An additional edge the phase 4 transformer wants in the
-- | network-simplex graph (e.g. for inverted ports).
type ExtraEdge =
  { srcGroup :: CGroupId
  , tgtGroup :: CGroupId
  , delta :: Int
  , weight :: Number
  }

-- | Hooks the layered-graph bridge supplies so this algorithm can
-- | reproduce ELK's `addEdgeConstraints` and the VS-vs-LNode weight
-- | bump without knowing anything about `LNode` / `VerticalSegment`.
type CompactionHooks a =
  { sameEdgeVerticalSegments :: CNode a -> CNode a -> Boolean
  , portAnchoredSegment :: CNode a -> Boolean
  , vsLNodePair :: CNode a -> CNode a -> Boolean
  , edgeLengthEdges :: CGraph a -> Array ExtraEdge
  }

defaultHooks :: forall a. CompactionHooks a
defaultHooks =
  { sameEdgeVerticalSegments: \_ _ -> false
  , portAnchoredSegment: \_ -> false
  , vsLNodePair: \_ _ -> false
  , edgeLengthEdges: \_ -> []
  }

-- | The compaction algorithm. Build a network-simplex graph from the
-- | constraint graph, run the simplex, and write the resulting layer
-- | values back into each `CNode`'s `hitbox.x`.
networkSimplexCompaction :: forall a. CompactionHooks a -> ICompactionAlgorithm a
networkSimplexCompaction hooks = ICompactionAlgorithm \st -> do
  let built = build hooks st
  let layers = runNetworkSimplex built.nodes built.edges
  applyLayers layers st

----------------------------------------------------------------
-- NGraph construction
----------------------------------------------------------------

-- | Identifiers used in the network-simplex graph. Group ids map
-- | one-to-one onto themselves; helpers and the artificial source
-- | are allocated from `nextCGroupId` upwards.
type Built =
  { nodes :: Array Int
  , edges :: Array (NEdge Int)
  }

type BuildState =
  { nodes :: Array Int
  , edges :: Array (NEdge Int)
  , nextNodeId :: Int
  , nextEid :: Int
  }

build :: forall a. CompactionHooks a -> OneDState a -> Built
build hooks st = do
  let groupIds = st.cGraph.cGroupOrder
  let
    s0 =
      { nodes: groupIds
      , edges: [] :: Array (NEdge Int)
      , nextNodeId: st.cGraph.nextCGroupId
      , nextEid: 0
      }
  let s1 = foldl (addSeparationsForNode hooks st) s0 (allCNodes st.cGraph)
  let s2 = foldl addExtraEdge s1 (hooks.edgeLengthEdges st.cGraph)
  let s3 = addArtificialSource s2
  { nodes: s3.nodes, edges: s3.edges }

addSeparationsForNode
  :: forall a
   . CompactionHooks a
  -> OneDState a
  -> BuildState
  -> CNode a
  -> BuildState
addSeparationsForNode hooks st s cNode =
  foldl (addSeparation hooks st cNode) s cNode.constraints

addSeparation
  :: forall a
   . CompactionHooks a
  -> OneDState a
  -> CNode a
  -> BuildState
  -> CNodeId
  -> BuildState
addSeparation hooks st cNode s incId = case lookupCNode incId st.cGraph of
  Nothing -> s
  Just incNode ->
    if cNode.cGroup == incNode.cGroup then s
    else case cNode.cGroup, incNode.cGroup of
      Just cg, Just incCg -> placeEdge hooks st cNode incNode cg incCg s
      _, _ -> s

placeEdge
  :: forall a
   . CompactionHooks a
  -> OneDState a
  -> CNode a
  -> CNode a
  -> CGroupId
  -> CGroupId
  -> BuildState
  -> BuildState
placeEdge hooks st cNode incNode cg incCg s = do
  let spacing = chooseSpacing st cNode incNode
  let
    rawDelta = cNode.cGroupOffset.x
      + cNode.hitbox.width
      + spacing
      - incNode.cGroupOffset.x
  let delta = max 0 (Int.ceil (rawDelta))
  -- Upstream defect correction: helper pairs can reverse regular bend
  -- segments, invalidating the scanline's transitive obstacle ordering.
  -- The actual ELK scanline and simplex reproduce CENTER crossings on
  -- generated fixtures 10, 25, and 39 with that topology. Preserve weak
  -- order for regular segments (same-edge spacing is zero); keep source
  -- reordering freedom where a north/south port anchor requires it.
  if
    hooks.sameEdgeVerticalSegments cNode incNode
      && (hooks.portAnchoredSegment cNode || hooks.portAnchoredSegment incNode) then
    addHelperPair cNode incNode cg incCg s
  else do
    let
      weight =
        if hooks.vsLNodePair cNode incNode then 2.0
        else separationWeight
    pushEdge { src: cg, tgt: incCg, delta, weight } s

-- | Source helper topology permits same-edge segments to exchange order.
-- | Adapter difference: ELK additionally adjusts a fractional LPort.x and
-- | its group's offset by the ceil rounding remainder. This representation
-- | has no mutable absolute port position; offsets are used unchanged here.
addHelperPair
  :: forall a
   . CNode a
  -> CNode a
  -> CGroupId
  -> CGroupId
  -> BuildState
  -> BuildState
addHelperPair cNode incNode cg incCg s = do
  let helperId = s.nextNodeId
  let offsetDelta = Int.ceil ((incNode.cGroupOffset.x - cNode.cGroupOffset.x))
  let s1 = s { nodes = s.nodes <> [ helperId ], nextNodeId = helperId + 1 }
  let s2 = pushEdge { src: helperId, tgt: cg, delta: max 0 offsetDelta, weight: separationWeight } s1
  pushEdge { src: helperId, tgt: incCg, delta: max 0 (-offsetDelta), weight: separationWeight } s2

chooseSpacing :: forall a. OneDState a -> CNode a -> CNode a -> Number
chooseSpacing st cNode incNode =
  if isHorizontalDir st.direction then st.spacingsHandler.horizontalSpacing cNode incNode
  else st.spacingsHandler.verticalSpacing cNode incNode

addExtraEdge :: BuildState -> ExtraEdge -> BuildState
addExtraEdge s e = pushEdge { src: e.srcGroup, tgt: e.tgtGroup, delta: e.delta, weight: e.weight } s

----------------------------------------------------------------
-- Artificial source for disconnected graphs
----------------------------------------------------------------

addArtificialSource :: BuildState -> BuildState
addArtificialSource s = do
  let incoming = foldl (\m e -> M.insertWith (+) e.tgt 1 m) (M.empty :: Map Int Int) s.edges
  let sources = A.filter (\n -> fromMaybe 0 (M.lookup n incoming) == 0) s.nodes
  if A.length sources <= 1 then s
  else do
    let dummy = s.nextNodeId
    let s1 = s { nodes = s.nodes <> [ dummy ], nextNodeId = dummy + 1 }
    foldl (\acc src -> pushEdge { src: dummy, tgt: src, delta: 1, weight: 0.0 } acc) s1 sources

----------------------------------------------------------------
-- Apply simplex result
----------------------------------------------------------------

applyLayers :: forall a. Map Int Int -> OneDState a -> OneDState a
applyLayers layers st = do
  let cg' = foldl (writeNode layers) st.cGraph (allCNodes st.cGraph)
  st { cGraph = cg' }

writeNode :: forall a. Map Int Int -> CGraph a -> CNode a -> CGraph a
writeNode layers g cNode = case cNode.cGroup of
  Nothing -> g
  Just gid -> do
    let layer = intToNum (fromMaybe 0 (M.lookup gid layers))
    updateCNode cNode.id
      (\n -> n { hitbox = n.hitbox { x = layer + n.cGroupOffset.x } })
      g

----------------------------------------------------------------
-- Builders
----------------------------------------------------------------

pushEdge
  :: { src :: Int, tgt :: Int, delta :: Int, weight :: Number }
  -> BuildState
  -> BuildState
pushEdge e s = do
  let
    nEdge =
      { src: e.src
      , tgt: e.tgt
      , delta: e.delta
      , weight: e.weight
      , eid: s.nextEid
      }
  s { edges = s.edges <> [ nEdge ], nextEid = s.nextEid + 1 }

----------------------------------------------------------------
-- Number / Int conversions
----------------------------------------------------------------

intToNum :: Int -> Number
intToNum = Int.toNumber
