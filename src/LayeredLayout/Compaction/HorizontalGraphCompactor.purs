-- Copyright (c) 2017 Kiel University and others.
-- SPDX-License-Identifier: EPL-2.0
-- Translated from ELK c831ba4613dfd6b0055851193956560351d2f907:
-- HorizontalGraphCompactor.process and specialSpacingsHandler.
--
-- | Phase 5 of the post-routing graph compaction port: the
-- | orchestrator that wires the phase 4 transformer, the phase 2
-- | `OneDimensionalCompactor` lifecycle and the phase 3
-- | `NetworkSimplexCompaction` algorithm together. Port of ELK's
-- | `org.eclipse.elk.alg.layered.intermediate.compaction.HorizontalGraphCompactor`.
-- |
-- | ELK's compactor compresses the X-axis (perpendicular to layers
-- | which grow horizontally in ELK). Markgraf runs DOWN internally,
-- | so the analogous "compress the layer-growth axis" operates on Y.
-- | We achieve that by transposing the routed layout (x↔y, w↔h,
-- | V↔H segments) before feeding it to the transformer, running the
-- | compactor with its default LEFT direction, and transposing the
-- | result back when applying the new positions.
module LayeredLayout.Compaction.HorizontalGraphCompactor
  ( CompactionStrategy(..)
  , WithinLayerSpacings
  , defaultWithinLayerSpacings
  , BetweenLayersSpacings
  , defaultBetweenLayersSpacings
  , compactPostRouting
  ) where

import Prelude

import Data.Array (null, uncons)
import Data.Foldable (all, foldl)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import LayeredLayout.Compaction.LGraphToCGraphTransformer
  ( TransformInput
  , applyLayout
  , buildHooks
  , buildRoutedEdges
  , transform
  )
import LayeredLayout.Compaction.NetworkSimplexCompaction (CompactionHooks, networkSimplexCompaction)
import LayeredLayout.Compaction.OneD
  ( CGraph
  , CNode
  , allCNodes
  , ICompactionAlgorithm
  , ISpacingsHandler
  , compact
  , finish
  , newOneD
  , setCompactionAlgorithm
  , setConstraintAlgorithm
  , setSpacingsHandler
  )
import LayeredLayout.Compaction.EdgeAwareScanlineConstraints (edgeAwareScanlineConstraints)
import LayeredLayout.EdgeRouting (scaleFactor)
import LayeredLayout.Graph (Edge, NodeId, Port, Side(..))
import LayeredLayout.Grid (GridPos(..), GridRect, GridSize(..), gridX, gridY, sizeH, sizeW)
import LayeredLayout.Result (Direction(..), EdgePath, EdgeSegment, NodePlacement) as R

-- | Which compaction algorithm to drive. Only `EdgeLength` is
-- | wired so far — it uses `NetworkSimplexCompaction` with the
-- | transformer's hooks.
data CompactionStrategy = EdgeLength

derive instance Eq CompactionStrategy

-- | Global perpendicular-axis spacings in fine-grid units. These drive
-- | the scanline hitboxes independently of the between-layer matrix.
type WithinLayerSpacings =
  { nodeNode :: Number
  , edgeEdge :: Number
  }

defaultWithinLayerSpacings :: WithinLayerSpacings
defaultWithinLayerSpacings = { nodeNode: 20.0, edgeEdge: 10.0 }

-- | The BETWEEN_LAYERS spacing matrix the compactor's spacings handler
-- | hands back per node-type pair. Mirrors ELK's
-- | `nodeTypeSpacingOptionsHorizontal` (BETWEEN_LAYERS column): the
-- | three pairs markgraf models are node↔node, edge↔node and
-- | edge↔edge. Values are in router-grid units (the unit every CNode
-- | is promoted to inside the compactor), which equal ELK's option
-- | values directly:
-- |   * `nodeNode` = `layered.spacing.nodeNodeBetweenLayers`
-- |   * `edgeNode` = `layered.spacing.edgeNodeBetweenLayers`
-- |   * `edgeEdge` = `layered.spacing.edgeEdgeBetweenLayers`
-- | LABEL/LABEL is the source matrix exception: it uses global edge-edge
-- | spacing even along the between-layer axis.
type BetweenLayersSpacings =
  { nodeNode :: Number
  , edgeNode :: Number
  , edgeEdge :: Number
  }

-- | The values the current test graphs (hugeGraph + ElkDiff panels)
-- | are validated against: hugeGraph overrides node-node and edge-node
-- | downward from ELK's 20/10 defaults, and edge-edge stays at ELK's
-- | `SPACING_EDGE_EDGE_BETWEEN_LAYERS` default of 10.
defaultBetweenLayersSpacings :: BetweenLayersSpacings
defaultBetweenLayersSpacings =
  { nodeNode: 8.0
  , edgeNode: 4.0
  , edgeEdge: 10.0
  }

----------------------------------------------------------------
-- Public API
----------------------------------------------------------------

-- | Run post-routing compaction along markgraf's layer-growth axis
-- | (Y). Returns placements, paths, and the complete coarse DOWN layout frame.
compactPostRouting
  :: CompactionStrategy
  -> WithinLayerSpacings
  -> BetweenLayersSpacings
  -> { nodes :: Array R.NodePlacement
     , edges :: Array Edge
     , paths :: Array R.EdgePath
     , ports :: Map NodeId (Array Port)
     }
  -> { nodes :: Array R.NodePlacement, edges :: Array R.EdgePath, boundingBox :: GridRect }
compactPostRouting strategy within spacings input = do
  let swapped = swapInput input
  let out = transform swapped
  let routed = buildRoutedEdges swapped
  let hooks = buildHooks out routed
  let
    state0 = newOneD out.cGraph
      # setSpacingsHandler (specialSpacings within spacings hooks)
      # setConstraintAlgorithm (edgeAwareScanlineConstraints within)
      # setCompactionAlgorithm (algorithmFor strategy hooks)
  let compacted = (compact state0 # finish).cGraph
  let applied = applyLayout compacted { nodes: swapped.nodes, edges: swapped.edges, paths: swapped.paths }
  { nodes: applied.nodes <#> swapNode
  , edges: applied.edges <#> swapPath
  , boundingBox: compactionBounds compacted
  }

-- | LGraphToCGraphTransformer.applyLayout measures every final hitbox.
-- | Virtual segments and reserved margins remain part of the component
-- | frame even when their extents are absent from the rendered paths.
compactionBounds :: forall a. CGraph a -> GridRect
compactionBounds graph = case uncons (allCNodes graph) of
  Nothing -> { pos: GridPos (0.0 /\ 0.0), size: GridSize (0.0 /\ 0.0) }
  Just { head, tail } ->
    let
      first = head.hitbox
      extent = foldl
        ( \a node ->
            let
              b = node.hitbox
            in
              { left: min a.left b.x, top: min a.top b.y, right: max a.right (b.x + b.width), bottom: max a.bottom (b.y + b.height) }
        )
        { left: first.x, top: first.y, right: first.x + first.width, bottom: first.y + first.height }
        tail
    in
      { pos: GridPos (extent.top / sf /\ extent.left / sf)
      , size: GridSize ((extent.bottom - extent.top) / sf /\ (extent.right - extent.left) / sf)
      }
  where
  sf = toNumber scaleFactor

algorithmFor
  :: forall a
   . CompactionStrategy
  -> CompactionHooks a
  -> ICompactionAlgorithm a
algorithmFor EdgeLength hooks = networkSimplexCompaction hooks

----------------------------------------------------------------
-- ISpacingsHandler
--
-- Source specialSpacingsHandler: same-edge VS pairs have horizontal
-- spacing zero; other pairs use the node-type matrix. ignoreSpacing
-- belongs to the scanline hitbox calculations, not this handler.
----------------------------------------------------------------

-- | Source node-type spacing lookup for NORMAL, LABEL, and LONG_EDGE.
-- | The vertical handler is used only by quadratic constraints upstream;
-- | this adapter selects the orthogonal edge-aware scanline.
specialSpacings
  :: forall a
   . WithinLayerSpacings
  -> BetweenLayersSpacings
  -> CompactionHooks a
  -> ISpacingsHandler a
specialSpacings within spacings hooks =
  { horizontalSpacing
  , verticalSpacing
  }
  where
  pairSpacing a b = spacingFor spacings (classify a b)

  horizontalSpacing a b
    | hooks.sameEdgeVerticalSegments a b = 0.0
    | a.kind == Just "label" && b.kind == Just "label" = within.edgeEdge
    | otherwise = pairSpacing a b

  verticalSpacing a b
    | hooks.sameEdgeVerticalSegments a b = 1.0
    | otherwise = case classify a b of
        NodeNode -> within.nodeNode
        EdgeNode -> 10.0 -- global SPACING_EDGE_NODE default; no adapter option
        EdgeEdge -> within.edgeEdge

-- | Pair classification used by the spacings matrix below. Mirrors
-- | ELK's `nodeTypeSpacingOptionsHorizontal` matrix (BETWEEN_LAYERS
-- | column) — markgraf only models the VS / non-VS distinction.
data PairKind = NodeNode | EdgeNode | EdgeEdge

classify :: forall a. CNode a -> CNode a -> PairKind
classify a b | a.kind == Just "label" && b.kind == Just "label" = EdgeEdge
classify a b = case isVS a, isVS b of
  true, true -> EdgeEdge
  true, false -> EdgeNode
  false, true -> EdgeNode
  false, false -> NodeNode
  where
  isVS n = n.kind == Just "vs"

-- | Look the pair's spacing up in the configured BETWEEN_LAYERS
-- | matrix. The compactor sees a swapped frame (markgraf DOWN →
-- | compactor LEFT), so "horizontal" here = ELK's between-layers axis.
spacingFor :: BetweenLayersSpacings -> PairKind -> Number
spacingFor spacings = case _ of
  NodeNode -> spacings.nodeNode
  EdgeNode -> spacings.edgeNode
  EdgeEdge -> spacings.edgeEdge

----------------------------------------------------------------
-- Axis swap (x ↔ y, w ↔ h, V ↔ H)
----------------------------------------------------------------

swapInput
  :: { nodes :: Array R.NodePlacement
     , edges :: Array Edge
     , paths :: Array R.EdgePath
     , ports :: Map NodeId (Array Port)
     }
  -> TransformInput
swapInput i =
  { nodes: i.nodes <#> swapNode
  , edges: i.edges
  , paths: i.paths <#> swapPath
  , ports: if all null i.ports then i.ports else i.ports <#> map swapPort
  }

swapPort :: Port -> Port
swapPort port = port
  { side = case port.side of
      North -> West
      South -> East
      East -> South
      West -> North
  }

swapNode :: R.NodePlacement -> R.NodePlacement
swapNode np = np
  { position = swapPos np.position
  , size = swapSize np.size
  }

swapPath :: R.EdgePath -> R.EdgePath
swapPath p = p
  { segments = p.segments <#> swapSegment
  , bends = p.bends <#> swapPos
  }

swapSegment :: R.EdgeSegment -> R.EdgeSegment
swapSegment s =
  { start: swapPos s.start
  , end: swapPos s.end
  , direction: swapDir s.direction
  }

swapDir :: R.Direction -> R.Direction
swapDir R.H = R.V
swapDir R.V = R.H

swapPos :: GridPos -> GridPos
swapPos g = GridPos (gridY g /\ gridX g)

swapSize :: GridSize -> GridSize
swapSize g = GridSize (sizeH g /\ sizeW g)

