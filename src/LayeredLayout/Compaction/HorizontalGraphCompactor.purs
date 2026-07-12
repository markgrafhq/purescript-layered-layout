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
  , BetweenLayersSpacings
  , defaultBetweenLayersSpacings
  , compactPostRouting
  ) where

import Prelude

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
  ( CNode
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
import LayeredLayout.Graph (Edge, NodeId, Port)
import LayeredLayout.Grid (GridPos(..), GridSize(..), gridX, gridY, sizeH, sizeW)
import LayeredLayout.Result (Direction(..), EdgePath, EdgeSegment, NodePlacement) as R

-- | Which compaction algorithm to drive. Only `EdgeLength` is
-- | wired so far — it uses `NetworkSimplexCompaction` with the
-- | transformer's hooks.
data CompactionStrategy = EdgeLength

derive instance Eq CompactionStrategy

-- | The BETWEEN_LAYERS spacing matrix the compactor's spacings handler
-- | hands back per node-type pair. Mirrors ELK's
-- | `nodeTypeSpacingOptionsHorizontal` (BETWEEN_LAYERS column): the
-- | three pairs markgraf models are node↔node, edge↔node and
-- | edge↔edge. Values are in router-grid units (the unit every CNode
-- | is promoted to inside the compactor), which equal ELK's option
-- | values directly:
-- |   * `nodeNode` = `layered.spacing.nodeNodeBetweenLayers`
-- |   * `edgeNode` = `layered.spacing.edgeNodeBetweenLayers`
-- |   * `edgeEdge` = `spacing.edgeEdge` (ELK's `SPACING_EDGE_EDGE`)
-- | The same `edgeEdge` drives the edge-aware scanline's hitbox
-- | inflation, so both read this one source.
type BetweenLayersSpacings =
  { nodeNode :: Number
  , edgeNode :: Number
  , edgeEdge :: Number
  }

-- | The values the current test graphs (hugeGraph + ElkDiff panels)
-- | are validated against: hugeGraph overrides node-node and edge-node
-- | downward from ELK's 20/10 defaults, and edge-edge stays at ELK's
-- | `SPACING_EDGE_EDGE` default of 10.
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
-- | (Y). Returns updated node placements and edge paths.
compactPostRouting
  :: CompactionStrategy
  -> BetweenLayersSpacings
  -> { nodes :: Array R.NodePlacement
     , edges :: Array Edge
     , paths :: Array R.EdgePath
     , ports :: Map NodeId (Array Port)
     }
  -> { nodes :: Array R.NodePlacement, edges :: Array R.EdgePath }
compactPostRouting strategy spacings input = do
  let swapped = swapInput input
  let out = transform swapped
  let routed = buildRoutedEdges swapped
  let hooks = buildHooks out routed
  let
    state0 = newOneD out.cGraph
      # setSpacingsHandler (specialSpacings spacings hooks)
      # setConstraintAlgorithm (edgeAwareScanlineConstraints spacings.edgeEdge)
      # setCompactionAlgorithm (algorithmFor strategy hooks)
  let compacted = (compact state0 # finish).cGraph
  let applied = applyLayout compacted { nodes: swapped.nodes, edges: swapped.edges, paths: swapped.paths }
  swapOutput applied

algorithmFor
  :: forall a
   . CompactionStrategy
  -> CompactionHooks a
  -> ICompactionAlgorithm a
algorithmFor EdgeLength hooks = networkSimplexCompaction hooks

----------------------------------------------------------------
-- ISpacingsHandler
--
-- Matches ELK's `specialSpacingsHandler` minus the per-LNode-type
-- lookup (markgraf does not type its nodes). Same-edge VS pairs
-- collapse to 0; otherwise hand back 1 grid unit so the compactor
-- still keeps daylight between hitboxes.
----------------------------------------------------------------

-- | All CNodes are now in router-grid; one node-grid unit of margin
-- | translates to `scaleFactor` router-grid units. Mirrors ELK's
-- | `specialSpacingsHandler` (HorizontalGraphCompactor.java:193-251):
-- | same-edge VS pairs collapse to 0 horizontally (1 vertically to
-- | preserve column overlap), and any CNode whose facing
-- | `ignoreSpacing` side is set returns 0 — used by N/S-port VSes
-- | anchored to a node edge.
specialSpacings :: forall a. BetweenLayersSpacings -> CompactionHooks a -> ISpacingsHandler a
specialSpacings spacings hooks =
  { horizontalSpacing
  , verticalSpacing
  }
  where
  pairSpacing a b = spacingFor spacings (classify a b)

  horizontalSpacing a b
    | hooks.sameEdgeVerticalSegments a b = 0.0
    | a.ignoreSpacing.right || b.ignoreSpacing.left = 0.0
    | otherwise = pairSpacing a b

  verticalSpacing a b
    | hooks.sameEdgeVerticalSegments a b = 1.0
    | facingVerticalIgnored a b = 0.0
    | otherwise = pairSpacing a b

  facingVerticalIgnored a b
    | a.hitbox.y <= b.hitbox.y = a.ignoreSpacing.down || b.ignoreSpacing.up
    | otherwise = a.ignoreSpacing.up || b.ignoreSpacing.down

-- | Pair classification used by the spacings matrix below. Mirrors
-- | ELK's `nodeTypeSpacingOptionsHorizontal` matrix (BETWEEN_LAYERS
-- | column) — markgraf only models the VS / non-VS distinction.
data PairKind = NodeNode | EdgeNode | EdgeEdge

classify :: forall a. CNode a -> CNode a -> PairKind
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
  , ports: i.ports
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

swapOutput
  :: { nodes :: Array R.NodePlacement, edges :: Array R.EdgePath }
  -> { nodes :: Array R.NodePlacement, edges :: Array R.EdgePath }
swapOutput o =
  { nodes: o.nodes <#> swapNode
  , edges: o.edges <#> swapPath
  }

