-- | Pre-BK port distribution.
-- |
-- | Computes per-edge port offsets on each node's side, so that BK's
-- | `insideBlockShift.portPosDiff` sees non-uniform values for same-source
-- | siblings. Without this, sibling edges all share `width/2` (center) and
-- | the inner-block shift collapses to zero, which makes BK produce a
-- | different layout than ELK.
-- |
-- | Port of ELK's `LGraphUtil.placePorts` semantics, simplified to use
-- | layer-order-as-proxy: siblings on a side are sorted by the layer-order
-- | index of the *other* endpoint. CrossingMin has already determined that
-- | order, and it's a faithful proxy for the post-BK left-to-right placement
-- | (BK preserves layer order — that's `BKNodePlacer.checkOrderConstraint`).
-- |
-- | Caller flow:
-- |   1. After CrossingMin, before BK: call `distributePorts` with the
-- |      ordered layers and dummy-augmented edges.
-- |   2. Pass the resulting `EdgePortOffsets` into `CoordAssignment.assign`.
-- |   3. Pass the same map into `EdgeRouting.PortAssignment.assignPorts`
-- |      so BK and routing agree on port positions.
module LayeredLayout.PortDistribution
  ( module ReExport
  , distributePorts
  , offsetFor
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import LayeredLayout.EdgeRouting.PortAssignment (EdgePortOffsets, distributeAlongSide) as ReExport
import LayeredLayout.EdgeRouting.PortAssignment (EdgePortOffsets, distributeAlongSide)
import LayeredLayout.Graph (Edge, EdgeId, NodeId, Side(..))
import LayeredLayout.Grid (GridSize, sizeW)
import LayeredLayout.DummyNodes (isDummy)

distributePorts
  :: Array (Array NodeId)
  -> Array Edge
  -> Map NodeId GridSize
  -> EdgePortOffsets
distributePorts layers edges sizeMap =
  M.union (perSideMap North) (perSideMap South)
  where
  layerIndex :: Map NodeId Int
  layerIndex = M.fromFoldable $ A.concat $ A.mapWithIndex
    (\i layer -> layer <#> \n -> n /\ i)
    layers

  orderIndex :: Map NodeId Int
  orderIndex = M.fromFoldable $ A.concat $ layers <#> \layer ->
    A.mapWithIndex (\i n -> n /\ i) layer

  -- | South side carries OUTGOING (forward) edges; the sibling order is
  -- | by the target's order index in its layer.
  -- | North side carries INCOMING edges; sibling order is by source's
  -- | order index in its layer.
  perSideMap :: Side -> EdgePortOffsets
  perSideMap side = foldl (\acc (nid /\ es) -> M.union (offsetsForNode side nid es) acc)
    M.empty
    (M.toUnfoldable (groupOnSide side) :: Array (NodeId /\ Array Edge))

  groupOnSide :: Side -> Map NodeId (Array Edge)
  groupOnSide side = foldl
    ( \m e ->
        if e.from.node == e.to.node then m -- self-loops route via east/west, not north/south
        else case ownerOnSide side e of
          Just nid -> M.insertWith (<>) nid [ e ] m
          Nothing -> m
    )
    M.empty
    edges

  ownerOnSide :: Side -> Edge -> Maybe NodeId
  ownerOnSide South e = Just e.from.node -- forward exit
  ownerOnSide North e = Just e.to.node -- forward entry
  ownerOnSide _ _ = Nothing

  -- | Sibling order: by the OTHER endpoint's order index. Targets to the
  -- | left get left ports; sources from the left feed left input ports.
  otherOrder :: Side -> Edge -> Int
  otherOrder South e = fromMaybe 0 (M.lookup e.to.node orderIndex)
  otherOrder North e = fromMaybe 0 (M.lookup e.from.node orderIndex)
  otherOrder _ _ = 0

  offsetsForNode :: Side -> NodeId -> Array Edge -> EdgePortOffsets
  offsetsForNode side nid es =
    M.fromFoldable
      ( (M.toUnfoldable distributed :: Array (EdgeId /\ Number)) <#> \(eid /\ x) ->
          (eid /\ side) /\ x
      )
    where
    -- ELK long-edge dummies are zero-width so the trunk shares the
    -- source-port column. Real nodes default to width 1 (grid) when
    -- absent, matching the ELK fallback for nodes that didn't get a
    -- size assigned.
    width = case M.lookup nid sizeMap of
      Just sz -> sizeW sz
      Nothing -> if isDummy nid then 0.0 else 1.0
    span = { lo: 0.0, hi: width }
    sorted = A.sortBy (\a b -> compare (otherOrder side a) (otherOrder side b)) es
    distributed = distributeAlongSide span (sorted <#> _.id)

-- | Look up the port offset for a given edge on a given side. Falls back to
-- | the node's centre (`width / 2`) when the edge isn't in the map — this
-- | matches the behavior of `insideBlockShift.portXOffset` before this port,
-- | so call sites can switch atomically.
offsetFor :: EdgePortOffsets -> EdgeId -> Side -> Number -> Number
offsetFor offsets eid side fallback =
  fromMaybe fallback (M.lookup (eid /\ side) offsets)
