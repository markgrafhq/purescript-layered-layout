-- | Port of ELK's `VerticalSegment` (orthogonal-only — spline tracking
-- | omitted because markgraf only routes orthogonal edges).
-- |
-- | A vertical segment is a piece of routed edge that runs along the
-- | y-axis. Segments at the same x-coordinate are merged before they
-- | are handed to the compactor, so the surviving record carries the
-- | union of contributing edges, bend points and ignore-spacing flags.
module LayeredLayout.Compaction.VerticalSegment
  ( VerticalSegment
  , PortRef
  , VSId
  , newVerticalSegment
  , intersects
  , joinWith
  , compareVS
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number (abs)
import LayeredLayout.Compaction.OneD (CNodeId, Quadruplet, emptyQuadruplet, fuzzyEq, fuzzyLt, quadOr)
import LayeredLayout.Graph (EdgeId, NodeId, Side)
import LayeredLayout.Grid (GridPos, gridX, gridY)

type VSId = Int

-- | Minimal port reference used by the compactor: just the side (for
-- | the inverted-port edge-constraint logic in `NetworkSimplexCompaction`)
-- | and the owning node id (for adjusting positions back on the LGraph).
type PortRef = { node :: NodeId, side :: Side }

type VerticalSegment =
  { id :: VSId
  , representedEdges :: Array EdgeId
  , affectedBends :: Array GridPos
  , hitbox :: { x :: Number, y :: Number, width :: Number, height :: Number }
  , ignoreSpacing :: Quadruplet
  , potentialGroupParents :: Array CNodeId
  , aPort :: Maybe PortRef
  }

-- | Construct a vertical segment from two bend points. Mirrors ELK's
-- | `VerticalSegment(KVector, KVector, CNode, LEdge)` constructor.
newVerticalSegment
  :: VSId
  -> GridPos
  -> GridPos
  -> Maybe CNodeId
  -> EdgeId
  -> VerticalSegment
newVerticalSegment vid bend1 bend2 mGroupParent edgeId =
  { id: vid
  , representedEdges: [ edgeId ]
  , affectedBends: [ bend1, bend2 ]
  , hitbox:
      { x: min (gridX bend1) (gridX bend2)
      , y: min (gridY bend1) (gridY bend2)
      , width: abs (gridX bend1 - gridX bend2)
      , height: abs (gridY bend1 - gridY bend2)
      }
  , ignoreSpacing: emptyQuadruplet
  , potentialGroupParents: case mGroupParent of
      Nothing -> []
      Just p -> [ p ]
  , aPort: Nothing
  }

-- | Two segments at the same x-coordinate (fuzzy) and overlapping in
-- | y are considered intersecting and will be merged.
intersects :: VerticalSegment -> VerticalSegment -> Boolean
intersects a b =
  fuzzyEq a.hitbox.x b.hitbox.x
    && not (fuzzyLt (a.hitbox.y + a.hitbox.height) b.hitbox.y)
    && not (fuzzyLt (b.hitbox.y + b.hitbox.height) a.hitbox.y)

-- | Merge `other` into `survivor`. The result keeps `survivor`'s id
-- | and unions every contributing list. Matches ELK's `joinWith`.
joinWith :: VerticalSegment -> VerticalSegment -> VerticalSegment
joinWith survivor other = do
  let newX = min survivor.hitbox.x other.hitbox.x
  let newY = min survivor.hitbox.y other.hitbox.y
  let maxX = max (survivor.hitbox.x + survivor.hitbox.width) (other.hitbox.x + other.hitbox.width)
  let maxY = max (survivor.hitbox.y + survivor.hitbox.height) (other.hitbox.y + other.hitbox.height)
  survivor
    { representedEdges = survivor.representedEdges <> other.representedEdges
    , affectedBends = survivor.affectedBends <> other.affectedBends
    , potentialGroupParents = survivor.potentialGroupParents <> other.potentialGroupParents
    , hitbox = { x: newX, y: newY, width: maxX - newX, height: maxY - newY }
    , ignoreSpacing = quadOr survivor.ignoreSpacing other.ignoreSpacing
    , aPort = case survivor.aPort of
        Just _ -> survivor.aPort
        Nothing -> other.aPort
    }

-- | Sort order: fuzzy-equal x coordinates collapse, ties broken by
-- | exact y. Port of ELK's `compareTo`.
compareVS :: VerticalSegment -> VerticalSegment -> Ordering
compareVS a b
  | fuzzyEq a.hitbox.x b.hitbox.x = compare a.hitbox.y b.hitbox.y
  | a.hitbox.x < b.hitbox.x = LT
  | otherwise = GT

