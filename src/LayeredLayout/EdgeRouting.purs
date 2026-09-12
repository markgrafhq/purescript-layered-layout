module LayeredLayout.EdgeRouting (routeAll, routeIncremental, scaleFactor) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as S
import Data.Number (abs)
import Data.Tuple.Nested ((/\))
import LayeredLayout.EdgeRouting.HyperEdges (SlotInfo, assignSlots)
import LayeredLayout.EdgeRouting.Orthogonal (FineRect, ObstacleMap, buildObstacleMap, findRouteSlot, isRouteClear, removeZeroLength, segmentsToObstacles, simplifySegments)
import LayeredLayout.EdgeRouting.PortAssignment (PortAssignment, assignPorts, scaleFactor) as PA
import LayeredLayout.EdgeRouting.PortAssignment (EdgePortOffsets)
import LayeredLayout.Graph (Edge, EdgeId, NodeId, Port, Side(..))
import LayeredLayout.Grid (GridPos(..), gridX, gridY, sizeH, sizeW)
import LayeredLayout.Result (Direction(..), EdgePath, EdgeSegment, NodePlacement)
import LayeredLayout.JavaRandom (Random)

scaleFactor :: Int
scaleFactor = PA.scaleFactor

routeAll :: Random -> Maybe (Map EdgeId SlotInfo) -> Array Edge -> Array NodePlacement -> Array NodePlacement -> Map NodeId (Array Port) -> Array { edgeId :: EdgeId, nodes :: Array NodeId } -> EdgePortOffsets -> Array EdgePath
routeAll random planned edges placements obstaclePlacements portMap chains portOffsets = selfLoopPaths <>
  if trustedPlan then
    let
      noObstacles = []
    in
      map (routeAssigned noObstacles noObstacles) ordered
  else
    let
      -- Unplanned and manually moved layouts still need obstacle-aware routing.
      nodeObstacles = buildObstacleMap obstaclePlacements
      posMap = foldl (\m p -> M.insert p.node p m) M.empty obstaclePlacements
      routeNext acc a =
        let
          filteredNodeObs = filteredFor a nodeObstacles posMap
          allObstacles = filteredNodeObs <> acc.edgeObstacles
          path = routeAssigned filteredNodeObs allObstacles a
        in
          { results: A.snoc acc.results path
          , edgeObstacles: acc.edgeObstacles <> segmentsToObstacles path.segments
          }
    in
      _.results (foldl routeNext { results: [], edgeObstacles: [] } ordered)
  where
  selfLoops = A.filter isSelfLoop edges
  regularEdges = A.filter (not <<< isSelfLoop) edges
  selfLoopPaths = if A.null selfLoops then [] else routeSelfLoops selfLoops placements

  assignments = PA.assignPorts regularEdges placements portMap chains portOffsets
  ordered = orderForRouting assignments placements
  slotMap = case planned of
    Just slots -> slots
    Nothing -> (assignSlots random assignments placements).slots
  gapStarts = layerEnds obstaclePlacements

  trustedPlan = case planned of
    Just _ -> A.all (\a -> a.fromSide == South && a.toSide == North && M.member a.edge.id slotMap) assignments
    Nothing -> false
  routeAssigned nodeObstacles obstacles a = case splitInfoFor gapStarts slotMap a of
    Just split -> routeSplit split a
    Nothing -> routeOne (channelYFor gapStarts slotMap a) nodeObstacles obstacles a

isSelfLoop :: Edge -> Boolean
isSelfLoop e = e.from.node == e.to.node

-- | Port of ELK's `selfLoopDistribution: EQUALLY`. Each self-loop is
-- | drawn as a horizontal-then-vertical-then-horizontal "C" attached
-- | to the west side of its node. When a node has multiple
-- | self-loops, the y-positions of the entry / exit stubs are
-- | distributed equally over the node's height (plus the bump
-- | depth) so loops don't overlap.
routeSelfLoops :: Array Edge -> Array NodePlacement -> Array EdgePath
routeSelfLoops selfLoops placements = A.concat (A.mapWithIndex perNode grouped)
  where
  posMap = foldl (\m p -> M.insert p.node p m) M.empty placements

  grouped :: Array { node :: NodeId, edges :: Array Edge }
  grouped = M.toUnfoldable byNode <#> \(k /\ es) -> { node: k, edges: es }

  byNode = foldl
    (\m e -> M.insertWith (<>) e.from.node [ e ] m)
    M.empty
    selfLoops

  perNode :: Int -> { node :: NodeId, edges :: Array Edge } -> Array EdgePath
  perNode _ entry = case M.lookup entry.node posMap of
    Nothing -> []
    Just placement -> A.mapWithIndex (\i e -> selfLoopPath placement i (A.length entry.edges) e) entry.edges

  selfLoopPath :: NodePlacement -> Int -> Int -> Edge -> EdgePath
  selfLoopPath placement idx total edge = do
    let sf = Int.toNumber PA.scaleFactor
    let x = gridX placement.position * sf
    let y = gridY placement.position * sf
    let h = sizeH placement.size * sf
    -- ELK's selfLoopDistribution=EQUALLY draws self-loops on the
    -- west (left) side, growing concentrically from the middle:
    -- the first edge takes the innermost stub pair and subsequent
    -- edges wrap around it. With denom = 2*total+1 this gives
    -- idx 0 → h*1/3, h*2/3 for one loop, or h*2/5, h*3/5 for two.
    -- Outer loops bump further out so multiple loops draw as nested
    -- C-shapes rather than overlapping at the same column.
    let bumpOut = sf * 2.5 * Int.toNumber (idx + 1)
    let denom = Int.toNumber (2 * total + 1)
    let exitY = y + h * Int.toNumber (total - idx) / denom
    let entryY = y + h * Int.toNumber (total + 1 + idx) / denom
    let exitPort = x /\ exitY
    let entryPort = x /\ entryY
    let outerX = x - bumpOut
    -- Path: exit west → bump out left → down → bump in right → enter west.
    let
      segs =
        [ { start: gridPos exitPort, end: gridPos (outerX /\ exitY), direction: H }
        , { start: gridPos (outerX /\ exitY), end: gridPos (outerX /\ entryY), direction: V }
        , { start: gridPos (outerX /\ entryY), end: gridPos entryPort, direction: H }
        ]
    { edge: edge.id
    , segments: segs
    , bends: A.zipWith (\a _ -> a.end) segs (A.drop 1 segs)
    , bendType: []
    , jumps: []
    , reversed: false
    }

  gridPos (cx /\ cy) = GridPos (cx /\ cy)

routeIncremental :: Random -> Set NodeId -> Array EdgePath -> Array Edge -> Array NodePlacement -> Map NodeId (Array Port) -> Array { edgeId :: EdgeId, nodes :: Array NodeId } -> EdgePortOffsets -> Array EdgePath
routeIncremental random changedNodes prevEdges edges placements portMap chains portOffsets = selfLoopPaths <> _.results (foldl routeNext { results: [], edgeObstacles: [] } ordered)
  where
  selfLoops = A.filter isSelfLoop edges
  regularEdges = A.filter (not <<< isSelfLoop) edges
  selfLoopPaths = routeSelfLoops selfLoops placements

  nodeObstacles = buildObstacleMap placements
  posMap = foldl (\m p -> M.insert p.node p m) M.empty placements
  assignments = PA.assignPorts regularEdges placements portMap chains portOffsets
  ordered = orderForRouting assignments placements
  slotMap = (assignSlots random assignments placements).slots
  gapStarts = layerEnds placements
  touchesChanged a =
    S.member a.edge.from.node changedNodes
      || S.member a.edge.to.node changedNodes
  prevMap = foldl (\m ep -> M.insert ep.edge ep m) M.empty prevEdges

  routeNext acc a = do
    let filteredNodeObs = filteredFor a nodeObstacles posMap
    let allObstacles = filteredNodeObs <> acc.edgeObstacles
    let
      path =
        if touchesChanged a then routeOne (channelYFor gapStarts slotMap a) filteredNodeObs allObstacles a
        else case M.lookup a.edge.id prevMap of
          Just prev -> prev
          Nothing -> routeOne (channelYFor gapStarts slotMap a) filteredNodeObs allObstacles a
    let newEdgeObs = segmentsToObstacles path.segments
    { results: acc.results <> [ path ], edgeObstacles: acc.edgeObstacles <> newEdgeObs }

-- Routing channels start after the whole reserved layer, not merely after
-- the connected ports of the shorter nodes in that layer.
layerEnds :: Array NodePlacement -> Map Int Number
layerEnds = foldl
  (\ends node -> M.insertWith max node.layer ((gridY node.position + sizeH node.size) * Int.toNumber scaleFactor) ends)
  M.empty

-- | Compute the slot-derived channel y for an edge, if any.
-- | y = gapTop + edgeNodeBetweenLayers + slot * edgeEdgeBetweenLayers
-- | with ELK's defaults edgeNodeBetweenLayers = 4 fine = 1 grid and
-- | edgeEdgeBetweenLayers = 10 fine = 2.5 grid (markgraf's fine = 4 ×
-- | grid). The gap is sized to fit the slots in
-- | `LayeredLayout.CoordAssignment.computeLayerGaps`, so channels
-- | never run past gapBottom.
channelYFor :: Map Int Number -> Map EdgeId SlotInfo -> PA.PortAssignment -> Maybe Number
channelYFor gapStarts slotMap a = do
  info <- M.lookup a.edge.id slotMap
  Just (slotY gapStarts info info.slot)

-- | Convert a slot index to absolute fine y inside its gap.
slotY :: Map Int Number -> SlotInfo -> Int -> Number
slotY gapStarts info slotIdx = fromMaybe 0.0 (M.lookup info.gap gapStarts) + scaledNodePad + Int.toNumber slotIdx * scaledSlotGap
  where
  scaledNodePad = 1.0 * Int.toNumber PA.scaleFactor
  scaledSlotGap = 2.5 * Int.toNumber PA.scaleFactor

-- | When a long-edge segment got split during slot-counting, the
-- | slotMap entry carries a `partner` describing the second-half slot
-- | and the split-x. Returns the data needed to draw a 4-bend trunk
-- | (slot1Y, splitX, slot2Y) for this edge.
splitInfoFor
  :: Map Int Number
  -> Map EdgeId SlotInfo
  -> PA.PortAssignment
  -> Maybe { slot1Y :: Number, splitX :: Number, slot2Y :: Number }
splitInfoFor gapStarts slotMap a = do
  info <- M.lookup a.edge.id slotMap
  partner <- info.partner
  Just
    { slot1Y: slotY gapStarts info info.slot
    , splitX: partner.splitX
    , slot2Y: slotY gapStarts info partner.slot
    }

filteredFor :: PA.PortAssignment -> ObstacleMap -> Map NodeId NodePlacement -> ObstacleMap
filteredFor a obstacles posMap = do
  let srcRect = nodeToRect <$> M.lookup a.edge.from.node posMap
  let dstRect = nodeToRect <$> M.lookup a.edge.to.node posMap
  A.filter (\r -> Just r /= srcRect && Just r /= dstRect) obstacles

orderForRouting :: Array PA.PortAssignment -> Array NodePlacement -> Array PA.PortAssignment
orderForRouting assignments placements = A.sortBy comparator assignments
  where
  posMap = foldl (\m p -> M.insert p.node p m) M.empty placements

  layerOf :: NodeId -> Int
  layerOf nid = case M.lookup nid posMap of
    Nothing -> 0
    Just p -> p.layer

  xOf :: NodeId -> Number
  xOf nid = case M.lookup nid posMap of
    Nothing -> 0.0
    Just p -> gridX p.position

  comparator a b = do
    let la = layerOf a.edge.from.node
    let lb = layerOf b.edge.from.node
    case compare la lb of
      EQ -> do
        let xa = xOf a.edge.from.node
        let xb = xOf b.edge.from.node
        case compare xa xb of
          EQ -> compare (xOf a.edge.to.node) (xOf b.edge.to.node)
          other -> other
      other -> other

routeOne :: Maybe Number -> ObstacleMap -> ObstacleMap -> PA.PortAssignment -> EdgePath
routeOne slotY' nodeObstacles obstacles assignment = do
  let
    segments = case slotY', assignment.fromSide, assignment.toSide of
      -- A planned layered channel already resolves trunk conflicts. Searching
      -- around previously routed edges can collapse distinct slots and change
      -- the separation constraints used by post-routing node compaction.
      Just channel, South, North ->
        let
          planned =
            if abs (sx - tx) <= 0.001 then
              [ { start: GridPos assignment.fromPos, end: GridPos assignment.toPos, direction: V } ]
            else removeZeroLength
              [ { start: GridPos assignment.fromPos, end: GridPos (sx /\ channel), direction: V }
              , { start: GridPos (sx /\ channel), end: GridPos (tx /\ channel), direction: H }
              , { start: GridPos (tx /\ channel), end: GridPos assignment.toPos, direction: V }
              ]
        in
          if isRouteClear nodeObstacles planned then planned
          else simplifySegments obstacles
            (findRouteSlot slotY' nodeObstacles obstacles assignment.fromSide assignment.fromPos assignment.toSide assignment.toPos)
      _, _, _ -> simplifySegments obstacles
        (findRouteSlot slotY' nodeObstacles obstacles assignment.fromSide assignment.fromPos assignment.toSide assignment.toPos)
  let bends = findBends segments
  { edge: assignment.edge.id
  , segments
  , bends
  , bendType: []
  , jumps: []
  , reversed: false
  }
  where
  sx /\ _ = assignment.fromPos
  tx /\ _ = assignment.toPos

-- | Route a long-edge segment whose hyperedge was split during
-- | critical-cycle breaking. The path takes two horizontal trunks
-- | joined by a vertical jog at `splitX`:
-- |   (sx,sy) → (sx, slot1Y) → (splitX, slot1Y)
-- |          → (splitX, slot2Y) → (tx, slot2Y) → (tx, ty)
-- | This is the port of `BaseRoutingDirectionStrategy.calculateBendPoints`'s
-- | split-aware branch.
routeSplit
  :: { slot1Y :: Number, splitX :: Number, slot2Y :: Number }
  -> PA.PortAssignment
  -> EdgePath
routeSplit split assignment = do
  let (sx /\ sy) = assignment.fromPos
  let (tx /\ ty) = assignment.toPos
  let
    segs =
      [ { start: GridPos (sx /\ sy), end: GridPos (sx /\ split.slot1Y), direction: V }
      , { start: GridPos (sx /\ split.slot1Y), end: GridPos (split.splitX /\ split.slot1Y), direction: H }
      , { start: GridPos (split.splitX /\ split.slot1Y), end: GridPos (split.splitX /\ split.slot2Y), direction: V }
      , { start: GridPos (split.splitX /\ split.slot2Y), end: GridPos (tx /\ split.slot2Y), direction: H }
      , { start: GridPos (tx /\ split.slot2Y), end: GridPos (tx /\ ty), direction: V }
      ]
  { edge: assignment.edge.id
  , segments: segs
  , bends: findBends segs
  , bendType: []
  , jumps: []
  , reversed: false
  }

findBends :: Array EdgeSegment -> Array GridPos
findBends segments = A.zipWith (\a _b -> a.end) segments (A.drop 1 segments)

nodeToRect :: NodePlacement -> FineRect
nodeToRect p = do
  let sf = Int.toNumber PA.scaleFactor
  let x = gridX p.position * sf
  let y = gridY p.position * sf
  let w = sizeW p.size * sf
  let h = sizeH p.size * sf
  { x: x - 2.0, y: y - 2.0, w: w + 4.0, h: h + 4.0 }
