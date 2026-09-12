-- | Brandes-Köpf node placement algorithm.
-- Translated from ELK's BKNodePlacer / BKAligner / BKCompactor.
-- Assigns x-coordinates to nodes in a top-to-bottom layered layout
-- by running 4 directional passes and taking the balanced median.
module LayeredLayout.CoordAssignment
  ( assign
  , assignFine
  , assignFineDiag
  , CoordConfig
  , NodeMargins
  , Diag
  , PassDiag
  , Postprocessable
  , PostProcessTrace
  , PostProcessPhase(..)
  , VDir(..)
  , HDir(..)
  , Chosen(..)
  , MarkedEdge(..)
  ) where

import Prelude

import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int as Int
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Set as S
import Data.Tuple.Nested (type (/\), (/\))
import LayeredLayout.EdgeRouting.HyperEdges (SlotInfo, assignSlots, slotCountByGap)
import LayeredLayout.EdgeRouting.PortAssignment (assignPorts)
import LayeredLayout.Graph (Alignment(..), Axis(..), Constraints(..), Edge, EdgeId, NodeId(..), Port, PortId, Side(..))
import LayeredLayout.PortDistribution (EdgePortOffsets, offsetFor)
import LayeredLayout.PortDummies (isPortDummy)
import LayeredLayout.Grid (GridPos(..), GridSize(..), gridX, gridY, sizeH, sizeW)
import LayeredLayout.DummyNodes (isDummy, isLabelDummy)
import LayeredLayout.Result (NodePlacement)
import LayeredLayout.JavaRandom (Random)

type CoordConfig =
  { nodeGap :: Int
  , layerGap :: Int
  }

-- | Reserved space around physical nodes, in fine units. `sizeMap`
-- | includes these margins; port offsets use the same reserved frame.
type NodeMargins = Map NodeId { left :: Number, right :: Number, top :: Number, bottom :: Number }

type EdgeIndex =
  { connecting :: Map (NodeId /\ NodeId) Edge
  , incoming :: Map NodeId (Array Edge)
  , outgoing :: Map NodeId (Array Edge)
  }

-- | Direction within a layer (Down = left-to-right, Up = right-to-left)
data VDir = VDown | VUp

derive instance Eq VDir
derive instance Ord VDir

instance Show VDir where
  show VDown = "DOWN"
  show VUp = "UP"

-- | Direction across layers (HRight = top-to-bottom, HLeft = bottom-to-top)
data HDir = HRight | HLeft

derive instance Eq HDir
derive instance Ord HDir

instance Show HDir where
  show HRight = "RIGHT"
  show HLeft = "LEFT"

-- | Which of the four candidate layouts was picked. `Balanced` is the
-- | median-of-four; `SmallestFeasible` is the smallest-width directional
-- | layout when the median violates order constraints; `FirstFallback` is
-- | the last-resort first directional layout when none were feasible.
data Chosen = Balanced | SmallestFeasible | FirstFallback

derive instance Eq Chosen
derive instance Ord Chosen

instance Show Chosen where
  show Balanced = "balanced"
  show SmallestFeasible = "smallest-feasible"
  show FirstFallback = "first"

-- | Synthetic key identifying a marked (type-1 conflict) edge.
-- | Built from the two endpoint NodeIds by `edgeKey`; opaque otherwise.
newtype MarkedEdge = MarkedEdge String

derive instance Newtype MarkedEdge _
derive newtype instance Eq MarkedEdge
derive newtype instance Ord MarkedEdge
derive newtype instance Show MarkedEdge

type Neighborhood =
  { preds :: Map NodeId (Array NodeId)
  , succs :: Map NodeId (Array NodeId)
  , nodeIndex :: Map NodeId Int
  }

type AlignResult =
  { root :: Map NodeId NodeId
  , align :: Map NodeId NodeId
  }

type PlaceState =
  { x :: Map NodeId (Maybe Number)
  , sink :: Map NodeId NodeId
  , classEdges :: Array { src :: NodeId, tgt :: NodeId, sep :: Number }
  -- | Port of `bal.su`: blocks already used to straighten an edge.
  -- | A block flagged here will not be moved again to straighten
  -- | another edge (mirroring ELK's SimpleThresholdStrategy guard).
  , su :: Map NodeId Boolean
  -- | Port of `blockFinished`: roots whose block has finished
  -- | placement. The threshold strategy only considers edges to
  -- | already-finished blocks (otherwise the threshold can't be
  -- | computed yet).
  , blockFinished :: Map NodeId Boolean
  -- | Port of `postProcessablesQueue`: edges enqueued during
  -- | `getBound` because there were valid candidates but none
  -- | of them targeted a finished block yet. Drained by
  -- | `postProcess` after `placeBlock` completes.
  , queue :: Array Postprocessable
  }

-- | Port of `ThresholdStrategy.Postprocessable`: a deferred edge-straighten
-- | candidate. `free` is the block-side node (root or last); `isRoot`
-- | mirrors ELK's flag so `pickEdge` knows which incident-edge set to
-- | scan during the post-process pass.
type Postprocessable =
  { free :: NodeId
  , isRoot :: Boolean
  }

-- | Scale factor: BK works in fine-grid (grid × sf) for precision,
-- then rounds to grid coordinates at the end.
sf :: Int
sf = 4

-- | Assign coordinates in grid units without rounding.
-- |
-- | The `chains` argument carries the dummy-edge groupings produced by
-- | `DummyNodes.insertDummies`; we need them to estimate inter-layer
-- | routing widths via the same hyperedge segment pipeline that the
-- | router itself runs (ports of `OrthogonalRoutingGenerator` and
-- | `OrthogonalEdgeRouter.process`).
assign
  :: Random
  -> CoordConfig
  -> Array Constraints
  -> Array (Array NodeId)
  -> Map NodeId GridSize
  -> NodeMargins
  -> Map NodeId (Array Port)
  -> Array Edge
  -> Array { edgeId :: EdgeId, nodes :: Array NodeId }
  -> EdgePortOffsets
  -> { placements :: Array NodePlacement, slots :: Map EdgeId SlotInfo, random :: Random }
assign random cfg constraints layers sizeMap nodeMargins portMap edges chains portOffsets =
  { placements: applyConstraints constraints result, slots: routing.slots, random: routing.random }
  where
  fine = assignFine cfg layers sizeMap nodeMargins portMap edges portOffsets
  layerHeights = layers <#> \layer ->
    foldl (\h nid -> max h (sizeH (fromMaybe (GridSize (1.0 /\ 1.0)) (M.lookup nid sizeMap)))) 1.0 layer
  depthOffsets = placeWithinLayers layers layerHeights sizeMap nodeMargins edges
  -- ELK's `OrthogonalEdgeRouter.process` walks layers and asks
  -- `OrthogonalRoutingGenerator.routeEdges` for the slot count per gap,
  -- then sets routingWidth = max(nodeNodeSpacing,
  --   (slotCount-1) * edgeEdgeSpacing + 2 * edgeNodeSpacing).
  -- We mirror the slot-count call here using a provisional set of
  -- placements (BK x + uniform layerGap y) so the gap widths feed back
  -- into the final y positions.
  routing = computeLayerGaps random cfg layers portMap edges chains portOffsets buildProvisional
  layerYs = cumulativeYWithGaps routing.gaps layerHeights
  -- Dummy nodes have no real size; use 0×1 like the BK pass does so
  -- the routing layer (which reads `n.size` to compute the dummy
  -- centre x for trunk routing) sees the same width.
  result = A.concat $ layers # A.mapWithIndex \layerIdx layer ->
    layer # A.mapWithIndex \orderIdx nodeId -> do
      let dummyDefault = GridSize (0.0 /\ 1.0)
      let realDefault = GridSize (1.0 /\ 1.0)
      let fallback = if isDummy nodeId then dummyDefault else realDefault
      let size = fromMaybe fallback (M.lookup nodeId sizeMap)
      let fineX = fromMaybe 0.0 (M.lookup nodeId fine)
      let x = fineX / Int.toNumber sf
      let y = fromMaybe 0.0 (A.index layerYs layerIdx) + fromMaybe 0.0 (M.lookup nodeId depthOffsets)
      { node: nodeId, position: GridPos (x /\ y), size, layer: layerIdx, order: orderIdx }

  -- Provisional placements built with a candidate per-gap width array.
  -- Routing conflicts depend on cross-axis coordinates, not absolute
  -- layer depths, so uniform layer gaps suffice for the shared slot plan.
  buildProvisional :: Array Number -> Array NodePlacement
  buildProvisional perGapWidths = applyConstraints constraints $ A.concat $ layers # A.mapWithIndex \layerIdx layer ->
    layer # A.mapWithIndex \orderIdx nodeId -> do
      let dummyDefault = GridSize (0.0 /\ 1.0)
      let realDefault = GridSize (1.0 /\ 1.0)
      let fallback = if isDummy nodeId then dummyDefault else realDefault
      let size = fromMaybe fallback (M.lookup nodeId sizeMap)
      let fineX = fromMaybe 0.0 (M.lookup nodeId fine)
      let x = fineX / Int.toNumber sf
      let y = fromMaybe 0.0 (A.index provisionalLayerYs layerIdx)
      { node: nodeId, position: GridPos (x /\ y), size, layer: layerIdx, order: orderIdx }
    where
    provisionalLayerYs = cumulativeYWithGaps perGapWidths layerHeights

-- | LGraphUtil.placeNodesHorizontally, transposed to DOWN coordinates.
-- | Physical port degrees choose the position within a layer's reserved
-- | depth. Return offsets in the reserved frame used by our placements.
placeWithinLayers
  :: Array (Array NodeId)
  -> Array Number
  -> Map NodeId GridSize
  -> NodeMargins
  -> Array Edge
  -> Map NodeId Number
placeWithinLayers layers heights sizeMap nodeMargins edges =
  foldl placeLayer M.empty (A.mapWithIndex (\i layer -> i /\ layer) layers)
  where
  ports = foldl
    ( \acc edge ->
        { incoming: addPort acc.incoming edge.id edge.to
        , outgoing: addPort acc.outgoing edge.id edge.from
        }
    )
    { incoming: M.empty, outgoing: M.empty }
    edges

  addPort acc edgeId endpoint =
    M.insertWith S.union endpoint.node (S.singleton key) acc
    where
    key = case endpoint.port of
      Just port -> Left port
      Nothing -> Right edgeId

  count :: Map NodeId (S.Set (Either PortId EdgeId)) -> NodeId -> Number
  count byNode node = Int.toNumber (S.size (fromMaybe S.empty (M.lookup node byNode)))

  margin node = fromMaybe { left: 0.0, right: 0.0, top: 0.0, bottom: 0.0 } (M.lookup node nodeMargins)

  placeLayer positions (index /\ layer) = foldl place positions layer
    where
    height = fromMaybe 0.0 (A.index heights index) * Int.toNumber sf
    maximum = foldl
      ( \acc node ->
          let
            m = margin node
          in
            { top: max acc.top m.top, bottom: max acc.bottom m.bottom }
      )
      { top: 0.0, bottom: 0.0 }
      layer

    place acc node = do
      let m = margin node
      let reserved = sizeH (fromMaybe (GridSize (1.0 /\ 1.0)) (M.lookup node sizeMap)) * Int.toNumber sf
      let physical = reserved - m.top - m.bottom
      let incoming = count ports.incoming node
      let outgoing = count ports.outgoing node
      let total = incoming + outgoing
      let ratio = if total == 0.0 then 0.5 else outgoing / total
      let
        adjustment =
          if ratio > 0.5 then -maximum.bottom * 2.0 * (ratio - 0.5)
          else maximum.top * 2.0 * (0.5 - ratio)
      let offset = max m.top (min (height - m.bottom - physical) ((height - physical) * ratio + adjustment))
      M.insert node ((offset - m.top) / Int.toNumber sf) acc

-- | Port of `OrthogonalEdgeRouter.process`'s per-gap routing-width rule:
-- |
-- |     if slotCount > 0:
-- |       routingWidth = (slotCount - 1) * edgeEdgeBetweenLayers
-- |                    + 2 * edgeNodeBetweenLayers
-- |       routingWidth = max(routingWidth, nodeNodeSpacing)
-- |     else:
-- |       routingWidth = nodeNodeSpacing
-- |
-- | `cfg.layerGap` is `nodeNodeSpacing`. Markgraf uses ELK's defaults
-- | `edgeNodeBetweenLayers = 1` grid (4 fine) and
-- | `edgeEdgeBetweenLayers = 2.5` grid (10 fine).
-- |
-- | Slot count comes from `OrthogonalRoutingGenerator.routeEdges`
-- | (ported in `LayeredLayout.EdgeRouting.HyperEdges.slotCountByGap`), which
-- | needs port assignments which need NodePlacements. Callers pass
-- | `provisionalFor` to seed one BK-pass-with-uniform-gap placement.
-- | The slot computation only reads x-positions for conflict counting,
-- | so the seed gap doesn't influence its outcome.
computeLayerGaps
  :: Random
  -> CoordConfig
  -> Array (Array NodeId)
  -> Map NodeId (Array Port)
  -> Array Edge
  -> Array { edgeId :: EdgeId, nodes :: Array NodeId }
  -> EdgePortOffsets
  -> (Array Number -> Array NodePlacement)
  -> { gaps :: Array Number, slots :: Map EdgeId SlotInfo, random :: Random }
computeLayerGaps random cfg layers portMap edges chains portOffsets provisionalFor =
  { gaps: A.mapWithIndex (\index _ -> gapWidth index) baseGaps
  , slots: planned.slots
  , random: planned.random
  }
  where
  numGaps = max 0 (A.length layers - 1)
  baseGap = Int.toNumber cfg.layerGap
  -- ELK defaults from `LayeredOptions`: edgeNodeSpacing.between-layers
  -- = 4 fine = 1 grid; edgeEdgeSpacing.between-layers = 10 fine = 2.5
  -- grid (markgraf uses fine = 4 × grid).
  edgeNodeBetweenLayers = 1.0
  edgeEdgeBetweenLayers = 2.5

  -- Provisional placements with a uniform baseGap. Used to feed the
  -- slot-count pipeline; result drives the actual gap widths.
  baseGaps = A.replicate numGaps baseGap
  provisional = provisionalFor baseGaps

  -- Run the same hyperedge-segment routing the real router will run,
  -- but only collect the slot count per gap.
  -- Preserve p3's physical port order for labelled and unlabelled graphs;
  -- redistributing here can reserve a different channel count than routing.
  assignments = assignPorts edges provisional portMap chains portOffsets
  planned = assignSlots random assignments provisional
  slots = slotCountByGap planned.slots

  gapWidth gapIdx = case M.lookup gapIdx slots of
    Just n | n > 0 ->
      max baseGap
        ( 2.0 * edgeNodeBetweenLayers
            + Int.toNumber (n - 1) * edgeEdgeBetweenLayers
        )
    _ -> baseGap

cumulativeYWithGaps :: Array Number -> Array Number -> Array Number
cumulativeYWithGaps gaps heights = A.mapWithIndex layerY heights
  where
  layerY i _ = foldl
    ( \acc j ->
        acc
          + fromMaybe 1.0 (A.index heights j)
          + fromMaybe 0.0 (A.index gaps j)
    )
    0.0
    (A.take i (A.range 0 (A.length heights - 1)))

-- | Per-pass diagnostic: aligned blocks (root,align maps), inner shifts,
-- | and final x map for one of the four BK passes. Also includes the
-- | accumulated `Postprocessable` queue (after `placeBlock`, before
-- | `postProcess`) and a per-item trace of what `postProcess` did.
type PassDiag =
  { vdir :: VDir
  , hdir :: HDir
  , root :: Map NodeId NodeId
  , align :: Map NodeId NodeId
  , innerShift :: Map NodeId Number
  , x :: Map NodeId Number
  , queue :: Array Postprocessable
  , postProcessTrace :: Array PostProcessTrace
  }

-- | Which drain phase a `PostProcessTrace` entry came from. ELK's
-- | `_.postProcess` first drains the FIFO queue (forward), pushing
-- | failed items onto a stack, then drains the stack in reverse.
data PostProcessPhase
  = ForwardPhase
  | StackPhase

derive instance eqPostProcessPhase :: Eq PostProcessPhase

instance showPostProcessPhase :: Show PostProcessPhase where
  show ForwardPhase = "forward"
  show StackPhase = "stack"

-- | Trace entry for one Postprocessable processed by `postProcess`.
-- | `shift` is the actual delta applied to the block (zero when no
-- | movement happened). `freeSu`, `hasEdges`, `candCount` expose why
-- | a given pp couldn't be straightened (skipped due to su, or no
-- | valid edges, etc.).
type PostProcessTrace =
  { phase :: PostProcessPhase
  , ppFree :: NodeId
  , ppIsRoot :: Boolean
  , edgeId :: Maybe EdgeId
  , delta :: Number
  , avail :: Number
  , shift :: Number
  , freeSu :: Boolean
  , hasEdges :: Boolean
  , candCount :: Int
  }

-- | Top-level diagnostic returned by `assignFineDiag`. Captures the
-- | inputs (layers, dummy-augmented edges) and the per-pass + balanced
-- | output so external code can inspect BK's intermediate state without
-- | needing access to internals.
type Diag =
  { layers :: Array (Array NodeId)
  , markedEdges :: Array MarkedEdge
  , passes :: Array PassDiag
  , balanced :: Map NodeId Number
  , chosen :: Map NodeId Number
  , chosenLabel :: Chosen
  }

-- | Assign coordinates in fine-grid resolution (no rounding).
-- Returns a map from node ID to fine-grid x-coordinate.
assignFine :: CoordConfig -> Array (Array NodeId) -> Map NodeId GridSize -> NodeMargins -> Map NodeId (Array Port) -> Array Edge -> EdgePortOffsets -> Map NodeId Number
assignFine cfg layers sizeMap nodeMargins portMap edges portOffsets = coords
  where
  ni = buildNeighborhood layers edges
  markedEdges = markConflicts ni layers

  -- Scale widths and gaps to fine-grid. Port offsets stay in grid space;
  -- they are scaled at use sites to keep the maps source-of-truth.
  -- Dummy nodes are absent from sizeMap, so when BK looks them up it
  -- falls back to (1.0, 1.0) — too wide. ELK treats dummies as
  -- effectively zero-width so a real node above can sit close to a
  -- real neighbour in the dummy's layer. Insert explicit zero-width
  -- entries for every dummy node we encounter on the layers.
  fineSizeMap = M.union dummySizes (map (\(GridSize (w /\ h)) -> GridSize (w * Int.toNumber sf /\ h)) sizeMap)
  -- LONG_EDGE dummies have width = EDGE_THICKNESS (default 1) in ELK's
  -- BK input — `LongEdgeSplitter.splitEdge` does `dummyNode.getSize().y
  -- = thickness`, and BK's x-axis is ELK's y. Treating dummies as
  -- zero-width here puts every real-node block one fine unit too far
  -- left whenever it neighbours a dummy chain.
  dummySizes = M.fromFoldable
    (A.concat layers # A.filter isDummy <#> \nid -> nid /\ GridSize (1.0 /\ 1.0))
  fineCfg = { nodeGap: cfg.nodeGap * sf, layerGap: cfg.layerGap }
  edgeIndex = indexEdges portMap fineSizeMap edges portOffsets

  layout1 = runLayout fineCfg ni layers fineSizeMap portMap edgeIndex portOffsets markedEdges VDown HRight
  layout2 = runLayout fineCfg ni layers fineSizeMap portMap edgeIndex portOffsets markedEdges VUp HRight
  layout3 = runLayout fineCfg ni layers fineSizeMap portMap edgeIndex portOffsets markedEdges VDown HLeft
  layout4 = runLayout fineCfg ni layers fineSizeMap portMap edgeIndex portOffsets markedEdges VUp HLeft
  layouts = [ layout1, layout2, layout3, layout4 ]

  -- Port of BKNodePlacer.process selection logic: prefer the balanced
  -- median; if it violates layer ordering (overlapping nodes), fall back
  -- to the smallest-width feasible directional layout. If none are
  -- feasible, default to the first directional layout.
  balanced = balanceLayouts fineSizeMap nodeMargins layouts
  coords =
    if checkOrderConstraint fineCfg layers fineSizeMap balanced then balanced
    else case smallestFeasible fineCfg layers fineSizeMap layouts of
      Just l -> l
      Nothing -> fromMaybe M.empty (A.head layouts)

-- | Same as `assignFine` but returns rich per-pass diagnostics for
-- | inspection. Used only by debug specs.
assignFineDiag :: CoordConfig -> Array (Array NodeId) -> Map NodeId GridSize -> NodeMargins -> Map NodeId (Array Port) -> Array Edge -> EdgePortOffsets -> Diag
assignFineDiag cfg layers sizeMap nodeMargins portMap edges portOffsets =
  { layers
  , markedEdges: A.fromFoldable markedEdges
  , passes
  , balanced
  , chosen: coords
  , chosenLabel: chosenLabel
  }
  where
  ni = buildNeighborhood layers edges
  markedEdges = markConflicts ni layers
  fineSizeMap = M.union dummySizes (map (\(GridSize (w /\ h)) -> GridSize (w * Int.toNumber sf /\ h)) sizeMap)
  -- LONG_EDGE dummies have width = EDGE_THICKNESS (default 1) in ELK's
  -- BK input — `LongEdgeSplitter.splitEdge` does `dummyNode.getSize().y
  -- = thickness`, and BK's x-axis is ELK's y. Treating dummies as
  -- zero-width here puts every real-node block one fine unit too far
  -- left whenever it neighbours a dummy chain.
  dummySizes = M.fromFoldable
    (A.concat layers # A.filter isDummy <#> \nid -> nid /\ GridSize (1.0 /\ 1.0))
  fineCfg = { nodeGap: cfg.nodeGap * sf, layerGap: cfg.layerGap }
  edgeIndex = indexEdges portMap fineSizeMap edges portOffsets

  passes = combos <#> \(vd /\ hd /\ _lbl) -> do
    let aligned = verticalAlignment ni layers markedEdges vd hd
    let innerShift = insideBlockShift aligned portMap fineSizeMap edgeIndex.connecting portOffsets
    let hcRes = horizontalCompactionDiag fineCfg ni layers fineSizeMap portMap edgeIndex portOffsets innerShift aligned vd hd
    let withShift = M.mapMaybeWithKey (\nid x -> Just (x + fromMaybe 0.0 (M.lookup nid innerShift))) hcRes.x
    { vdir: vd
    , hdir: hd
    , root: aligned.root
    , align: aligned.align
    , innerShift
    , x: withShift
    , queue: hcRes.queue
    , postProcessTrace: hcRes.trace
    }
  combos =
    [ VDown /\ HRight /\ "DR"
    , VUp /\ HRight /\ "UR"
    , VDown /\ HLeft /\ "DL"
    , VUp /\ HLeft /\ "UL"
    ]
  layouts = passes <#> _.x
  balanced = balanceLayouts fineSizeMap nodeMargins layouts
  feasibleBalanced = checkOrderConstraint fineCfg layers fineSizeMap balanced
  coords =
    if feasibleBalanced then balanced
    else case smallestFeasible fineCfg layers fineSizeMap layouts of
      Just l -> l
      Nothing -> fromMaybe M.empty (A.head layouts)
  chosenLabel =
    if feasibleBalanced then Balanced
    else case smallestFeasible fineCfg layers fineSizeMap layouts of
      Just _ -> SmallestFeasible
      Nothing -> FirstFallback

-- ══════════════════════════════════════════════════════════════════
--  Neighborhood
-- ══════════════════════════════════════════════════════════════════

buildNeighborhood :: Array (Array NodeId) -> Array Edge -> Neighborhood
buildNeighborhood layers edges = { preds, succs, nodeIndex }
  where
  nodeIndex = M.fromFoldable $ A.concat $
    layers <#> A.mapWithIndex (\i nid -> nid /\ i)

  sortByIdx arr = A.sortBy (\a b -> compare (idx a) (idx b)) arr
  idx nid = fromMaybe 0 (M.lookup nid nodeIndex)

  rawPreds = foldl (\m e -> M.insertWith (<>) e.to.node [ e.from.node ] m) M.empty edges
  rawSuccs = foldl (\m e -> M.insertWith (<>) e.from.node [ e.to.node ] m) M.empty edges
  preds = map sortByIdx rawPreds
  succs = map sortByIdx rawSuccs

-- ══════════════════════════════════════════════════════════════════
--  Type 1 conflict marking
-- ══════════════════════════════════════════════════════════════════

-- | Mark edges involved in type 1 conflicts: non-inner segments that
-- cross inner segments (dummy-to-dummy edges). These edges will be
-- ignored during alignment so that long edges stay straight.
markConflicts :: Neighborhood -> Array (Array NodeId) -> S.Set MarkedEdge
markConflicts ni layers =
  if A.length layers < 3 then S.empty
  else foldl markLayer S.empty (A.range 1 (A.length layers - 2))
  where
  markLayer marked i = do
    let upperLayer = fromMaybe [] (A.index layers i)
    let lowerLayer = fromMaybe [] (A.index layers (i + 1))
    let upperSize = A.length upperLayer
    scanLower marked lowerLayer upperLayer upperSize i 0 0 0

  scanLower marked lowerLayer upperLayer upperSize layerI k0 first l = do
    let lowerSize = A.length lowerLayer
    if l >= lowerSize then marked
    else do
      let vl = fromMaybe (NodeId "") (A.index lowerLayer l)
      let isLast = l == lowerSize - 1
      let isInner = isInnerSegment ni vl layerI
      if isLast || isInner then do
        let
          k1 =
            if isInner then
              case A.head (fromMaybe [] (M.lookup vl ni.preds)) of
                Just u -> fromMaybe (upperSize - 1) (M.lookup u ni.nodeIndex)
                Nothing -> upperSize - 1
            else upperSize - 1
        let marked' = markRange marked lowerLayer upperLayer k0 k1 first l layerI
        scanLower marked' lowerLayer upperLayer upperSize layerI k1 (l + 1) (l + 1)
      else scanLower marked lowerLayer upperLayer upperSize layerI k0 first (l + 1)

  -- Each interval ends at an inner segment (or the layer end). Earlier
  -- nodes must not be tested again against the next interval's k0.
  markRange marked lowerLayer _upperLayer k0 k1 first upToL layerI =
    foldl
      ( \m ll -> do
          let vl = fromMaybe (NodeId "") (A.index lowerLayer ll)
          if isInnerSegment ni vl layerI then m
          else foldl
            ( \m' pred' -> do
                let k = fromMaybe 0 (M.lookup pred' ni.nodeIndex)
                if k < k0 || k > k1 then S.insert (edgeKey pred' vl) m'
                else m'
            )
            m
            (fromMaybe [] (M.lookup vl ni.preds))
      )
      marked
      (A.range first upToL)

  isInnerSegment :: Neighborhood -> NodeId -> Int -> Boolean
  isInnerSegment _ni node _layerI =
    isDummy node && A.all isDummy (fromMaybe [] (M.lookup node ni.preds))

-- | Build a key for an edge between two nodes (for marked edge lookup)
edgeKey :: NodeId -> NodeId -> MarkedEdge
edgeKey (NodeId from) (NodeId to) = MarkedEdge (from <> "→" <> to)

-- ══════════════════════════════════════════════════════════════════
--  Single directional layout
-- ══════════════════════════════════════════════════════════════════

runLayout :: CoordConfig -> Neighborhood -> Array (Array NodeId) -> Map NodeId GridSize -> Map NodeId (Array Port) -> EdgeIndex -> EdgePortOffsets -> S.Set MarkedEdge -> VDir -> HDir -> Map NodeId Number
runLayout cfg ni layers sizeMap portMap edgeIndex portOffsets markedEdges vdir hdir = withShift
  where
  aligned = verticalAlignment ni layers markedEdges vdir hdir
  -- Port of `BKAligner.insideBlockShift`. For each block in this pass,
  -- walk the align ring and set per-node shift so that the connected
  -- ports of consecutive block members share the same X. Without
  -- explicit ports the shift collapses to 0 and we keep the existing
  -- centre-aligned block placement.
  innerShift = insideBlockShift aligned portMap sizeMap edgeIndex.connecting portOffsets
  xCoords = horizontalCompaction cfg ni layers sizeMap portMap edgeIndex portOffsets innerShift aligned vdir hdir
  withShift = M.mapMaybeWithKey
    ( \nid x ->
        Just (x + fromMaybe 0.0 (M.lookup nid innerShift))
    )
    xCoords

-- ══════════════════════════════════════════════════════════════════
--  Vertical alignment (block building)
-- ══════════════════════════════════════════════════════════════════

verticalAlignment :: Neighborhood -> Array (Array NodeId) -> S.Set MarkedEdge -> VDir -> HDir -> AlignResult
verticalAlignment ni layers markedEdges vdir hdir = { root: final.root, align: final.align }
  where
  allNodes = A.concat layers
  initRoot = M.fromFoldable (allNodes <#> \n -> n /\ n)
  initAlign = M.fromFoldable (allNodes <#> \n -> n /\ n)

  orderedLayers = case hdir of
    HRight -> layers
    HLeft -> A.reverse layers

  final = foldl processLayer { root: initRoot, align: initAlign } orderedLayers

  processLayer acc layer = do
    let
      nodes = case vdir of
        VDown -> layer
        VUp -> A.reverse layer
    let
      initR = case vdir of
        VDown -> -1
        VUp -> 999999
    let s = foldl (processNode nodes) { root: acc.root, align: acc.align, r: initR } nodes
    { root: s.root, align: s.align }

  processNode _layerNodes state nid = do
    let
      neighbors = case hdir of
        HRight -> fromMaybe [] (M.lookup nid ni.preds)
        HLeft -> fromMaybe [] (M.lookup nid ni.succs)
    let d = A.length neighbors
    if d == 0 then state
    else do
      let low = (d - 1) / 2
      let high = d / 2
      let
        range = case vdir of
          VDown -> A.range low high
          VUp -> A.reverse (A.range low high)
      foldl (tryAlign nid neighbors) state range

  tryAlign nid neighbors st m = do
    let selfAlign = fromMaybe nid (M.lookup nid st.align)
    if selfAlign /= nid then st
    else case A.index neighbors m of
      Nothing -> st
      Just u -> do
        let uIdx = fromMaybe 0 (M.lookup u ni.nodeIndex)
        let
          isMarked = S.member (edgeKey u nid) markedEdges
            || S.member (edgeKey nid u) markedEdges
        let
          canAlign = not isMarked && case vdir of
            VDown -> st.r < uIdx
            VUp -> st.r > uIdx
        if canAlign then do
          let uRoot = fromMaybe u (M.lookup u st.root)
          { root: M.insert nid uRoot st.root
          , align: M.insert u nid (M.insert nid uRoot st.align)
          , r: uIdx
          }
        else st

-- ══════════════════════════════════════════════════════════════════
--  Horizontal compaction (coordinate assignment)
-- ══════════════════════════════════════════════════════════════════

-- | Result of `horizontalCompaction`. `x` is the final per-node
-- | coordinate map. `queue` and `trace` are debug-only and used by
-- | `assignFineDiag`; they expose the post-processing pipeline.
type HCResult =
  { x :: Map NodeId Number
  , queue :: Array Postprocessable
  , trace :: Array PostProcessTrace
  }

horizontalCompaction :: CoordConfig -> Neighborhood -> Array (Array NodeId) -> Map NodeId GridSize -> Map NodeId (Array Port) -> EdgeIndex -> EdgePortOffsets -> Map NodeId Number -> AlignResult -> VDir -> HDir -> Map NodeId Number
horizontalCompaction cfg ni layers sizeMap portMap edgeIndex portOffsets innerShift aligned vdir hdir =
  (horizontalCompactionDiag cfg ni layers sizeMap portMap edgeIndex portOffsets innerShift aligned vdir hdir).x

horizontalCompactionDiag :: CoordConfig -> Neighborhood -> Array (Array NodeId) -> Map NodeId GridSize -> Map NodeId (Array Port) -> EdgeIndex -> EdgePortOffsets -> Map NodeId Number -> AlignResult -> VDir -> HDir -> HCResult
horizontalCompactionDiag cfg ni layers sizeMap portMap edgeIndex portOffsets innerShift aligned vdir hdir =
  { x: ppResult.x, queue: placed.queue, trace: ppResult.trace }
  where
  nodeW nid = sizeW (fromMaybe (GridSize (1.0 /\ 1.0)) (M.lookup nid sizeMap))

  allNodes = A.concat layers
  initSink = M.fromFoldable (allNodes <#> \n -> n /\ n)

  -- Map node → its layer index, used by upper/lower neighbour lookups
  -- and the same-layer edge filter in `pickEdge`.
  layerIndexOf :: Map NodeId Int
  layerIndexOf = M.fromFoldable $ A.concat $
    layers # A.mapWithIndex \li layer -> map (\n -> n /\ li) layer

  layerOf :: NodeId -> Int
  layerOf nid = fromMaybe (-1) (M.lookup nid layerIndexOf)

  layerArrayOf :: NodeId -> Array NodeId
  layerArrayOf nid = fromMaybe [] (A.index layers (layerOf nid))

  orderedLayers = case hdir of
    HRight -> layers
    HLeft -> A.reverse layers

  -- Per-block "only-dummies" flag (port of `bal.od`). ELK initialises
  -- `od[root] = true` for every block and updates `od[root[v]] &= (v is
  -- LONG_EDGE)` only for *non-root* members `v` joining the block. We
  -- replicate that here so a singleton real-node block keeps the
  -- initial `true` (lets `pickEdge` consider its same-layer edges)
  -- while a chain that absorbed any non-dummy member becomes `false`.
  blockOd :: Map NodeId Boolean
  blockOd = foldl bumpOd initialOd allNodes
    where
    allRoots = A.nub (A.fromFoldable (M.values aligned.root))
    initialOd = M.fromFoldable (allRoots <#> \r -> r /\ true)
    bumpOd m v = do
      let r = rootOf v
      if v == r then m
      else M.alter (\b -> Just (fromMaybe true b && isDummy v)) r m

  -- Threshold selection and inside-block alignment use the same physical
  -- port order, precomputed once before the four directional passes.
  incomingByNode = edgeIndex.incoming
  outgoingByNode = edgeIndex.outgoing

  innerShiftOf :: NodeId -> Number
  innerShiftOf nid = fromMaybe 0.0 (M.lookup nid innerShift)

  rootOf :: NodeId -> NodeId
  rootOf nid = fromMaybe nid (M.lookup nid aligned.root)

  -- Place all blocks
  placed = foldl
    ( \st layer -> do
        let
          nodes = case vdir of
            VDown -> layer
            VUp -> A.reverse layer
        foldl
          ( \st' nid -> do
              let rootId = rootOf nid
              if rootId == nid then placeBlock rootId st'
              else st'
          )
          st
          nodes
    )
    { x: M.fromFoldable (allNodes <#> \n -> n /\ (Nothing :: Maybe Number))
    , sink: initSink
    , classEdges: []
    , su: M.empty
    , blockFinished: M.empty
    , queue: []
    }
    orderedLayers

  -- Place classes (longest path on class graph)
  classShifts = placeClasses placed.classEdges placed.sink vdir

  -- Propagate root x to every block member and add the class-graph
  -- sink shift. ELK's `$horizontalCompaction` writes
  -- `y[v] = y[root[v]] + sinkShift` into a per-node array which the
  -- post-processing pass then mutates directly. We mirror that here:
  -- `nodeX0` is the per-node coordinate before postProcess runs.
  nodeX0 :: Map NodeId Number
  nodeX0 = M.fromFoldable (allNodes <#> \nid -> nid /\ nodeX0Of nid)
    where
    nodeX0Of nid = do
      let rootId = rootOf nid
      let rx = fromMaybe 0.0 (join (M.lookup rootId placed.x))
      let sinkId = fromMaybe rootId (M.lookup rootId placed.sink)
      let ss = fromMaybe 0.0 (M.lookup sinkId classShifts)
      rx + ss

  ppResult = postProcess placed.queue placed.su nodeX0

  -- Recursive block placement. After every block finishes placement
  -- it gets recorded in `blockFinished` so the threshold strategy can
  -- consider edges into it for the next root.
  placeBlock :: NodeId -> PlaceState -> PlaceState
  placeBlock rootId state = case join (M.lookup rootId state.x) of
    Just _ -> state
    Nothing -> do
      let state0 = state { x = M.insert rootId (Just 0.0) state.x }
      let blockNodes = getBlockRing aligned rootId
      let
        initThresh = case vdir of
          VDown -> infNeg
          VUp -> infPos
      let result = foldl (processBlockNode rootId) { st: state0, initial: true, thresh: initThresh } blockNodes
      result.st { blockFinished = M.insert rootId true result.st.blockFinished }

  -- ELK uses different spacings for adjacent nodes inside a layer:
  --   nodeNode      (= cfg.nodeGap)  between two real nodes
  --   edgeNode      (~ 4 fine)       between a real node and an edge
  --                                  dummy
  --   edgeEdge      (~ 2 fine)       between two edge dummies
  -- We expose this by switching the spacing in processBlockNode
  -- based on whether either of the adjacent pair is a dummy.
  -- Empirically tuned to match elkjs output: dummy-to-real is 2
  -- fine, dummy-to-dummy is 1 fine. (ELK exposes these as
  -- spacing.edgeNode and spacing.edgeEdge but with smaller
  -- effective values once the BK aligner adjusts for the shared
  -- column of a long edge.)
  -- ELK Spacings.getVerticalSpacing rules:
  --   NORMAL ↔ NORMAL:     SPACING_NODE_NODE (our cfg.nodeGap, fine)
  --   NORMAL ↔ LONG_EDGE:  SPACING_EDGE_NODE (default 10 fine)
  --   LONG_EDGE ↔ LONG_EDGE: SPACING_EDGE_EDGE (default 10 fine)
  edgeNodeSpacing = 10.0
  edgeEdgeSpacing = 10.0
  labelNodeSpacing = 5.0

  spacingBetween :: NodeId -> NodeId -> Number
  spacingBetween a b
    | isPortDummy a && isLabelDummy b || isLabelDummy a && isPortDummy b = labelNodeSpacing
    | isPortDummy a && isPortDummy b = edgeEdgeSpacing
    | isPortDummy a || isPortDummy b = edgeNodeSpacing
    | isLabelDummy a && isLabelDummy b = edgeEdgeSpacing
    | isDummy a && isDummy b = edgeEdgeSpacing
    | isDummy a || isDummy b = edgeNodeSpacing
    | otherwise = Int.toNumber cfg.nodeGap

  processBlockNode :: NodeId -> { st :: PlaceState, initial :: Boolean, thresh :: Number } -> NodeId -> { st :: PlaceState, initial :: Boolean, thresh :: Number }
  processBlockNode rootId acc currentNode = do
    let currentIdx = fromMaybe 0 (M.lookup currentNode ni.nodeIndex)
    let layer = findNodeLayer currentNode orderedLayers
    let layerSize = A.length layer
    let
      hasNeighbor = case vdir of
        VDown -> currentIdx > 0
        VUp -> currentIdx < layerSize - 1
    if not hasNeighbor then do
      -- Even when we can't act on a layer-boundary node, ELK still
      -- recomputes the threshold so the next iteration sees the most
      -- up-to-date value (`SimpleThresholdStrategy.calculateThreshold`
      -- still runs in the `else` branch of placeBlock).
      let { thresh: thresh', state: st' } = calculateThresholdSimple rootId currentNode acc.thresh acc.st
      acc { st = st', thresh = thresh' }
    else do
      let
        neighborIdx = case vdir of
          VDown -> currentIdx - 1
          VUp -> currentIdx + 1
      case A.index layer neighborIdx of
        Nothing -> acc
        Just neighbor -> do
          let neighborRoot = fromMaybe neighbor (M.lookup neighbor aligned.root)
          let st1 = placeBlock neighborRoot acc.st
          -- Threshold is updated AFTER the recursive placeBlock so the
          -- candidate other-block sees finished status.
          let { thresh: thresh', state: st1' } = calculateThresholdSimple rootId currentNode acc.thresh st1

          let curSink = fromMaybe rootId (M.lookup rootId st1'.sink)
          let
            st2 =
              if curSink == rootId then
                st1' { sink = M.insert rootId (fromMaybe neighborRoot (M.lookup neighborRoot st1'.sink)) st1'.sink }
              else st1'

          let neighborSink = fromMaybe neighborRoot (M.lookup neighborRoot st2.sink)
          let rootSink = fromMaybe rootId (M.lookup rootId st2.sink)

          if rootSink == neighborSink then do
            let neighborRootX = fromMaybe 0.0 (join (M.lookup neighborRoot st2.x))
            let currentRootX = fromMaybe 0.0 (join (M.lookup rootId st2.x))
            let spacing = spacingBetween currentNode neighbor
            -- Preserve BKCompactor.placeBlock's evaluation order: subtract
            -- the current inner shift last. Precomputing a shift difference
            -- can move a port across a strict routing-conflict boundary.
            case vdir of
              VDown -> do
                let newPos = neighborRootX + innerShiftOf neighbor + nodeW neighbor + spacing - innerShiftOf currentNode
                let newClamped = max newPos thresh'
                let
                  finalPos =
                    if acc.initial then newClamped
                    else max currentRootX newClamped
                { st: st2 { x = M.insert rootId (Just finalPos) st2.x }, initial: false, thresh: thresh' }
              VUp -> do
                let newPos = neighborRootX + innerShiftOf neighbor - spacing - nodeW currentNode - innerShiftOf currentNode
                let newClamped = min newPos thresh'
                let
                  finalPos =
                    if acc.initial then newClamped
                    else min currentRootX newClamped
                { st: st2 { x = M.insert rootId (Just finalPos) st2.x }, initial: false, thresh: thresh' }
          else do
            let neighborRootX = fromMaybe 0.0 (join (M.lookup neighborRoot st2.x))
            let currentRootX = fromMaybe 0.0 (join (M.lookup rootId st2.x))
            -- ELK BKCompactor.placeBlock CLASSES branch: spacing is
            -- always `SPACING_NODE_NODE` (cfg.nodeGap), not the
            -- node-type-specific `spacingBetween`. The reasoning is
            -- that the class graph compaction must reserve enough
            -- room for any pair of class members; using the smaller
            -- edge-edge / edge-node value would let unrelated blocks
            -- collide once the class shifts are propagated.
            let spacing = Int.toNumber cfg.nodeGap
            let dShift = innerShiftOf currentNode - innerShiftOf neighbor
            let
              sep = case vdir of
                VDown -> currentRootX + dShift - neighborRootX - nodeW neighbor - spacing
                VUp -> currentRootX + dShift + nodeW currentNode + spacing - neighborRootX
            let newEdge = { src: rootSink, tgt: neighborSink, sep }
            { st: st2 { classEdges = st2.classEdges <> [ newEdge ] }, initial: acc.initial, thresh: thresh' }

  -- Port of `ThresholdStrategy.SimpleThresholdStrategy.calculateThreshold`.
  -- Only fires when `currentNode` is the block's root or its last
  -- member; picks the first incident edge to a node whose block is
  -- already `blockFinished`; computes the trunk x where this block
  -- could sit so the chosen edge becomes straight.
  calculateThresholdSimple
    :: NodeId
    -> NodeId
    -> Number
    -> PlaceState
    -> { thresh :: Number, state :: PlaceState }
  calculateThresholdSimple rootId currentNode oldThresh st = do
    let isRoot = currentNode == rootId
    let isLast = (fromMaybe currentNode (M.lookup currentNode aligned.align)) == rootId
    if not (isRoot || isLast) then { thresh: oldThresh, state: st }
    else do
      let
        r1 =
          if isRoot && not (isFiniteThresh oldThresh) then getBound currentNode true st
          else { thresh: oldThresh, state: st }
      if not (isFiniteThresh r1.thresh) && isLast then getBound currentNode false r1.state
      else r1

  isFiniteThresh :: Number -> Boolean
  isFiniteThresh t = case vdir of
    VDown -> t > infNeg
    VUp -> t < infPos

  getBound
    :: NodeId
    -> Boolean
    -> PlaceState
    -> { thresh :: Number, state :: PlaceState }
  getBound currentNode isRoot st = do
    let
      invalid = case vdir of
        VDown -> infNeg
        VUp -> infPos
    let pp = { free: currentNode, isRoot }
    let res = pickEdge pp st
    case res.edge of
      Nothing ->
        -- ELK enqueues `pp` for later post-processing iff there were
        -- valid edges but none targeted a finished block yet.
        if res.hasEdges then { thresh: invalid, state: st { queue = st.queue <> [ pp ] } }
        else { thresh: invalid, state: st }
      Just e -> do
        let other = otherNode e currentNode
        let otherRoot = rootOf other
        let otherY = fromMaybe 0.0 (join (M.lookup otherRoot st.x))
        -- Use the same port-position resolution as insideBlockShift:
        -- explicit port → distributed port (`portOffsets`) → centre.
        let curPortOff = thresholdPortOffsetX e currentNode (sideForCurrent isRoot)
        let otherPortOff = thresholdPortOffsetX e other (sideForOther isRoot)
        let
          threshold = otherY
            + innerShiftOf other
            + otherPortOff
            - innerShiftOf currentNode
            - curPortOff
        -- Mark BOTH endpoints' block roots as `su` (ELK's two
        -- assignments). A block flagged `su` won't be picked again
        -- by either threshold lookups or postProcess.
        let
          st' = st
            { su = M.insert (rootOf e.from.node) true
                (M.insert (rootOf e.to.node) true st.su)
            }
        { thresh: threshold, state: st' }

  -- | Port of `ThresholdStrategy.pickEdge`. Walks the candidate edge
  -- | set for `pp.free` (incoming or outgoing depending on `isRoot`
  -- | and `hdir`), filters out edges that should be skipped, and
  -- | returns the first candidate whose other-block is already
  -- | finished. `hasEdges` is set whenever at least one non-skipped
  -- | candidate exists — even if no finished pick was found, so that
  -- | the caller knows to enqueue for post-processing.
  pickEdge
    :: Postprocessable
    -> PlaceState
    -> { edge :: Maybe Edge, hasEdges :: Boolean }
  pickEdge pp st = foldl step { edge: Nothing, hasEdges: false } candidates
    where
    free = pp.free
    freeRoot = rootOf free
    onlyDummies = fromMaybe true (M.lookup freeRoot blockOd)
    -- ELK's two-arm pickEdge selects which incident-edge set to walk
    -- based on (isRoot, hdir). The pattern is symmetric.
    candidates = case pp.isRoot /\ hdir of
      true /\ HRight -> fromMaybe [] (M.lookup free incomingByNode)
      true /\ HLeft -> fromMaybe [] (M.lookup free outgoingByNode)
      false /\ HRight -> fromMaybe [] (M.lookup free outgoingByNode)
      false /\ HLeft -> fromMaybe [] (M.lookup free incomingByNode)

    step acc e = case acc.edge of
      Just _ -> acc
      Nothing -> do
        let src = e.from.node
        let tgt = e.to.node
        let isSelfLoop = src == tgt
        let sameLayer = layerOf src == layerOf tgt
        -- ELK skip 1: real-node blocks ignore intra-layer non-self
        -- edges (sibling edges). Dummy-only blocks consider them
        -- because long-edge dummies route via in-layer neighbours.
        let skipSameLayer = (not onlyDummies) && (not isSelfLoop) && sameLayer
        -- ELK skip 2: free's block already used to straighten
        -- another edge.
        let skipSu = fromMaybe false (M.lookup freeRoot st.su)
        if skipSameLayer || skipSu then acc
        else do
          let other = otherNode e free
          let otherRoot = rootOf other
          let isFinished = fromMaybe false (M.lookup otherRoot st.blockFinished)
          let differentBlock = otherRoot /= freeRoot
          if differentBlock && isFinished then acc { edge = Just e, hasEdges = true }
          else acc { hasEdges = acc.hasEdges || differentBlock }

  otherNode :: Edge -> NodeId -> NodeId
  otherNode e nid =
    if e.from.node == nid then e.to.node
    else e.from.node

  -- Side of the port on `currentNode` for the picked edge: for an
  -- HRight/root pair the edge points downward into currentNode (it
  -- enters from north). HLeft/root reverses.
  sideForCurrent :: Boolean -> Side
  sideForCurrent isRoot = case isRoot /\ hdir of
    true /\ HRight -> North
    true /\ HLeft -> South
    false /\ HRight -> South
    false /\ HLeft -> North

  sideForOther :: Boolean -> Side
  sideForOther isRoot = case isRoot /\ hdir of
    true /\ HRight -> South
    true /\ HLeft -> North
    false /\ HRight -> North
    false /\ HLeft -> South

  -- | Resolve the port offset for an edge endpoint on a given side, used
  -- | by the threshold strategy. Resolution order matches
  -- | `insideBlockShift.edgePortX`: explicit `Port` → distributed offset
  -- | from `portOffsets` → node centre. Only N/S sides contribute an x
  -- | offset; E/W return 0.
  thresholdPortOffsetX :: Edge -> NodeId -> Side -> Number
  thresholdPortOffsetX e node side = case side of
    North -> resolved
    South -> resolved
    _ -> 0.0
    where
    width = sizeW (fromMaybe (GridSize (1.0 /\ 1.0)) (M.lookup node sizeMap))
    centre = width / 2.0
    resolved = case explicitPortX of
      Just x -> x
      Nothing -> offsetFor portOffsets e.id side centre

    explicitPortX = do
      pid <- case e.from.node == node, e.to.node == node of
        true, _ -> e.from.port
        _, true -> e.to.port
        _, _ -> Nothing
      ports <- M.lookup node portMap
      p <- A.find (\pp -> pp.id == pid) ports
      Just (Int.toNumber p.offset * Int.toNumber sf)

  -- ══════════════════════════════════════════════════════════════════
  --  Post-processing pass (port of SimpleThresholdStrategy.postProcess)
  -- ══════════════════════════════════════════════════════════════════
  --
  -- Drains the queue accumulated during placeBlock: each Postprocessable
  -- represents a getBound call that found valid edge candidates but
  -- none with a finished other-block. Now every block IS finished, so
  -- pickEdge succeeds and we can shift the block to straighten the
  -- chosen edge.
  --
  -- Two passes: forward (FIFO) then leftover (LIFO via stack). Items
  -- that produced no movement (no available space) get deferred to
  -- the leftover stack, mirroring ELK's two-stage drain.
  postProcess
    :: Array Postprocessable
    -> Map NodeId Boolean
    -> Map NodeId Number
    -> { x :: Map NodeId Number, trace :: Array PostProcessTrace }
  postProcess queue suInit initX =
    { x: leftoverResult.x, trace: leftoverResult.trace }
    where
    forwardResult = foldl (drainOne ForwardPhase) emptyAcc queue
    -- Reset the stack so the leftover phase doesn't re-defer items to
    -- itself, but keep the cumulative trace so it spans both phases.
    leftoverResult = foldl (drainOne StackPhase) (forwardResult { stack = [] })
      (A.reverse forwardResult.stack)
    emptyAcc = { x: initX, su: suInit, stack: [], trace: [] }

    drainOne phase acc pp = do
      let res = step phase pp acc.x acc.su
      let acc' = acc { x = res.x, trace = acc.trace <> [ res.entry ] }
      if res.moved then acc'
      else acc' { stack = acc'.stack <> [ pp ] }

    -- Pick a new edge from the now-fully-finished blocks; if none, no
    -- movement; otherwise compute delta and shift block by available
    -- space (clamped to the gap to the upper/lower neighbour).
    step phase pp x su = do
      -- Re-pick using current `su` and (saturated) blockFinished. We
      -- rebuild a transient PlaceState so pickEdge can reuse its rules.
      let
        stForPick =
          { x: M.empty
          , sink: M.empty
          , classEdges: []
          , su
          , blockFinished: blockFinishedAll
          , queue: []
          }
      let res = pickEdge pp stForPick
      let freeRoot = rootOf pp.free
      let freeSu = fromMaybe false (M.lookup freeRoot su)
      let candCount = candidateCount pp
      let
        baseEntry =
          { phase
          , ppFree: pp.free
          , ppIsRoot: pp.isRoot
          , edgeId: Nothing
          , delta: 0.0
          , avail: 0.0
          , shift: 0.0
          , freeSu
          , hasEdges: res.hasEdges
          , candCount
          }
      case res.edge of
        Nothing -> { x, moved: false, entry: baseEntry }
        Just e -> processEdge baseEntry pp e x

    -- For the trace: how many edges sit in pp.free's incident-edge set
    -- before any filtering. A `candCount` of 0 means there's no edge
    -- left to chase regardless of su / same-layer rules.
    candidateCount pp =
      A.length $ case pp.isRoot /\ hdir of
        true /\ HRight -> fromMaybe [] (M.lookup pp.free incomingByNode)
        true /\ HLeft -> fromMaybe [] (M.lookup pp.free outgoingByNode)
        false /\ HRight -> fromMaybe [] (M.lookup pp.free outgoingByNode)
        false /\ HLeft -> fromMaybe [] (M.lookup pp.free incomingByNode)

    processEdge baseEntry pp e x = do
      let free = pp.free
      let src = e.from.node
      let tgt = e.to.node
      -- ELK's $process_82: `block` is the endpoint on free; `fix` is
      -- the other endpoint.
      let block /\ fix = if src == free then src /\ tgt else tgt /\ src
      let blockSide = sideOfEndpoint e block
      let fixSide = sideOfEndpoint e fix
      let blockPos = absPortX e block blockSide x
      let fixPos = absPortX e fix fixSide x
      let delta = blockPos - fixPos
      let blockRoot = rootOf block
      let entry0 = baseEntry { edgeId = Just e.id, delta = delta }
      if delta > 0.0 && delta < 1.0e300 then do
        let avail = checkSpaceAbove blockRoot delta x
        let shift = if avail > 0.0 then negate avail else 0.0
        let x' = if avail > 0.0 then shiftBlockX blockRoot shift x else x
        { x: x', moved: avail > 0.0, entry: entry0 { avail = avail, shift = shift } }
      else if delta < 0.0 && (negate delta) < 1.0e300 then do
        let avail = checkSpaceBelow blockRoot (negate delta) x
        let shift = if avail > 0.0 then avail else 0.0
        let x' = if avail > 0.0 then shiftBlockX blockRoot shift x else x
        { x: x', moved: avail > 0.0, entry: entry0 { avail = avail, shift = shift } }
      else
        { x, moved: false, entry: entry0 }

    -- All blocks are finished by the time postProcess runs.
    blockFinishedAll = M.fromFoldable
      ((A.nub (A.fromFoldable (M.values aligned.root))) <#> \r -> r /\ true)

  -- | Sweep direction changes block traversal, not physical ports.
  -- | This must agree with insideBlockShift and getBound, including
  -- | fixed off-centre label ports.
  sideOfEndpoint :: Edge -> NodeId -> Side
  sideOfEndpoint e node = if e.from.node == node then South else North

  -- | Absolute port x for a node endpoint of an edge, given the
  -- | current per-node x map. Mirrors ELK's
  -- |   y[node] + innerShift[node] + port.pos.y + port.anchor.y.
  absPortX :: Edge -> NodeId -> Side -> Map NodeId Number -> Number
  absPortX e node side x = do
    let nx = fromMaybe 0.0 (M.lookup node x)
    nx + innerShiftOf node + thresholdPortOffsetX e node side

  -- | Port of `$shiftBlock`: walk the block's align ring and add
  -- | `delta` to every member's per-node x.
  shiftBlockX :: NodeId -> Number -> Map NodeId Number -> Map NodeId Number
  shiftBlockX blockRoot delta x = foldl bump x (getBlockRing aligned blockRoot)
    where
    bump m v = M.insert v (fromMaybe 0.0 (M.lookup v m) + delta) m

  -- | Port of `$checkSpaceAbove`: how much we can shift the block
  -- | toward smaller in-layer index without colliding with the
  -- | upper layer-neighbour of any member. Returns
  -- |   min(delta, min over members of (member.left - upperNeighbor.right - spacing))
  checkSpaceAbove :: NodeId -> Number -> Map NodeId Number -> Number
  checkSpaceAbove blockRoot delta x =
    foldl (gapStep upperNeighbor minMaxAbove) delta (getBlockRing aligned blockRoot)
    where
    minMaxAbove current neighbor = do
      let curX = fromMaybe 0.0 (M.lookup current x)
      let nbrX = fromMaybe 0.0 (M.lookup neighbor x)
      let minXcurrent = curX + innerShiftOf current
      let maxXneighbor = nbrX + innerShiftOf neighbor + nodeW neighbor
      minXcurrent - (maxXneighbor + spacingBetween current neighbor)

  checkSpaceBelow :: NodeId -> Number -> Map NodeId Number -> Number
  checkSpaceBelow blockRoot delta x =
    foldl (gapStep lowerNeighbor minMaxBelow) delta (getBlockRing aligned blockRoot)
    where
    minMaxBelow current neighbor = do
      let curX = fromMaybe 0.0 (M.lookup current x)
      let nbrX = fromMaybe 0.0 (M.lookup neighbor x)
      let maxXcurrent = curX + innerShiftOf current + nodeW current
      let minXneighbor = nbrX + innerShiftOf neighbor
      minXneighbor - (maxXcurrent + spacingBetween current neighbor)

  gapStep
    :: (NodeId -> Maybe NodeId)
    -> (NodeId -> NodeId -> Number)
    -> Number
    -> NodeId
    -> Number
  gapStep neighborOf gapOf avail current = case neighborOf current of
    Nothing -> avail
    Just neighbor -> min avail (gapOf current neighbor)

  upperNeighbor :: NodeId -> Maybe NodeId
  upperNeighbor n = A.index (layerArrayOf n) (layerNodeIndex n - 1)

  lowerNeighbor :: NodeId -> Maybe NodeId
  lowerNeighbor n = A.index (layerArrayOf n) (layerNodeIndex n + 1)

  layerNodeIndex :: NodeId -> Int
  layerNodeIndex n = fromMaybe (-1) (M.lookup n ni.nodeIndex)

  infPos :: Number
  infPos = 1.0e18

  infNeg :: Number
  infNeg = -1.0e18

  findNodeLayer :: NodeId -> Array (Array NodeId) -> Array NodeId
  findNodeLayer nid lrs = fromMaybe [] (A.find (\layer -> A.elem nid layer) lrs)

-- | Walk the align ring starting from root, collecting all block members.
getBlockRing :: AlignResult -> NodeId -> Array NodeId
getBlockRing aligned rootId = go (fromMaybe rootId (M.lookup rootId aligned.align)) [ rootId ]
  where
  go current acc
    | current == rootId = acc
    | otherwise = go (fromMaybe rootId (M.lookup current aligned.align)) (acc <> [ current ])

-- ══════════════════════════════════════════════════════════════════
--  insideBlockShift (port of BKAligner.insideBlockShift)
-- ══════════════════════════════════════════════════════════════════

-- | BKNodePlacer.getEdge and ThresholdStrategy.pickEdge both scan the
-- | current node's connected ports, not the graph's edge list. Build
-- | their indices once for all four passes. ELK's clockwise port order
-- | transposes to WEST/SOUTH/EAST/NORTH in our DOWN coordinates.
indexEdges
  :: Map NodeId (Array Port)
  -> Map NodeId GridSize
  -> Array Edge
  -> EdgePortOffsets
  -> EdgeIndex
indexEdges portMap fineSizeMap edges portOffsets =
  { connecting: map _.edge indexed.connecting
  , incoming: map ordered indexed.incoming
  , outgoing: map ordered indexed.outgoing
  }
  where
  indexed = foldl addEdge
    { connecting: M.empty, incoming: M.empty, outgoing: M.empty, next: 0 }
    edges

  ordered entries = map _.edge $ A.sortBy (\a b -> compare a.order b.order) $ A.fromFoldable entries

  addEdge acc e = do
    let source = endpoint e e.from.node South acc.next
    let target = endpoint e e.to.node North acc.next
    { connecting: addConnection e.to.node e.from.node target
        (addConnection e.from.node e.to.node source acc.connecting)
    , incoming: M.insertWith (\old new -> new <> old) e.to.node (Cons target Nil) acc.incoming
    , outgoing: M.insertWith (\old new -> new <> old) e.from.node (Cons source Nil) acc.outgoing
    , next: acc.next + 1
    }

  addConnection node other candidate = M.insertWith
    (\existing next -> if next.order < existing.order then next else existing)
    (node /\ other)
    candidate

  endpoint e node defaultSide index = do
    let
      port = endpointPort portMap e node
      side = fromMaybe defaultSide (port <#> _.side)
      width = sizeW (fromMaybe (GridSize (1.0 /\ 1.0)) (M.lookup node fineSizeMap))
      offset = case port of
        Just p -> Int.toNumber p.offset * Int.toNumber sf
        Nothing -> offsetFor portOffsets e.id side (width / 2.0)
      position = case side of
        West -> 0 /\ offset
        South -> 1 /\ offset
        East -> 2 /\ negate offset
        North -> 3 /\ negate offset
    -- Equal/shared ports retain their incident-edge insertion order.
    { edge: e, order: position /\ index }

endpointPort :: Map NodeId (Array Port) -> Edge -> NodeId -> Maybe Port
endpointPort portMap e node = do
  pid <- if e.from.node == node then e.from.port else e.to.port
  ports <- M.lookup node portMap
  A.find (\p -> p.id == pid) ports

-- | For each block, walk the align ring; for every consecutive pair
-- | (current, next) connected by an edge, compute the X-offset between
-- | the connected ports so the edge becomes a straight vertical line:
-- |
-- |     current.x + currentPortOffsetX == next.x + nextPortOffsetX
-- |
-- | The accumulated shift is recorded per node and applied to BK's
-- | block-aligned coordinates. ELK's algorithm is symmetric; we walk
-- | from root through align[root], align[align[root]], ... back to root.
-- |
-- | For HRight passes, the connecting edge points current → next; for
-- | HLeft, the edge points next → current. The "connected port" on
-- | current is south (HRight) or north (HLeft); on next it's the
-- | opposite. Without an explicit port we use the centre offset (no
-- | shift contribution).
insideBlockShift
  :: AlignResult
  -> Map NodeId (Array Port)
  -> Map NodeId GridSize
  -> Map (NodeId /\ NodeId) Edge
  -> EdgePortOffsets
  -> Map NodeId Number
insideBlockShift aligned portMap fineSizeMap connectedEdges portOffsets =
  foldl shiftBlock M.empty roots
  where
  roots = A.nub (A.fromFoldable (M.values aligned.root))

  shiftBlock acc root = do
    let ring = getBlockRing aligned root
    let walked = walk M.empty 0.0 root ring
    -- Port of the second pass in `BKAligner.insideBlockShift`: every
    -- block member's inner shift is bumped by `spaceAbove`, which is
    -- max(0, max over members of (margin.top - shift)). With margins
    -- = 0 this collapses to max(0, -shift), i.e. the most-negative
    -- shift in the block (negated).
    let
      spaceAbove = foldl (\m (_ /\ s) -> max m (negate s)) 0.0
        (M.toUnfoldable walked :: Array (NodeId /\ Number))
    foldl (\a (k /\ s) -> M.insert k (s + spaceAbove) a) acc
      (M.toUnfoldable walked :: Array (NodeId /\ Number))

  walk acc shift current ring = case A.uncons ring of
    Nothing -> M.insert current shift acc -- single-node block
    Just { head: first, tail: rest } ->
      stepThrough (M.insert first shift acc) shift first rest

  stepThrough acc shift prev rest = case A.uncons rest of
    Nothing -> acc
    Just { head: nxt, tail: more } -> do
      let portDiff = portPosDiff prev nxt
      let nextShift = shift + portDiff
      stepThrough (M.insert nxt nextShift acc) nextShift nxt more

  -- portPosDiff prev nxt = prevPortX - nextPortX (relative to node x).
  -- After: nxt.x + nextPortX == prev.x + prevPortX.
  portPosDiff prev nxt = case M.lookup (prev /\ nxt) connectedEdges of
    Nothing -> 0.0
    Just e -> do
      let { source, sourceSide, targetSide } = orient e prev nxt
      let prevSide = if source == prev then sourceSide else targetSide
      let nxtSide = if source == nxt then sourceSide else targetSide
      let prevX = edgePortX e prev prevSide
      let nxtX = edgePortX e nxt nxtSide
      prevX - nxtX

  orient e _ _ = do
    let source = e.from.node
    let target = e.to.node
    -- Port sides are graph-truth for a forward DOWN edge: source exits
    -- south, target enters north. The HLeft sweep reverses chain
    -- traversal but port positions don't move, so sides do not depend
    -- on hdir.
    { source, sourceSide: South, target, targetSide: North }

  -- Port X offset for one endpoint of an edge in fine-grid units.
  -- Resolution order: explicit `Port` declared on the node → distributed
  -- offset from `portOffsets` (pre-BK distribution) → node centre. Only
  -- North/South sides contribute an x offset; East/West return 0.
  edgePortX :: Edge -> NodeId -> Side -> Number
  edgePortX e node side = case side of
    North -> resolved
    South -> resolved
    _ -> 0.0
    where
    width = sizeW (fromMaybe (GridSize (1.0 /\ 1.0)) (M.lookup node fineSizeMap))
    centre = width / 2.0
    -- `portOffsets` is keyed in fine units (caller passes a fine sizeMap).
    resolved = case explicitPortX e node of
      Just x -> x
      Nothing -> offsetFor portOffsets e.id side centre

  explicitPortX :: Edge -> NodeId -> Maybe Number
  explicitPortX e node = endpointPort portMap e node <#> \p ->
    Int.toNumber p.offset * Int.toNumber sf

-- ══════════════════════════════════════════════════════════════════
--  Class placement (longest path on class graph)
-- ══════════════════════════════════════════════════════════════════

placeClasses :: Array { src :: NodeId, tgt :: NodeId, sep :: Number } -> Map NodeId NodeId -> VDir -> Map NodeId Number
placeClasses classEdges sinkMap vdir = shifts
  where
  allSinks = S.fromFoldable (map _.src classEdges <> map _.tgt classEdges <> A.fromFoldable (M.values sinkMap))
  adjMap = foldl (\m e -> M.insertWith (<>) e.src [ { target: e.tgt, sep: e.sep } ] m) M.empty classEdges
  indegMap = foldl (\m e -> M.insertWith (+) e.tgt 1 m) M.empty classEdges

  allSinkArr = S.toUnfoldable allSinks :: Array NodeId
  initialSinks = A.filter (\s -> fromMaybe 0 (M.lookup s indegMap) == 0) allSinkArr

  shifts = propagate initialSinks indegMap (foldl (\m s -> M.insert s 0.0 m) M.empty allSinkArr)

  propagate :: Array NodeId -> Map NodeId Int -> Map NodeId Number -> Map NodeId Number
  propagate queue indeg result = case A.uncons queue of
    Nothing -> result
    Just { head: n, tail: rest } -> do
      let nShift = fromMaybe 0.0 (M.lookup n result)
      let outEdges = fromMaybe [] (M.lookup n adjMap)
      let
        { newQueue, result: result', indeg: indeg' } = foldl
          ( \acc e -> do
              let tgtShift = M.lookup e.target acc.result
              let proposed = nShift + e.sep
              let
                newShift = case tgtShift of
                  Nothing -> proposed
                  Just cur -> case vdir of
                    VDown -> min cur proposed
                    VUp -> max cur proposed
              let newIndeg = fromMaybe 0 (M.lookup e.target acc.indeg) - 1
              let indeg'' = M.insert e.target newIndeg acc.indeg
              let q = if newIndeg == 0 then acc.newQueue <> [ e.target ] else acc.newQueue
              { newQueue: q, result: M.insert e.target newShift acc.result, indeg: indeg'' }
          )
          { newQueue: [], result: result, indeg: indeg }
          outEdges
      propagate (rest <> newQueue) indeg' result'

-- ══════════════════════════════════════════════════════════════════
--  Layout selection
-- ══════════════════════════════════════════════════════════════════

-- | Port of `BKNodePlacer.createBalancedLayout`.
-- |
-- |     for each layout i:
-- |       min[i] = min over nodes of (y + innerShift)
-- |       max[i] = max over nodes of (y + innerShift + size)
-- |     refIdx = argmin width
-- |     shift[i] = (vdir == DOWN) ? min[ref] - min[i] : max[ref] - max[i]
-- |     for each node:
-- |       sort the 4 (y + innerShift + shift[i]) values
-- |       balanced.y = (sorted[1] + sorted[2]) / 2
-- |
-- | Layouts arrive with `innerShift` already folded into the values. The
-- | `max` boundary adds nodeWidth so a zero-width dummy at x=K doesn't
-- | out-rank a real node at x=K-w+ε (matching ELK's `nodePosY +
-- | n.getSize().y`).
balanceLayouts :: Map NodeId GridSize -> NodeMargins -> Array (Map NodeId Number) -> Map NodeId Number
balanceLayouts sizeMap nodeMargins layouts = normalizeLayout balanced
  where
  nodeW nid = sizeW (fromMaybe (GridSize (1.0 /\ 1.0)) (M.lookup nid sizeMap))
  marginStart nid = fromMaybe 0.0 (M.lookup nid nodeMargins <#> _.left)
  marginEnd nid = fromMaybe 0.0 (M.lookup nid nodeMargins <#> _.right)

  sized = A.mapWithIndex (\i l -> { i, l, w: layoutSize sizeMap l }) layouts
  refIdx = case A.head (A.sortBy (\a b -> compare a.w b.w) sized) of
    Just s -> s.i
    Nothing -> 0
  refMin = case A.index layouts refIdx of
    Just l -> minVal l
    Nothing -> 0.0
  refMax = case A.index layouts refIdx of
    Just l -> maxVal l
    Nothing -> 0.0
  shifts = A.mapWithIndex (\i l -> if i `mod` 2 == 0 then refMin - minVal l else refMax - maxVal l) layouts
  shifted = A.zipWith (\l s -> map (_ + s) l) layouts shifts
  allKeys = A.nub (A.concat (shifted <#> \m -> A.fromFoldable (M.keys m)))
  balanced = foldl
    ( \acc k -> do
        let vals = A.sort (A.mapMaybe (M.lookup k) shifted)
        let
          med = case A.length vals of
            4 -> case A.index vals 1 /\ A.index vals 2 of
              Just a /\ Just b -> (a + b) / 2.0
              _ -> 0.0
            _ -> case A.head vals of
              Just v -> v
              Nothing -> 0.0
        M.insert k med acc
    )
    M.empty
    allKeys

  -- The reference size includes reserved margins, but balancing aligns
  -- the physical node boundaries (BKNodePlacer.createBalancedLayout).
  minVal m = foldl
    (\acc (nid /\ x) -> min acc (x + marginStart nid))
    999999.0
    (M.toUnfoldable m :: Array (NodeId /\ Number))
  maxVal m = foldl
    (\acc (nid /\ x) -> max acc (x + nodeW nid - marginEnd nid))
    (-999999.0)
    (M.toUnfoldable m :: Array (NodeId /\ Number))

-- | Port of `BKNodePlacer.checkOrderConstraint`. For each layer, walks
-- | the nodes in their layer order; verifies that each node's left edge
-- | sits beyond the cumulative position established by previous nodes
-- | (no overlaps). Returns true when every layer is feasible.
-- |
-- | We don't have node margins, so the check reduces to: x_i+1 >= x_i + width_i.
-- | A small epsilon allows for floating-point round-off equal to ELK's
-- | EPSILON tolerance in fuzzy comparisons.
checkOrderConstraint
  :: CoordConfig -> Array (Array NodeId) -> Map NodeId GridSize -> Map NodeId Number -> Boolean
checkOrderConstraint _cfg layers sizeMap coords = A.all layerFeasible layers
  where
  eps = 0.0001

  layerFeasible layer = (foldl step { ok: true, pos: bottom } layer).ok
    where
    bottom = -1.0e18 -- ELK: Double.NEGATIVE_INFINITY

  step acc nid =
    if not acc.ok then acc
    else do
      let x = fromMaybe 0.0 (M.lookup nid coords)
      let w = sizeW (fromMaybe (GridSize (1.0 /\ 1.0)) (M.lookup nid sizeMap))
      let top = x
      let bot = x + w
      if top + eps > acc.pos && bot + eps > acc.pos then { ok: true, pos: bot }
      else { ok: false, pos: acc.pos }

-- | Pick the smallest-width directional layout that is feasible under
-- | `checkOrderConstraint`. Returns Nothing if none are feasible.
smallestFeasible
  :: CoordConfig -> Array (Array NodeId) -> Map NodeId GridSize -> Array (Map NodeId Number) -> Maybe (Map NodeId Number)
smallestFeasible cfg layers sizeMap candidates = A.head sorted <#> _.l
  where
  feasible = candidates # A.filter (checkOrderConstraint cfg layers sizeMap)
  sized = feasible <#> \l -> { l, w: layoutSize sizeMap l }
  sorted = A.sortBy (\a b -> compare a.w b.w) sized

normalizeLayout :: Map NodeId Number -> Map NodeId Number
normalizeLayout m = do
  let vals = A.fromFoldable (M.values m)
  let minX = foldl min 999999.0 vals
  if minX == 0.0 || A.length vals == 0 then m
  else map (\x -> x - minX) m

-- | Inner shifts are already included in coordinates. The bounding
-- | interval of all reserved node rectangles equals the extent of the
-- | aligned blocks, including zero-width nodes and dummy edge thickness.
layoutSize :: Map NodeId GridSize -> Map NodeId Number -> Number
layoutSize sizeMap positions = bounds.maximum - bounds.minimum
  where
  bounds = foldl
    ( \acc (nid /\ x) ->
        let
          width = sizeW (fromMaybe (GridSize (1.0 /\ 1.0)) (M.lookup nid sizeMap))
        in
          { minimum: min acc.minimum x, maximum: max acc.maximum (x + width) }
    )
    { minimum: 999999.0, maximum: -999999.0 }
    (M.toUnfoldable positions :: Array (NodeId /\ Number))

-- ══════════════════════════════════════════════════════════════════
--  Utilities
-- ══════════════════════════════════════════════════════════════════

applyConstraints :: Array Constraints -> Array NodePlacement -> Array NodePlacement
applyConstraints constraints placements = foldl applyOne placements constraints
  where
  applyOne ps = case _ of
    AlignGroup { nodes: groupNodes, axis, alignment, justify: _ } -> alignNodes ps groupNodes axis alignment
    RelativePosition { anchor, target, offset } -> applyRelative ps anchor target offset
    _ -> ps

alignNodes :: Array NodePlacement -> Array NodeId -> Axis -> Alignment -> Array NodePlacement
alignNodes placements groupNodes axis alignment = do
  let groupPlacements = A.filter (\p -> A.elem p.node groupNodes) placements
  let
    coord = case axis /\ alignment of
      Vertical /\ Start -> foldl (\mn p -> min mn (gridX p.position)) 99999.0 groupPlacements
      Vertical /\ End -> foldl (\mx p -> max mx (gridX p.position)) 0.0 groupPlacements
      Vertical /\ Center -> do
        let total = foldl (\s p -> s + gridX p.position) 0.0 groupPlacements
        if A.length groupPlacements == 0 then 0.0 else total / Int.toNumber (A.length groupPlacements)
      Horizontal /\ Start -> foldl (\mn p -> min mn (gridY p.position)) 99999.0 groupPlacements
      Horizontal /\ End -> foldl (\mx p -> max mx (gridY p.position)) 0.0 groupPlacements
      Horizontal /\ Center -> do
        let total = foldl (\s p -> s + gridY p.position) 0.0 groupPlacements
        if A.length groupPlacements == 0 then 0.0 else total / Int.toNumber (A.length groupPlacements)
  placements <#> \p ->
    if A.elem p.node groupNodes then
      case axis of
        Vertical -> p { position = GridPos (coord /\ gridY p.position) }
        Horizontal -> p { position = GridPos (gridX p.position /\ coord) }
    else p

applyRelative :: Array NodePlacement -> NodeId -> NodeId -> GridPos -> Array NodePlacement
applyRelative placements anchor target offset = do
  let anchorPos = A.findMap (\p -> if p.node == anchor then Just p.position else Nothing) placements
  case anchorPos of
    Nothing -> placements
    Just aPos -> placements <#> \p ->
      if p.node == target then
        p { position = GridPos ((gridX aPos + gridX offset) /\ (gridY aPos + gridY offset)) }
      else p
