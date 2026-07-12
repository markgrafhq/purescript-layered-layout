-- | Port of `OrthogonalRoutingGenerator` hyperedge segment construction
-- | and slot assignment.
-- |
-- | Pipeline (matching ELK):
-- |   1. Group edges by (sourceNode, sourcePort) per gap → segments.
-- |   2. For every segment pair compute crossings + conflicts and add a
-- |      directed `HyperEdgeSegmentDependency`. Penalties: CONFLICT_PENALTY=1,
-- |      CROSSING_PENALTY=16. Two thresholds: regular conflictThreshold
-- |      (0.5 * edgeSpacing) marks a non-critical conflict; the smaller
-- |      criticalConflictThreshold (0.2 * minimumHorizontalSegmentDistance)
-- |      marks a CRITICAL conflict — both orderings would force overlap.
-- |   3. `breakCriticalCycles`: run cycle detection on CRITICAL deps
-- |      only; feed the leftward criticals to `splitSegments` (port of
-- |      `HyperEdgeSegmentSplitter`). Splitting cuts a segment into two
-- |      parts joined through a free area, regenerating the deps for
-- |      the new segments.
-- |   4. `breakNonCriticalCycles`: run the Eades-Lin-Smyth feedback-arc-set
-- |      heuristic over the full dep graph; reverse leftward regular
-- |      deps and remove zero-weight ones.
-- |   5. Topologically number the resulting DAG into routing slots
-- |      (`OrthogonalRoutingGenerator.topologicalNumbering`).
-- |
-- | The slot index drives each edge's horizontal-trunk y via
-- |
-- |     y = gapTop + edgeNodeBetweenLayers
-- |               + slot * edgeEdgeBetweenLayers
module LayeredLayout.EdgeRouting.HyperEdges
  ( SlotInfo
  , assignSlots
  , slotCountByGap
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (un)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import LayeredLayout.EdgeRouting.PortAssignment (PortAssignment, scaleFactor)
import LayeredLayout.Graph (EdgeId(..), NodeId(..), PortId(..))
import LayeredLayout.Result (NodePlacement)

type SlotInfo =
  { slot :: Int
  , slotCount :: Int
  , gapTop :: Number
  , gapBottom :: Number
  , partner :: Maybe { slot :: Int, splitX :: Number }
  }

data DepKind = Regular | Critical

derive instance Eq DepKind

isCritical :: DepKind -> Boolean
isCritical Critical = true
isCritical Regular = false

type Dep = { src :: Int, tgt :: Int, weight :: Int, kind :: DepKind }

type Segment =
  { id :: Int
  , members :: Array EdgeId
  , incoming :: Array Number
  , outgoing :: Array Number
  , slot :: Int
  , mark :: Int
  , splitBy :: Maybe Int
  , splitPartner :: Maybe Int
  }

type ConflictResult = { conflicts :: Int, critical :: Boolean }

-- | Per-gap routing-slot count, keyed by gap index. The slot count
-- | is `OrthogonalRoutingGenerator.routeEdges`'s `rankCount + 1`:
-- | the number of distinct horizontal routing channels the orthogonal
-- | router will need in that gap. Straight pass-through segments
-- | don't add a rank, but every segment with a horizontal trunk does.
-- |
-- | We derive this from `assignSlots`' output by bucketing edges back to
-- | their gap (via the placement layers) and reading out the shared
-- | `slotCount` field.
slotCountByGap :: Array PortAssignment -> Array NodePlacement -> Map Int Int
slotCountByGap assignments placements =
  foldl perEdge M.empty assignments
  where
  slots = assignSlots assignments placements

  placedByNode :: Map NodeId NodePlacement
  placedByNode = foldl (\m p -> M.insert p.node p m) M.empty placements

  layerOfNode :: NodeId -> Maybe Int
  layerOfNode nid = M.lookup nid placedByNode <#> _.layer

  gapIndex a = case layerOfNode a.edge.from.node /\ layerOfNode a.edge.to.node of
    Just s /\ Just t | s /= t -> Just (min s t)
    _ -> Nothing

  perEdge acc a = case gapIndex a /\ M.lookup a.edge.id slots of
    Just gi /\ Just info -> M.insert gi info.slotCount acc
    _ -> acc

assignSlots :: Array PortAssignment -> Array NodePlacement -> Map EdgeId SlotInfo
assignSlots assignments placements =
  foldl mergeGap M.empty grouped
  where
  mergeGap acc (gap /\ assigns) =
    foldl (\m (eid /\ info) -> M.insert eid info m) acc (slotsForGap gap assigns)

  layerOfNode :: NodeId -> Maybe Int
  layerOfNode nid = M.lookup nid placedByNode <#> _.layer

  orderOfNode :: NodeId -> Maybe Int
  orderOfNode nid = M.lookup nid placedByNode <#> _.order

  placedByNode :: Map NodeId NodePlacement
  placedByNode = foldl (\m p -> M.insert p.node p m) M.empty placements

  -- Sort assignments by source-node (layer, order) so that segments
  -- inside each gap are seeded in the same order ELK iterates them
  -- (`$createHyperEdgeSegments` walks `sourceLayerNodes` in layer
  -- order). This iteration order propagates into segment IDs and
  -- changes the FAS tie-breaks in `computeMarks`, which decides which
  -- segment the splitter picks for a critical-cycle break.
  orderedAssignments :: Array PortAssignment
  orderedAssignments = A.sortBy compareByLayerOrder assignments
    where
    keyOf a =
      fromMaybe 1000000 (layerOfNode a.edge.from.node)
        /\ fromMaybe 1000000 (orderOfNode a.edge.from.node)
    compareByLayerOrder a b = compare (keyOf a) (keyOf b)

  grouped :: Array (Int /\ Array PortAssignment)
  grouped = M.toUnfoldable (foldl groupOne M.empty orderedAssignments)
    where
    groupOne acc a = case gapIndex a of
      Just gi -> M.insertWith (<>) gi [ a ] acc
      Nothing -> acc

  gapIndex :: PortAssignment -> Maybe Int
  gapIndex a = case layerOfNode a.edge.from.node /\ layerOfNode a.edge.to.node of
    Just s /\ Just t | s /= t -> Just (min s t)
    _ -> Nothing

  -- ELK constants from `OrthogonalRoutingGenerator`.
  conflictPenalty = 1
  crossingPenalty = 16
  -- ELK's `OrthogonalEdgeRouter.process` constructs the routing
  -- generator with `edgeSpacing = SPACING_EDGE_EDGE_BETWEEN_LAYERS`
  -- (default 10 fine = 2.5 grid). The conflict threshold is
  -- `CONFLICT_THRESHOLD_FACTOR * edgeSpacing` with factor 0.5, so
  -- 5 fine = 1.25 grid.
  conflictThreshold = 0.5 * 2.5 * Int.toNumber scaleFactor

  slotsForGap :: Int -> Array PortAssignment -> Array (EdgeId /\ SlotInfo)
  slotsForGap _ assigns = do
    let segments0 = buildSegments assigns
    if A.null segments0 then []
    else do
      let critThreshold = 0.2 * minimumHorizontalSegmentDistance segments0
      let withDeps = addDependencies critThreshold segments0
      let postSplit = breakCriticalCycles critThreshold withDeps
      let acyclic = breakNonCriticalCycles postSplit
      let numbered = topologicalNumbering acyclic
      -- Port of `OrthogonalRoutingGenerator.routeEdges` return:
      -- `rankCount = max over segments of routingSlot`; the function
      -- returns `rankCount + 1`. Each segment's slot counts as a
      -- routing rank that needs its own y; straight pass-through
      -- segments don't contribute a rank.
      let total = 1 + foldl (\m seg -> max m seg.slot) 0 numbered
      let { gapTop, gapBottom } = gapBounds numbered
      -- A long-edge segment that got split during critical-cycle
      -- breaking now has two halves in `numbered`: `halfA` (carries
      -- the original `incoming` and `splitBy = Just causing`) and
      -- `partner` (carries the original `outgoing` and `splitBy =
      -- Nothing`). They share `members`, so we emit ONE entry per
      -- `members` keyed by the SOURCE-half (halfA) and attach the
      -- partner's slot + splitX so the router can place a second
      -- horizontal trunk after the split. We drop the partner's
      -- direct emission — `splitBy = Nothing && splitPartner = Just`
      -- with the partner's id pointing to a `splitBy = Just` segment
      -- means "this is the trailing half".
      let segById = M.fromFoldable (numbered <#> \s -> s.id /\ s)
      let
        isTrailingHalf s = case s.splitPartner of
          Just pid -> case M.lookup pid segById of
            Just p | isJust p.splitBy -> true
            _ -> false
          Nothing -> false
      let useful = A.filter (not <<< isTrailingHalf) numbered
      A.concat
        ( useful <#> \seg ->
            seg.members <#> \eid -> do
              let
                partnerInfo = case seg.splitPartner of
                  Just pid -> case M.lookup pid segById of
                    Just p -> Just { slot: p.slot, splitX: fromMaybe 0.0 (A.head p.incoming) }
                    Nothing -> Nothing
                  Nothing -> Nothing
              eid /\
                { slot: seg.slot, slotCount: total, gapTop, gapBottom, partner: partnerInfo }
        )

  -- ── Step 1: hyperedge segments grouped by source port ────────────

  -- Include straight segments too, and preserve source-layer-node
  -- iteration order. ELK's `$createHyperEdgeSegments` walks layer
  -- nodes in order and adds every OUTPUT port: each new port either
  -- joins an existing hyperedge (shared port) or seeds a new one. The
  -- rankCount loop later skips straight segments, but the FAS marker
  -- (`computeMarks`) and the splitter (`decideWhichSegmentsToSplit`)
  -- BOTH see the full ordered list, so the iteration order decides
  -- which segment gets picked when a critical cycle has equal
  -- outflow ties. Module deps' gap 2 is the canonical case: with e6
  -- (straight, layer-order 0) excluded and a Map-keyed seed (`$d:`
  -- sorts before `API`), markgraf flipped to splitting e7 instead of
  -- e5_2.
  buildSegments :: Array PortAssignment -> Array Segment
  buildSegments assigns = numberIds ordered
    where
    seeded = foldl addOne { entries: M.empty, order: [] } assigns
    ordered = A.mapMaybe (\k -> M.lookup k seeded.entries) seeded.order

    addOne acc a = do
      let key = segmentKey a
      let inX = trunkSourceX a
      let outX = trunkTargetX a
      case M.lookup key acc.entries of
        Nothing -> acc
          { entries = M.insert key
              { id: 0
              , members: [ a.edge.id ]
              , incoming: [ inX ]
              , outgoing: [ outX ]
              , slot: 0
              , mark: 0
              , splitBy: Nothing
              , splitPartner: Nothing
              }
              acc.entries
          , order = acc.order <> [ key ]
          }
        Just s -> acc
          { entries = M.insert key
              ( s
                  { members = s.members <> [ a.edge.id ]
                  , incoming = insertSorted inX s.incoming
                  , outgoing = insertSorted outX s.outgoing
                  }
              )
              acc.entries
          }

    numberIds segs = A.mapWithIndex (\i s -> s { id = i }) segs

  trunkSourceX a = fst a.fromPos
  trunkTargetX a = fst a.toPos

  segmentKey a = do
    let srcId = un NodeId a.edge.from.node
    let
      pidStr = case a.edge.from.port of
        Just pid -> un PortId pid
        Nothing -> "_auto_" <> un EdgeId a.edge.id
    srcId <> "|" <> pidStr

  -- ── Step 2: pairwise dependency calculation ─────────────────────

  addDependencies :: Number -> Array Segment -> { segments :: Array Segment, deps :: Array Dep }
  addDependencies critThreshold segs = { segments: segs, deps }
    where
    n = A.length segs
    deps = A.concatMap pairDeps (pairs n)
    pairs total = do
      i <- A.range 0 (total - 2)
      j <- A.range (i + 1) (total - 1)
      pure (i /\ j)

    pairDeps (i /\ j) = case A.index segs i /\ A.index segs j of
      Just a /\ Just b -> dependencyBetween critThreshold a b
      _ -> []

  -- Port of `OrthogonalRoutingGenerator.createDependencyIfNecessary`.
  -- Critical-conflict detection (ELK's CRITICAL_CONFLICTS_DETECTED = -1):
  -- when conflicts1 is "critical" we know one ordering would force
  -- overlap, so we issue a CRITICAL dep he2→he1 (he1 must NOT be left
  -- of he2). Same logic mirrors for conflicts2.
  dependencyBetween :: Number -> Segment -> Segment -> Array Dep
  dependencyBetween critThreshold he1 he2 =
    if isStraightSegment he1 || isStraightSegment he2 then []
    else do
      let cr1 = countConflictsCrit critThreshold he1.outgoing he2.incoming
      let cr2 = countConflictsCrit critThreshold he2.outgoing he1.incoming
      if cr1.critical || cr2.critical then
        (if cr1.critical then [ { src: he2.id, tgt: he1.id, weight: 1, kind: Critical } ] else [])
          <>
            (if cr2.critical then [ { src: he1.id, tgt: he2.id, weight: 1, kind: Critical } ] else [])
      else do
        let s1 = segStart he1
        let e1 = segEnd he1
        let s2 = segStart he2
        let e2 = segEnd he2
        let
          crossings1 = countCrossings he1.outgoing s2 e2
            + countCrossings he2.incoming s1 e1
        let
          crossings2 = countCrossings he2.outgoing s1 e1
            + countCrossings he1.incoming s2 e2
        let depValue1 = conflictPenalty * cr1.conflicts + crossingPenalty * crossings1
        let depValue2 = conflictPenalty * cr2.conflicts + crossingPenalty * crossings2
        if depValue1 < depValue2 then
          [ { src: he1.id, tgt: he2.id, weight: depValue2 - depValue1, kind: Regular } ]
        else if depValue1 > depValue2 then
          [ { src: he2.id, tgt: he1.id, weight: depValue1 - depValue2, kind: Regular } ]
        else if depValue1 > 0 then
          [ { src: he1.id, tgt: he2.id, weight: 0, kind: Regular }
          , { src: he2.id, tgt: he1.id, weight: 0, kind: Regular }
          ]
        else []

  -- Port of `OrthogonalRoutingGenerator.countConflicts` returning the
  -- regular conflict count plus a critical-detected flag.
  countConflictsCrit :: Number -> Array Number -> Array Number -> ConflictResult
  countConflictsCrit critThreshold posis1 posis2 =
    walk posis1 posis2 { conflicts: 0, critical: false }
    where
    walk a b acc
      | acc.critical = acc
      | otherwise = case A.uncons a /\ A.uncons b of
          Just { head: p1, tail: ar } /\ Just { head: p2, tail: br } ->
            let
              near c = p1 > p2 - c && p1 < p2 + c
              acc' =
                if near critThreshold then acc { critical = true }
                else if near conflictThreshold then acc { conflicts = acc.conflicts + 1 }
                else acc
            in
              if acc'.critical then acc'
              else if p1 <= p2 then walk ar b acc'
              else walk a br acc'
          _ -> acc

  countCrossings :: Array Number -> Number -> Number -> Int
  countCrossings posis start end =
    foldl (\acc p -> if p > end then acc else if p >= start then acc + 1 else acc) 0 posis

  -- ── Step 3a: critical cycle breaking (HyperEdgeSegmentSplitter) ──

  -- Run the Eades-Lin-Smyth heuristic restricted to CRITICAL deps,
  -- collect the leftward criticals as the "dependencies to resolve",
  -- and split the implicated segments at the best free-area position.
  -- After splitting, the dep graph for the new segments is regenerated
  -- so the regular-cycle pass sees a consistent state.
  breakCriticalCycles
    :: Number
    -> { segments :: Array Segment, deps :: Array Dep }
    -> { segments :: Array Segment, deps :: Array Dep }
  breakCriticalCycles critThreshold input = do
    let critDeps = A.filter (\d -> isCritical d.kind) input.deps
    if A.length critDeps < 2 then input
    else do
      let marked = computeMarks input.segments critDeps
      let markMap = M.fromFoldable (marked <#> \s -> s.id /\ s.mark)
      let leftward = A.filter (\d -> markOf markMap d.src > markOf markMap d.tgt) critDeps
      if A.null leftward then input
      else splitSegments critThreshold leftward input

  markOf :: Map Int Int -> Int -> Int
  markOf m k = fromMaybe 0 (M.lookup k m)

  -- Port of `HyperEdgeSegmentSplitter.splitSegments`.
  splitSegments
    :: Number
    -> Array Dep
    -> { segments :: Array Segment, deps :: Array Dep }
    -> { segments :: Array Segment, deps :: Array Dep }
  splitSegments critThreshold cycleDeps input = do
    let segMap0 = M.fromFoldable (input.segments <#> \s -> s.id /\ s)
    let decisions = decideWhichSegmentsToSplit cycleDeps segMap0
    let decided = decisions.decisions
    let segMap1 = decisions.segMap
    let freeAreas0 = findFreeAreas input.segments critThreshold
    let
      ordered = A.sortBy (\a b -> compare (segLength a) (segLength b))
        (A.mapMaybe (\sid -> M.lookup sid segMap1) decided)
    let nextId0 = (A.length input.segments)
    let
      result = foldl
        (\st seg -> splitOne critThreshold input.deps seg st)
        { segMap: segMap1, freeAreas: freeAreas0, nextId: nextId0 }
        ordered
    let allSegs = A.fromFoldable (M.values result.segMap)
    let regenerated = regenerateDeps critThreshold input.deps result.segMap
    { segments: allSegs, deps: regenerated }

  -- Port of `decideWhichSegmentsToSplit`. For each cycle dep choose the
  -- segment to split (prefer non-hyperedge); set splitBy on it.
  decideWhichSegmentsToSplit
    :: Array Dep
    -> Map Int Segment
    -> { decisions :: Array Int, segMap :: Map Int Segment }
  decideWhichSegmentsToSplit deps segMap0 =
    foldl step { decisions: [], segMap: segMap0 } deps
    where
    step acc dep =
      if A.elem dep.src acc.decisions || A.elem dep.tgt acc.decisions then acc
      else case M.lookup dep.src acc.segMap /\ M.lookup dep.tgt acc.segMap of
        Just src /\ Just tgt -> do
          let pickTarget = representsHyperedge src && not (representsHyperedge tgt)
          let toSplit = if pickTarget then tgt else src
          let causing = if pickTarget then src else tgt
          let updated = toSplit { splitBy = Just causing.id }
          { decisions: acc.decisions <> [ toSplit.id ]
          , segMap: M.insert toSplit.id updated acc.segMap
          }
        _ -> acc

  representsHyperedge :: Segment -> Boolean
  representsHyperedge s = A.length s.incoming + A.length s.outgoing > 2

  -- Splitter inner loop. Updates segMap (replacing the original with
  -- its split half plus inserting the new partner) and the freeAreas.
  splitOne
    :: Number
    -> Array Dep
    -> Segment
    -> { segMap :: Map Int Segment, freeAreas :: Array FreeArea, nextId :: Int }
    -> { segMap :: Map Int Segment, freeAreas :: Array FreeArea, nextId :: Int }
  splitOne critThreshold origDeps seg st = do
    let positionResult = computePositionToSplit seg origDeps st.segMap st.freeAreas critThreshold
    let splitPos = positionResult.position
    let partnerId = st.nextId
    let
      halfA = seg
        { outgoing = [ splitPos ]
        , splitPartner = Just partnerId
        }
    let halfARecomputed = recomputeExtent halfA
    let
      halfB =
        { id: partnerId
        , members: seg.members
        , incoming: [ splitPos ]
        , outgoing: seg.outgoing
        , slot: 0
        , mark: 0
        , splitBy: Nothing
        , splitPartner: Just seg.id
        }
    let halfBRecomputed = recomputeExtent halfB
    let
      segMap' = M.insert halfBRecomputed.id halfBRecomputed
        $ M.insert halfARecomputed.id halfARecomputed
        $ st.segMap
    { segMap: segMap'
    , freeAreas: positionResult.freeAreas
    , nextId: st.nextId + 1
    }

  recomputeExtent :: Segment -> Segment
  recomputeExtent s = s
    -- start/end are derived via segStart/segEnd; nothing stored to recompute.
    { incoming = A.sort s.incoming
    , outgoing = A.sort s.outgoing
    }

  -- Port of `computePositionToSplitAndUpdateFreeAreas`. Picks the best
  -- free area within the segment's [start..end] range using ELK's
  -- ranking: minimum crossings → minimum dependencies → maximum size,
  -- via simulateSplit + rateArea + isBetter. Falls back to the
  -- segment's centre when no candidate area is reachable.
  computePositionToSplit
    :: Segment
    -> Array Dep
    -> Map Int Segment
    -> Array FreeArea
    -> Number
    -> { position :: Number, freeAreas :: Array FreeArea }
  computePositionToSplit seg origDeps segMap freeAreas critThreshold = do
    let segStartC = segStart seg
    let segEndC = segEnd seg
    let
      possible = A.mapWithIndex (\i a -> { i, a }) freeAreas
        # A.filter (\r -> r.a.startPosition <= segEndC && r.a.endPosition >= segStartC)
    case possible of
      [] -> { position: (segStartC + segEndC) / 2.0, freeAreas }
      _ -> do
        let best = chooseBestArea seg origDeps segMap possible
        let chosen = best.a
        let position = (chosen.startPosition + chosen.endPosition) / 2.0
        let freeAreas' = useArea freeAreas best.i critThreshold
        { position, freeAreas: freeAreas' }

  -- Port of `chooseBestAreaIndex` + `rateArea` + `isBetter`. For each
  -- candidate area, simulate the split (the splitSegment retains the
  -- segment's incoming coords; the splitPartner retains the outgoing;
  -- both are linked at the area's centre) and rate the resulting
  -- crossings + dependencies against the segment's existing dep list.
  chooseBestArea
    :: Segment
    -> Array Dep
    -> Map Int Segment
    -> Array { i :: Int, a :: FreeArea }
    -> { i :: Int, a :: FreeArea }
  chooseBestArea seg origDeps segMap candidates = case A.head candidates of
    Nothing -> { i: 0, a: { startPosition: 0.0, endPosition: 0.0, size: 0.0 } }
    Just first
      | A.length candidates == 1 -> first
      | otherwise -> do
          let rated = candidates <#> \c -> { c, rating: rateArea seg origDeps segMap c.a }
          let
            best = foldl pickBetter
              ( fromMaybe { c: first, rating: dummyRating }
                  (A.head rated)
              )
              rated
          best.c
    where
    dummyRating = { crossings: 1000000, deps: 1000000 }

    pickBetter acc r =
      if isBetterArea r.c.a r.rating acc.c.a acc.rating then r else acc

  isBetterArea
    :: FreeArea
    -> { crossings :: Int, deps :: Int }
    -> FreeArea
    -> { crossings :: Int, deps :: Int }
    -> Boolean
  isBetterArea cur curR best bestR
    | curR.crossings < bestR.crossings = true
    | curR.crossings > bestR.crossings = false
    | curR.deps < bestR.deps = true
    | curR.deps > bestR.deps = false
    | otherwise = cur.size > best.size

  -- Port of `rateArea`. Simulates the split using the candidate area's
  -- centre as the bridge connection between the two halves, then walks
  -- every dep on the original segment counting how many crossings and
  -- ordering-conflicts each ordering would introduce.
  rateArea :: Segment -> Array Dep -> Map Int Segment -> FreeArea -> { crossings :: Int, deps :: Int }
  rateArea seg origDeps segMap area = do
    let centre = (area.startPosition + area.endPosition) / 2.0
    let splitSeg = seg { outgoing = [ centre ] }
    let splitPartner = seg { incoming = [ centre ] }
    let incoming = A.filter (\d -> d.tgt == seg.id) origDeps
    let outgoing = A.filter (\d -> d.src == seg.id) origDeps
    let acc0 = { crossings: 0, deps: 0 }
    let acc1 = foldl (rateAgainst segMap splitSeg splitPartner _.src) acc0 incoming
    let acc2 = foldl (rateAgainst segMap splitSeg splitPartner _.tgt) acc1 outgoing
    -- splitSegment → splitBy → splitPartner — fixed chain, two deps.
    case seg.splitBy >>= \sid -> M.lookup sid segMap of
      Just splitBy -> acc2
        { deps = acc2.deps + 2
        , crossings = acc2.crossings
            + countCrossingsBetween splitSeg splitBy
            + countCrossingsBetween splitBy splitPartner
        }
      Nothing -> acc2

  rateAgainst
    :: Map Int Segment
    -> Segment
    -> Segment
    -> (Dep -> Int)
    -> { crossings :: Int, deps :: Int }
    -> Dep
    -> { crossings :: Int, deps :: Int }
  rateAgainst segMap splitSeg splitPartner pickId acc dep = case M.lookup (pickId dep) segMap of
    Nothing -> acc
    Just other -> do
      let acc' = updateBothOrderings acc splitSeg other
      updateBothOrderings acc' splitPartner other

  -- Port of `updateConsideringBothOrderings`.
  updateBothOrderings
    :: { crossings :: Int, deps :: Int }
    -> Segment
    -> Segment
    -> { crossings :: Int, deps :: Int }
  updateBothOrderings acc s1 s2 = do
    let c12 = countCrossingsBetween s1 s2
    let c21 = countCrossingsBetween s2 s1
    if c12 == c21 then
      if c12 > 0 then acc { deps = acc.deps + 2, crossings = acc.crossings + c12 }
      else acc
    else acc
      { deps = acc.deps + 1
      , crossings = acc.crossings + min c12 c21
      }

  -- Port of `countCrossingsForSingleOrdering`.
  countCrossingsBetween :: Segment -> Segment -> Int
  countCrossingsBetween left right =
    countCrossings left.outgoing (segStart right) (segEnd right)
      + countCrossings right.incoming (segStart left) (segEnd left)

  -- Port of `useArea`: removes the consumed area and re-inserts the
  -- two pieces around the centre when each piece is large enough.
  useArea :: Array FreeArea -> Int -> Number -> Array FreeArea
  useArea freeAreas usedIndex critThreshold = case A.index freeAreas usedIndex of
    Nothing -> freeAreas
    Just oldArea -> do
      let withoutOld = fromMaybe freeAreas (A.deleteAt usedIndex freeAreas)
      if oldArea.size / 2.0 < critThreshold then withoutOld
      else do
        let oldCentre = (oldArea.startPosition + oldArea.endPosition) / 2.0
        let newEnd1 = oldCentre - critThreshold
        let newStart2 = oldCentre + critThreshold
        let
          part1 =
            if oldArea.startPosition <= newEnd1 then [ { startPosition: oldArea.startPosition, endPosition: newEnd1, size: newEnd1 - oldArea.startPosition } ]
            else []
        let
          part2 =
            if newStart2 <= oldArea.endPosition then [ { startPosition: newStart2, endPosition: oldArea.endPosition, size: oldArea.endPosition - newStart2 } ]
            else []
        insertManyAt usedIndex (part1 <> part2) withoutOld

  insertManyAt :: forall a. Int -> Array a -> Array a -> Array a
  insertManyAt idx items xs = A.take idx xs <> items <> A.drop idx xs

  -- Port of `findFreeAreas`: collect all in/out connection coords from
  -- every segment, sort, and produce the gaps that are at least
  -- twice the critical threshold wide.
  findFreeAreas :: Array Segment -> Number -> Array FreeArea
  findFreeAreas segs critThreshold = do
    let raw = (segs >>= \s -> s.incoming) <> (segs >>= \s -> s.outgoing)
    let sorted = A.sort raw
    A.zipWith pair sorted (A.drop 1 sorted)
      # A.mapMaybe identity
    where
    pair lo hi
      | hi - lo >= 2.0 * critThreshold = Just
          { startPosition: lo + critThreshold
          , endPosition: hi - critThreshold
          , size: (hi - lo) - 2.0 * critThreshold
          }
      | otherwise = Nothing

  -- Regenerate non-critical dependencies for the post-split segment
  -- set. Critical deps are kept as-is (the cycle-resolving criticals
  -- have been turned into actual splits + updateDependencies-style
  -- chains: split → splitBy → splitPartner — translated into ordering
  -- via the regular dep regeneration below).
  -- Mirrors ELK's `$updateDependencies`: after a split, the split
  -- segment and its partner share their split position as a connection
  -- coordinate, so a naive all-pairs `dependencyBetween` would flag a
  -- spurious critical conflict between them. ELK avoids this by only
  -- (a) adding the chain segment → splitBy → splitPartner and
  -- (b) recomputing regular deps between OTHER segments and each of
  -- (segment, splitPartner). We replicate that by skipping any pair
  -- where one is the other's `splitPartner`.
  regenerateDeps
    :: Number
    -> Array Dep
    -> Map Int Segment
    -> Array Dep
  regenerateDeps critThreshold _origDeps segMap = do
    let allSegs = A.fromFoldable (M.values segMap)
    let n = A.length allSegs
    let
      pairs = do
        i <- A.range 0 (n - 2)
        j <- A.range (i + 1) (n - 1)
        pure (i /\ j)
    let
      regular = pairs >>= \(i /\ j) -> case A.index allSegs i /\ A.index allSegs j of
        Just a /\ Just b
          | arePartners a b -> []
          | otherwise -> dependencyBetween critThreshold a b
        _ -> []
    let chainDeps = allSegs >>= chainCriticalsFor segMap
    regular <> chainDeps

  arePartners :: Segment -> Segment -> Boolean
  arePartners a b = a.splitPartner == Just b.id || b.splitPartner == Just a.id

  chainCriticalsFor :: Map Int Segment -> Segment -> Array Dep
  chainCriticalsFor segMap seg = case seg.splitBy /\ seg.splitPartner of
    Just causingId /\ Just partnerId
      | isJust (M.lookup partnerId segMap)
      , isJust (M.lookup causingId segMap) ->
          [ { src: seg.id, tgt: causingId, weight: 1, kind: Critical }
          , { src: causingId, tgt: partnerId, weight: 1, kind: Critical }
          ]
    _ -> []

  -- ── Step 3b: regular cycle breaking (Eades-Lin-Smyth FAS) ───────

  -- Computes a linear-ordering mark for each segment, then reverses or
  -- removes dependencies that point "leftward". Zero-weight deps in a
  -- two-cycle are removed; others are reversed. Critical deps are
  -- never reversed (the splitter handled their cycles already), only
  -- preserved in their original direction.
  breakNonCriticalCycles
    :: { segments :: Array Segment, deps :: Array Dep }
    -> { segments :: Array Segment, deps :: Array Dep }
  breakNonCriticalCycles input =
    { segments: marked
    , deps: A.mapMaybe rewrite input.deps
    }
    where
    marked = computeMarks input.segments input.deps
    markMap = M.fromFoldable (marked <#> \s -> s.id /\ s.mark)

    rewrite d
      | isCritical d.kind = Just d
      | markOf markMap d.src > markOf markMap d.tgt =
          if d.weight == 0 then Nothing
          else Just { src: d.tgt, tgt: d.src, weight: d.weight, kind: d.kind }
      | otherwise = Just d

  -- Linear-ordering marks via the Eades-Lin-Smyth feedback-arc heuristic.
  computeMarks :: Array Segment -> Array Dep -> Array Segment
  computeMarks segs deps = go initial
    where
    n = A.length segs
    markBase = n
    initial =
      { remaining: segs <#> \s -> s.id
      , marks: M.empty :: Map Int Int
      , inWeight: foldl (\m d -> M.insertWith (+) d.tgt d.weight m) M.empty deps
      , outWeight: foldl (\m d -> M.insertWith (+) d.src d.weight m) M.empty deps
      , depsBySrc: foldl (\m d -> M.insertWith (<>) d.src [ d ] m) M.empty deps
      , depsByTgt: foldl (\m d -> M.insertWith (<>) d.tgt [ d ] m) M.empty deps
      , nextSink: markBase - 1
      , nextSource: markBase + 1
      }

    weight m k = fromMaybe 0 (M.lookup k m)

    go st = case drainSinks st of
      st' -> case drainSources st' of
        st'' ->
          if A.null st''.remaining then finalize st''
          else go (pickMaxOutflow st'')

    drainSinks st = case A.find (\sid -> weight st.outWeight sid == 0) st.remaining of
      Nothing -> st
      Just sid ->
        drainSinks (removeSegment sid st.nextSink (\s -> s { nextSink = s.nextSink - 1 }) st)

    drainSources st = case A.find (\sid -> weight st.inWeight sid == 0) st.remaining of
      Nothing -> st
      Just sid ->
        drainSources (removeSegment sid st.nextSource (\s -> s { nextSource = s.nextSource + 1 }) st)

    pickMaxOutflow st = case A.head sorted of
      Nothing -> st
      Just sid ->
        removeSegment sid st.nextSource (\s -> s { nextSource = s.nextSource + 1 }) st
      where
      sorted = A.sortBy
        (\a b -> compare (outflow b) (outflow a))
        st.remaining
      outflow sid = weight st.outWeight sid - weight st.inWeight sid

    removeSegment sid mark advance st = do
      let outDeps = fromMaybe [] (M.lookup sid st.depsBySrc)
      let inDeps = fromMaybe [] (M.lookup sid st.depsByTgt)
      let inWeight' = foldl (\m d -> M.insertWith (+) d.tgt (-d.weight) m) st.inWeight outDeps
      let outWeight' = foldl (\m d -> M.insertWith (+) d.src (-d.weight) m) st.outWeight inDeps
      advance st
        { remaining = A.filter (_ /= sid) st.remaining
        , marks = M.insert sid mark st.marks
        , inWeight = inWeight'
        , outWeight = outWeight'
        }

    finalize st = segs <#> \seg -> do
      let raw = fromMaybe seg.id (M.lookup seg.id st.marks)
      let shifted = if raw < markBase then raw + n + 1 else raw
      seg { mark = shifted }

  -- ── Step 4: topological numbering ───────────────────────────────

  topologicalNumbering
    :: { segments :: Array Segment, deps :: Array Dep }
    -> Array Segment
  topologicalNumbering input = result
    where
    deps = input.deps
    initialInDegree = foldl (\m d -> M.insertWith (+) d.tgt 1 m) M.empty deps
    initial =
      { slots: M.fromFoldable (input.segments <#> \s -> s.id /\ 0)
      , inDegree: initialInDegree
      , adj: foldl (\m d -> M.insertWith (<>) d.src [ d.tgt ] m) M.empty deps
      , queue: input.segments
          # A.filter (\s -> 0 == fromMaybe 0 (M.lookup s.id initialInDegree))
          # map _.id
      }

    finalState = drain initial

    drain st = case A.uncons st.queue of
      Nothing -> st
      Just { head: nid, tail: rest } -> do
        let nslot = fromMaybe 0 (M.lookup nid st.slots)
        let outs = fromMaybe [] (M.lookup nid st.adj)
        let st' = foldl (advance nslot) (st { queue = rest }) outs
        drain st'

    advance srcSlot st tgtId = do
      let prevTgtSlot = fromMaybe 0 (M.lookup tgtId st.slots)
      let newSlot = max prevTgtSlot (srcSlot + 1)
      let prevIn = fromMaybe 0 (M.lookup tgtId st.inDegree)
      let nextIn = prevIn - 1
      let nextQueue = if nextIn == 0 then st.queue <> [ tgtId ] else st.queue
      st
        { slots = M.insert tgtId newSlot st.slots
        , inDegree = M.insert tgtId nextIn st.inDegree
        , queue = nextQueue
        }

    result = A.sortBy (\a b -> compare a.slot b.slot) (input.segments <#> assignSlot)
    assignSlot seg = seg { slot = fromMaybe 0 (M.lookup seg.id finalState.slots) }

  -- ── Helpers ─────────────────────────────────────────────────────

  segStart :: Segment -> Number
  segStart s = min (foldl min 1.0e18 s.incoming) (foldl min 1.0e18 s.outgoing)

  segEnd :: Segment -> Number
  segEnd s = max (foldl max (-1.0e18) s.incoming) (foldl max (-1.0e18) s.outgoing)

  segLength :: Segment -> Number
  segLength s = segEnd s - segStart s

  isStraightSegment s = segLength s < 1.0e-3

  insertSorted :: Number -> Array Number -> Array Number
  insertSorted v xs = A.takeWhile (_ < v) xs <> [ v ] <> A.dropWhile (_ <= v) xs

  -- Port of `minimumHorizontalSegmentDistance`: minimum gap between
  -- distinct sorted in/out coordinates across all segments. When fewer
  -- than two distinct positions exist, fall back to the regular
  -- conflict threshold so the critical threshold doesn't trigger
  -- spuriously. Coordinates that differ by less than `epsilon` are
  -- treated as duplicates: `gridX * sf` round-trips through divisions
  -- accumulate ~1e-14 rounding, and ELK's exact-equality `distinct`
  -- never sees the noise because its computation paths happen to land
  -- on the same `Double`. Treating those near-duplicates as one keeps
  -- `criticalConflictThreshold` realistic instead of collapsing to ~0.
  minimumHorizontalSegmentDistance :: Array Segment -> Number
  minimumHorizontalSegmentDistance segs = do
    let raw = (segs >>= \s -> s.incoming) <> (segs >>= \s -> s.outgoing)
    let sorted = nubNear epsilon (A.sort raw)
    if A.length sorted < 2 then conflictThreshold
    else (foldl scan { prev: Nothing, mn: 1.0e18 } sorted).mn
    where
    epsilon = 1.0e-9
    scan acc x = case acc.prev of
      Nothing -> { prev: Just x, mn: acc.mn }
      Just p -> { prev: Just x, mn: min acc.mn (x - p) }

  nubNear :: Number -> Array Number -> Array Number
  nubNear eps xs = (foldl step { prev: Nothing, out: [] } xs).out
    where
    step acc x = case acc.prev of
      Just p | x - p < eps -> acc
      _ -> { prev: Just x, out: acc.out <> [ x ] }

  -- Bounds of the inter-layer gap.
  gapBounds segs = do
    let memberIds = A.concatMap _.members segs
    let memberAssigns = A.filter (\a -> A.elem a.edge.id memberIds) assignments
    let srcYs = memberAssigns <#> \a -> snd a.fromPos
    let tgtYs = memberAssigns <#> \a -> snd a.toPos
    let top = foldl max (-1.0e18) srcYs
    let bot = foldl min 1.0e18 tgtYs
    if top > bot then { gapTop: bot, gapBottom: top }
    else { gapTop: top, gapBottom: bot }

type FreeArea =
  { startPosition :: Number
  , endPosition :: Number
  , size :: Number
  }
