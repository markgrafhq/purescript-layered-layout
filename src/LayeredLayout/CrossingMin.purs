module LayeredLayout.CrossingMin
  ( minimize
  , countCrossings
  , countAllCrossings
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl, sum)
import Data.Int as Int
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as S
import Data.Tuple.Nested (type (/\), (/\))
import LayeredLayout.Graph (Constraints(..), Edge, EdgeId, NodeId)
import LayeredLayout.JavaRandom (Random)
import LayeredLayout.JavaRandom as JR

type Config =
  { iterations :: Int
  , constraints :: Array Constraints
  , modelOrder :: Map NodeId Int
  }

-- | Port of `LayerSweepCrossingMinimizer.process()` from
-- | `org.eclipse.elk.alg.layered.p3order.LayerSweepCrossingMinimizer`.
-- |
-- | Multi-run randomized layer sweep:
-- |   1. `compareDifferentRandomizedLayouts` runs THOROUGHNESS iterations
-- |   2. Each iteration calls `minimizeCrossingsWithCounter` which:
-- |        * picks a random sweep direction via `random.nextBoolean()`
-- |        * randomizes the first-layer barycenters (`setFirstLayerOrder`)
-- |        * sweeps until no further crossing reduction (alternating direction)
-- |   3. The layout with the fewest total crossings is kept (best-of-N).
-- |
-- | The single shared `Random` instance threads through all iterations,
-- | matching ELK's `random = rootGraph.getProperty(InternalProperties.RANDOM)`
-- | and the `random.setSeed(randomSeed)` reset that is performed once at the
-- | start of `compareDifferentRandomizedLayouts`.
minimize :: Config -> Array (Array NodeId) -> Array Edge -> Array (Array NodeId)
minimize cfg layers edges =
  if A.length layers <= 0 || cfg.iterations <= 0 then layers else best.layout
  where
  thoroughness = cfg.iterations
  orderConstraintss = orderConstraintsOf cfg.constraints

  -- markgraf deviation from ELK: only shuffle connected nodes during
  -- randomization. Disconnected nodes have no influence on crossings, and
  -- shuffling them would disturb their layout (e.g. for locked-but-disconnected
  -- nodes that should not perturb the connected subgraph).
  connectedNodes = foldl
    (\s e -> S.insert (e.from.node) (S.insert (e.to.node) s))
    S.empty
    edges

  initialRandom = afterReset
    where
    -- ELK: random = new Random(seed); randomSeed = random.nextLong();
    -- compareDifferentRandomizedLayouts: random.setSeed(randomSeed).
    -- setSeed(s) and `new Random(s)` both apply (s XOR multiplier) AND mask48,
    -- so `mkRandomBI seed` reproduces `setSeed(seed)`.
    --
    -- BigInt-precision is required: Random(1).nextLong() = -4964420948893066024
    -- exceeds Number's 53-bit mantissa, so a Number-typed nextLong silently
    -- drops low-order bits, the subsequent setSeed lands on a different
    -- state from ELK, and every following `next 24` jitter call produces
    -- different bits — flipping tie-break sort decisions (Cycle (loop)
    -- L2 forward sweep was the smoking gun).
    rootRandom = JR.mkRandom 1.0
    randomSeed /\ _ = JR.nextLongBI rootRandom
    afterReset = JR.mkRandomBI randomSeed

  -- ELK `$compareDifferentRandomizedLayouts` (line 51182) sets
  -- FIRST_TRY_WITH_INITIAL_ORDER=true at the start when strategy != NONE.
  -- The flag tracks across iterations and overrides the random sweep
  -- direction inside `$minimizeCrossingsWithCounter` (line 51365):
  -- iter 0 (FIRST_TRY=true) → forward sweep; iter 1 (SECOND_TRY=true,
  -- FIRST_TRY=false) → backward; iter 2+ honour the random bit.
  -- The random bit is still consumed at line 51360 even when
  -- overridden, so RNG advancement matches across implementations.
  initialFirstTry = not (M.isEmpty cfg.modelOrder)

  best = (foldl runIteration seed (A.range 1 thoroughness)).result
    where
    seed =
      { result: { layout: layers, crossings: top, random: initialRandom }
      , firstTry: initialFirstTry
      , secondTry: false
      }
    top = 1000000000 -- ELK: Integer.MAX_VALUE; any concrete sentinel suffices.

  runIteration acc _
    | acc.result.crossings == 0 = acc -- ELK early-exit: bestCrossings == 0 ⇒ break
    | otherwise = do
        let res = minimizeCrossingsWithCounter acc.result.random acc.firstTry acc.secondTry
        let
          nextResult =
            if res.crossings < acc.result.crossings then { layout: res.layout, crossings: res.crossings, random: res.random }
            else acc.result { random = res.random }
        let secondAfter = if acc.secondTry then false else acc.secondTry
        let
          firstAfter /\ secondAfter' =
            if acc.firstTry then false /\ true else acc.firstTry /\ secondAfter
        { result: nextResult, firstTry: firstAfter, secondTry: secondAfter' }

  -- ELK: minimizeCrossingsWithCounter.
  -- 1. forward sweep direction from random.nextBoolean()
  -- 2. setFirstLayerOrder (randomize barycenters of the first layer)
  -- 3. sweepReducingCrossings (firstSweep=true)
  -- 4. while (oldCrossings > newCrossings): flip direction, sweep, recount.
  minimizeCrossingsWithCounter :: Random -> Boolean -> Boolean -> { layout :: Array (Array NodeId), crossings :: Int, random :: Random }
  minimizeCrossingsWithCounter r0 firstTry secondTry = do
    let randomBit /\ r1 = JR.nextBoolean r0
    -- ELK line 51365: when FIRST_TRY or SECOND_TRY is set AND strategy
    -- != NONE, the random sweep direction is REPLACED with FIRST_TRY's
    -- boolean value. The random bit is still consumed (line 51360) so
    -- the RNG sequence advances identically across iterations.
    let strategyIsNone = M.isEmpty cfg.modelOrder
    let useOverride = (firstTry || secondTry) && not strategyIsNone
    let isForwardSweep = if useOverride then firstTry else randomBit
    -- When forceNodeModelOrder is on (cfg.modelOrder populated) ELK
    -- skips setFirstLayerOrder for iters 0/1 (FIRST_TRY/SECOND_TRY take
    -- the right branch at line 51365). For iter 2+ it calls
    -- setFirstLayerOrder which randomises the first layer. We
    -- approximate that here by always skipping when modelOrder is set,
    -- matching iter 0/1 exactly; iter 2+ behaviour is deferred work.
    let
      randomized /\ r2 =
        if not strategyIsNone then layers /\ r1
        else setFirstLayerOrder isForwardSweep layers r1
    let firstSwept /\ r3 = sweepReducingCrossings randomized isForwardSweep r2
    let initialCross = countAll firstSwept
    converge firstSwept (not isForwardSweep) initialCross r3
    where
    converge cur dir oldCross r =
      if oldCross == 0 then { layout: cur, crossings: 0, random: r }
      else do
        let cur' /\ r' = sweepReducingCrossings cur dir r
        let newCross = countAll cur'
        if newCross < oldCross then converge cur' (not dir) newCross r'
        else { layout: cur, crossings: oldCross, random: r' }

  -- ELK: BarycenterHeuristic.setFirstLayerOrder. Randomizes the first layer's
  -- barycenters via `random.nextDouble()` per node, then sorts by barycenter.
  -- markgraf deviation: only the connected nodes are shuffled; disconnected
  -- nodes keep their original positions.
  setFirstLayerOrder :: Boolean -> Array (Array NodeId) -> Random -> Array (Array NodeId) /\ Random
  setFirstLayerOrder isForwardSweep ls r0 =
    case A.index ls startIdx of
      Just first | A.length first > 1 -> do
        let connected = A.filter isConnected first
        if A.length connected > 1 then do
          let shuffled /\ r1 = JR.randomShuffle r0 connected
          let reassembled = mergeBack first shuffled
          let withOrder = enforceOrder reassembled
          fromMaybe (ls /\ r0) (A.updateAt startIdx withOrder ls <#> (_ /\ r1))
        else ls /\ r0
      _ -> ls /\ r0
    where
    startIdx = if isForwardSweep then 0 else max 0 (A.length ls - 1)
    isConnected n = S.member n connectedNodes
    mergeBack original shuffled = _.result $ foldl step { idx: 0, result: [] } original
      where
      step { idx, result } n =
        if not (isConnected n) then { idx, result: result <> [ n ] }
        else case A.index shuffled idx of
          Just s -> { idx: idx + 1, result: result <> [ s ] }
          Nothing -> { idx, result: result <> [ n ] }

  -- ELK: sweepReducingCrossings walks layers in the sweep direction, sorting
  -- each free layer by barycenter relative to the already-fixed reference layer.
  -- The Random thread runs through every layer's `sortByBarycenter` so the
  -- per-node jitter (port of ELK `BarycenterHeuristic.calculateBarycenter`'s
  -- `summedWeight += nextDouble * 0.07 - 0.035`) consumes random bits in
  -- ELK's order.
  sweepReducingCrossings :: Array (Array NodeId) -> Boolean -> Random -> Array (Array NodeId) /\ Random
  sweepReducingCrossings ls forward r0 = foldl step (ls /\ r0) indices
    where
    n = A.length ls
    indices =
      if forward then A.range 1 (n - 1)
      else A.reverse (A.range 0 (n - 2))

    step (acc /\ r) i = fromMaybe (acc /\ r) do
      let refIdx = if forward then i - 1 else i + 1
      refLayer <- A.index acc refIdx
      curLayer <- A.index acc i
      let sorted /\ r' = sortByBarycenter curLayer refLayer forward r
      let switched = greedySwitch sorted refLayer edges orderConstraintss
      acc' <- A.updateAt i switched acc
      pure (acc' /\ r')

  -- Pre-computed once: input-order index of each segment edge in the edges array.
  -- Used as the sort key for EAST output ports (matches ELK's
  -- ModelOrderPortComparator behaviour where forward and reversed edges share
  -- the same comparable space — edge model order — when there is at most one
  -- edge per source-target pair).
  edgeIdx :: Map EdgeId Int
  edgeIdx = M.fromFoldable (A.mapWithIndex (\i e -> e.id /\ i) edges)

  sortByBarycenter :: Array NodeId -> Array NodeId -> Boolean -> Random -> Array NodeId /\ Random
  sortByBarycenter curLayer refLayer forward r0 = enforceOrder sorted /\ rFinal
    where
    refPos = M.fromFoldable (A.mapWithIndex (\i n -> n /\ i) refLayer)
    freePos = M.fromFoldable (A.mapWithIndex (\i n -> n /\ i) curLayer)

    ranks :: Map EdgeId Number
    ranks =
      if forward then outputRanks refLayer refPos freePos edges edgeIdx
      else inputRanks refLayer refPos freePos edges edgeIdx

    -- Port of ELK `BarycenterHeuristic.calculateBarycenters`: walks nodes in
    -- layer order and consumes one `next 24` per node WITH neighbours.
    -- The jitter is added to the summed weight before division by degree so
    -- the effective barycenter shifts by ±0.035/degree per call. Matches
    -- elkjs line 50369: `summedWeight += nextInternal(24) / 2^24 * 0.07 - 0.035`.
    barysAndRandom = foldl stepBary { items: [], r: r0 } (A.mapWithIndex (/\) curLayer)
    stepBary acc (i /\ n) = do
      let
        connecting =
          if forward then A.filter
            ( \e -> e.to.node == n
                && M.member (e.from.node) refPos
            )
            edges
          else A.filter
            ( \e -> e.from.node == n
                && M.member (e.to.node) refPos
            )
            edges
      let rs = A.mapMaybe (\e -> M.lookup e.id ranks) connecting
      if A.null rs then acc { items = acc.items <> [ { n, key: Nothing, origIdx: i } ] }
      else do
        let bits /\ r' = JR.next 24 acc.r
        -- ELK line 50369 uses Java float constants `0.07f`/`0.035f`
        -- which promote to the doubles `0.07000000029802322` /
        -- `0.03500000014901161`. Using ordinary `0.07`/`0.035` doubles
        -- introduces a 1e-10 jitter delta that flips exact-tie sort
        -- decisions. intern_81 = 1/2^24 = 5.9604644775390625E-8.
        let jitter = Int.toNumber bits * 5.9604644775390625e-8 * 0.07000000029802322 - 0.03500000014901161
        let key = (sum rs + jitter) / toNumber (A.length rs)
        { items: acc.items <> [ { n, key: Just key, origIdx: i } ], r: r' }

    raw = barysAndRandom.items
    rFinal = barysAndRandom.r
    filled = fillInUnknownBarycenters raw

    -- ELK's plain `BarycenterHeuristic` sort: a pure-barycenter compare
    -- under GWT's `Collections.sort` (stable merge sort). Model order is
    -- not a tiebreak here — it is established earlier by layer seeding.
    sorted = collectionsSortBarycenter filled <#> _.n

  -- Port of `BarycenterHeuristic.fillInUnknownBarycenters` (preOrdered branch).
  -- Walks the layer in current order; for each node whose barycenter is
  -- undefined, assigns (lastDefined + nextDefined) / 2, where nextDefined
  -- is the next node with a defined barycenter (or lastDefined+1 if none).
  -- Without this, nodes with no neighbours in the reference layer would all
  -- get barycenter 0 and clump at the front.
  fillInUnknownBarycenters
    :: Array { n :: NodeId, key :: Maybe Number, origIdx :: Int }
    -> Array { n :: NodeId, key :: Number, origIdx :: Int }
  fillInUnknownBarycenters nodes = walk 0 (-1.0) []
    where
    walk i lastValue acc = case A.index nodes i of
      Nothing -> acc
      Just node -> case node.key of
        Just k ->
          walk (i + 1) k (acc <> [ { n: node.n, key: k, origIdx: node.origIdx } ])
        Nothing -> do
          let nextV = nextDefined (i + 1) (lastValue + 1.0)
          let v = (lastValue + nextV) / 2.0
          walk (i + 1) v (acc <> [ { n: node.n, key: v, origIdx: node.origIdx } ])

    nextDefined startIdx fallback = case A.index nodes startIdx of
      Nothing -> fallback
      Just node -> case node.key of
        Just k -> k
        Nothing -> nextDefined (startIdx + 1) fallback

  enforceOrder :: Array NodeId -> Array NodeId
  enforceOrder layer = foldl applyOne layer orderConstraintss
    where
    applyOne arr { before, after } = case A.elemIndex before arr /\ A.elemIndex after arr of
      Just bi /\ Just ai | bi > ai -> do
        let without = fromMaybe arr (A.deleteAt bi arr)
        fromMaybe without (A.insertAt ai before without)
      _ -> arr

  countAll :: Array (Array NodeId) -> Int
  countAll ls = countAllCrossings ls edges

-- ── crossing-minimization sort ────────────────────────────────────
-- markgraf ships ELK's soft `considerModelOrder` (d2's default). ELK then
-- uses the plain `BarycenterHeuristic` (LayerSweepGraphOrderingProcessor):
-- a stateless pure-barycenter compare under `Collections.sort` (a stable
-- merge sort). Model order is NOT a sort tiebreak; it is established
-- earlier by `SortByInputModelProcessor` seeding the layer in declaration
-- order. (forceNodeModelOrder / the stateful `ModelOrderBarycenterHeuristic`
-- is not implemented — markgraf never pins authored order.)

-- | A single node awaiting placement in its layer, with its (already
-- | filled-in) barycenter value.
type BaryNode = { n :: NodeId, key :: Number, origIdx :: Int }

-- | Port of the plain `BarycenterHeuristic` soft sort: `Collections.sort`
-- | over the STATELESS pure-barycenter comparator. The comparator is
-- | `BarycenterState.barycenter.compareTo` (both values are defined after
-- | `fillInUnknownBarycenters`), so this reduces to comparing the filled-in
-- | keys. The sort itself reproduces the exact stable merge sort GWT compiles
-- | `Collections.sort` to (elk-worker.js `mergeSort_0`): runs shorter than 7
-- | use a swap-based insertion sort, larger runs split at the midpoint,
-- | recurse, and merge with a `<= 0` stability bias. Model order is absent
-- | here — ELK enforces it upstream, never as a barycenter tiebreak.
collectionsSortBarycenter :: Array BaryNode -> Array BaryNode
collectionsSortBarycenter = mergeSort
  where
  cmp a b = compare a.key b.key

  mergeSort arr
    | A.length arr < 7 = insertionRun arr
    | otherwise = do
        let mid = A.length arr / 2
        let left = mergeSort (A.slice 0 mid arr)
        let right = mergeSort (A.slice mid (A.length arr) arr)
        merge left right

  -- GWT `insertionSort` (the run-level one): swap-based, walks each element
  -- left while the predecessor compares greater.
  insertionRun arr0 = foldl outer arr0 (A.range 1 (A.length arr0 - 1))
    where
    outer arr i = inner arr i
    inner arr j = case A.index arr (j - 1) /\ A.index arr j of
      Just prev /\ Just here | j > 0 ->
        case cmp prev here of
          GT -> case swap (j - 1) j arr of
            Just arr' -> inner arr' (j - 1)
            Nothing -> arr
          _ -> arr
      _ -> arr

  -- GWT `merge`: stable two-way merge, taking the left element while
  -- `compare(left, right) <= 0`.
  merge left right = go [] 0 0
    where
    go acc i j = case A.index left i /\ A.index right j of
      Just l /\ Just rr ->
        case cmp l rr of
          GT -> go (A.snoc acc rr) i (j + 1)
          _ -> go (A.snoc acc l) (i + 1) j
      Just _ /\ Nothing -> acc <> A.drop i left
      Nothing /\ _ -> acc <> A.drop j right

swap :: forall a. Int -> Int -> Array a -> Maybe (Array a)
swap i j arr = do
  vi <- A.index arr i
  vj <- A.index arr j
  arr' <- A.updateAt i vj arr
  A.updateAt j vi arr'

-- | Port of `LayerTotalPortDistributor.calculatePortRanks` for OUTPUT ports
-- | (elk-worker.js:51770).
-- |
-- | Walks ref-layer nodes in position order, accumulating `consumedRank`
-- | across the layer. Within each node, output edges are sorted by edge
-- | model order; the j-th edge gets integer rank `consumedRank + (j+1)`.
-- | After processing a node, `consumedRank += k`. Edges to/from other
-- | layers are excluded.
-- |
-- | ELK chooses between this and `NodeRelativePortDistributor` (fractional
-- | ranks) via a single RNG bit consumed during `GraphInfoHolder`
-- | construction (`create_14`, elk-worker.js:50103). For seed=1 the bit
-- | lands on 0 → LayerTotal. We hardcode LayerTotal because that's what
-- | ELK picks under our fixed seed configuration.
outputRanks
  :: Array NodeId
  -> Map NodeId Int
  -> Map NodeId Int
  -> Array Edge
  -> Map EdgeId Int
  -> Map EdgeId Number
outputRanks refLayer _refPos freePos edges edgeIdx =
  M.fromFoldable (foldl perNode { ranks: [], rankSum: 0 } refLayer).ranks
  where
  perNode acc nodeId = do
    let outE = A.filter inFree (A.filter (originatesAt nodeId) edges)
    let sorted = A.sortBy (\a b -> compare (keyOf a) (keyOf b)) outE
    let k = A.length sorted
    let
      newRanks = A.mapWithIndex
        (\j e -> e.id /\ toNumber (acc.rankSum + j + 1))
        sorted
    { ranks: acc.ranks <> newRanks, rankSum: acc.rankSum + k }

  inFree e = M.member (e.to.node) freePos
  originatesAt n e = e.from.node == n
  keyOf e = fromMaybe 1000000 (M.lookup e.id edgeIdx)

-- | Port of `LayerTotalPortDistributor.calculatePortRanks` for INPUT ports
-- | (elk-worker.js:51743).
-- |
-- | Walks ref-layer nodes in position order, accumulating `consumedRank`.
-- | Within each node, input edges are iterated in west-side CCW order
-- | (top-to-bottom = source-position descending in our flat-port case);
-- | the first iterated port (top, from highest-positioned source) gets
-- | rank `consumedRank + inputCount`, the next gets `consumedRank +
-- | inputCount - 1`, …, the last gets `consumedRank + 1`. After the
-- | node, `consumedRank += inputCount`.
inputRanks
  :: Array NodeId
  -> Map NodeId Int
  -> Map NodeId Int
  -> Array Edge
  -> Map EdgeId Int
  -> Map EdgeId Number
inputRanks refLayer _refPos freePos edges edgeIdx =
  M.fromFoldable (foldl perNode { ranks: [], rankSum: 0 } refLayer).ranks
  where
  perNode acc nodeId = do
    let inE = A.filter inFree (A.filter (terminatesAt nodeId) edges)
    let sorted = A.sortBy compareDesc inE
    let k = A.length sorted
    let
      newRanks = A.mapWithIndex
        (\j e -> e.id /\ toNumber (acc.rankSum + k - j))
        sorted
    { ranks: acc.ranks <> newRanks, rankSum: acc.rankSum + k }

  inFree e = M.member (e.from.node) freePos
  terminatesAt n e = e.to.node == n
  sourcePosOf e = fromMaybe (-1) (M.lookup (e.from.node) freePos)
  keyOf e = fromMaybe 1000000 (M.lookup e.id edgeIdx)
  compareDesc a b = case compare (sourcePosOf b) (sourcePosOf a) of
    EQ -> compare (keyOf a) (keyOf b)
    other -> other

orderConstraintsOf :: Array Constraints -> Array { before :: NodeId, after :: NodeId }
orderConstraintsOf = A.mapMaybe case _ of
  OrderConstraint r -> Just { before: r.before, after: r.after }
  _ -> Nothing

-- | Total crossings across all adjacent layer pairs.
-- | ELK: GraphInfoHolder.crossCounter().countAllCrossings(currentNodeOrder).
countAllCrossings :: Array (Array NodeId) -> Array Edge -> Int
countAllCrossings layers edges = foldl addPair 0 (A.range 0 (A.length layers - 2))
  where
  addPair total i = fromMaybe total do
    a <- A.index layers i
    b <- A.index layers (i + 1)
    pure (total + countCrossings a b edges)

countCrossings :: Array NodeId -> Array NodeId -> Array Edge -> Int
countCrossings layer1 layer2 edges = do
  let posA = M.fromFoldable (A.mapWithIndex (\i n -> n /\ i) layer1)
  let posB = M.fromFoldable (A.mapWithIndex (\i n -> n /\ i) layer2)
  let
    relevant = A.mapMaybe
      ( \e ->
          case M.lookup (e.from.node) posA /\ M.lookup (e.to.node) posB of
            Just u /\ Just v -> Just (u /\ v)
            _ -> case M.lookup (e.from.node) posB /\ M.lookup (e.to.node) posA of
              Just v /\ Just u -> Just (u /\ v)
              _ -> Nothing
      )
      edges
  countPairs relevant
  where
  countPairs pairs = do
    let n = A.length pairs
    foldl
      ( \acc i ->
          foldl
            ( \acc2 j -> case A.index pairs i /\ A.index pairs j of
                Just (u1 /\ v1) /\ Just (u2 /\ v2) ->
                  if (u1 - u2) * (v1 - v2) < 0 then acc2 + 1 else acc2
                _ -> acc2
            )
            acc
            (A.range (i + 1) (n - 1))
      )
      0
      (A.range 0 (n - 2))

-- | Port of `GreedySwitchHeuristic` (ONE_SIDED) from
-- | `intermediate.greedyswitch.GreedySwitchHeuristic.java`.
-- |
-- | `sweepDownwardInLayer`: walks adjacent pairs (i, i+1) of the free layer
-- | and switches if doing so reduces crossings against the fixed reference
-- | layer. `continueSwitchingUntilNoImprovementInLayer` repeats sweeps until
-- | a full pass makes no swap.
-- |
-- | Used here as a refinement applied after the barycenter sort within each
-- | layer step, matching the typical post-barycenter cleanup pattern. ELK
-- | proper invokes ONE_SIDED greedy switch as an alternative crossMinType in
-- | `LayerSweepCrossingMinimizer`; combining it with barycenter is strictly
-- | non-worsening since switches are gated on a reduced crossing count.
greedySwitch
  :: Array NodeId
  -> Array NodeId
  -> Array Edge
  -> Array { before :: NodeId, after :: NodeId }
  -> Array NodeId
greedySwitch layer refLayer edges orderConstraintss = continueSwitching layer
  where
  continueSwitching cur = do
    let next = sweepDownward cur 0
    if next == cur then cur
    else continueSwitching next

  sweepDownward cur upperIdx
    | upperIdx >= A.length cur - 1 = cur
    | otherwise = case A.index cur upperIdx /\ A.index cur (upperIdx + 1) of
        Just a /\ Just b ->
          if violatesOrder a b then sweepDownward cur (upperIdx + 1)
          else do
            let
              swapped = fromMaybe cur do
                s1 <- A.updateAt upperIdx b cur
                A.updateAt (upperIdx + 1) a s1
            if doesSwitchReduceCrossings cur swapped then sweepDownward swapped (upperIdx + 1)
            else sweepDownward cur (upperIdx + 1)
        _ -> cur

  doesSwitchReduceCrossings cur swapped =
    countCrossings refLayer swapped edges < countCrossings refLayer cur edges

  violatesOrder before after = A.any (\c -> c.before == before && c.after == after) orderConstraintss

