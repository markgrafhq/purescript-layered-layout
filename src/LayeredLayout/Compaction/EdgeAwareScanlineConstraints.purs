-- | Port of ELK's `ScanlineConstraintCalculator` +
-- | `EdgeAwareScanlineConstraintCalculation`. See
-- | `EdgeAwareScanlineConstraintsSpec` for the worked examples.
module LayeredLayout.Compaction.EdgeAwareScanlineConstraints
  ( edgeAwareScanlineConstraints
  , scanlineConstraints
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import LayeredLayout.Compaction.OneD
  ( CGraph
  , CNode
  , CNodeId
  , IConstraintCalculationAlgorithm(..)
  , allCGroups
  , allCNodes
  , lookupCNode
  , updateCNode
  )

----------------------------------------------------------------
-- Public algorithms
----------------------------------------------------------------

-- | Pure base scanline: one sweep over every CNode, no hitbox inflation.
scanlineConstraints :: forall a. IConstraintCalculationAlgorithm a
scanlineConstraints = IConstraintCalculationAlgorithm \st ->
  sweep (\_ -> true) (clearConstraints st.cGraph)

-- | Edge-aware scanline for the orthogonal edge-routing case. Three
-- | phases of inflate → sweep → restore, accumulating constraints in
-- | the original (un-inflated) graph. The `edgeEdgeSpacing` is the same
-- | value the compactor's spacings handler returns for an edge/edge
-- | pair — both read it from the layout config so they never diverge.
edgeAwareScanlineConstraints :: forall a. Number -> IConstraintCalculationAlgorithm a
edgeAwareScanlineConstraints edgeEdgeSpacing = IConstraintCalculationAlgorithm \st ->
  edgeAwareOrthogonal edgeEdgeSpacing (clearConstraints st.cGraph)

clearConstraints :: forall a. CGraph a -> CGraph a
clearConstraints g = foldl reset g (allCNodes g)
  where
  reset acc n = updateCNode n.id (_ { constraints = [] }) acc

----------------------------------------------------------------
-- Orthogonal driver: three phases of inflate → sweep → restore.
--   1. VS-only sweep with VSes inflated by edgeEdge/2 − ε
--   2. LNode-only sweep with LNodes inflated by edgeEdge/2 − ε
--   3. full sweep with group masters + member VSes inflated
----------------------------------------------------------------

edgeAwareOrthogonal :: forall a. Number -> CGraph a -> CGraph a
edgeAwareOrthogonal edgeEdgeSpacing g0 = do
  let inflate = inflateBy edgeEdgeSpacing
  let g1 = sweepInto g0 isVS (inflateAll inflate g0 isVS)
  let g2 = sweepInto g1 isLNode (inflateAll inflate g1 isLNode)
  sweepInto g2 (\_ -> true) (inflateGroups inflate g2)

-- | Run a sweep over the *inflated* graph and merge the constraints it
-- | discovers into `base`, whose un-inflated geometry is preserved.
-- | This is the functional analogue of ELK's blow-up → sweep →
-- | normalize cycle: inflation is local to one sweep and never leaks
-- | into the next phase or into the final hitboxes.
sweepInto
  :: forall a
   . CGraph a
  -> (CNode a -> Boolean)
  -> CGraph a
  -> CGraph a
sweepInto base filt inflated = applyConstraints (sweepConstraints filt inflated) base

isVS :: forall a. CNode a -> Boolean
isVS n = n.kind == Just "vs"

isLNode :: forall a. CNode a -> Boolean
isLNode n = not (isVS n)

----------------------------------------------------------------
-- Hitbox inflation
----------------------------------------------------------------

-- | Hitbox inflation for one sweep. Both the VS sweep and the LNode
-- | sweep inflate by *edge-edge* spacing (not node-node) so nodes pack
-- | as tightly as the edges allow. The `edgeEdgeSpacing` is ELK's
-- | `verticalEdgeEdgeSpacing` (`LayeredOptions.SPACING_EDGE_EDGE`,
-- | default 10), threaded in from the layout config so it stays in
-- | lock-step with the value the compactor's spacings handler hands
-- | back for an edge/edge pair. It is **not** the node grid
-- | `scaleFactor`: the scanline inflates hitboxes by
-- | `edgeEdgeSpacing / 2 - epsilon`, so using the grid factor here
-- | under-inflates nodes and drops the node↔long-edge separation
-- | constraints ELK generates.
inflateBy :: Number -> Number
inflateBy edgeEdgeSpacing = max 0.0 (edgeEdgeSpacing / 2.0 - epsilon)

epsilon :: Number
epsilon = 0.5

smallEpsilon :: Number
smallEpsilon = 0.01

-- | Phase 1/2 helper: inflate every CNode that satisfies `filt`. VSes
-- | grow on the side(s) their `ignoreSpacing` flags don't veto; LNodes
-- | grow on both sides by the full spacing.
inflateAll
  :: forall a
   . Number
  -> CGraph a
  -> (CNode a -> Boolean)
  -> CGraph a
inflateAll spacing g filt = foldl step g (allCNodes g)
  where
  step acc n
    | filt n = updateCNode n.id (alterHitbox spacing) acc
    | otherwise = acc

alterHitbox :: forall a. Number -> CNode a -> CNode a
alterHitbox spacing n
  | isVS n =
      if not n.ignoreSpacing.up then
        n
          { hitbox = n.hitbox
              { y = n.hitbox.y - spacing - smallEpsilon
              , height = n.hitbox.height + spacing + smallEpsilon
              }
          }
      else if not n.ignoreSpacing.down then
        n { hitbox = n.hitbox { height = n.hitbox.height + spacing + smallEpsilon } }
      else n
  | otherwise =
      n
        { hitbox = n.hitbox
            { y = n.hitbox.y - spacing
            , height = n.hitbox.height + 2.0 * spacing
            }
        }

-- | Phase 3 helper: inflate every group's master by `spacing`, and
-- | every other group member (a VS by construction) according to its
-- | `ignoreSpacing` flags.
inflateGroups :: forall a. Number -> CGraph a -> CGraph a
inflateGroups spacing g = foldl alterGroup g (allCGroups g)
  where
  alterGroup acc grp = case grp.master of
    Nothing -> case A.head grp.cNodes of
      Just mid -> alterAllInGroup acc grp.cNodes mid
      Nothing -> acc
    Just mid -> alterAllInGroup acc grp.cNodes mid

  alterAllInGroup acc members master = do
    let acc1 = updateCNode master (alterHitbox spacing) acc
    if A.length members <= 1 then acc1
    else foldl (alterVSMember master) acc1 members

  alterVSMember master acc cid =
    if cid == master then acc
    else updateCNode cid alterVSInGroup acc

  alterVSInGroup n
    | n.ignoreSpacing.up = n
        { hitbox = n.hitbox
            { y = n.hitbox.y + spacing + smallEpsilon
            , height = n.hitbox.height - spacing - smallEpsilon
            }
        }
    | n.ignoreSpacing.down = n
        { hitbox = n.hitbox
            { height = n.hitbox.height - spacing - smallEpsilon }
        }
    | otherwise = n

----------------------------------------------------------------
-- Core sweep
----------------------------------------------------------------

-- | Run one scanline sweep over `g`'s CNodes that pass `filt`. Returns
-- | the graph with newly-discovered constraints appended (existing
-- | constraints on each CNode are preserved). The hitbox values used
-- | here are whatever `g` carries — callers inflate them before
-- | sweeping and restore them after.
sweep
  :: forall a
   . (CNode a -> Boolean)
  -> CGraph a
  -> CGraph a
sweep filt g = applyConstraints (sweepConstraints filt g) g

-- | The constraint-discovery core of a single sweep. Returns only the
-- | constraints found in *this* sweep (callers merge them where they
-- | want); the input graph's hitboxes are read, never written.
sweepConstraints
  :: forall a
   . (CNode a -> Boolean)
  -> CGraph a
  -> Map CNodeId (Array CNodeId)
sweepConstraints filt g = do
  let included = A.filter filt (allCNodes g)
  let events = A.sortBy cmpEvent (included >>= twoEvents)
  let result = foldl handle initial events
  result.constraints
  where
  initial = { intervals: ([] :: Array (CNode a)), cand: M.empty, constraints: M.empty }
  twoEvents n = [ { node: n, low: true }, { node: n, low: false } ]

type Event a = { node :: CNode a, low :: Boolean }

type Handler a =
  { intervals :: Array (CNode a)
  , cand :: Map CNodeId (Maybe CNodeId)
  , constraints :: Map CNodeId (Array CNodeId)
  }

-- | Sort events by y; on ties, high comes before low so nodes that
-- | *barely* touch don't pick up a constraint.
cmpEvent :: forall a. Event a -> Event a -> Ordering
cmpEvent p1 p2 = case compare (yOf p1) (yOf p2) of
  EQ -> case p1.low, p2.low of
    false, true -> LT
    true, false -> GT
    _, _ -> EQ
  ord -> ord
  where
  yOf p = if p.low then p.node.hitbox.y else p.node.hitbox.y + p.node.hitbox.height

handle :: forall a. Handler a -> Event a -> Handler a
handle st ev
  | ev.low = insert st ev.node
  | otherwise = delete st ev.node

-- | Insert into the active set; record the left neighbour as this
-- | node's candidate, and overwrite the right neighbour's candidate
-- | with this node.
insert :: forall a. Handler a -> CNode a -> Handler a
insert st node = do
  let intervals' = insertSorted node st.intervals
  let leftCandId = _.id <$> lowerNeighbour node intervals'
  let rightNeighbour = higherNeighbour node intervals'
  let cand' = M.insert node.id leftCandId st.cand
  let
    cand'' = case rightNeighbour of
      Just r -> M.insert r.id (Just node.id) cand'
      Nothing -> cand'
  st { intervals = intervals', cand = cand'' }

-- | Remove from the active set; emit `left → node` (and `node → right`)
-- | when the recorded candidates still match the immediate neighbours.
delete :: forall a. Handler a -> CNode a -> Handler a
delete st node = do
  let left = lowerNeighbour node st.intervals
  let right = higherNeighbour node st.intervals
  let
    c1 = case left of
      Just l | M.lookup node.id st.cand == Just (Just l.id) && differentGroup l node ->
        addConstraint l.id node.id st.constraints
      _ -> st.constraints
  let
    c2 = case right of
      Just r | M.lookup r.id st.cand == Just (Just node.id) && differentGroup node r ->
        addConstraint node.id r.id c1
      _ -> c1
  st { constraints = c2, intervals = A.filter (\m -> m.id /= node.id) st.intervals }
  where
  differentGroup a b = case a.cGroup, b.cGroup of
    Just g1, Just g2 -> g1 /= g2
    _, _ -> false

addConstraint :: CNodeId -> CNodeId -> Map CNodeId (Array CNodeId) -> Map CNodeId (Array CNodeId)
addConstraint from to = M.insertWith (<>) from [ to ]

----------------------------------------------------------------
-- Sorted-array primitives (active interval set)
----------------------------------------------------------------

-- | Sort key: ELK uses `hitbox.x + width/2` (centre). Ties are
-- | resolved by CNodeId so the array stays a strict total order even
-- | when two CNodes happen to share a centre.
sortKey :: forall a. CNode a -> Number /\ CNodeId
sortKey n = (n.hitbox.x + n.hitbox.width / 2.0) /\ n.id

insertSorted :: forall a. CNode a -> Array (CNode a) -> Array (CNode a)
insertSorted n arr = case A.findIndex (\m -> compareNode n m == LT) arr of
  Just i -> fromMaybe arr (A.insertAt i n arr)
  Nothing -> A.snoc arr n

compareNode :: forall a. CNode a -> CNode a -> Ordering
compareNode a b = compare (sortKey a) (sortKey b)

lowerNeighbour :: forall a. CNode a -> Array (CNode a) -> Maybe (CNode a)
lowerNeighbour n arr = A.last (A.filter (\m -> compareNode m n == LT) arr)

higherNeighbour :: forall a. CNode a -> Array (CNode a) -> Maybe (CNode a)
higherNeighbour n arr = A.find (\m -> compareNode n m == LT) arr

----------------------------------------------------------------
-- Constraint application
----------------------------------------------------------------

applyConstraints
  :: forall a
   . Map CNodeId (Array CNodeId)
  -> CGraph a
  -> CGraph a
applyConstraints m g = foldl step g (M.toUnfoldable m :: Array _)
  where
  step acc (cid /\ tgts) = case lookupCNode cid acc of
    Just _ -> updateCNode cid (\n -> n { constraints = n.constraints <> tgts }) acc
    Nothing -> acc

