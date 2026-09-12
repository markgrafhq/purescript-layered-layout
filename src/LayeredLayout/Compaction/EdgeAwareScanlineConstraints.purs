-- Copyright (c) 2017 Kiel University and others.
-- SPDX-License-Identifier: EPL-2.0
-- Translated from ELK c831ba4613dfd6b0055851193956560351d2f907:
-- EdgeAwareScanlineConstraintCalculation.calculateForOrthogonal,
-- alterHitbox, alterGroupedHitboxOrthogonal; ScanlineConstraintCalculator.
--
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
  , lookupCGroup
  , lookupCNode
  , updateCNode
  )

----------------------------------------------------------------
-- Public algorithms
----------------------------------------------------------------

-- | Pure base scanline: one sweep over every CNode, no hitbox inflation.
scanlineConstraints :: forall a. IConstraintCalculationAlgorithm a
scanlineConstraints = IConstraintCalculationAlgorithm \st ->
  sweep (\_ -> true) st.cGraph

-- | Orthogonal edge-aware scanline. Constraints accumulate across all
-- | three sweeps and preserve the lifecycle's predefined constraints.
edgeAwareScanlineConstraints
  :: forall a
   . { nodeNode :: Number, edgeEdge :: Number }
  -> IConstraintCalculationAlgorithm a
edgeAwareScanlineConstraints spacings = IConstraintCalculationAlgorithm \st ->
  edgeAwareOrthogonal spacings st.cGraph

----------------------------------------------------------------
-- Orthogonal driver: three phases of inflate → sweep → restore.
--   1. VS-only sweep with VSes inflated by edgeEdge/2 − ε
--   2. LNode-only sweep with LNodes inflated by edgeEdge/2 − ε
--   3. full sweep with group masters + member VSes inflated
----------------------------------------------------------------

edgeAwareOrthogonal
  :: forall a
   . { nodeNode :: Number, edgeEdge :: Number }
  -> CGraph a
  -> CGraph a
edgeAwareOrthogonal spacings g0 = do
  let spacing = inflateBy spacings.edgeEdge
  let g1 = inflateAll (-spacing) (sweep isVS (inflateAll spacing g0 isVS)) isVS
  let g2 = inflateAll (-spacing) (sweep isLNode (inflateAll spacing g1 isLNode)) isLNode
  let
    nodeSpacing n = inflateBy (if isVS n then spacings.edgeEdge else spacings.nodeNode)
    minSpacing = case A.uncons (allCNodes g2) of
      Nothing -> 0.0
      Just { head, tail } -> foldl (\acc n -> min acc (nodeSpacing n)) (nodeSpacing head) tail
  inflateGroups (-minSpacing) (sweep (\_ -> true) (inflateGroups minSpacing g2))

isVS :: forall a. CNode a -> Boolean
isVS n = n.kind == Just "vs"

isLNode :: forall a. CNode a -> Boolean
isLNode n = not (isVS n)

----------------------------------------------------------------
-- Hitbox inflation
----------------------------------------------------------------

-- | ELK uses global edge-edge spacing for the first two sweeps,
-- | then the minimum of global node-node and edge-edge half-spacing
-- | for the complete sweep. These are not between-layer spacings.
inflateBy :: Number -> Number
inflateBy edgeEdgeSpacing = max 0.0 (edgeEdgeSpacing / 2.0 - epsilon)

epsilon :: Number
epsilon = 0.5

smallEpsilon :: Number
smallEpsilon = 0.01

-- | Signed spacing is ELK's spacing * fac. SMALL_EPSILON remains
-- | positive on restoration too, matching alterHitbox exactly; restoring
-- | a vertical segment therefore retains the source's 0.02 hitbox change.
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
  -- Empty intervals have no sweep interior. Emitting their high event
  -- before their low event would leave them active forever and create
  -- spurious barriers, including positive cycles between node groups.
  let included = A.filter (\node -> filt node && node.hitbox.height > 0.0) (allCNodes g)
  let events = A.sortBy cmpEvent (included >>= twoEvents)
  let result = foldl handle initial events
  projectGroupMasters filt g result.constraints
  where
  initial = { intervals: ([] :: Array (CNode a)), cand: M.empty, constraints: M.empty }
  twoEvents n = [ { node: n, low: true }, { node: n, low: false } ]

type Event a = { node :: CNode a, low :: Boolean }

type Handler a =
  { intervals :: Array (CNode a)
  , cand :: Map CNodeId (Maybe CNodeId)
  , constraints :: Map CNodeId (Array CNodeId)
  }

-- | Correct the upstream sweep's grouped-obstacle shadowing: an interior
-- | member can hide its master's reserved frame. Retain the member's
-- | constraint (its spacing may be stronger) and protect the master too.
-- | Only overlapping transverse interiors with an already-valid
-- | longitudinal order can contribute a projected constraint.
projectGroupMasters
  :: forall a
   . (CNode a -> Boolean)
  -> CGraph a
  -> Map CNodeId (Array CNodeId)
  -> Map CNodeId (Array CNodeId)
projectGroupMasters filt g found = foldl projectSource found
  (M.toUnfoldable found :: Array (CNodeId /\ Array CNodeId))
  where
  projectSource acc (sourceId /\ targets) = case lookupCNode sourceId g of
    Nothing -> acc
    Just source -> foldl (projectTarget source (masterFor source)) acc targets

  projectTarget source sourceMaster acc targetId = case lookupCNode targetId g of
    Nothing -> acc
    Just target ->
      let
        targetMaster = masterFor target
        withSource = case sourceMaster of
          Nothing -> acc
          Just master -> add master target acc
        withTarget = case targetMaster of
          Nothing -> withSource
          Just master -> add source master withSource
      in
        case sourceMaster, targetMaster of
          Just a, Just b -> add a b withTarget
          _, _ -> withTarget

  masterFor node = do
    groupId <- node.cGroup
    group <- lookupCGroup groupId g
    masterId <- case group.master of
      Just id -> Just id
      Nothing -> A.head group.cNodes
    if masterId == node.id then Nothing
    else do
      master <- lookupCNode masterId g
      if filt master then Just master else Nothing

  add source target acc
    | source.cGroup == target.cGroup = acc
    | source.hitbox.x + source.hitbox.width > target.hitbox.x = acc
    | max source.hitbox.y target.hitbox.y >=
        min (source.hitbox.y + source.hitbox.height) (target.hitbox.y + target.hitbox.height) = acc
    | A.elem target.id source.constraints = acc
    | A.elem target.id (fromMaybe [] (M.lookup source.id acc)) = acc
    | otherwise = addConstraint source.id target.id acc

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

