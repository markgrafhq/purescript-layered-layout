-- | Constraint-graph data model and infrastructure for the
-- | post-routing one-dimensional compactor.
-- |
-- | This is a port of ELK's `org.eclipse.elk.alg.common.compaction.oned`
-- | package: `CGraph` / `CGroup` / `CNode` / `Quadruplet` /
-- | `ISpacingsHandler` / `ICompactionAlgorithm` /
-- | `IConstraintCalculationAlgorithm` / `OneDimensionalCompactor`.
-- |
-- | The Java code mutates the state in place. The PureScript port
-- | threads an `OneDState a` record through each step. The `origin`
-- | field on a `CNode` is polymorphic so the layered package can
-- | tag it with `LNode | VerticalSegment` without this module
-- | knowing the concrete sum.
-- |
-- | Phase 2 of the post-routing compaction port (the network simplex
-- | algorithm lives in `LayeredLayout.NetworkSimplex`; the layered-graph
-- | bridge lives in `LayeredLayout.Compaction.LGraphTransformer` /
-- | `LayeredLayout.Compaction.HorizontalGraphCompactor`).
module LayeredLayout.Compaction.OneD
  ( Direction(..)
  , isHorizontalDir
  , isVerticalDir
  , Rect
  , KVec
  , zeroVec
  , CNodeId
  , CGroupId
  , CNode
  , CGroup
  , CGraph
  , OneDState
  , ISpacingsHandler
  , ILockFunction
  , ICompactionAlgorithm(..)
  , IConstraintCalculationAlgorithm(..)
  , runCompactionAlgorithm
  , runConstraintAlgorithm
  , Quadruplet
  , emptyQuadruplet
  , quadGet
  , quadSet
  , quadOr
  , fuzzyTolerance
  , fuzzyEq
  , fuzzyGt
  , fuzzyLt
  , fuzzyGe
  , fuzzyLe
  , defaultSpacingsHandler
  , newCGraph
  , addCNode
  , addCNodeWithGroup
  , addCGroup
  , addCNodeToGroup
  , removeCNodeFromGroup
  , supports
  , lookupCNode
  , lookupCGroup
  , updateCNode
  , setCNodeIgnoreSpacing
  , updateCGroup
  , allCNodes
  , allCGroups
  , newOneD
  , setSpacingsHandler
  , setCompactionAlgorithm
  , setConstraintAlgorithm
  , setLockFunction
  , compact
  , finish
  , changeDirection
  , calculateGroupOffsets
  , forceConstraintsRecalculation
  , isNodeLocked
  , isGroupLocked
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (abs)
import Data.Set (Set)
import Data.Set as S
import Data.Tuple.Nested (type (/\), (/\))

-- | Sentinel `-infinity` used as the un-positioned marker on a CNode.
-- | Hand-written because psgo's `Data.Number.infinity` FFI is a thunk
-- | that doesn't survive direct numeric ops.
negInfinity :: Number
negInfinity = -1.0e308

----------------------------------------------------------------
-- Direction
----------------------------------------------------------------

data Direction = LEFT | RIGHT | UP | DOWN | UNDEFINED

derive instance Eq Direction
derive instance Ord Direction

isHorizontalDir :: Direction -> Boolean
isHorizontalDir LEFT = true
isHorizontalDir RIGHT = true
isHorizontalDir _ = false

isVerticalDir :: Direction -> Boolean
isVerticalDir UP = true
isVerticalDir DOWN = true
isVerticalDir _ = false

----------------------------------------------------------------
-- Geometry
----------------------------------------------------------------

type Rect = { x :: Number, y :: Number, width :: Number, height :: Number }

type KVec = { x :: Number, y :: Number }

zeroVec :: KVec
zeroVec = { x: 0.0, y: 0.0 }

----------------------------------------------------------------
-- CompareFuzzy
----------------------------------------------------------------

fuzzyTolerance :: Number
fuzzyTolerance = 0.0001

fuzzyEq :: Number -> Number -> Boolean
fuzzyEq a b = abs (a - b) <= fuzzyTolerance

fuzzyGt :: Number -> Number -> Boolean
fuzzyGt a b = a - b > fuzzyTolerance

fuzzyLt :: Number -> Number -> Boolean
fuzzyLt a b = b - a > fuzzyTolerance

fuzzyGe :: Number -> Number -> Boolean
fuzzyGe a b = a - b >= -fuzzyTolerance

fuzzyLe :: Number -> Number -> Boolean
fuzzyLe a b = b - a >= -fuzzyTolerance

----------------------------------------------------------------
-- Quadruplet
----------------------------------------------------------------

type Quadruplet =
  { left :: Boolean
  , right :: Boolean
  , up :: Boolean
  , down :: Boolean
  }

emptyQuadruplet :: Quadruplet
emptyQuadruplet = { left: false, right: false, up: false, down: false }

quadGet :: Direction -> Quadruplet -> Boolean
quadGet LEFT q = q.left
quadGet RIGHT q = q.right
quadGet UP q = q.up
quadGet DOWN q = q.down
quadGet UNDEFINED _ = false

quadSet :: Direction -> Boolean -> Quadruplet -> Quadruplet
quadSet LEFT v q = q { left = v }
quadSet RIGHT v q = q { right = v }
quadSet UP v q = q { up = v }
quadSet DOWN v q = q { down = v }
quadSet UNDEFINED _ q = q

quadOr :: Quadruplet -> Quadruplet -> Quadruplet
quadOr a b =
  { left: a.left || b.left
  , right: a.right || b.right
  , up: a.up || b.up
  , down: a.down || b.down
  }

----------------------------------------------------------------
-- IDs
----------------------------------------------------------------

type CNodeId = Int
type CGroupId = Int

----------------------------------------------------------------
-- CNode / CGroup / CGraph
----------------------------------------------------------------

-- | Node in the constraint graph.
-- |
-- | The `origin` field is polymorphic — the layered-graph bridge
-- | tags it with a sum of `LNode` and `VerticalSegment`, but this
-- | module does not need to look inside.
type CNode a =
  { id :: CNodeId
  , origin :: Maybe a
  , kind :: Maybe String
  , cGroup :: Maybe CGroupId
  , cGroupOffset :: KVec
  , hitbox :: Rect
  , hitboxPreCompaction :: Rect
  , constraints :: Array CNodeId
  , startPos :: Number
  , ignoreSpacing :: Quadruplet
  }

-- | Group of nodes whose relative offsets are preserved during
-- | compaction.
type CGroup =
  { id :: CGroupId
  , master :: Maybe CNodeId
  , cNodes :: Array CNodeId
  , startPos :: Number
  , incomingConstraints :: Array CNodeId
  , outDegree :: Int
  , outDegreeReal :: Int
  , reference :: Maybe CNodeId
  , delta :: Number
  , deltaNormalized :: Number
  }

-- | Constraint graph.
type CGraph a =
  { cNodes :: Map CNodeId (CNode a)
  , cNodeOrder :: Array CNodeId
  , cGroups :: Map CGroupId CGroup
  , cGroupOrder :: Array CGroupId
  , supportedDirections :: Set Direction
  , predefinedHorizontalConstraints :: Array (CNodeId /\ CNodeId)
  , predefinedVerticalConstraints :: Array (CNodeId /\ CNodeId)
  , nextCNodeId :: Int
  , nextCGroupId :: Int
  }

newCGraph :: forall a. Set Direction -> CGraph a
newCGraph dirs =
  { cNodes: M.empty
  , cNodeOrder: []
  , cGroups: M.empty
  , cGroupOrder: []
  , supportedDirections: dirs
  , predefinedHorizontalConstraints: []
  , predefinedVerticalConstraints: []
  , nextCNodeId: 0
  , nextCGroupId: 0
  }

-- | Create a new free-standing CNode in the graph. The node is not
-- | placed in any group yet — the OneDimensionalCompactor constructor
-- | wraps every group-less node in a singleton group.
addCNode
  :: forall a
   . { origin :: Maybe a, kind :: Maybe String, hitbox :: Rect }
  -> CGraph a
  -> { id :: CNodeId, graph :: CGraph a }
addCNode spec g = do
  let nid = g.nextCNodeId
  let
    n =
      { id: nid
      , origin: spec.origin
      , kind: spec.kind
      , cGroup: Nothing
      , cGroupOffset: zeroVec
      , hitbox: spec.hitbox
      , hitboxPreCompaction: spec.hitbox
      , constraints: []
      , startPos: negInfinity
      , ignoreSpacing: emptyQuadruplet
      }
  { id: nid
  , graph: g
      { cNodes = M.insert nid n g.cNodes
      , cNodeOrder = g.cNodeOrder <> [ nid ]
      , nextCNodeId = nid + 1
      }
  }

-- | Add a node and immediately group it with `parent`. Mirrors
-- | `CNodeBuilder.groupWith(parent).create(graph)`.
addCNodeWithGroup
  :: forall a
   . { origin :: Maybe a, kind :: Maybe String, hitbox :: Rect }
  -> CNodeId
  -> CGraph a
  -> { id :: CNodeId, graph :: CGraph a }
addCNodeWithGroup spec parent g = do
  let added = addCNode spec g
  let g1 = added.graph
  case M.lookup parent g1.cNodes of
    Nothing -> added
    Just parentN -> case parentN.cGroup of
      Just gid -> { id: added.id, graph: addCNodeToGroup added.id gid g1 }
      Nothing -> do
        let r = addCGroup { master: Nothing, nodes: [ parent, added.id ] } g1
        { id: added.id, graph: r.graph }

-- | Create a new CGroup containing `nodes`. The first node in
-- | `nodes` becomes the initial reference.
addCGroup
  :: forall a
   . { master :: Maybe CNodeId, nodes :: Array CNodeId }
  -> CGraph a
  -> { id :: CGroupId, graph :: CGraph a }
addCGroup spec g = do
  let gid = g.nextCGroupId
  let
    grp =
      { id: gid
      , master: spec.master
      , cNodes: []
      , startPos: negInfinity
      , incomingConstraints: []
      , outDegree: 0
      , outDegreeReal: 0
      , reference: Nothing
      , delta: 0.0
      , deltaNormalized: 0.0
      }
  let
    g1 = g
      { cGroups = M.insert gid grp g.cGroups
      , cGroupOrder = g.cGroupOrder <> [ gid ]
      , nextCGroupId = gid + 1
      }
  let g2 = foldl (\acc nid -> addCNodeToGroup nid gid acc) g1 spec.nodes
  { id: gid, graph: g2 }

-- | Move a node into a group. Mirrors `CGroup.addCNode` (which
-- | throws if the node already belongs to a group). The first node
-- | added becomes the group's reference.
addCNodeToGroup :: forall a. CNodeId -> CGroupId -> CGraph a -> CGraph a
addCNodeToGroup nid gid g = case M.lookup nid g.cNodes /\ M.lookup gid g.cGroups of
  Just n /\ Just grp ->
    if isJust n.cGroup && n.cGroup /= Just gid then g
    else do
      let n' = n { cGroup = Just gid }
      let
        grp' = grp
          { cNodes = if A.elem nid grp.cNodes then grp.cNodes else grp.cNodes <> [ nid ]
          , reference = case grp.reference of
              Nothing -> Just nid
              Just r -> Just r
          }
      g
        { cNodes = M.insert nid n' g.cNodes
        , cGroups = M.insert gid grp' g.cGroups
        }
  _ -> g

removeCNodeFromGroup :: forall a. CNodeId -> CGroupId -> CGraph a -> CGraph a
removeCNodeFromGroup nid gid g = case M.lookup nid g.cNodes /\ M.lookup gid g.cGroups of
  Just n /\ Just grp -> do
    let n' = n { cGroup = Nothing }
    let grp' = grp { cNodes = A.filter (_ /= nid) grp.cNodes }
    g
      { cNodes = M.insert nid n' g.cNodes
      , cGroups = M.insert gid grp' g.cGroups
      }
  _ -> g

supports :: forall a. Direction -> CGraph a -> Boolean
supports d g = S.member d g.supportedDirections

lookupCNode :: forall a. CNodeId -> CGraph a -> Maybe (CNode a)
lookupCNode nid g = M.lookup nid g.cNodes

lookupCGroup :: forall a. CGroupId -> CGraph a -> Maybe CGroup
lookupCGroup gid g = M.lookup gid g.cGroups

updateCNode :: forall a. CNodeId -> (CNode a -> CNode a) -> CGraph a -> CGraph a
updateCNode nid f g = case M.lookup nid g.cNodes of
  Nothing -> g
  Just n -> g { cNodes = M.insert nid (f n) g.cNodes }

-- | OR `q` into the CNode's existing ignoreSpacing flags. Mirrors
-- | the merge semantics of `VerticalSegment.unionInto` so flags set
-- | by the transformer accumulate when a node is touched multiple
-- | times.
setCNodeIgnoreSpacing :: forall a. CNodeId -> Quadruplet -> CGraph a -> CGraph a
setCNodeIgnoreSpacing nid q = updateCNode nid \n -> n { ignoreSpacing = quadOr n.ignoreSpacing q }

updateCGroup :: forall a. CGroupId -> (CGroup -> CGroup) -> CGraph a -> CGraph a
updateCGroup gid f g = case M.lookup gid g.cGroups of
  Nothing -> g
  Just grp -> g { cGroups = M.insert gid (f grp) g.cGroups }

-- | Iterate every CNode in insertion order.
allCNodes :: forall a. CGraph a -> Array (CNode a)
allCNodes g = A.mapMaybe (\nid -> M.lookup nid g.cNodes) g.cNodeOrder

-- | Iterate every CGroup in insertion order.
allCGroups :: forall a. CGraph a -> Array CGroup
allCGroups g = A.mapMaybe (\gid -> M.lookup gid g.cGroups) g.cGroupOrder

----------------------------------------------------------------
-- Spacings / lock / algorithm hooks
----------------------------------------------------------------

type ISpacingsHandler a =
  { horizontalSpacing :: CNode a -> CNode a -> Number
  , verticalSpacing :: CNode a -> CNode a -> Number
  }

defaultSpacingsHandler :: forall a. ISpacingsHandler a
defaultSpacingsHandler =
  { horizontalSpacing: \_ _ -> 0.0
  , verticalSpacing: \_ _ -> 0.0
  }

type ILockFunction a = CNode a -> Direction -> Boolean

newtype ICompactionAlgorithm a = ICompactionAlgorithm (OneDState a -> OneDState a)

newtype IConstraintCalculationAlgorithm a =
  IConstraintCalculationAlgorithm (OneDState a -> CGraph a)

runCompactionAlgorithm :: forall a. ICompactionAlgorithm a -> OneDState a -> OneDState a
runCompactionAlgorithm (ICompactionAlgorithm f) = f

runConstraintAlgorithm :: forall a. IConstraintCalculationAlgorithm a -> OneDState a -> CGraph a
runConstraintAlgorithm (IConstraintCalculationAlgorithm f) = f

----------------------------------------------------------------
-- OneDimensionalCompactor state
----------------------------------------------------------------

type OneDState a =
  { cGraph :: CGraph a
  , direction :: Direction
  , compactionAlgorithm :: Maybe (ICompactionAlgorithm a)
  , constraintAlgorithm :: Maybe (IConstraintCalculationAlgorithm a)
  , spacingsHandler :: ISpacingsHandler a
  , lockFun :: Maybe (ILockFunction a)
  , finished :: Boolean
  }

-- | Construct a new compactor for the given graph. Matches the
-- | Java constructor: (1) compute group offsets so the left-most
-- | node in each group becomes the reference, (2) wrap any plain
-- | node in a singleton group, (3) snapshot the pre-compaction
-- | hitbox.
newOneD :: forall a. CGraph a -> OneDState a
newOneD g0 = do
  let g1 = calculateGroupOffsetsGraph g0
  let g2 = wrapPlainNodesInSingletonGroups g1
  let g3 = snapshotHitboxes g2
  { cGraph: g3
  , direction: UNDEFINED
  , compactionAlgorithm: Nothing
  , constraintAlgorithm: Nothing
  , spacingsHandler: defaultSpacingsHandler
  , lockFun: Nothing
  , finished: false
  }

setSpacingsHandler :: forall a. ISpacingsHandler a -> OneDState a -> OneDState a
setSpacingsHandler h s = s { spacingsHandler = h }

setCompactionAlgorithm :: forall a. ICompactionAlgorithm a -> OneDState a -> OneDState a
setCompactionAlgorithm a s = s { compactionAlgorithm = Just a }

setConstraintAlgorithm
  :: forall a. IConstraintCalculationAlgorithm a -> OneDState a -> OneDState a
setConstraintAlgorithm a s = s { constraintAlgorithm = Just a }

setLockFunction :: forall a. ILockFunction a -> OneDState a -> OneDState a
setLockFunction f s = s { lockFun = Just f }

----------------------------------------------------------------
-- The lifecycle: compact / finish / changeDirection
----------------------------------------------------------------

-- | Compact in the current direction. Defaults to LEFT if no
-- | direction has been set, then resets per-group `outDegree` and
-- | per-node `startPos` before running the configured algorithm.
compact :: forall a. OneDState a -> OneDState a
compact s0
  | s0.finished = s0
  | otherwise = do
      let s1 = if s0.direction == UNDEFINED then changeDirection LEFT s0 else s0
      let g2 = resetCompactionFields s1.cGraph
      let s2 = s1 { cGraph = g2 }
      case s2.compactionAlgorithm of
        Nothing -> s2
        Just alg -> runCompactionAlgorithm alg s2

-- | Mark the compactor as finished and restore the canonical LEFT
-- | orientation so any hitbox mirrors / transposes are undone.
finish :: forall a. OneDState a -> OneDState a
finish s = (changeDirection LEFT s) { finished = true }

-- | Switch compaction direction. Mirrors / transposes hitboxes as
-- | needed and either recalculates the constraints or reverses
-- | them (for the LEFT<->RIGHT and UP<->DOWN flips).
changeDirection :: forall a. Direction -> OneDState a -> OneDState a
changeDirection dir s
  | s.finished = s
  | not (supports dir s.cGraph) = s
  | dir == s.direction = s
  | otherwise = applyTransition s.direction dir s

applyTransition :: forall a. Direction -> Direction -> OneDState a -> OneDState a
applyTransition oldDir newDir s0 = do
  let s1 = s0 { direction = newDir }
  case oldDir of
    UNDEFINED -> case newDir of
      LEFT -> calculateConstraints s1
      RIGHT -> calculateConstraints (mapGraph mirrorHitboxes s1)
      UP -> calculateConstraints (mapGraph transposeHitboxes s1)
      DOWN -> calculateConstraints (mapGraph (mirrorHitboxes <<< transposeHitboxes) s1)
      _ -> s1
    LEFT -> case newDir of
      RIGHT -> reverseConstraints (mapGraph mirrorHitboxes s1)
      UP -> calculateConstraints (mapGraph transposeHitboxes s1)
      DOWN -> calculateConstraints (mapGraph (mirrorHitboxes <<< transposeHitboxes) s1)
      _ -> s1
    RIGHT -> case newDir of
      LEFT -> reverseConstraints (mapGraph mirrorHitboxes s1)
      UP -> calculateConstraints (mapGraph (transposeHitboxes <<< mirrorHitboxes) s1)
      DOWN -> calculateConstraints
        (mapGraph (mirrorHitboxes <<< transposeHitboxes <<< mirrorHitboxes) s1)
      _ -> s1
    UP -> case newDir of
      LEFT -> calculateConstraints (mapGraph transposeHitboxes s1)
      RIGHT -> calculateConstraints (mapGraph (mirrorHitboxes <<< transposeHitboxes) s1)
      DOWN -> reverseConstraints (mapGraph mirrorHitboxes s1)
      _ -> s1
    DOWN -> case newDir of
      LEFT -> calculateConstraints (mapGraph (transposeHitboxes <<< mirrorHitboxes) s1)
      RIGHT -> calculateConstraints
        (mapGraph (mirrorHitboxes <<< transposeHitboxes <<< mirrorHitboxes) s1)
      UP -> reverseConstraints (mapGraph mirrorHitboxes s1)
      _ -> s1

mapGraph :: forall a. (CGraph a -> CGraph a) -> OneDState a -> OneDState a
mapGraph f s = s { cGraph = f s.cGraph }

-- | Run the constraint calculation again without changing direction.
forceConstraintsRecalculation :: forall a. OneDState a -> OneDState a
forceConstraintsRecalculation = calculateConstraints

----------------------------------------------------------------
-- Locks
----------------------------------------------------------------

isNodeLocked :: forall a. CNode a -> Direction -> OneDState a -> Boolean
isNodeLocked n d s = case s.lockFun of
  Nothing -> false
  Just f -> f n d

isGroupLocked :: forall a. CGroupId -> Direction -> OneDState a -> Boolean
isGroupLocked gid d s = case lookupCGroup gid s.cGraph of
  Nothing -> false
  Just grp -> A.any nodeLocked grp.cNodes
    where
    nodeLocked nid = case lookupCNode nid s.cGraph of
      Nothing -> false
      Just n -> isNodeLocked n d s

----------------------------------------------------------------
-- Group offsets / wrapping / pre-snapshot
----------------------------------------------------------------

-- | Public re-export of `calculateGroupOffsetsGraph` operating on
-- | the full state.
calculateGroupOffsets :: forall a. OneDState a -> OneDState a
calculateGroupOffsets = mapGraph calculateGroupOffsetsGraph

calculateGroupOffsetsGraph :: forall a. CGraph a -> CGraph a
calculateGroupOffsetsGraph g = foldl computeOne g g.cGroupOrder
  where
  computeOne acc gid = case M.lookup gid acc.cGroups of
    Nothing -> acc
    Just grp -> do
      let refId = pickLeftMostReference grp.cNodes acc
      let acc1 = updateCGroup gid (_ { reference = refId }) acc
      case refId of
        Nothing -> acc1
        Just rid -> case M.lookup rid acc1.cNodes of
          Nothing -> acc1
          Just refNode -> foldl (offsetOne refNode) acc1 grp.cNodes

  pickLeftMostReference nodes acc =
    let
      step bestId nid = case M.lookup nid acc.cNodes of
        Nothing -> bestId
        Just n -> case bestId of
          Nothing -> Just nid
          Just bid -> case M.lookup bid acc.cNodes of
            Nothing -> Just nid
            Just b -> if n.hitbox.x < b.hitbox.x then Just nid else Just bid
    in
      foldl step Nothing nodes

  offsetOne refNode acc nid = updateCNode nid
    ( \n -> n
        { cGroupOffset =
            { x: n.hitbox.x - refNode.hitbox.x
            , y: n.hitbox.y - refNode.hitbox.y
            }
        }
    )
    acc

wrapPlainNodesInSingletonGroups :: forall a. CGraph a -> CGraph a
wrapPlainNodesInSingletonGroups g0 = foldl wrapOne g0 g0.cNodeOrder
  where
  wrapOne acc nid = case M.lookup nid acc.cNodes of
    Just n | n.cGroup == Nothing -> (addCGroup { master: Nothing, nodes: [ nid ] } acc).graph
    _ -> acc

snapshotHitboxes :: forall a. CGraph a -> CGraph a
snapshotHitboxes g = foldl snap g g.cNodeOrder
  where
  snap acc nid = updateCNode nid (\n -> n { hitboxPreCompaction = n.hitbox }) acc

resetCompactionFields :: forall a. CGraph a -> CGraph a
resetCompactionFields g0 = do
  let g1 = foldl resetGroup g0 g0.cGroupOrder
  foldl resetNode g1 g1.cNodeOrder
  where
  resetGroup acc gid = updateCGroup gid (\grp -> grp { outDegree = grp.outDegreeReal }) acc
  resetNode acc nid = updateCNode nid (_ { startPos = negInfinity }) acc

----------------------------------------------------------------
-- Hitbox transformations
----------------------------------------------------------------

mirrorHitboxes :: forall a. CGraph a -> CGraph a
mirrorHitboxes g = calculateGroupOffsetsGraph (mapAllNodes mirrorOne g)
  where
  mirrorOne n = n { hitbox = n.hitbox { x = -n.hitbox.x - n.hitbox.width } }

transposeHitboxes :: forall a. CGraph a -> CGraph a
transposeHitboxes g = calculateGroupOffsetsGraph (mapAllNodes transposeOne g)
  where
  transposeOne n = n
    { hitbox =
        { x: n.hitbox.y
        , y: n.hitbox.x
        , width: n.hitbox.height
        , height: n.hitbox.width
        }
    , cGroupOffset = { x: n.cGroupOffset.y, y: n.cGroupOffset.x }
    }

mapAllNodes :: forall a. (CNode a -> CNode a) -> CGraph a -> CGraph a
mapAllNodes f g = g { cNodes = map f g.cNodes }

----------------------------------------------------------------
-- Constraint calculation / reversal
----------------------------------------------------------------

-- | Recompute constraints from scratch. Clears per-node
-- | constraints, layers any pre-defined ones for the current
-- | direction, runs the configured constraint algorithm, then
-- | rolls the per-node constraints up to group-level counts.
calculateConstraints :: forall a. OneDState a -> OneDState a
calculateConstraints s0 = do
  let g1 = clearConstraints s0.cGraph
  let g2 = applyPredefinedConstraints s0.direction g1
  let s1 = s0 { cGraph = g2 }
  let
    g3 = case s1.constraintAlgorithm of
      Nothing -> s1.cGraph
      Just alg -> runConstraintAlgorithm alg s1
  let s2 = s1 { cGraph = g3 }
  mapGraph calculateConstraintsForCGroups s2

clearConstraints :: forall a. CGraph a -> CGraph a
clearConstraints g = mapAllNodes (_ { constraints = [] }) g

applyPredefinedConstraints :: forall a. Direction -> CGraph a -> CGraph a
applyPredefinedConstraints dir g = do
  let
    pairs =
      if isHorizontalDir dir then g.predefinedHorizontalConstraints
      else g.predefinedVerticalConstraints
  foldl addPair g pairs
  where
  addPair acc (a /\ b)
    | dir == LEFT || dir == UP = updateCNode a (\n -> n { constraints = n.constraints <> [ b ] }) acc
    | otherwise = updateCNode b (\n -> n { constraints = n.constraints <> [ a ] }) acc

calculateConstraintsForCGroups :: forall a. CGraph a -> CGraph a
calculateConstraintsForCGroups g0 = do
  let g1 = foldl resetGroup g0 g0.cGroupOrder
  foldl rollUp g1 g1.cNodeOrder
  where
  resetGroup acc gid = updateCGroup gid
    ( _
        { outDegree = 0
        , outDegreeReal = 0
        , incomingConstraints = []
        }
    )
    acc

  rollUp acc nid = case M.lookup nid acc.cNodes of
    Nothing -> acc
    Just n -> case n.cGroup of
      Nothing -> acc
      Just gid -> foldl (rollEdge gid) acc n.constraints

  rollEdge gid acc incId = case M.lookup incId acc.cNodes of
    Nothing -> acc
    Just inc -> case inc.cGroup of
      Just incGid | incGid /= gid -> do
        let
          acc1 = updateCGroup gid
            ( \grp ->
                if A.elem incId grp.incomingConstraints then grp
                else grp { incomingConstraints = grp.incomingConstraints <> [ incId ] }
            )
            acc
        updateCGroup incGid
          ( \grp -> grp
              { outDegree = grp.outDegree + 1
              , outDegreeReal = grp.outDegreeReal + 1
              }
          )
          acc1
      _ -> acc

reverseConstraints :: forall a. OneDState a -> OneDState a
reverseConstraints s0 = do
  let g1 = reverseConstraintsGraph s0.cGraph
  let g2 = foldl (\acc nid -> updateCNode nid (_ { startPos = negInfinity }) acc) g1 g1.cNodeOrder
  let g3 = calculateConstraintsForCGroups g2
  s0 { cGraph = g3 }

reverseConstraintsGraph :: forall a. CGraph a -> CGraph a
reverseConstraintsGraph g = do
  let reversed = foldl collect M.empty g.cNodeOrder
  foldl
    (\acc nid -> updateCNode nid (_ { constraints = fromMaybe [] (M.lookup nid reversed) }) acc)
    g
    g.cNodeOrder
  where
  collect :: Map CNodeId (Array CNodeId) -> CNodeId -> Map CNodeId (Array CNodeId)
  collect m nid = case M.lookup nid g.cNodes of
    Nothing -> m
    Just n -> foldl
      (\acc inc -> M.insertWith (<>) inc [ nid ] acc)
      m
      n.constraints

