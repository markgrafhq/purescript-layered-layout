-- | Port of ELK's `LGraphToCGraphTransformer` adapted to markgraf's
-- | post-routing data model. Builds a compaction graph from a
-- | `LayoutResult`-shaped input (node placements + routed edges) and
-- | maps the compacted x-coordinates back onto those inputs.
-- |
-- | Only orthogonal edges are supported (markgraf does not route
-- | splines). Comment boxes and external port dummies are not part
-- | of markgraf either, so those branches are dropped.
module LayeredLayout.Compaction.LGraphToCGraphTransformer
  ( CNodeOrigin(..)
  , RoutedEdge
  , TransformInput
  , TransformOutput
  , transform
  , applyLayout
  , buildHooks
  , buildRoutedEdges
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as S
import Data.Tuple.Nested (type (/\), (/\))
import LayeredLayout.Compaction.NetworkSimplexCompaction (CompactionHooks, ExtraEdge, edgeWeight)
import LayeredLayout.Compaction.OneD
  ( CGraph
  , CGroupId
  , CNode
  , CNodeId
  , Direction(..)
  , Quadruplet
  , Rect
  , addCGroup
  , addCNode
  , addCNodeToGroup
  , allCNodes
  , emptyQuadruplet
  , lookupCNode
  , newCGraph
  , setCNodeIgnoreSpacing
  )
import LayeredLayout.Compaction.VerticalSegment (VerticalSegment, compareVS, intersects, joinWith, newVerticalSegment)
import LayeredLayout.EdgeRouting (scaleFactor)
import Data.Int (toNumber)
import LayeredLayout.Graph (Edge, EdgeId, NodeId, Port, PortId, Side(..))
import LayeredLayout.Grid (GridPos(..), gridX, gridY, sizeH, sizeW)
import LayeredLayout.Result (Direction(..), EdgePath, EdgeSegment, NodePlacement) as R

-- | What kind of layered-graph object each `CNode` represents.
data CNodeOrigin
  = NodeOrigin NodeId
  | SegmentOrigin VerticalSegment

type RoutedEdge =
  { edgeId :: EdgeId
  , src :: NodeId
  , tgt :: NodeId
  , srcSide :: Side
  , tgtSide :: Side
  , path :: R.EdgePath
  }

type TransformInput =
  { nodes :: Array R.NodePlacement
  , edges :: Array Edge
  , paths :: Array R.EdgePath
  , ports :: Map NodeId (Array Port)
  }

type TransformOutput =
  { cGraph :: CGraph CNodeOrigin
  , nodeToC :: Map NodeId CNodeId
  , edgeToCs :: Map EdgeId (Array CNodeId)
  , lockMap :: Map CNodeId Quadruplet
  }

----------------------------------------------------------------
-- transform
----------------------------------------------------------------

transform :: TransformInput -> TransformOutput
transform input = do
  let degrees = edgeDegrees input.edges
  let withNodes = transformNodes degrees input.nodes (initial input)
  let routed = buildRoutedEdges input
  transformEdges input.edges routed withNodes

initial :: TransformInput -> TransformOutput
initial _ =
  { cGraph: newCGraph (S.fromFoldable [ UNDEFINED, LEFT, RIGHT ])
  , nodeToC: M.empty
  , edgeToCs: M.empty
  , lockMap: M.empty
  }

-- | Per-node `(incoming /\ outgoing)` edge counts driving the
-- | direction-asymmetry lock per ELK LGraphToCGraphTransformer.java:168-174.
edgeDegrees :: Array Edge -> Map NodeId (Int /\ Int)
edgeDegrees edges = foldl tally M.empty edges
  where
  tally acc e =
    M.insertWith addPair e.to.node (1 /\ 0)
      (M.insertWith addPair e.from.node (0 /\ 1) acc)
  addPair (a /\ b) (c /\ d) = (a + c) /\ (b + d)

transformNodes :: Map NodeId (Int /\ Int) -> Array R.NodePlacement -> TransformOutput -> TransformOutput
transformNodes degrees nodes out = foldl placeNode out nodes
  where
  placeNode acc np = do
    let added = addCNode { origin: Just (NodeOrigin np.node), kind: Nothing, hitbox: hitboxFor np } acc.cGraph
    let group = addCGroup { master: Just added.id, nodes: [ added.id ] } added.graph
    let inc /\ outd = fromMaybe (0 /\ 0) (M.lookup np.node degrees)
    let lock = nodeLockFor (inc - outd)
    acc
      { cGraph = group.graph
      , nodeToC = M.insert np.node added.id acc.nodeToC
      , lockMap = M.insert added.id lock acc.lockMap
      }

-- | ELK lines 168-174: difference = incoming - outgoing; <0 locks
-- | LEFT, >0 locks RIGHT. Used by the (not-yet-ported)
-- | LEFT_RIGHT_CONNECTION_LOCKING strategy.
nodeLockFor :: Int -> Quadruplet
nodeLockFor difference
  | difference < 0 = emptyQuadruplet { left = true }
  | difference > 0 = emptyQuadruplet { right = true }
  | otherwise = emptyQuadruplet

-- | Node positions enter in node-grid units, edge segments live in
-- | router-grid (`scaleFactor`-finer). The compactor's spacing/delta
-- | math only makes sense when every CNode shares one unit system, so
-- | we promote node hitboxes to router-grid here and divide back out
-- | in `applyLayout`.
hitboxFor :: R.NodePlacement -> Rect
hitboxFor np =
  { x: gridX np.position * sf
  , y: gridY np.position * sf
  , width: sizeW np.size * sf
  , height: sizeH np.size * sf
  }
  where
  sf = toNumber scaleFactor

----------------------------------------------------------------
-- transformEdges + vertical segments
----------------------------------------------------------------

transformEdges :: Array Edge -> Array RoutedEdge -> TransformOutput -> TransformOutput
transformEdges allEdges routed out = mergeAndPlace allEdges (collectSegments routed out) out

collectSegments :: Array RoutedEdge -> TransformOutput -> Array VerticalSegment
collectSegments edges out = _.segments $
  foldl (collectForEdge out) { nextId: 0, segments: [] } edges

collectForEdge
  :: TransformOutput
  -> { nextId :: Int, segments :: Array VerticalSegment }
  -> RoutedEdge
  -> { nextId :: Int, segments :: Array VerticalSegment }
collectForEdge out s0 e
  -- Self-loops are routed separately and are not part of the compaction
  -- graph (ELK LGraphToCGraphTransformer.java:478). Collecting their
  -- vertical segment lets the simplex slide the loop's far edge away
  -- from its node, breaking the C-shape. Mirror the line 427 skip.
  | e.src == e.tgt = s0
collectForEdge out s0 e = do
  let mSrcId = M.lookup e.src out.nodeToC
  let mTgtId = M.lookup e.tgt out.nodeToC
  let mSrcHB = mSrcId >>= \nid -> lookupCNode nid out.cGraph <#> _.hitbox
  let mTgtHB = mTgtId >>= \nid -> lookupCNode nid out.cGraph <#> _.hitbox
  let verticals = verticalSegmentsOnPath e.path
  let lastIdx = A.length verticals - 1
  let
    placed = foldl
      (placeSeg e mSrcHB mTgtHB lastIdx)
      s0
      (A.mapWithIndex (\i seg -> i /\ seg) verticals)
  -- ELK mirrors edge.getSource()/getTarget() being on NORTH/SOUTH ports
  -- by appending a synthetic VS anchored to the node edge with the
  -- node-facing ignoreSpacing side set. Port:LGraphToCGraphTransformer.java:234-247, 296-317.
  let
    afterSrcNS = case e.srcSide, A.head verticals, mSrcId, mSrcHB of
      North, Just first, Just nid, Just hb -> appendSourceNS e nid hb first { side: North, down: true } placed
      South, Just first, Just nid, Just hb -> appendSourceNS e nid hb first { side: South, down: false } placed
      _, _, _, _ -> placed
  case e.tgtSide, A.last verticals, mTgtId, mTgtHB of
    North, Just last_, Just nid, Just hb -> appendTargetNS e nid hb last_ { side: North, down: true } afterSrcNS
    South, Just last_, Just nid, Just hb -> appendTargetNS e nid hb last_ { side: South, down: false } afterSrcNS
    _, _, _, _ -> afterSrcNS

verticalSegmentsOnPath :: R.EdgePath -> Array { start :: GridPos, end :: GridPos }
verticalSegmentsOnPath p = A.mapMaybe asVertical p.segments
  where
  asVertical :: R.EdgeSegment -> Maybe { start :: GridPos, end :: GridPos }
  asVertical seg = case seg.direction of
    R.V -> Just { start: seg.start, end: seg.end }
    R.H -> Nothing

placeSeg
  :: RoutedEdge
  -> Maybe Rect
  -> Maybe Rect
  -> Int
  -> { nextId :: Int, segments :: Array VerticalSegment }
  -> Int /\ { start :: GridPos, end :: GridPos }
  -> { nextId :: Int, segments :: Array VerticalSegment }
placeSeg e mSrcHB mTgtHB lastIdx s (i /\ seg) = do
  let isFirst = i == 0
  let isLast = i == lastIdx
  let vs0 = newVerticalSegment s.nextId seg.start seg.end Nothing e.edgeId
  let vs1 = if isFirst then applyFirstRegularFlags vs0 mSrcHB seg.end else vs0
  let vs2 = if isLast then applyLastRegularFlags vs1 mTgtHB seg.start else vs1
  { nextId: s.nextId + 1, segments: s.segments <> [ vs2 ] }

nsSide :: Side -> Boolean
nsSide North = true
nsSide South = true
nsSide _ = false

-- | ELK LGraphToCGraphTransformer.java:257-271. The first vertical segment
-- | of an outgoing edge has its `ignoreSpacing` set based on where bend2
-- | (= seg.end) sits relative to the source node.
applyFirstRegularFlags :: VerticalSegment -> Maybe Rect -> GridPos -> VerticalSegment
applyFirstRegularFlags vs Nothing _ = vs
applyFirstRegularFlags vs (Just hb) bend2 = vs { ignoreSpacing = flagFor hb bend2 vs.ignoreSpacing }

-- | ELK LGraphToCGraphTransformer.java:280-291. The last vertical segment
-- | uses bend1 (= seg.start) against the target node.
applyLastRegularFlags :: VerticalSegment -> Maybe Rect -> GridPos -> VerticalSegment
applyLastRegularFlags vs Nothing _ = vs
applyLastRegularFlags vs (Just hb) bend1 = vs { ignoreSpacing = flagFor hb bend1 vs.ignoreSpacing }

flagFor :: Rect -> GridPos -> Quadruplet -> Quadruplet
flagFor hb bend q
  | gridY bend < hb.y = q { down = true }
  | gridY bend > hb.y + hb.height = q { up = true }
  | otherwise = q { up = true, down = true }

-- | Append a synthetic VS anchored from the first bend (port position)
-- | down/up to the source node edge, mirroring ELK lines 234-247.
-- | `info.side` records the port, `info.down` says which ignoreSpacing
-- | flag faces the node interior (NORTH ports anchor onto node.top → the
-- | DOWN side of the synthetic VS touches the node).
appendSourceNS
  :: RoutedEdge
  -> CNodeId
  -> Rect
  -> { start :: GridPos, end :: GridPos }
  -> { side :: Side, down :: Boolean }
  -> { nextId :: Int, segments :: Array VerticalSegment }
  -> { nextId :: Int, segments :: Array VerticalSegment }
appendSourceNS e nid hb firstSeg info s = do
  let anchor = anchorY hb info
  let bend1 = firstSeg.start
  let
    vs = (newVerticalSegment s.nextId bend1 (movedY anchor bend1) (Just nid) e.edgeId)
      { aPort = Just { node: e.src, side: info.side }
      , ignoreSpacing = setFacing info emptyIgnore
      }
  { nextId: s.nextId + 1, segments: s.segments <> [ vs ] }

-- | Mirror of `appendSourceNS` for the target end (ELK lines 296-317).
appendTargetNS
  :: RoutedEdge
  -> CNodeId
  -> Rect
  -> { start :: GridPos, end :: GridPos }
  -> { side :: Side, down :: Boolean }
  -> { nextId :: Int, segments :: Array VerticalSegment }
  -> { nextId :: Int, segments :: Array VerticalSegment }
appendTargetNS e nid hb lastSeg info s = do
  let anchor = anchorY hb info
  let bend1 = lastSeg.end
  let
    vs = (newVerticalSegment s.nextId bend1 (movedY anchor bend1) (Just nid) e.edgeId)
      { aPort = Just { node: e.tgt, side: info.side }
      , ignoreSpacing = setFacing info emptyIgnore
      }
  { nextId: s.nextId + 1, segments: s.segments <> [ vs ] }

anchorY :: Rect -> { side :: Side, down :: Boolean } -> Number
anchorY hb info = if info.down then hb.y else hb.y + hb.height

movedY :: Number -> GridPos -> GridPos
movedY y p = GridPos (gridX p /\ y)

emptyIgnore :: Quadruplet
emptyIgnore = emptyQuadruplet

setFacing :: { side :: Side, down :: Boolean } -> Quadruplet -> Quadruplet
setFacing info q = if info.down then q { down = true } else q { up = true }

----------------------------------------------------------------
-- merge segments + create CNodes
----------------------------------------------------------------

mergeAndPlace :: Array Edge -> Array VerticalSegment -> TransformOutput -> TransformOutput
mergeAndPlace allEdges segments out = case A.uncons (A.sortBy compareVS segments) of
  Nothing -> out
  Just { head, tail } -> do
    let result = foldl step { survivor: head, merged: [] } tail
    let final = result.merged <> [ result.survivor ]
    foldl (placeMerged allEdges) out final
  where
  step st next =
    if intersects st.survivor next then st { survivor = joinWith st.survivor next }
    else { survivor: next, merged: st.merged <> [ st.survivor ] }

placeMerged :: Array Edge -> TransformOutput -> VerticalSegment -> TransformOutput
placeMerged allEdges out vs = do
  let
    added = addCNode
      { origin: Just (SegmentOrigin vs)
      , kind: Just "vs"
      , hitbox: vs.hitbox
      }
      out.cGraph
  let cgWithFlags = setCNodeIgnoreSpacing added.id vs.ignoreSpacing added.graph
  let
    cg' = case A.head vs.potentialGroupParents of
      Just parentId -> case lookupCNode parentId cgWithFlags of
        Just parent -> case parent.cGroup of
          Just gid -> addCNodeToGroup added.id gid cgWithFlags
          Nothing -> cgWithFlags
        Nothing -> cgWithFlags
      Nothing -> (addCGroup { master: Just added.id, nodes: [ added.id ] } cgWithFlags).graph
  out
    { cGraph = cg'
    , edgeToCs = foldl
        (\m eid -> M.insertWith (<>) eid [ added.id ] m)
        out.edgeToCs
        vs.representedEdges
    , lockMap = M.insert added.id (vsLockFor allEdges vs.representedEdges) out.lockMap
    }

-- | ELK lines 408-421. Counts distinct source vs target ports across
-- | the VS's `representedLEdges`. Fewer source ports than target
-- | ports → lock LEFT (and unlock RIGHT), and vice versa.
vsLockFor :: Array Edge -> Array EdgeId -> Quadruplet
vsLockFor allEdges representedEdges = case compare incSize outSize of
  LT -> emptyQuadruplet { left = true, right = false }
  GT -> emptyQuadruplet { left = false, right = true }
  EQ -> emptyQuadruplet
  where
  byId = M.fromFoldable (allEdges <#> \e -> e.id /\ e)
  myEdges = A.mapMaybe (\eid -> M.lookup eid byId) representedEdges
  incSize = S.size (S.fromFoldable (myEdges <#> \e -> e.from.node /\ e.from.port))
  outSize = S.size (S.fromFoldable (myEdges <#> \e -> e.to.node /\ e.to.port))

----------------------------------------------------------------
-- buildRoutedEdges
----------------------------------------------------------------

buildRoutedEdges :: TransformInput -> Array RoutedEdge
buildRoutedEdges input = A.mapMaybe pair input.paths
  where
  edgeById = M.fromFoldable (input.edges <#> \e -> e.id /\ e)

  -- | ELK works on the post-cycle-break "layout direction": for an edge
  -- | the cycle breaker reversed, the layout source is the logical
  -- | target and vice versa. The routed segments are stored in layout
  -- | direction (see `shiftSegments`' `reversed` branch), so the
  -- | compactor needs src/tgt and the port sides flipped too —
  -- | otherwise the edge-length minimization pulls the higher layer
  -- | DOWN toward the lower layer and collapses layering.
  pair p = do
    e <- M.lookup p.edge edgeById
    let
      srcN /\ srcPort /\ tgtN /\ tgtPort =
        if p.reversed then e.to.node /\ e.to.port /\ e.from.node /\ e.from.port
        else e.from.node /\ e.from.port /\ e.to.node /\ e.to.port
    Just
      { edgeId: p.edge
      , src: srcN
      , tgt: tgtN
      , srcSide: portSide East input.ports srcN srcPort
      , tgtSide: portSide West input.ports tgtN tgtPort
      , path: p
      }

-- | The compactor frame is horizontal: a *normal* source (output) port
-- | sits EAST and a normal target (input) port WEST. Only the opposite
-- | sides (WEST output / EAST input) are inverted ports — the only case
-- | ELK's `addEdgeConstraints` emits its delta=1 pull edges for. Edges
-- | that carry no explicit `LPort` (the common case) must therefore
-- | default to their *normal* side per endpoint, otherwise every
-- | forward edge would masquerade as an EAST-input inverted port and
-- | drag its vertical segments up against the source node.
portSide :: Side -> Map NodeId (Array Port) -> NodeId -> Maybe PortId -> Side
portSide dflt portsMap node mPortId = fromMaybe dflt $ do
  pid <- mPortId
  ports <- M.lookup node portsMap
  p <- A.find (\pt -> pt.id == pid) ports
  Just p.side

----------------------------------------------------------------
-- hooks for NetworkSimplexCompaction
----------------------------------------------------------------

buildHooks :: TransformOutput -> Array RoutedEdge -> CompactionHooks CNodeOrigin
buildHooks out routedEdges =
  { sameEdgeVerticalSegments
  , vsLNodePair
  , edgeLengthEdges: \_ -> extraEdges
  }
  where
  sameEdgeVerticalSegments a b = case a.origin, b.origin of
    Just (SegmentOrigin va), Just (SegmentOrigin vb) ->
      A.any (\e -> A.elem e vb.representedEdges) va.representedEdges
    _, _ -> false

  vsLNodePair :: CNode CNodeOrigin -> CNode CNodeOrigin -> Boolean
  vsLNodePair a b = case a.origin, b.origin of
    Just (SegmentOrigin _), Just (NodeOrigin _) -> true
    Just (NodeOrigin _), Just (SegmentOrigin _) -> true
    _, _ -> false

  extraEdges :: Array ExtraEdge
  extraEdges = routedEdges >>= edgesForLEdge

  -- | ELK NetworkSimplexCompaction.java:199-289. One main edge per
  -- | LEdge pulling source-group → target-group with high weight,
  -- | plus extra delta=1 edges for inverted ports — WEST source
  -- | (output) pulls each VS-CNode left of source close, and EAST
  -- | target (input) pulls each VS-CNode right of source close.
  edgesForLEdge :: RoutedEdge -> Array ExtraEdge
  edgesForLEdge re
    | re.src == re.tgt = [] -- skip self-loops (ELK isSelfLoop check)
    | nsSide re.srcSide && nsSide re.tgtSide = []
    | otherwise =
        case mainEdge of
          Nothing -> []
          Just edge -> [ edge ] <> invertedSourceEdges <> invertedTargetEdges
        where
        mSrcN = M.lookup re.src out.nodeToC
        mTgtN = M.lookup re.tgt out.nodeToC
        mSrcCN = mSrcN >>= \nid -> lookupCNode nid out.cGraph
        mTgtCN = mTgtN >>= \nid -> lookupCNode nid out.cGraph
        mainEdge = do
          srcCN <- mSrcCN
          tgtCN <- mTgtCN
          sg <- srcCN.cGroup
          tg <- tgtCN.cGroup
          Just { srcGroup: sg, tgtGroup: tg, delta: 0, weight: edgeWeight }

        edgeVSCNodes :: Array (CNode CNodeOrigin)
        edgeVSCNodes = A.mapMaybe (\nid -> lookupCNode nid out.cGraph)
          (fromMaybe [] (M.lookup re.edgeId out.edgeToCs))

        invertedSourceEdges = case mSrcCN, re.srcSide of
          Just srcCN, West ->
            A.mapMaybe (invertedEdge (\vsX -> vsX < srcCN.hitbox.x) toSrc) edgeVSCNodes
            where
            toSrc vsGroup sg = { srcGroup: vsGroup, tgtGroup: sg, delta: 1, weight: edgeWeight }
          _, _ -> []

        invertedTargetEdges = case mSrcCN, re.tgtSide of
          Just srcCN, East ->
            A.mapMaybe (invertedEdge (\vsX -> vsX > srcCN.hitbox.x) fromSrc) edgeVSCNodes
            where
            fromSrc vsGroup sg = { srcGroup: sg, tgtGroup: vsGroup, delta: 1, weight: edgeWeight }
          _, _ -> []

        invertedEdge
          :: (Number -> Boolean)
          -> (CGroupId -> CGroupId -> ExtraEdge)
          -> CNode CNodeOrigin
          -> Maybe ExtraEdge
        invertedEdge pred mk vsCN = do
          srcCN <- mSrcCN
          sg <- srcCN.cGroup
          vsg <- vsCN.cGroup
          _ <- if pred vsCN.hitbox.x && vsg /= sg then Just unit else Nothing
          Just (mk vsg sg)

----------------------------------------------------------------
-- applyLayout
----------------------------------------------------------------

applyLayout
  :: CGraph CNodeOrigin
  -> { nodes :: Array R.NodePlacement, edges :: Array Edge, paths :: Array R.EdgePath }
  -> { nodes :: Array R.NodePlacement, edges :: Array R.EdgePath }
-- | Port of ELK's `LGraphToCGraphTransformer.applyLayout`. After the
-- | compactor finishes, every CNode carries a `hitbox.x` and its
-- | `hitboxPreCompaction.x` from before compaction. ELK applies the
-- | delta in two passes: LNodes get their position rewritten, and
-- | each VS-origin CNode shifts the bends it represents by
-- | `hitbox.x - hitboxPreCompaction.x`. We mirror that here, except
-- | (a) node positions are demoted back from router-grid to node-grid
-- | (the unit they live in outside the compactor) and (b) we update
-- | segment endpoints by matching their pre-compaction position
-- | against the VS's `affectedBends` instead of mutating shared
-- | references (which markgraf's value-typed paths don't have).
applyLayout cg input =
  { nodes: input.nodes <#> \np ->
      case M.lookup np.node nodeXs of
        Just newX -> np { position = mkPos (newX / sf) (gridY np.position) }
        Nothing -> np
  , edges: input.paths <#> shiftPath
  }
  where
  sf = toNumber scaleFactor
  nodeXs = collectNodeXs cg
  nodeDeltas = collectNodeDeltas cg
  bendDeltas = collectBendDeltas cg
  edgeEndpoints = M.fromFoldable (input.edges <#> \e -> e.id /\ (e.from.node /\ e.to.node))

  shiftPath ep = do
    let segments' = shiftSegments ep.reversed ep.edge ep.segments
    ep
      { segments = segments'
      , bends = bendsFromSegments segments'
      }

  bendsFromSegments segs = A.zipWith (\s _ -> s.end) segs (A.drop 1 segs)

  -- `stitchChains` reverses segments for cycle-broken edges, so
  -- segments[0].start sits geometrically at the original *target* and
  -- segments[-1].end at the original *source*. Flip the deltas here
  -- so each endpoint follows the node it's actually attached to.
  shiftSegments reversed eid segments = case M.lookup eid edgeEndpoints of
    Nothing -> segments
    Just (src /\ tgt) -> do
      let srcD = fromMaybe 0.0 (M.lookup src nodeDeltas)
      let tgtD = fromMaybe 0.0 (M.lookup tgt nodeDeltas)
      let firstDx = if reversed then tgtD else srcD
      let lastDx = if reversed then srcD else tgtD
      let n = A.length segments
      A.mapWithIndex (shiftOne firstDx lastDx n) segments

  shiftOne firstDx lastDx n i seg = case seg.direction of
    R.V -> do
      let
        dx
          | i == 0 = firstDx
          | i == n - 1 = lastDx
          | otherwise = bendDx seg.start
      seg { start = shiftX dx seg.start, end = shiftX dx seg.end }
    R.H -> do
      let
        ds
          | i == 0 = firstDx
          | otherwise = bendDx seg.start
        de
          | i == n - 1 = lastDx
          | otherwise = bendDx seg.end
      seg { start = shiftX ds seg.start, end = shiftX de seg.end }

  bendDx p = fromMaybe 0.0 (M.lookup p bendDeltas)

  shiftX dx p = mkPos (gridX p + dx) (gridY p)

collectNodeXs :: CGraph CNodeOrigin -> Map NodeId Number
collectNodeXs cg = foldl step M.empty (allCNodes cg)
  where
  step m n = case n.origin of
    Just (NodeOrigin nid) -> M.insert nid n.hitbox.x m
    _ -> m

collectNodeDeltas :: CGraph CNodeOrigin -> Map NodeId Number
collectNodeDeltas cg = foldl step M.empty (allCNodes cg)
  where
  step m n = case n.origin of
    Just (NodeOrigin nid) -> M.insert nid (n.hitbox.x - n.hitboxPreCompaction.x) m
    _ -> m

-- | Port of ELK's `applyLayout` VS pass: each VS-origin CNode shifts
-- | every `affectedBend` it knows about by `hitbox.x -
-- | hitboxPreCompaction.x`. We index by the pre-compaction GridPos
-- | so `shiftSegments` can look up a corner's delta in O(log n).
collectBendDeltas :: CGraph CNodeOrigin -> Map GridPos Number
collectBendDeltas cg = foldl step M.empty (allCNodes cg)
  where
  step m n = case n.origin of
    Just (SegmentOrigin vs) -> do
      let dx = n.hitbox.x - n.hitboxPreCompaction.x
      foldl (\acc b -> M.insert b dx acc) m vs.affectedBends
    _ -> m

mkPos :: Number -> Number -> GridPos
mkPos x y = GridPos (x /\ y)
