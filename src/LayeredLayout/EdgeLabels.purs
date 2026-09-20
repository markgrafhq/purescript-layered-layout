-- Copyright (c) 2012, 2020 Kiel University and others.
-- SPDX-License-Identifier: EPL-2.0
--
-- Functional translation of ELK's LabelDummyInserter, LabelDummySwitcher
-- (MEDIAN_LAYER), LabelSideSelector (SMART_DOWN), and LabelDummyRemover:
-- https://github.com/eclipse-elk/elk/tree/c831ba4613dfd6b0055851193956560351d2f907/plugins/org.eclipse.elk.alg.layered/src/org/eclipse/elk/alg/layered/intermediate
--
-- The public model supplies a measured CENTER or source-owned TAIL label per
-- edge. Labels use coarse input sizes; routes and final labels use fine units.
-- Frame conversion lives in dummySize, dummyPortOffset, and placements.
module LayeredLayout.EdgeLabels
  ( EndLabelAlignment(..)
  , LabelPlacement(..)
  , EdgeLabelSpec
  , LabelState
  , LabelDummy
  , LabelSide(..)
  , insert
  , TailCell
  , TailCells
  , prepareTailCells
  , routingObstacles
  , placements
  , portOffsets
  , reservedPlacements
  , selectSides
  , switchDummies
  , tailPlacements
  , restore
  ) where

import Prelude

import Data.Array as A
import Data.Array (concatMap, drop, filter, find, head, index, last, length, mapMaybe, null, reverse, snoc, take, takeWhile, uncons, zipWith)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import LayeredLayout.CoordAssignment (NodeMargins)
import LayeredLayout.DummyNodes (DummyResult, isDummy)
import LayeredLayout.EdgeRouting (scaleFactor)
import LayeredLayout.Graph (Edge, EdgeId(..), Graph, Node, NodeId(..), Shape(..), Side(..))
import LayeredLayout.Grid (GridPos(..), GridSize(..), gridX, gridY, sizeH, sizeW)
import LayeredLayout.PortDistribution (EdgePortOffsets, offsetFor)
import LayeredLayout.Result (Direction(..), EdgeLabelPlacement, EdgePath, NodePlacement)

-- Sides are named in ELK's normalized RIGHT frame. In DOWN, Above is
-- right of the edge and Below is left; the order within a layer reverses.
-- | TAIL labels follow their authored source even when cycle removal reverses
-- | the routing direction. `Adjacent` is ELK's end-label behaviour; the
-- | explicit terminal-run mode is a renderer-neutral extension.
data EndLabelAlignment = Adjacent | CenterTerminalRun

derive instance Eq EndLabelAlignment
derive instance Ord EndLabelAlignment

data LabelPlacement = Center | Tail EndLabelAlignment

derive instance Eq LabelPlacement
derive instance Ord LabelPlacement

type EdgeLabelSpec =
  { size :: GridSize
  , placement :: LabelPlacement
  }

data LabelSide = Above | Below

type LabelDummy =
  { node :: NodeId
  , edge :: Edge
  , tail :: EdgeId
  , size :: GridSize
  , side :: LabelSide
  }

type LabelState =
  { specs :: Map EdgeId EdgeLabelSpec
  , reservations :: Map EdgeId NodeId
  , dummies :: Array LabelDummy
  , nodes :: Array Node
  , edges :: Array Edge
  }

-- LabelDummyInserter.process: only CENTER labels become dummy nodes. TAIL
-- labels reserve their source frame after port ordering and are materialized
-- from the final, restored route.
insert :: Map EdgeId EdgeLabelSpec -> Graph -> Array Edge -> LabelState
insert specs _ edges | Map.isEmpty specs = { specs, reservations: Map.empty, dummies: [], nodes: [], edges }
insert specs graph edges = (foldl add initial edges).labels
  where
  initial =
    { labels: { specs, reservations: Map.empty, dummies: [], nodes: [], edges: [] }
    , usedNodes: Set.fromFoldable (graph.nodes <#> _.id)
    , usedEdges: Set.fromFoldable (graph.edges <#> _.id)
    }
  freshNode occupied text =
    if Set.member (NodeId text) occupied then freshNode occupied (text <> "'") else NodeId text
  freshEdge occupied text =
    if Set.member (EdgeId text) occupied then freshEdge occupied (text <> "'") else EdgeId text
  add acc edge = case Map.lookup edge.id specs of
    Just { size, placement: Center } | edge.from.node /= edge.to.node -> do
      let node = freshNode acc.usedNodes ("$label:" <> un EdgeId edge.id)
      let tail = freshEdge acc.usedEdges ("$label-tail:" <> un EdgeId edge.id)
      let dummy = { node, edge, tail, size, side: Below }
      let n = { id: node, size: dummySize size, ports: [], label: Nothing, shape: Rectangle }
      acc
        { usedNodes = Set.insert node acc.usedNodes
        , usedEdges = Set.insert tail acc.usedEdges
        , labels = acc.labels
            { dummies = snoc acc.labels.dummies
                dummy
            , nodes = snoc acc.labels.nodes n
            , edges = acc.labels.edges <>
                [ edge { to = { node, port: Nothing }, label = Nothing }
                , edge { id = tail, from = { node, port: Nothing }, label = Nothing }
                ]
            }
        }
    Just { placement: Tail _ } ->
      let
        reservation = freshNode acc.usedNodes ("$tail-label:" <> un EdgeId edge.id)
      in
        acc
          { usedNodes = Set.insert reservation acc.usedNodes
          , labels = acc.labels
              { reservations = Map.insert edge.id reservation acc.labels.reservations
              , edges = snoc acc.labels.edges edge
              }
          }
    _ -> acc { labels = acc.labels { edges = snoc acc.labels.edges edge } }

-- Existing layout defaults, in fine units. With one label there is no
-- label-label spacing to retain from LabelDummyInserter's stacked extent.
edgeLabelSpacing :: Number
edgeLabelSpacing = 2.0

edgeThickness :: Number
edgeThickness = 1.0

fineScale :: Number
fineScale = toNumber scaleFactor

dummySize :: GridSize -> GridSize
dummySize size = GridSize ((sizeW size + (edgeLabelSpacing + edgeThickness) / fineScale) /\ sizeH size)

-- LabelDummySwitcher.findMedianLayerTargetId / swapNodes. The chain includes
-- both real endpoints, so its lower interior median is (length - 1) / 2.
-- Swapping identities preserves the layer slots established by crossing min.
-- Our chain adapter rebuilds segment IDs after rewiring; its two independent
-- halves also encode ELK's LONG_EDGE_BEFORE_LABEL_DUMMY boundary.
switchDummies :: LabelState -> DummyResult -> { labels :: LabelState, dummies :: DummyResult }
switchDummies labels dummies = foldl switchOne { labels, dummies } labels.dummies
  where
  switchOne acc label = case find (\c -> c.edgeId == label.edge.id) acc.dummies.chains, find (\c -> c.edgeId == label.tail) acc.dummies.chains of
    Just firstChain, Just lastChain -> do
      let chain = firstChain.nodes <> drop 1 lastChain.nodes
      let middle = (length chain - 1) `div` 2
      case index chain middle of
        Just target | target /= label.node -> do
          let
            swap n
              | n == label.node = target
              | n == target = label.node
              | otherwise = n
          let nodes = map swap chain
          let left = take (middle + 1) nodes
          let right = drop middle nodes
          let
            chains = acc.dummies.chains <#> \c ->
              if c.edgeId == label.edge.id then c { nodes = left }
              else if c.edgeId == label.tail then c { nodes = right }
              else c
          let replaced = Set.fromFoldable (firstChain.nodes <> lastChain.nodes)
          let retained = filter (\e -> not (Set.member e.from.node replaced && Set.member e.to.node replaced && (isDummy e.from.node || isDummy e.to.node || e.from.node == label.node || e.to.node == label.node))) acc.dummies.edges
          let halfHead = label.edge { to = { node: label.node, port: Nothing }, label = Nothing }
          let halfTail = label.edge { id = label.tail, from = { node: label.node, port: Nothing }, label = Nothing }
          acc
            { dummies = acc.dummies
                { layers = map (map swap) acc.dummies.layers
                , chains = chains
                , edges = retained <> chainEdges halfHead left <> chainEdges halfTail right
                }
            }
        _ -> acc
    _, _ -> acc

chainEdges :: Edge -> Array NodeId -> Array Edge
chainEdges edge nodes = zipWith make nodes (drop 1 nodes)
  where
  make a b = edge
    { id = if length nodes == 2 then edge.id else EdgeId (un EdgeId edge.id <> ":" <> un NodeId a <> "->" <> un NodeId b)
    , from = { node: a, port: if Just a == head nodes then edge.from.port else Nothing }
    , to = { node: b, port: if Just b == last nodes then edge.to.port else Nothing }
    }

-- LabelSideSelector.smart / smartForConsecutiveDummyNodeRun /
-- applyForDummyNodeRunWithSimpleLoops. Only CENTER labels are represented,
-- so smartForRegularNode's end-label decisions have no work in this model.
selectSides :: Array (Array NodeId) -> DummyResult -> LabelState -> LabelState
selectSides _ _ labels | null labels.dummies = labels
selectSides layers dummies labels = labels
  { dummies = labels.dummies <#> \d -> d { side = if Set.member d.node aboveNodes then Above else Below } }
  where
  labelNodes = Set.fromFoldable (labels.dummies <#> _.node)
  isLabel n = Set.member n labelNodes
  isVirtual n = isDummy n || isLabel n
  -- LongEdgeSplitter.setDummyProperties propagates the original endpoints
  -- through both halves, rather than treating the label as a real endpoint.
  endpoints = Map.fromFoldable $ concatMap
    ( \c ->
        let
          ends = case find (\d -> d.edge.id == c.edgeId || d.tail == c.edgeId) labels.dummies of
            Just d -> Just d.edge.from.node /\ Just d.edge.to.node
            Nothing -> head c.nodes /\ last c.nodes
        in
          c.nodes <#> \n -> n /\ ends
    )
    dummies.chains
  aboveNodes = foldl layerSides Set.empty layers
  layerSides selected layer = scan selected true (reverse layer)
  scan selected top rest = case uncons rest of
    Nothing -> selected
    Just { head: first, tail: remaining } | not (isVirtual first) -> scan selected false remaining
    _ -> do
      let run = takeWhile isVirtual rest
      let remaining = drop (length run) rest
      let bottom = null remaining
      let count = length (filter isLabel run)
      let
        selected' =
          if top && (not bottom || length run > 1) && count == 1 && fromMaybe false (isLabel <$> head run) then
            selectAbove (head run) selected
          else if bottom && (not top || length run > 1) && count == 1 && fromMaybe false (isLabel <$> last run) then selected
          else if length run == 2 then selectAbove (head run) selected
          else simpleRuns selected run
      scan selected' false remaining
  selectAbove Nothing selected = selected
  selectAbove (Just n) selected = if isLabel n then Set.insert n selected else selected
  simpleRuns selected rest = case uncons rest of
    Nothing -> selected
    Just { head: first } -> do
      let run = takeWhile (\n -> Map.lookup n endpoints == Map.lookup first endpoints) rest
      let selected' = if length run == 2 then selectAbove (head run) selected else selected
      simpleRuns selected' (drop (length run) rest)

-- LabelDummyInserter.createLabelDummy starts ports at floor(thickness/2).
-- LabelSideSelector.applyLabelSide moves ABOVE ports to extent-ceil(t/2).
-- Transforming their one-unit extent into DOWN yields these local offsets.
dummyPortOffset :: LabelDummy -> Number
dummyPortOffset d = case d.side of
  Above -> 0.0
  Below -> sizeW d.size * fineScale + edgeLabelSpacing

portOffsets :: LabelState -> Array Edge -> EdgePortOffsets -> EdgePortOffsets
portOffsets labels _ offsets | null labels.dummies = offsets
portOffsets labels edges offsets = foldl add offsets edges
  where
  byNode = Map.fromFoldable (labels.dummies <#> \d -> d.node /\ d)
  add acc edge = set edge.id North edge.to.node (set edge.id South edge.from.node acc)
  set eid side node acc = case Map.lookup node byNode of
    Nothing -> acc
    Just d -> Map.insert (eid /\ side) (dummyPortOffset d) acc

-- LabelDummyRemover.placeLabelsForVerticalLayout, transformed into DOWN.
-- A single label fills its longitudinal space; only its side offset remains.
placements :: LabelState -> Array NodePlacement -> Array EdgeLabelPlacement
placements labels _ | null labels.dummies = []
placements labels nodes = mapMaybe place labels.dummies
  where
  byNode = Map.fromFoldable (nodes <#> \n -> n.node /\ n)
  place d = Map.lookup d.node byNode <#> \n ->
    let
      sideOffset = case d.side of
        Above -> edgeThickness + edgeLabelSpacing
        Below -> 0.0
    in
      { edge: d.edge.id
      , position: GridPos ((gridX n.position * fineScale + sideOffset) /\ (gridY n.position * fineScale))
      , size: GridSize (sizeW d.size * fineScale /\ sizeH d.size * fineScale)
      }

-- | A source-owned end label is placed beside, rather than on, its departure
-- | segment. Reserve enough of the source frame for that sidecar before BK and
-- | routing run. Reservations with overlapping terminal tracks stack only as
-- | needed, so their obstacle boxes are non-overlapping without wasting space.
type TailEntry =
  { edge :: Edge
  , size :: GridSize
  , alignment :: EndLabelAlignment
  }

zeroMargin :: { left :: Number, right :: Number, top :: Number, bottom :: Number }
zeroMargin = { left: 0.0, right: 0.0, top: 0.0, bottom: 0.0 }

visualMargin :: NodeMargins -> NodeId -> { left :: Number, right :: Number, top :: Number, bottom :: Number }
visualMargin visual node = fromMaybe zeroMargin (Map.lookup node visual)

tailEntries :: Graph -> LabelState -> Array TailEntry
tailEntries graph labels = mapMaybe entry graph.edges
  where
  entry edge = case Map.lookup edge.id labels.specs of
    Just { size, placement: Tail alignment } -> Just { edge, size, alignment }
    _ -> Nothing

type TailCell =
  { edge :: EdgeId
  , reservation :: NodeId
  , source :: NodeId
  , side :: Side
  , x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  }

type TailCells =
  { cells :: Array TailCell
  , margins :: NodeMargins
  }

-- | Measure every tail cell in its authored source's physical coordinate
-- | system before coordinate assignment. The resulting envelope feeds layer
-- | sizing; materialization later is only a translation by the source's
-- | physical placement, so it cannot grow a planned routing gap afterwards.
prepareTailCells
  :: NodeMargins
  -> Graph
  -> LabelState
  -> Array { edgeId :: EdgeId, nodes :: Array NodeId }
  -> EdgePortOffsets
  -> Map NodeId GridSize
  -> TailCells
prepareTailCells visual graph labels chains offsets sizes = case tailEntries graph labels of
  [] -> { cells: [], margins: Map.empty }
  entries -> measureEntries entries
  where
  measureEntries entries =
    { cells: measured.cells
    , margins: foldl addEnvelope Map.empty measured.cells
    }
    where
    chainsByEdge = Map.fromFoldable (chains <#> \chain -> chain.edgeId /\ chain)

    measured = foldl measure { cells: [] } entries

    sourceAnchor edge = case sourcePort edge of
      Just anchor -> Just anchor
      Nothing -> routedAnchor edge

    portAnchor endpoint = do
      portId <- endpoint.port
      node <- find (\candidate -> candidate.id == endpoint.node) graph.nodes
      port <- find (\candidate -> candidate.id == portId) node.ports
      pure
        { side: port.side
        , offset: toNumber port.offset * fineScale
        }

    sourcePort edge = portAnchor edge.from

    chainFor edge = fromMaybe { edgeId: edge.id, nodes: [ edge.from.node, edge.to.node ] } (Map.lookup edge.id chainsByEdge)

    segmentIds chain
      | length chain.nodes <= 2 = [ chain.edgeId ]
      | otherwise = zipWith
          (\source target -> EdgeId (un EdgeId chain.edgeId <> ":" <> un NodeId source <> "->" <> un NodeId target))
          chain.nodes
          (drop 1 chain.nodes)

    endpointAnchor node side segment = do
      sourceSize <- Map.lookup node sizes
      let width = sizeW sourceSize * fineScale
      let height = sizeH sourceSize * fineScale
      pure
        { side
        , offset: case side of
            North -> offsetFor offsets segment North (width / 2.0)
            South -> offsetFor offsets segment South (width / 2.0)
            East -> height / 2.0
            West -> height / 2.0
        }

    routedAnchor edge = do
      let chain = chainFor edge
      let first = fromMaybe edge.from.node (head chain.nodes)
      let finalNode = fromMaybe edge.to.node (A.last chain.nodes)
      let ids = segmentIds chain
      let firstSegment = fromMaybe edge.id (head ids)
      let lastSegment = fromMaybe edge.id (A.last ids)
      if first == edge.from.node then
        endpointAnchor first South firstSegment
      else if finalNode == edge.from.node then
        endpointAnchor finalNode North lastSegment
      else
        endpointAnchor edge.from.node South edge.id

    -- Every route endpoint that leaves a source-local owner is a potential
    -- spine. This includes the routing source of a cycle-reversed edge at its
    -- authored target, as well as explicitly sided endpoints.
    routedEndpoints edge =
      let
        chain = chainFor edge
        first = fromMaybe edge.from.node (head chain.nodes)
        finalNode = fromMaybe edge.to.node (A.last chain.nodes)
        ids = segmentIds chain
        firstSegment = fromMaybe edge.id (head ids)
        lastSegment = fromMaybe edge.id (A.last ids)
        anchor node side segment =
          case
            (if node == edge.from.node then sourcePort edge else portAnchor edge.to)
            of
            Just explicit -> Just (node /\ explicit)
            Nothing -> (node /\ _) <$> endpointAnchor node side segment
      in
        mapMaybe identity
          [ anchor first South firstSegment
          , anchor finalNode North lastSegment
          ]

    allDepartures = foldl
      (\acc edge -> foldl (\next (node /\ anchor) -> Map.insertWith (<>) node [ anchor.offset ] next) acc (routedEndpoints edge))
      Map.empty
      graph.edges

    measure state entry = case Map.lookup entry.edge.from.node sizes, Map.lookup entry.edge.id labels.reservations, sourceAnchor entry.edge of
      Just sourceSize, Just reservation, Just anchor ->
        let
          source = entry.edge.from.node
          visual' = visualMargin visual source
          width = sizeW entry.size * fineScale
          height = sizeH entry.size * fineScale
          sourceWidth = sizeW sourceSize * fineScale
          sourceHeight = sizeH sourceSize * fineScale
          departures = fromMaybe [] (Map.lookup source allDepartures)
          right = anchor.offset + edgeLabelSpacing
          left = anchor.offset - edgeLabelSpacing - width
          -- The orthogonal router inflates obstacles by edgeLabelSpacing and
          -- accepts a spine exactly on that boundary. Use the same open interior
          -- policy while choosing a source-local side.
          x = case anchor.side of
            East -> sourceWidth + visual'.right + edgeLabelSpacing
            West -> -visual'.left - edgeLabelSpacing - width
            _ ->
              if A.any (\departure -> departure > right - edgeLabelSpacing && departure < right + width + edgeLabelSpacing) departures then left
              else right
          baseY = case anchor.side of
            North -> -visual'.top - edgeLabelSpacing - height
            _ -> sourceHeight + visual'.bottom + edgeLabelSpacing
          overlapsX cell = cell.source == source && cell.side == anchor.side && x < cell.x + cell.width && x + width > cell.x
          overlapping = filter overlapsX state.cells
          y = case anchor.side of
            North -> foldl (\top cell -> min top (cell.y - edgeLabelSpacing - height)) baseY overlapping
            _ -> foldl (\bottom cell -> max bottom (cell.y + cell.height + edgeLabelSpacing)) baseY overlapping
        in
          state { cells = snoc state.cells { edge: entry.edge.id, reservation, source, side: anchor.side, x, y, width, height } }
      _, _, _ -> state

    addEnvelope margins cell = case Map.lookup cell.source sizes of
      Nothing -> margins
      Just sourceSize ->
        let
          old = visualMargin margins cell.source
          sourceWidth = sizeW sourceSize * fineScale
          sourceHeight = sizeH sourceSize * fineScale
          next =
            { left: max old.left (-cell.x)
            , right: max old.right (cell.x + cell.width - sourceWidth)
            , top: max old.top (-cell.y)
            , bottom: max old.bottom (cell.y + cell.height - sourceHeight)
            }
        in
          Map.insert cell.source next margins

-- | Materialize pre-measured source-local cells after physical owner
-- | placements are known. Do not recalculate side choice or stacking here:
-- | the exact cells that sized the layer gaps must be the routing obstacles.
routingObstacles :: TailCells -> Array NodePlacement -> Array NodePlacement
routingObstacles tailCells nodes = mapMaybe materialize tailCells.cells
  where
  byNode = Map.fromFoldable (nodes <#> \node -> node.node /\ node)
  materialize cell = Map.lookup cell.source byNode <#> \source ->
    { node: cell.reservation
    , position: GridPos ((gridX source.position * fineScale + cell.x) / fineScale /\ (gridY source.position * fineScale + cell.y) / fineScale)
    , size: GridSize (cell.width / fineScale /\ cell.height / fineScale)
    , layer: source.layer
    , order: source.order
    }

reservedPlacements :: Graph -> LabelState -> Array NodePlacement -> Map EdgeId EdgeLabelPlacement
reservedPlacements graph labels nodes = Map.fromFoldable (mapMaybe placement (tailEntries graph labels))
  where
  byNode = Map.fromFoldable (nodes <#> \node -> node.node /\ node)
  placement entry = Map.lookup entry.edge.id labels.reservations >>= flip Map.lookup byNode <#> \node ->
    entry.edge.id /\
      { edge: entry.edge.id
      , position: GridPos (gridX node.position * fineScale /\ gridY node.position * fineScale)
      , size: GridSize (sizeW node.size * fineScale /\ sizeH node.size * fineScale)
      }

-- | Final source-owned placement runs after routing and compaction. Bent
-- | terminal runs center along their own axis; their compacted reservation is
-- | retained as the only fallback when route-adjacent candidates are blocked.
tailPlacements
  :: NodeMargins
  -> Graph
  -> LabelState
  -> Array NodePlacement
  -> Array EdgePath
  -> Map EdgeId EdgeLabelPlacement
  -> Array EdgeLabelPlacement
  -> Array EdgeLabelPlacement
tailPlacements visual graph labels nodes paths fallbacks existing = _.placed (foldl place initial entries)
  where
  entries = tailEntries graph labels
  nodeById = Map.fromFoldable (nodes <#> \node -> node.node /\ node)
  pathById = Map.fromFoldable (paths <#> \path -> path.edge /\ path)
  visualBoxes = nodes <#> visualBox
  routeSegments = A.concatMap _.segments paths
  reservationBoxes = A.fromFoldable (Map.values fallbacks)
  initial = { placed: [], occupied: existing }

  place state entry =
    let
      pendingReservations = filter (\reservation -> reservation.edge /= entry.edge.id) reservationBoxes
      fixedObstacles = visualBoxes <> pendingReservations
      clear candidate =
        not (anyOverlap candidate fixedObstacles)
          && not (anyOverlap candidate state.occupied)
          && not (anySegmentCrosses candidate routeSegments)
      accept candidate = state { placed = snoc state.placed candidate, occupied = snoc state.occupied candidate }
      fallback = Map.lookup entry.edge.id fallbacks
    in
      case Map.lookup entry.edge.from.node nodeById, Map.lookup entry.edge.id pathById of
        Just node, Just path -> case find clear (tailCandidates entry node path) of
          Just candidate -> accept candidate
          -- The compacted reservation is an engine constraint, not a best
          -- effort candidate. It remains the source-owned safe placement.
          Nothing -> case fallback of
            Just candidate -> accept candidate
            Nothing -> state
        _, _ -> case fallback of
          Just candidate -> accept candidate
          Nothing -> state

  visualBox node =
    let
      margin = visualMargin visual node.node
    in
      { edge: EdgeId ("$visual:" <> un NodeId node.node)
      , position: GridPos
          ( (gridX node.position * fineScale - margin.left) /\
              (gridY node.position * fineScale - margin.top)
          )
      , size: GridSize
          ( (sizeW node.size * fineScale + margin.left + margin.right) /\
              (sizeH node.size * fineScale + margin.top + margin.bottom)
          )
      }

  tailCandidates entry node path = case head path.segments of
    Nothing -> []
    Just first ->
      let
        centered = entry.alignment == CenterTerminalRun && length path.segments > 1
      in
        case first.direction of
          V -> verticalCandidates centered entry node first
          H -> horizontalCandidates centered entry node first

  verticalCandidates centered entry node segment =
    let
      margin = visualMargin visual node.node
      height = sizeH entry.size * fineScale
      width = sizeW entry.size * fineScale
      down = gridY segment.end >= gridY segment.start
      visible =
        if down then (gridY node.position + sizeH node.size) * fineScale + margin.bottom
        else gridY node.position * fineScale - margin.top
      terminal = gridY segment.end
      low = min visible terminal
      high = max visible terminal
      fitsRun = centered && high - low > height
      y =
        if fitsRun then low + (high - low - height) / 2.0
        else if down then visible + edgeLabelSpacing
        else visible - edgeLabelSpacing - height
      right = gridX segment.start + edgeLabelSpacing
      left = gridX segment.start - edgeLabelSpacing - width
    in
      [ labelAt entry right y, labelAt entry left y ]

  horizontalCandidates centered entry node segment =
    let
      margin = visualMargin visual node.node
      width = sizeW entry.size * fineScale
      height = sizeH entry.size * fineScale
      rightward = gridX segment.end >= gridX segment.start
      visible =
        if rightward then (gridX node.position + sizeW node.size) * fineScale + margin.right
        else gridX node.position * fineScale - margin.left
      terminal = gridX segment.end
      low = min visible terminal
      high = max visible terminal
      fitsRun = centered && high - low > width
      x =
        if fitsRun then low + (high - low - width) / 2.0
        else if rightward then visible + edgeLabelSpacing
        else visible - edgeLabelSpacing - width
      below = gridY segment.start + edgeLabelSpacing
      above = gridY segment.start - edgeLabelSpacing - height
    in
      [ labelAt entry x below, labelAt entry x above ]

  labelAt entry x y =
    { edge: entry.edge.id
    , position: GridPos (x /\ y)
    , size: GridSize (sizeW entry.size * fineScale /\ sizeH entry.size * fineScale)
    }

  anyOverlap label = foldl (\found other -> found || overlaps label other) false
  overlaps a b =
    gridX a.position < gridX b.position + sizeW b.size
      && gridX a.position + sizeW a.size > gridX b.position
      && gridY a.position < gridY b.position + sizeH b.size
      && gridY a.position + sizeH a.size > gridY b.position

  anySegmentCrosses label = foldl (\found segment -> found || segmentCrosses label segment) false
  segmentCrosses label segment
    | gridX segment.start == gridX segment.end =
        gridX segment.start > gridX label.position
          && gridX segment.start < gridX label.position + sizeW label.size
          && max (min (gridY segment.start) (gridY segment.end)) (gridY label.position)
            < min (max (gridY segment.start) (gridY segment.end)) (gridY label.position + sizeH label.size)
    | gridY segment.start == gridY segment.end =
        gridY segment.start > gridY label.position
          && gridY segment.start < gridY label.position + sizeH label.size
          && max (min (gridX segment.start) (gridX segment.end)) (gridX label.position)
            < min (max (gridX segment.start) (gridX segment.end)) (gridX label.position + sizeW label.size)
    | otherwise = false

-- LabelDummyRemover / LongEdgeJoiner.joinAt: join at the fixed ports through
-- the dummy, never at an edge midpoint. Final routing removes collinear bends.
restore :: LabelState -> Array EdgePath -> Array EdgePath
restore labels paths | null labels.dummies = paths
restore labels paths = mapMaybe (\p -> Map.lookup p.edge joined) paths
  where
  joined = foldl join (Map.fromFoldable (paths <#> \p -> p.edge /\ p)) labels.dummies
  join acc d = case Map.lookup d.edge.id acc, Map.lookup d.tail acc of
    Just firstPath, Just lastPath -> do
      let
        bridge = case last firstPath.segments, head lastPath.segments of
          Just a, Just b | a.end /= b.start -> [ { start: a.end, end: b.start, direction: V } ]
          _, _ -> []
      let segments = firstPath.segments <> bridge <> lastPath.segments
      Map.insert d.edge.id (firstPath { segments = segments }) (Map.delete d.tail acc)
    _, _ -> acc
