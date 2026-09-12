-- Copyright (c) 2011, 2012, 2015 Kiel University and others.
-- SPDX-License-Identifier: EPL-2.0
-- Functional translation of ELK ComponentsProcessor and SimpleRowGraphPlacer
-- at c831ba4613dfd6b0055851193956560351d2f907. Geometry here is coarse DOWN;
-- routes and labels are converted only at their public fine-grid boundary.
module LayeredLayout.Components (partition, restrict, pack, bounds, translate, moveNode) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (sqrt)
import Data.Set as S
import Data.Tuple.Nested ((/\))
import LayeredLayout.Graph (Constraints(..), Graph, NodeId)
import LayeredLayout.Grid (GridPos(..), GridRect, GridSize(..), gridX, gridY, sizeH, sizeW)
import LayeredLayout.Result (EdgeLabelPlacement, EdgePath, LayoutResult, NodePlacement)

-- Constraints are connections too: splitting an alignment or relative-position
-- group would make its independently computed coordinates meaningless.
partition :: Graph -> Array Graph
partition graph = groups <#> \ids -> restrict ids graph
  where
  links = (graph.edges <#> \e -> [ e.from.node, e.to.node ]) <> map constraintNodes graph.constraints
  adjacent = foldl addGroup M.empty links
  addGroup acc group = case A.uncons group of
    Nothing -> acc
    Just { head, tail } -> foldl (\m n -> M.insertWith (<>) head [ n ] (M.insertWith (<>) n [ head ] m)) acc tail
  groups = _.groups $ foldl visit { seen: S.empty, groups: [] } graph.nodes
  visit state n | S.member n.id state.seen = state
  visit state n =
    let
      ids = collect [ n.id ] S.empty
    in
      { seen: S.union state.seen ids, groups: A.snoc state.groups ids }
  collect queue seen = case A.uncons queue of
    Nothing -> seen
    Just { head, tail } | S.member head seen -> collect tail seen
    Just { head, tail } -> collect (fromMaybe [] (M.lookup head adjacent) <> tail) (S.insert head seen)

restrict :: S.Set NodeId -> Graph -> Graph
restrict ids graph =
  { nodes: A.filter (\n -> S.member n.id ids) graph.nodes
  , edges: A.filter (\e -> S.member e.from.node ids && S.member e.to.node ids) graph.edges
  , constraints: A.filter (A.all (flip S.member ids) <<< constraintNodes) graph.constraints
  }

constraintNodes :: Constraints -> Array NodeId
constraintNodes = case _ of
  AlignGroup { nodes } -> nodes
  SameLayer { nodes } -> nodes
  LayerConstraint { node } -> [ node ]
  OrderConstraint { before, after } -> [ before, after ]
  RelativePosition { anchor, target } -> [ anchor, target ]

-- Return offsets in input order, although placement uses ELK's stable ascending
-- component-area order. Its defaults are spacing.componentComponent = 20 fine
-- units and aspectRatio = Java float 1.6 promoted to double.
pack :: Array LayoutResult -> Array { offset :: GridPos, result :: LayoutResult }
pack results = A.mapWithIndex placed results
  where
  measured = A.mapWithIndex (\index result -> { index, box: result.boundingBox }) results
  width item = sizeW item.box.size
  height item = sizeH item.box.size
  area item = width item * height item
  sorted = A.sortBy (\a b -> compare (area a) (area b)) measured
  rowWidth = max (foldl (\w item -> max w (width item)) 0.0 measured)
    (sqrt (foldl (\a item -> a + area item) 0.0 measured) * 1.600000023841858)
  packing = foldl place { x: 0.0, y: 0.0, height: 0.0, offsets: M.empty } sorted
  place state item =
    let
      newRow = state.x + width item > rowWidth
      x = if newRow then 0.0 else state.x
      y = if newRow then state.y + state.height + 5.0 else state.y
      rowHeight = if newRow then 0.0 else state.height
      offset = GridPos ((x - gridX item.box.pos) /\ (y - gridY item.box.pos))
    in
      { x: x + width item + 5.0
      , y
      , height: max rowHeight (height item)
      , offsets: M.insert item.index offset state.offsets
      }
  placed index result =
    let
      offset = fromMaybe (GridPos (0.0 /\ 0.0)) (M.lookup index packing.offsets)
    in
      { offset, result: translate offset result }

-- Rendered-content bounds, for layouts without a compaction frame.
bounds
  :: forall r
   . { nodes :: Array NodePlacement, edges :: Array EdgePath, edgeLabels :: Array EdgeLabelPlacement | r }
  -> GridRect
bounds result = case A.uncons boxes of
  Nothing -> { pos: GridPos (0.0 /\ 0.0), size: GridSize (0.0 /\ 0.0) }
  Just { head, tail } ->
    let
      extent = foldl (\a b -> { left: min a.left b.left, top: min a.top b.top, right: max a.right b.right, bottom: max a.bottom b.bottom }) head tail
    in
      { pos: GridPos (extent.left /\ extent.top), size: GridSize ((extent.right - extent.left) /\ (extent.bottom - extent.top)) }
  where
  box scale p size = { left: gridX p / scale, top: gridY p / scale, right: (gridX p + sizeW size) / scale, bottom: (gridY p + sizeH size) / scale }
  boxes = (result.nodes <#> \n -> box 1.0 n.position n.size)
    <> (result.edgeLabels <#> \l -> box 4.0 l.position l.size)
    <> A.concatMap (\e -> A.concatMap (\s -> [ box 4.0 s.start zeroSize, box 4.0 s.end zeroSize ]) e.segments) result.edges
  zeroSize = GridSize (0.0 /\ 0.0)

translate :: GridPos -> LayoutResult -> LayoutResult
translate offset result = moved
  where
  point p = GridPos ((gridX p + 4.0 * gridX offset) /\ (gridY p + 4.0 * gridY offset))
  moved = result
    { nodes = map (moveNode offset) result.nodes
    , edges = result.edges <#> \e -> e
        { segments = e.segments <#> \s -> s { start = point s.start, end = point s.end }
        , bends = map point e.bends
        , jumps = e.jumps <#> \j -> j { position = point j.position }
        }
    , edgeLabels = result.edgeLabels <#> \l -> l { position = point l.position }
    , boundingBox = result.boundingBox
        { pos = GridPos
            ((gridX result.boundingBox.pos + gridX offset) /\ (gridY result.boundingBox.pos + gridY offset))
        }
    }

moveNode :: GridPos -> NodePlacement -> NodePlacement
moveNode offset node = node { position = GridPos ((gridX node.position + gridX offset) /\ (gridY node.position + gridY offset)) }
