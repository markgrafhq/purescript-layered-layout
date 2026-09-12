module LayeredLayout.EdgeRouting.Orthogonal (findRoute, findRouteSlot, isRouteClear, simplifySegments, mergeCollinear, removeZeroLength, FineRect, ObstacleMap, buildObstacleMap, segmentsToObstacles) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as A
import Data.Foldable (any)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Int as Int
import LayeredLayout.EdgeRouting.PortAssignment (scaleFactor)
import LayeredLayout.Graph (Side(..))
import LayeredLayout.Grid (GridPos(..), gridX, gridY, sizeH, sizeW)
import LayeredLayout.Result (Direction(..), EdgeSegment, NodePlacement)

type FineRect = { x :: Number, y :: Number, w :: Number, h :: Number }

type ObstacleMap = Array FineRect

sf :: Number
sf = Int.toNumber scaleFactor

buildObstacleMap :: Array NodePlacement -> ObstacleMap
buildObstacleMap = map toRect
  where
  toRect p = do
    let x = gridX p.position * sf
    let y = gridY p.position * sf
    let w = sizeW p.size * sf
    let h = sizeH p.size * sf
    { x: x - 2.0, y: y - 2.0, w: w + 4.0, h: h + 4.0 }

-- | Route an edge using ELK-style layer-channel routing.
-- For South→North: VHV (vertical down, horizontal across, vertical down).
-- For other side combinations: exit in the port direction, then
-- route to an unblocked channel, then enter from the target direction.
-- nodeObstacles: only node rects (used for straight-line feasibility).
-- obstacles: all obstacles including edge segments (used for channel selection).
findRoute :: ObstacleMap -> ObstacleMap -> Side -> Number /\ Number -> Side -> Number /\ Number -> Array EdgeSegment
findRoute = findRouteSlot Nothing

-- | Like `findRoute`, but uses a precomputed slot y for the horizontal
-- | trunk (port of `OrthogonalRoutingGenerator.routeEdges` slot
-- | placement). When a slot y is provided and the corresponding channel
-- | is clear, it's used instead of the greedy `pickChannelY` search.
findRouteSlot :: Maybe Number -> ObstacleMap -> ObstacleMap -> Side -> Number /\ Number -> Side -> Number /\ Number -> Array EdgeSegment
findRouteSlot slotY nodeObstacles obstacles fromSide (sx /\ sy) toSide (ex /\ ey) = fullRoute
  where
  -- Step away from ports to ensure a visible exit/entry stub
  dx /\ dy = stepAway fromSide (sx /\ sy)
  ax /\ ay = stepAway toSide (ex /\ ey)
  exitDir = sideDir fromSide
  entryDir = sideDir toSide
  exitSeg = { start: GridPos (sx /\ sy), end: GridPos (dx /\ dy), direction: exitDir }
  entrySeg = { start: GridPos (ax /\ ay), end: GridPos (ex /\ ey), direction: entryDir }

  fullRoute =
    if dx == ax && dy == ay then
      [ { start: GridPos (sx /\ sy), end: GridPos (ex /\ ey), direction: exitDir } ]
    else
      mergeSegments exitSeg route entrySeg

  -- Straight-line checks use only node obstacles so that
  -- previously-routed edge segments don't prevent straight lines
  vStraightClear x y1 y2 = not (vSegCrossesAny nodeObstacles (min y1 y2) (max y1 y2) x)
  hStraightClear y x1 x2 = not (hSegCrossesAny nodeObstacles (min x1 x2) (max x1 x2) y)
  -- Channel/detour checks use all obstacles
  vClear x y1 y2 = not (vSegCrossesAny obstacles (min y1 y2) (max y1 y2) x)
  hClear y x1 x2 = not (hSegCrossesAny obstacles (min x1 x2) (max x1 x2) y)

  -- ELK slot-based channel y. When a precomputed slot y exists and the
  -- horizontal segment at that y is unobstructed across [x1, x2], use
  -- it; otherwise fall back to the greedy search.
  slotChannelY x1 x2 y1 y2 = case slotY of
    Just y | not (hSegCrossesAny obstacles (min x1 x2) (max x1 x2) y) -> y
    _ -> pickChannelY nodeObstacles x1 x2 y1 y2

  route = case fromSide /\ toSide of
    South /\ North
      | dx == ax && vStraightClear dx dy ay -> straight V dx dy ax ay
      | otherwise -> vhv dx dy ax ay
    North /\ South
      | dx == ax && vStraightClear dx dy ay -> straight V dx dy ax ay
      | otherwise -> vhv dx dy ax ay
    East /\ West
      | dy == ay && hStraightClear dy dx ax -> straight H dx dy ax ay
      | otherwise -> hvh dx dy ax ay
    West /\ East
      | dy == ay && hStraightClear dy dx ax -> straight H dx dy ax ay
      | otherwise -> hvh dx dy ax ay
    South /\ East -> vThenH dx dy ax ay
    South /\ West -> vThenH dx dy ax ay
    North /\ East -> vThenH dx dy ax ay
    North /\ West -> vThenH dx dy ax ay
    East /\ North -> hThenV dx dy ax ay
    East /\ South -> hThenV dx dy ax ay
    West /\ North -> hThenV dx dy ax ay
    West /\ South -> hThenV dx dy ax ay
    _ -> vhv dx dy ax ay

  straight dir x1 y1 x2 y2 =
    [ { start: GridPos (x1 /\ y1), end: GridPos (x2 /\ y2), direction: dir } ]

  -- Vertical exit → horizontal entry.
  -- Try VH L-shape first; if either leg is blocked, use VHV with a clear channel.
  vThenH x1 y1 x2 y2
    | vClear x1 y1 y2 && hClear y2 x1 x2 =
        [ { start: GridPos (x1 /\ y1), end: GridPos (x1 /\ y2), direction: V }
        , { start: GridPos (x1 /\ y2), end: GridPos (x2 /\ y2), direction: H }
        ]
    | otherwise = do
        let midY = slotChannelY x1 x2 y1 y2
        [ { start: GridPos (x1 /\ y1), end: GridPos (x1 /\ midY), direction: V }
        , { start: GridPos (x1 /\ midY), end: GridPos (x2 /\ midY), direction: H }
        , { start: GridPos (x2 /\ midY), end: GridPos (x2 /\ y2), direction: V }
        ]

  -- Horizontal exit → vertical entry.
  -- Try HV L-shape first; if either leg is blocked, use HVH with a clear channel.
  hThenV x1 y1 x2 y2
    | hClear y1 x1 x2 && vClear x2 y1 y2 =
        [ { start: GridPos (x1 /\ y1), end: GridPos (x2 /\ y1), direction: H }
        , { start: GridPos (x2 /\ y1), end: GridPos (x2 /\ y2), direction: V }
        ]
    | otherwise = do
        let midX = pickChannelX nodeObstacles y1 y2 x1 x2
        [ { start: GridPos (x1 /\ y1), end: GridPos (midX /\ y1), direction: H }
        , { start: GridPos (midX /\ y1), end: GridPos (midX /\ y2), direction: V }
        , { start: GridPos (midX /\ y2), end: GridPos (x2 /\ y2), direction: H }
        ]

  -- Vertical-Horizontal-Vertical: exit down/up, jog horizontally, enter down/up.
  -- If endpoints share the same x (but straight is blocked), detour sideways
  -- past the blocking obstacle.
  vhv x1 y1 x2 y2
    | x1 == x2 = do
        -- Find the blocking obstacle and route around it (node obstacles only)
        let detourX = pickDetourX nodeObstacles y1 y2 x1
        let midY1 = findGapBeforeBlock nodeObstacles x1 y1 y2
        let midY2 = findGapAfterBlock nodeObstacles x1 y1 y2
        [ { start: GridPos (x1 /\ y1), end: GridPos (x1 /\ midY1), direction: V }
        , { start: GridPos (x1 /\ midY1), end: GridPos (detourX /\ midY1), direction: H }
        , { start: GridPos (detourX /\ midY1), end: GridPos (detourX /\ midY2), direction: V }
        , { start: GridPos (detourX /\ midY2), end: GridPos (x2 /\ midY2), direction: H }
        , { start: GridPos (x2 /\ midY2), end: GridPos (x2 /\ y2), direction: V }
        ]
    | otherwise = do
        let midY = slotChannelY x1 x2 y1 y2
        [ { start: GridPos (x1 /\ y1), end: GridPos (x1 /\ midY), direction: V }
        , { start: GridPos (x1 /\ midY), end: GridPos (x2 /\ midY), direction: H }
        , { start: GridPos (x2 /\ midY), end: GridPos (x2 /\ y2), direction: V }
        ]

  -- Horizontal-Vertical-Horizontal: exit left/right, jog vertically, enter left/right.
  -- If endpoints share the same y (but straight is blocked), detour vertically.
  hvh x1 y1 x2 y2
    | y1 == y2 = do
        let detourY = pickDetourY nodeObstacles x1 x2 y1
        let midX1 = findGapBeforeBlockH nodeObstacles y1 x1 x2
        let midX2 = findGapAfterBlockH nodeObstacles y1 x1 x2
        [ { start: GridPos (x1 /\ y1), end: GridPos (midX1 /\ y1), direction: H }
        , { start: GridPos (midX1 /\ y1), end: GridPos (midX1 /\ detourY), direction: V }
        , { start: GridPos (midX1 /\ detourY), end: GridPos (midX2 /\ detourY), direction: H }
        , { start: GridPos (midX2 /\ detourY), end: GridPos (midX2 /\ y2), direction: V }
        , { start: GridPos (midX2 /\ y2), end: GridPos (x2 /\ y2), direction: H }
        ]
    | otherwise = do
        let midX = pickChannelX nodeObstacles y1 y2 x1 x2
        [ { start: GridPos (x1 /\ y1), end: GridPos (midX /\ y1), direction: H }
        , { start: GridPos (midX /\ y1), end: GridPos (midX /\ y2), direction: V }
        , { start: GridPos (midX /\ y2), end: GridPos (x2 /\ y2), direction: H }
        ]

-- | Find the Y just before the first obstacle blocking a vertical path at x from y1 toward y2.
findGapBeforeBlock :: ObstacleMap -> Number -> Number -> Number -> Number
findGapBeforeBlock obstacles x y1 y2 = do
  let minY = min y1 y2
  let maxY = max y1 y2
  let blockers = A.filter (\r -> x >= r.x && x < r.x + r.w && r.y + r.h > minY && r.y < maxY) obstacles
  let goingDown = y2 > y1
  if goingDown then case A.head (A.sortBy (\a b -> compare a.y b.y) blockers) of
    Just r -> r.y - 1.0
    Nothing -> (y1 + y2) / 2.0
  else case A.head (A.sortBy (\a b -> compare b.y a.y) (blockers <#> \r -> r { y = r.y + r.h })) of
    Just r -> r.y + 1.0
    Nothing -> (y1 + y2) / 2.0

-- | Find the Y just after the last obstacle blocking a vertical path at x from y1 toward y2.
findGapAfterBlock :: ObstacleMap -> Number -> Number -> Number -> Number
findGapAfterBlock obstacles x y1 y2 = do
  let minY = min y1 y2
  let maxY = max y1 y2
  let blockers = A.filter (\r -> x >= r.x && x < r.x + r.w && r.y + r.h > minY && r.y < maxY) obstacles
  let goingDown = y2 > y1
  if goingDown then case A.head (A.sortBy (\a b -> compare b.y a.y) (blockers <#> \r -> r { y = r.y + r.h })) of
    Just r -> r.y
    Nothing -> (y1 + y2) / 2.0
  else case A.head (A.sortBy (\a b -> compare a.y b.y) blockers) of
    Just r -> r.y - 1.0
    Nothing -> (y1 + y2) / 2.0

-- | Find an X coordinate that clears all obstacles blocking a vertical path.
-- | Clearance from obstacles for detour routing, matching ELK's
-- edgeNodeBetweenLayers spacing.
detourClearance :: Number
detourClearance = 4.0

pickDetourX :: ObstacleMap -> Number -> Number -> Number -> Number
pickDetourX obstacles y1 y2 x = do
  let minY = min y1 y2
  let maxY = max y1 y2
  let blockers = A.filter (\r -> x >= r.x && x < r.x + r.w && r.y + r.h > minY && r.y < maxY) obstacles
  let rightEdge = A.foldl (\acc r -> max acc (r.x + r.w + detourClearance)) (x + detourClearance) blockers
  let leftEdge = A.foldl (\acc r -> min acc (r.x - detourClearance)) (x - detourClearance) blockers
  if iabs (rightEdge - x) <= iabs (leftEdge - x) then rightEdge else leftEdge

pickDetourY :: ObstacleMap -> Number -> Number -> Number -> Number
pickDetourY obstacles x1 x2 y = do
  let minX = min x1 x2
  let maxX = max x1 x2
  let blockers = A.filter (\r -> y >= r.y && y < r.y + r.h && r.x + r.w > minX && r.x < maxX) obstacles
  let bottomEdge = A.foldl (\acc r -> max acc (r.y + r.h + detourClearance)) (y + detourClearance) blockers
  let topEdge = A.foldl (\acc r -> min acc (r.y - detourClearance)) (y - detourClearance) blockers
  if iabs (bottomEdge - y) <= iabs (topEdge - y) then bottomEdge else topEdge

-- | Find X gap before/after block on horizontal path (mirrors vertical versions).
findGapBeforeBlockH :: ObstacleMap -> Number -> Number -> Number -> Number
findGapBeforeBlockH obstacles y x1 x2 = do
  let minX = min x1 x2
  let maxX = max x1 x2
  let blockers = A.filter (\r -> y >= r.y && y < r.y + r.h && r.x + r.w > minX && r.x < maxX) obstacles
  let goingRight = x2 > x1
  if goingRight then case A.head (A.sortBy (\a b -> compare a.x b.x) blockers) of
    Just r -> r.x - 1.0
    Nothing -> (x1 + x2) / 2.0
  else case A.head (A.sortBy (\a b -> compare b.x a.x) (blockers <#> \r -> r { x = r.x + r.w })) of
    Just r -> r.x + 1.0
    Nothing -> (x1 + x2) / 2.0

findGapAfterBlockH :: ObstacleMap -> Number -> Number -> Number -> Number
findGapAfterBlockH obstacles y x1 x2 = do
  let minX = min x1 x2
  let maxX = max x1 x2
  let blockers = A.filter (\r -> y >= r.y && y < r.y + r.h && r.x + r.w > minX && r.x < maxX) obstacles
  let goingRight = x2 > x1
  if goingRight then case A.head (A.sortBy (\a b -> compare b.x a.x) (blockers <#> \r -> r { x = r.x + r.w })) of
    Just r -> r.x
    Nothing -> (x1 + x2) / 2.0
  else case A.head (A.sortBy (\a b -> compare a.x b.x) blockers) of
    Just r -> r.x - 1.0
    Nothing -> (x1 + x2) / 2.0

iabs :: Number -> Number
iabs n = if n < 0.0 then negate n else n

-- | Find a clear horizontal channel Y between y1 and y2.
-- Prefers y1 (source step-away) to match ELK's source-biased channel placement.
pickChannelY :: ObstacleMap -> Number -> Number -> Number -> Number -> Number
pickChannelY obstacles x1 x2 y1 y2 = do
  let minX = min x1 x2
  let maxX = max x1 x2
  let crosses y = hSegCrossesRect minX maxX y obstacles
  if not (crosses y1) then y1
  else if not (crosses y2) then y2
  else do
    let mid = (y1 + y2) / 2.0
    if not (crosses mid) then mid
    else findClearChannel crosses mid 1.0

-- | Find a clear vertical channel X between x1 and x2.
-- Prefers x1 (source step-away) to match ELK's source-biased channel placement.
pickChannelX :: ObstacleMap -> Number -> Number -> Number -> Number -> Number
pickChannelX obstacles y1 y2 x1 x2 = do
  let minY = min y1 y2
  let maxY = max y1 y2
  let crosses x = vSegCrossesRect minY maxY x obstacles
  if not (crosses x1) then x1
  else if not (crosses x2) then x2
  else do
    let mid = (x1 + x2) / 2.0
    if not (crosses mid) then mid
    else findClearChannel crosses mid 1.0

-- | Search outward from mid by ±offset until a clear channel is found.
findClearChannel :: (Number -> Boolean) -> Number -> Number -> Number
findClearChannel crosses mid offset
  | offset > 100.0 = mid
  | not (crosses (mid + offset)) = mid + offset
  | not (crosses (mid - offset)) = mid - offset
  | otherwise = findClearChannel crosses mid (offset + 1.0)

-- Segment simplification --

simplifySegments :: ObstacleMap -> Array EdgeSegment -> Array EdgeSegment
simplifySegments obstacles = go <<< mergeCollinear <<< removeZeroLength
  where
  go segs = do
    let afterTriples = trySimplifyOnce obstacles segs
    let afterPairs = trySimplifyPairOnce obstacles afterTriples
    let simplified = mergeCollinear (removeZeroLength afterPairs)
    if A.length simplified < A.length segs then go simplified
    else simplified

removeZeroLength :: Array EdgeSegment -> Array EdgeSegment
removeZeroLength = A.filter \s -> not (nearlyEq s.start s.end)
  where
  nearlyEq a b = abs (gridX a - gridX b) < epsilon && abs (gridY a - gridY b) < epsilon
  abs n = if n < 0.0 then -n else n
  epsilon = 1.0e-6

mergeCollinear :: Array EdgeSegment -> Array EdgeSegment
mergeCollinear segs = case A.uncons segs of
  Nothing -> []
  Just { head, tail } -> collapse head tail
  where
  collapse current rest = case A.uncons rest of
    Nothing -> [ current ]
    Just { head: next, tail }
      | collinear current next ->
          collapse { start: current.start, end: next.end, direction: current.direction } tail
      | otherwise ->
          A.cons current (collapse next tail)

  -- A dummy-chain join can retrace the same axis. Retaining that reversal
  -- leaves an axial bend with no transverse segment for the compactor to
  -- move, so it stays behind when nearby CENTER boxes compact. Cancel the
  -- redundant excursion, but never merge parallel runs on distinct tracks.
  collinear a b = a.direction == b.direction && case a.direction of
    H -> sameCoordinate (gridY a.start) (gridY b.start) && sameCoordinate (gridY a.end) (gridY b.end)
    V -> sameCoordinate (gridX a.start) (gridX b.start) && sameCoordinate (gridX a.end) (gridX b.end)

  sameCoordinate a b = abs (a - b) < 1.0e-6
  abs n = if n < 0.0 then -n else n

trySimplifyOnce :: ObstacleMap -> Array EdgeSegment -> Array EdgeSegment
trySimplifyOnce obstacles segs = go 0
  where
  n = A.length segs
  go idx
    | idx + 2 >= n = segs
    | otherwise = case simplifyTriple obstacles segs idx n of
        Just result -> result
        Nothing -> go (idx + 1)

simplifyTriple :: ObstacleMap -> Array EdgeSegment -> Int -> Int -> Maybe (Array EdgeSegment)
simplifyTriple obstacles segs idx n = do
  s0 <- A.index segs idx
  s2 <- A.index segs (idx + 2)
  let sx = gridX s0.start
  let sy = gridY s0.start
  let ex' = gridX s2.end
  let ey' = gridY s2.end
  let firstOk d = not isFirst || s0.direction == d
  let lastOk d = not isLast || s2.direction == d
  tryStraight sx sy ex' ey' firstOk lastOk s0.start s2.end
    <|> tryVH sx sy ex' ey' firstOk lastOk s0.start s2.end
    <|> tryHV sx sy ex' ey' firstOk lastOk s0.start s2.end
  where
  prefix = A.take idx segs
  suffix = A.drop (idx + 3) segs
  isFirst = idx == 0
  isLast = idx + 2 == n - 1

  vClear x y1 y2 = not (vSegCrossesAny obstacles (min y1 y2) (max y1 y2) x)
  hClear y x1 x2 = not (hSegCrossesAny obstacles (min x1 x2) (max x1 x2) y)

  tryStraight sx sy ex' ey' firstOk lastOk start end
    | sx == ex' && firstOk V && lastOk V && vClear sx sy ey' =
        Just (prefix <> [ { start, end, direction: V } ] <> suffix)
    | sy == ey' && firstOk H && lastOk H && hClear sy sx ex' =
        Just (prefix <> [ { start, end, direction: H } ] <> suffix)
    | otherwise = Nothing

  tryVH sx sy ex' ey' firstOk lastOk start end
    | firstOk V && lastOk H && vClear sx sy ey' && hClear ey' sx ex' = do
        let corner = GridPos (sx /\ ey')
        Just (prefix <> [ { start, end: corner, direction: V }, { start: corner, end, direction: H } ] <> suffix)
    | otherwise = Nothing

  tryHV sx sy ex' ey' firstOk lastOk start end
    | firstOk H && lastOk V && hClear sy sx ex' && vClear ex' sy ey' = do
        let corner = GridPos (ex' /\ sy)
        Just (prefix <> [ { start, end: corner, direction: H }, { start: corner, end, direction: V } ] <> suffix)
    | otherwise = Nothing

trySimplifyPairOnce :: ObstacleMap -> Array EdgeSegment -> Array EdgeSegment
trySimplifyPairOnce obstacles segs = go 0
  where
  n = A.length segs
  go idx
    | idx + 1 >= n = segs
    | otherwise = case simplifyPair obstacles segs idx n of
        Just result -> result
        Nothing -> go (idx + 1)

simplifyPair :: ObstacleMap -> Array EdgeSegment -> Int -> Int -> Maybe (Array EdgeSegment)
simplifyPair obstacles segs idx n = do
  s0 <- A.index segs idx
  s1 <- A.index segs (idx + 1)
  let sx = gridX s0.start
  let sy = gridY s0.start
  let ex' = gridX s1.end
  let ey' = gridY s1.end
  let firstOk d = not isFirst || s0.direction == d
  let lastOk d = not isLast || s1.direction == d
  tryV sx sy ex' ey' firstOk lastOk s0.start s1.end
    <|> tryH sx sy ex' ey' firstOk lastOk s0.start s1.end
  where
  prefix = A.take idx segs
  suffix = A.drop (idx + 2) segs
  isFirst = idx == 0
  isLast = idx + 1 == n - 1

  vClear x y1 y2 = not (vSegCrossesAny obstacles (min y1 y2) (max y1 y2) x)
  hClear y x1 x2 = not (hSegCrossesAny obstacles (min x1 x2) (max x1 x2) y)

  tryV sx sy ex' ey' firstOk lastOk start end
    | sx == ex' && firstOk V && lastOk V && vClear sx sy ey' =
        Just (prefix <> [ { start, end, direction: V } ] <> suffix)
    | otherwise = Nothing

  tryH sx sy ex' ey' firstOk lastOk start end
    | sy == ey' && firstOk H && lastOk H && hClear sy sx ex' =
        Just (prefix <> [ { start, end, direction: H } ] <> suffix)
    | otherwise = Nothing

-- Step 4 cells away from the port in the exit/entry direction,
-- matching ELK's edgeNodeBetweenLayers spacing.
stepAway :: Side -> Number /\ Number -> Number /\ Number
stepAway side (x /\ y) = case side of
  South -> x /\ (y + 4.0)
  North -> x /\ (y - 4.0)
  East -> (x + 4.0) /\ y
  West -> (x - 4.0) /\ y

sideDir :: Side -> Direction
sideDir South = V
sideDir North = V
sideDir East = H
sideDir West = H

-- Merge exit stub + middle route + entry stub, collapsing collinear segments
mergeSegments :: EdgeSegment -> Array EdgeSegment -> EdgeSegment -> Array EdgeSegment
mergeSegments entry middle exit = case A.uncons middle of
  Nothing -> [ { start: entry.start, end: exit.end, direction: entry.direction } ]
  Just { head: first, tail } -> do
    let
      front =
        if first.direction == entry.direction then [ { start: entry.start, end: first.end, direction: entry.direction } ]
        else [ entry, first ]
    case A.unsnoc tail of
      Nothing -> case A.last front of
        Just lastFront | lastFront.direction == exit.direction ->
          A.dropEnd 1 front <> [ { start: lastFront.start, end: exit.end, direction: exit.direction } ]
        _ -> front <> [ exit ]
      Just { init, last: lastSeg } ->
        if lastSeg.direction == exit.direction then front <> init <> [ { start: lastSeg.start, end: exit.end, direction: exit.direction } ]
        else front <> tail <> [ exit ]

-- Obstacle intersection checks --

isRouteClear :: ObstacleMap -> Array EdgeSegment -> Boolean
isRouteClear obstacles = A.all \segment -> case segment.direction of
  V -> not (vSegCrossesAny obstacles (gridY segment.start) (gridY segment.end) (gridX segment.start))
  H -> not (hSegCrossesAny obstacles (gridX segment.start) (gridX segment.end) (gridY segment.start))

vSegCrossesAny :: ObstacleMap -> Number -> Number -> Number -> Boolean
vSegCrossesAny obstacles y1 y2 x = vSegCrossesRect (min y1 y2) (max y1 y2) x obstacles

hSegCrossesAny :: ObstacleMap -> Number -> Number -> Number -> Boolean
hSegCrossesAny obstacles x1 x2 y = hSegCrossesRect (min x1 x2) (max x1 x2) y obstacles

hSegCrossesRect :: Number -> Number -> Number -> ObstacleMap -> Boolean
hSegCrossesRect x1 x2 y rects = any (hLineIntersects x1 x2 y) rects

vSegCrossesRect :: Number -> Number -> Number -> ObstacleMap -> Boolean
vSegCrossesRect y1 y2 x rects = any (vLineIntersects y1 y2 x) rects

hLineIntersects :: Number -> Number -> Number -> FineRect -> Boolean
hLineIntersects x1 x2 y r =
  y >= r.y && y < r.y + r.h && x2 > r.x && x1 < r.x + r.w

vLineIntersects :: Number -> Number -> Number -> FineRect -> Boolean
vLineIntersects y1 y2 x r =
  x >= r.x && x < r.x + r.w && y2 > r.y && y1 < r.y + r.h

-- | Convert routed edge segments into obstacle rects so that
-- subsequent edges avoid overlapping them. Width of 2 ensures
-- visible separation between parallel edges.
segmentsToObstacles :: Array EdgeSegment -> Array FineRect
segmentsToObstacles = A.concatMap segToRect
  where
  segToRect seg = case seg.direction of
    H -> do
      let y = gridY seg.start
      let x1 = min (gridX seg.start) (gridX seg.end)
      let x2 = max (gridX seg.start) (gridX seg.end)
      [ { x: x1, y: y - 1.0, w: x2 - x1, h: 2.0 } ]
    V -> do
      let x = gridX seg.start
      let y1 = min (gridY seg.start) (gridY seg.end)
      let y2 = max (gridY seg.start) (gridY seg.end)
      [ { x: x - 1.0, y: y1, w: 2.0, h: y2 - y1 } ]
