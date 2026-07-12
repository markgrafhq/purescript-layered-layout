module LayeredLayout.EdgeRouting.LineJump (detectJumps) where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import LayeredLayout.Graph (EdgeId)
import LayeredLayout.Grid (GridPos(..), gridX, gridY)
import LayeredLayout.Result (Direction(..), EdgePath, EdgeSegment, LineJump)

detectJumps :: Array EdgePath -> Array EdgePath
detectJumps paths = paths # A.mapWithIndex \i path ->
  path { jumps = findJumpsForPath i path paths }

findJumpsForPath :: Int -> EdgePath -> Array EdgePath -> Array LineJump
findJumpsForPath pathIdx path allPaths = do
  let otherPaths = A.filter (\p -> p.edge /= path.edge) allPaths
  let perpendicular = findPerpendicularJumps path otherPaths
  let overlaps = findOverlapJumps pathIdx path allPaths
  perpendicular <> overlaps

findPerpendicularJumps :: EdgePath -> Array EdgePath -> Array LineJump
findPerpendicularJumps path otherPaths = do
  let hSegments = A.filter (\s -> s.direction == H) path.segments
  hSegments >>= \hSeg -> otherPaths >>= \other ->
    A.filter (\s -> s.direction == V) other.segments
      # A.mapMaybe \vSeg -> findCrossing hSeg vSeg other.edge

findOverlapJumps :: Int -> EdgePath -> Array EdgePath -> Array LineJump
findOverlapJumps pathIdx path allPaths = do
  let laterPaths = A.drop (pathIdx + 1) allPaths
  path.segments >>= \seg -> laterPaths >>= \other ->
    A.filter (\s -> s.direction == seg.direction) other.segments
      # A.mapMaybe \otherSeg -> findOverlap seg otherSeg other.edge

findCrossing :: EdgeSegment -> EdgeSegment -> EdgeId -> Maybe LineJump
findCrossing hSeg vSeg crossingEdge = do
  let hY = gridY hSeg.start
  let hMinX = min (gridX hSeg.start) (gridX hSeg.end)
  let hMaxX = max (gridX hSeg.start) (gridX hSeg.end)
  let vX = gridX vSeg.start
  let vMinY = min (gridY vSeg.start) (gridY vSeg.end)
  let vMaxY = max (gridY vSeg.start) (gridY vSeg.end)
  if vX > hMinX && vX < hMaxX && hY > vMinY && hY < vMaxY then
    Just { position: GridPos (vX /\ hY), crossingEdge }
  else
    Nothing

findOverlap :: EdgeSegment -> EdgeSegment -> EdgeId -> Maybe LineJump
findOverlap seg other crossingEdge = case seg.direction of
  H -> findHOverlap seg other crossingEdge
  V -> findVOverlap seg other crossingEdge

findHOverlap :: EdgeSegment -> EdgeSegment -> EdgeId -> Maybe LineJump
findHOverlap seg other crossingEdge
  | gridY seg.start /= gridY other.start = Nothing
  | otherwise =
      do
        let overlapStart = max segMinX otherMinX
        let overlapEnd = min segMaxX otherMaxX
        if overlapStart < overlapEnd then
          Just { position: GridPos (mid overlapStart overlapEnd /\ gridY seg.start), crossingEdge }
        else
          Nothing
      where
      segMinX = min (gridX seg.start) (gridX seg.end)
      segMaxX = max (gridX seg.start) (gridX seg.end)
      otherMinX = min (gridX other.start) (gridX other.end)
      otherMaxX = max (gridX other.start) (gridX other.end)

findVOverlap :: EdgeSegment -> EdgeSegment -> EdgeId -> Maybe LineJump
findVOverlap seg other crossingEdge
  | gridX seg.start /= gridX other.start = Nothing
  | otherwise =
      do
        let overlapStart = max segMinY otherMinY
        let overlapEnd = min segMaxY otherMaxY
        if overlapStart < overlapEnd then
          Just { position: GridPos (gridX seg.start /\ mid overlapStart overlapEnd), crossingEdge }
        else
          Nothing
      where
      segMinY = min (gridY seg.start) (gridY seg.end)
      segMaxY = max (gridY seg.start) (gridY seg.end)
      otherMinY = min (gridY other.start) (gridY other.end)
      otherMaxY = max (gridY other.start) (gridY other.end)

mid :: Number -> Number -> Number
mid a b = (a + b) / 2.0
