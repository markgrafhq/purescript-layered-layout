module LayeredLayout.Aesthetics (allMetrics, bendCount, nodeOverlapCount) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import LayeredLayout.Grid (manhattan, overlaps)
import LayeredLayout.Result (EdgePath, Metrics, NodePlacement)

allMetrics :: Array NodePlacement -> Array EdgePath -> Int -> Metrics
allMetrics nodes edges violations =
  { crossingCount: foldl (\acc e -> acc + A.length e.jumps) 0 edges
  , bendCount: bendCount edges
  , totalEdgeLength: foldl (\acc e -> acc + edgeLength e) 0.0 edges
  , maxEdgeLength: foldl (\acc e -> max acc (edgeLength e)) 0.0 edges
  , nodeOverlapCount: nodeOverlapCount nodes
  , constraintViolations: violations
  , jumpCount: foldl (\acc e -> acc + A.length e.jumps) 0 edges
  }

bendCount :: Array EdgePath -> Int
bendCount edges = foldl (\acc e -> acc + A.length e.bends) 0 edges

nodeOverlapCount :: Array NodePlacement -> Int
nodeOverlapCount nodes = countOverlaps 0 0
  where
  len = A.length nodes

  countOverlaps :: Int -> Int -> Int
  countOverlaps i acc
    | i >= len = acc
    | otherwise = countOverlaps (i + 1) (countInner (i + 1) acc)
        where
        countInner :: Int -> Int -> Int
        countInner j acc'
          | j >= len = acc'
          | otherwise = case A.index nodes i of
              Nothing -> acc'
              Just a -> case A.index nodes j of
                Nothing -> countInner (j + 1) acc'
                Just b ->
                  let
                    rectA = { pos: a.position, size: a.size }
                    rectB = { pos: b.position, size: b.size }
                  in
                    countInner (j + 1) (if overlaps rectA rectB then acc' + 1 else acc')

edgeLength :: EdgePath -> Number
edgeLength path = foldl (\acc seg -> acc + manhattan seg.start seg.end) 0.0 path.segments
