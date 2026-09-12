-- | Turn the physical port order selected by crossing minimization into
-- | offsets shared by BK and routing. A rank identifies a physical port:
-- | several edges at the same rank occupy one slot, not several slots.
module LayeredLayout.PortDistribution
  ( module ReExport
  , PortOrder
  , distributePorts
  , offsetFor
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import LayeredLayout.EdgeRouting.PortAssignment (EdgePortOffsets, distributeAlongSide) as ReExport
import LayeredLayout.EdgeRouting.PortAssignment (EdgePortOffsets, distributeAlongSide)
import LayeredLayout.Graph (Edge, EdgeId, NodeId, Side(..))
import LayeredLayout.Grid (GridSize, sizeW)
import LayeredLayout.DummyNodes (isDummy)

type PortOrder = Map (EdgeId /\ Side) Int

distributePorts
  :: PortOrder
  -> Array (Array NodeId)
  -> Array Edge
  -> Map NodeId GridSize
  -> EdgePortOffsets
distributePorts order _layers edges sizes = foldl distribute M.empty
  (M.toUnfoldable grouped :: Array ((NodeId /\ Side) /\ Array Edge))
  where
  grouped = foldl add M.empty edges
  add acc e
    | e.from.node == e.to.node = acc
    | otherwise = M.insertWith (<>) (e.from.node /\ South) [ e ]
        (M.insertWith (<>) (e.to.node /\ North) [ e ] acc)
  distribute acc ((node /\ side) /\ es) =
    let
      width = fromMaybe (if isDummy node then 0.0 else 1.0) (sizeW <$> M.lookup node sizes)
      rank e = fromMaybe 0 (M.lookup (e.id /\ side) order)
      representatives = A.nubBy (\a b -> compare (rank a) (rank b)) (A.sortBy (\a b -> compare (rank a) (rank b)) es)
      distributed = distributeAlongSide { lo: 0.0, hi: width } (map _.id representatives)
      slots = M.fromFoldable (representatives <#> \e -> rank e /\ fromMaybe (width / 2.0) (M.lookup e.id distributed))
      offset e = fromMaybe (width / 2.0) (M.lookup (rank e) slots)
    in
      foldl (\m e -> M.insert (e.id /\ side) (offset e) m) acc es

offsetFor :: EdgePortOffsets -> EdgeId -> Side -> Number -> Number
offsetFor offsets eid side fallback = fromMaybe fallback (M.lookup (eid /\ side) offsets)
