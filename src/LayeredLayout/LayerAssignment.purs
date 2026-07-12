module LayeredLayout.LayerAssignment
  ( assignLayers
  , assignLayersWith
  , LayeredGraph
  , LayererStrategy(..)
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import LayeredLayout.Graph (Constraints(..), Edge, LayerPin(..), NodeId)
import LayeredLayout.LayerAssignment.NetworkSimplex (networkSimplex)

type LayeredGraph =
  { layers :: Array (Array NodeId)
  , nodeLayer :: Map NodeId Int
  }

data LayererStrategy = LongestPath | NetworkSimplex

derive instance Eq LayererStrategy

assignLayers :: Array Constraints -> Array Edge -> Array NodeId -> LayeredGraph
assignLayers = assignLayersWith LongestPath

assignLayersWith :: LayererStrategy -> Array Constraints -> Array Edge -> Array NodeId -> LayeredGraph
assignLayersWith strat constraints edges allNodeIds = do
  let adj = buildAdj edges
  let revAdj = buildRevAdj edges
  let pinned = layerPins constraints
  let
    ranks = case strat of
      LongestPath -> longestPath adj revAdj allNodeIds pinned
      NetworkSimplex -> networkSimplexRanks edges allNodeIds pinned
  let sameGroups = sameLayerGroups constraints
  let ranks' = unifySameLayers sameGroups ranks
  toLayers allNodeIds ranks'

networkSimplexRanks :: Array Edge -> Array NodeId -> Map NodeId Int -> Map NodeId Int
networkSimplexRanks edges allNodeIds pinned = applyPins (networkSimplex allNodeIds rawEdges)
  where
  knownNodes = foldl (\s n -> M.insert n true s) M.empty allNodeIds
  rawEdges = A.mapMaybe filterEdge edges
  filterEdge e = do
    let s = e.from.node
    let t = e.to.node
    if s == t then Nothing
    else if not (fromMaybe false (M.lookup s knownNodes)) then Nothing
    else if not (fromMaybe false (M.lookup t knownNodes)) then Nothing
    else Just { src: s, tgt: t }
  applyPins r = foldl (\m (node /\ layer) -> M.insert node layer m) r
    (M.toUnfoldable pinned :: Array (NodeId /\ Int))

longestPath :: Map NodeId (Array NodeId) -> Map NodeId (Array NodeId) -> Array NodeId -> Map NodeId Int -> Map NodeId Int
longestPath adj _revAdj allNodes pinned = applyPins ranks
  where
  heights = foldl visit M.empty allNodes

  visit :: Map NodeId Int -> NodeId -> Map NodeId Int
  visit acc node = case M.lookup node acc of
    Just _ -> acc
    Nothing -> do
      let children = A.filter (_ /= node) (fromMaybe [] (M.lookup node adj))
      let acc' = foldl visit acc children
      let childHeights = A.mapMaybe (\c -> M.lookup c acc') children
      let h = 1 + foldl max 0 childHeights
      M.insert node h acc'

  totalLayers = foldl max 1 (M.values heights)

  ranks = map (\h -> totalLayers - h) heights

  applyPins r = foldl (\m (node /\ layer) -> M.insert node layer m) r
    (M.toUnfoldable pinned :: Array (NodeId /\ Int))

toLayers :: Array NodeId -> Map NodeId Int -> LayeredGraph
toLayers inputOrder ranks = do
  let maxLayer = foldl max 0 (M.values ranks)
  let
    layers = A.range 0 maxLayer <#> \l ->
      A.filter (\n -> M.lookup n ranks == Just l) inputOrder
  { layers, nodeLayer: ranks }

buildAdj :: Array Edge -> Map NodeId (Array NodeId)
buildAdj = foldl (\m e -> M.insertWith (<>) e.from.node [ e.to.node ] m) M.empty

buildRevAdj :: Array Edge -> Map NodeId (Array NodeId)
buildRevAdj = foldl (\m e -> M.insertWith (<>) e.to.node [ e.from.node ] m) M.empty

layerPins :: Array Constraints -> Map NodeId Int
layerPins = foldl go M.empty
  where
  go acc = case _ of
    LayerConstraint { node, pin: SpecificLayer n } -> M.insert node n acc
    LayerConstraint { node, pin: FirstLayer } -> M.insert node 0 acc
    _ -> acc

sameLayerGroups :: Array Constraints -> Array (Array NodeId)
sameLayerGroups = A.mapMaybe case _ of
  SameLayer { nodes } -> Just nodes
  _ -> Nothing

unifySameLayers :: Array (Array NodeId) -> Map NodeId Int -> Map NodeId Int
unifySameLayers groups ranks = foldl unifyGroup ranks groups
  where
  unifyGroup :: Map NodeId Int -> Array NodeId -> Map NodeId Int
  unifyGroup r group = do
    let layers = A.mapMaybe (\n -> M.lookup n r) group
    let targetLayer = foldl max 0 layers
    foldl (\r' n -> M.insert n targetLayer r') r group
