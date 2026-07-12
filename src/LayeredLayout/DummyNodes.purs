module LayeredLayout.DummyNodes (insertDummies, DummyResult, isDummy) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.String as Str
import Data.Tuple.Nested ((/\))
import LayeredLayout.Graph (Edge, EdgeId(..), NodeId(..))

type DummyResult =
  { layers :: Array (Array NodeId)
  , edges :: Array Edge
  , chains :: Array { edgeId :: EdgeId, nodes :: Array NodeId }
  }

insertDummies :: Map NodeId Int -> Array Edge -> Array (Array NodeId) -> DummyResult
insertDummies nodeLayer edges layers = foldl processEdge { layers: layers, edges: [], chains: [] } edges
  where
  processEdge acc edge = do
    let fromId = edge.from.node
    let toId = edge.to.node
    let fromLayer = fromMaybe 0 (M.lookup fromId nodeLayer)
    let toLayer = fromMaybe 0 (M.lookup toId nodeLayer)
    let span = toLayer - fromLayer
    if span <= 1 then
      acc { edges = acc.edges <> [ edge ], chains = acc.chains <> [ { edgeId: edge.id, nodes: [ fromId, toId ] } ] }
    else do
      let edgeKey = un EdgeId edge.id
      let dummyIds = A.range 1 (span - 1) <#> \i -> NodeId ("$d:" <> edgeKey <> ":" <> show i)
      let allIds = [ fromId ] <> dummyIds <> [ toId ]
      let
        newEdges = A.zipWith
          ( \a b ->
              { id: EdgeId (edgeKey <> ":" <> un NodeId a <> "->" <> un NodeId b)
              , from: { node: a, port: edge.from.port }
              , to: { node: b, port: edge.to.port }
              , label: Nothing
              }
          )
          allIds
          (A.drop 1 allIds)
      let
        newLayers = foldl
          ( \ls (idx /\ did) ->
              let
                layerIdx = fromLayer + idx
              in
                fromMaybe ls (A.modifyAt layerIdx (\l -> l <> [ did ]) ls)
          )
          acc.layers
          (A.zipWith (/\) (A.range 1 (span - 1)) dummyIds)
      acc
        { layers = newLayers
        , edges = acc.edges <> newEdges
        , chains = acc.chains <> [ { edgeId: edge.id, nodes: allIds } ]
        }

isDummy :: NodeId -> Boolean
isDummy nid = Str.take 3 (un NodeId nid) == "$d:"
