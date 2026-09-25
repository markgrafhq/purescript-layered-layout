module LayeredLayout.Npm (layout, scaleFactor) where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Uncurried (EffectFn2, mkEffectFn2)
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Object
import LayeredLayout as Layout
import LayeredLayout.Compaction.HorizontalGraphCompactor (BetweenLayersSpacings)
import LayeredLayout.CycleRemoval as Cycle
import LayeredLayout.EdgeLabels as Labels
import LayeredLayout.EdgeRouting as Routing
import LayeredLayout.Graph (Constraints, Edge, EdgeId(..), Graph, Node, NodeId(..), Port, Shape(..))
import LayeredLayout.Grid (GridSize)
import LayeredLayout.LayerAssignment as Layers
import Yoga.JSON (class ReadForeign)
import Yoga.JSON as JSON

type InputNode =
  { id :: NodeId
  , size :: GridSize
  , ports :: Maybe (Array Port)
  , label :: Maybe String
  , shape :: Maybe Shape
  }

type InputGraph =
  { nodes :: Array InputNode
  , edges :: Array Edge
  , constraints :: Maybe (Array Constraints)
  }

type Margins = { left :: Number, right :: Number, top :: Number, bottom :: Number }

type InputLabel = { size :: GridSize, placement :: Maybe String }

type InputOptions =
  { nodeGap :: Maybe Int
  , layerGap :: Maybe Int
  , iterations :: Maybe Int
  , maxGapCount :: Maybe Int
  , layerer :: Maybe String
  , cycleBreaker :: Maybe String
  , compactPostRouting :: Maybe Boolean
  , compactionSpacings :: Maybe BetweenLayersSpacings
  , edgeLabels :: Maybe (Object InputLabel)
  , nodeVisualMargins :: Maybe (Object Margins)
  }

-- | Direct JavaScript calling convention, with no Effect thunk or PS values
-- | crossing the public boundary. All codecs are owned by yoga-json.
layout :: EffectFn2 Foreign Foreign Foreign
layout = mkEffectFn2 \graphValue optionsValue -> do
  input <- decode graphValue
  options <- decode optionsValue
  config <- case options of
    Nothing -> pure Layout.defaultConfig
    Just supplied -> toConfig supplied
  pure $ JSON.write $ Layout.layout config (toGraph input)

scaleFactor :: Int
scaleFactor = Routing.scaleFactor

decode :: forall a. ReadForeign a => Foreign -> Effect a
decode value = case JSON.read value of
  Left errors -> throw ("Invalid layered-layout input: " <> show errors)
  Right result -> pure result

toGraph :: InputGraph -> Graph
toGraph input =
  { nodes: map toNode input.nodes
  , edges: input.edges
  , constraints: fromMaybe [] input.constraints
  }

toNode :: InputNode -> Node
toNode input =
  { id: input.id
  , size: input.size
  , ports: fromMaybe [] input.ports
  , label: input.label
  , shape: fromMaybe Rectangle input.shape
  }

toConfig :: InputOptions -> Effect Layout.Config
toConfig input = do
  layerer <- case input.layerer of
    Nothing -> pure defaults.layerer
    Just value -> readLayerer value
  cycleBreaker <- case input.cycleBreaker of
    Nothing -> pure defaults.cycleBreaker
    Just value -> readCycleBreaker value
  edgeLabels <- traverse toLabel (entries input.edgeLabels)
  pure
    { nodeGap: fromMaybe defaults.nodeGap input.nodeGap
    , layerGap: fromMaybe defaults.layerGap input.layerGap
    , iterations: fromMaybe defaults.iterations input.iterations
    , maxGapCount: fromMaybe defaults.maxGapCount input.maxGapCount
    , layerer
    , cycleBreaker
    , compactPostRouting: fromMaybe defaults.compactPostRouting input.compactPostRouting
    , compactionSpacings: fromMaybe defaults.compactionSpacings input.compactionSpacings
    , edgeLabels: Map.fromFoldable edgeLabels
    , nodeVisualMargins: Map.fromFoldable (entries input.nodeVisualMargins <#> \(id /\ margins) -> NodeId id /\ margins)
    }
  where
  defaults = Layout.defaultConfig

entries :: forall a. Maybe (Object a) -> Array (String /\ a)
entries = Object.toUnfoldable <<< fromMaybe Object.empty

toLabel :: String /\ InputLabel -> Effect (EdgeId /\ Labels.EdgeLabelSpec)
toLabel (id /\ input) = do
  placement <- case input.placement of
    Nothing -> pure Labels.Center
    Just value -> readPlacement value
  pure (EdgeId id /\ { size: input.size, placement })

readLayerer :: String -> Effect Layers.LayererStrategy
readLayerer = case _ of
  "NetworkSimplex" -> pure Layers.NetworkSimplex
  "LongestPath" -> pure Layers.LongestPath
  value -> throw ("Unknown layered-layout layerer: " <> value)

readCycleBreaker :: String -> Effect Cycle.CycleStrategy
readCycleBreaker = case _ of
  "Greedy" -> pure Cycle.Greedy
  "DepthFirst" -> pure Cycle.DepthFirst
  value -> throw ("Unknown layered-layout cycleBreaker: " <> value)

readPlacement :: String -> Effect Labels.LabelPlacement
readPlacement = case _ of
  "Center" -> pure Labels.Center
  "Tail" -> pure (Labels.Tail Labels.Adjacent)
  "TailCenterTerminalRun" -> pure (Labels.Tail Labels.CenterTerminalRun)
  value -> throw ("Unknown layered-layout label placement: " <> value)
