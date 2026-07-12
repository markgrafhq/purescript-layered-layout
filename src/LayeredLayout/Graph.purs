module LayeredLayout.Graph where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.Newtype (class Newtype)
import Foreign (F, Foreign)
import LayeredLayout.Grid (GridPos, GridSize)
import Yoga.JSON (class ReadForeign, class WriteForeign)
import Yoga.JSON.Derive (readVia, writeVia)
import Yoga.JSON.Generics.EnumSumRep (class GenericEnumSumRep, genericReadForeignEnum, genericWriteForeignEnum)
import Yoga.JSON.Generics.EnumSumRep as Enum
import Yoga.JSON.Generics.TaggedSumRep (genericReadForeignTaggedSum, genericWriteForeignTaggedSum)
import Yoga.JSON.Generics.TaggedSumRep as Tagged

newtype NodeId = NodeId String

derive instance Newtype NodeId _
derive newtype instance Eq NodeId
derive newtype instance Ord NodeId
derive newtype instance Show NodeId
derive newtype instance Semigroup NodeId

instance ReadForeign NodeId where
  readImpl = readVia @String

instance WriteForeign NodeId where
  writeImpl = writeVia @String

newtype PortId = PortId String

derive instance Newtype PortId _
derive newtype instance Eq PortId
derive newtype instance Ord PortId
derive newtype instance Show PortId

instance ReadForeign PortId where
  readImpl = readVia @String

instance WriteForeign PortId where
  writeImpl = writeVia @String

newtype EdgeId = EdgeId String

derive instance Newtype EdgeId _
derive newtype instance Eq EdgeId
derive newtype instance Ord EdgeId
derive newtype instance Show EdgeId

instance ReadForeign EdgeId where
  readImpl = readVia @String

instance WriteForeign EdgeId where
  writeImpl = writeVia @String

data Shape
  = Rectangle
  | Cylinder
  | Parallelogram
  | Diamond
  | Ellipse
  | Document
  | Cloud

derive instance Eq Shape
derive instance Ord Shape
derive instance Generic Shape _

instance Show Shape where
  show Rectangle = "Rectangle"
  show Cylinder = "Cylinder"
  show Parallelogram = "Parallelogram"
  show Diamond = "Diamond"
  show Ellipse = "Ellipse"
  show Document = "Document"
  show Cloud = "Cloud"

instance ReadForeign Shape where
  readImpl = readEnum

instance WriteForeign Shape where
  writeImpl = writeEnum

-- | How far a shape's drawn silhouette pokes past its bounding rect, per
-- | edge, for a node of width `w` and height `h`. The Cloud's arcs rise
-- | above the top, the Cylinder's bulge and the Document's wave droop below
-- | the bottom. Rectangles (and pill/parallelogram/diamond, which stay
-- | inside the rect) overflow nowhere. The renderer's path builders and the
-- | layout's framing both read these so the painted region always covers
-- | the silhouette. Canonical home for the silhouette constants; `Render.Draw`
-- | re-exports them so the path builders stay in lockstep.
silhouetteOverflow :: Shape -> Number -> Number -> { top :: Number, bottom :: Number, left :: Number, right :: Number }
silhouetteOverflow shape _w h = case shape of
  Cloud -> none { top = h * cloudHatRatio }
  Cylinder -> none { bottom = cylinderBottomDrop }
  Document -> none { bottom = h * documentWaveDrop }
  _ -> none
  where
  none = { top: 0.0, bottom: 0.0, left: 0.0, right: 0.0 }

-- | The cylinder body extends this many pixels past `pos.y + h` so its bottom
-- | bulge covers the first slice of an incoming edge.
cylinderBottomDrop :: Number
cylinderBottomDrop = 5.0

-- | Cloud arcs reach this fraction of the node height above the bbox top.
cloudHatRatio :: Number
cloudHatRatio = 0.38

-- | Fraction of node height the document wave dips below the bbox bottom
-- | (the wave bottoms out at 1.05·h in `Render.Draw.documentPath`).
documentWaveDrop :: Number
documentWaveDrop = 0.05

-- | Parse a user-facing shape name (case-insensitive) from `+node id {shape: cylinder}`.
parseShape :: String -> Maybe Shape
parseShape s = case s of
  "rectangle" -> Just Rectangle
  "rect" -> Just Rectangle
  "cylinder" -> Just Cylinder
  "cyl" -> Just Cylinder
  "parallelogram" -> Just Parallelogram
  "diamond" -> Just Diamond
  "ellipse" -> Just Ellipse
  "document" -> Just Document
  "doc" -> Just Document
  "cloud" -> Just Cloud
  _ -> Nothing

-- | Inverse of `parseShape`. Used when serializing a Node back to a Document.
shapeName :: Shape -> String
shapeName = case _ of
  Rectangle -> "rectangle"
  Cylinder -> "cylinder"
  Parallelogram -> "parallelogram"
  Diamond -> "diamond"
  Ellipse -> "ellipse"
  Document -> "document"
  Cloud -> "cloud"

-- | A displayable text label attached to a token, node, or edge.
-- | Distinct from node/edge ids so the type system catches accidental
-- | swaps with `NodeId`/`EdgeId` strings.
newtype Label = Label String

derive instance Newtype Label _
derive newtype instance Eq Label
derive newtype instance Ord Label
derive newtype instance Show Label
derive newtype instance Semigroup Label

instance ReadForeign Label where
  readImpl = readVia @String

instance WriteForeign Label where
  writeImpl = writeVia @String

data Side = North | South | East | West

derive instance Eq Side
derive instance Ord Side
derive instance Generic Side _

instance Show Side where
  show North = "North"
  show South = "South"
  show East = "East"
  show West = "West"

instance ReadForeign Side where
  readImpl = readEnum

instance WriteForeign Side where
  writeImpl = writeEnum

data Axis = Horizontal | Vertical

derive instance Eq Axis
derive instance Ord Axis
derive instance Generic Axis _

instance Show Axis where
  show Horizontal = "Horizontal"
  show Vertical = "Vertical"

instance ReadForeign Axis where
  readImpl = readEnum

instance WriteForeign Axis where
  writeImpl = writeEnum

data Alignment = Start | Center | End

derive instance Eq Alignment
derive instance Ord Alignment
derive instance Generic Alignment _

instance Show Alignment where
  show Start = "Start"
  show Center = "Center"
  show End = "End"

instance ReadForeign Alignment where
  readImpl = readEnum

instance WriteForeign Alignment where
  writeImpl = writeEnum

data Justify = JustifyStart | JustifyEnd | JustifyCenter | SpaceBetween | SpaceAround

derive instance Eq Justify
derive instance Ord Justify
derive instance Generic Justify _

instance Show Justify where
  show JustifyStart = "JustifyStart"
  show JustifyEnd = "JustifyEnd"
  show JustifyCenter = "JustifyCenter"
  show SpaceBetween = "SpaceBetween"
  show SpaceAround = "SpaceAround"

instance ReadForeign Justify where
  readImpl = readEnum

instance WriteForeign Justify where
  writeImpl = writeEnum

data LayerPin = FirstLayer | LastLayer | SpecificLayer Int

derive instance Eq LayerPin
derive instance Ord LayerPin
derive instance Generic LayerPin _

instance Show LayerPin where
  show FirstLayer = "FirstLayer"
  show LastLayer = "LastLayer"
  show (SpecificLayer n) = "(SpecificLayer " <> show n <> ")"

instance ReadForeign LayerPin where
  readImpl = genericReadForeignTaggedSum Tagged.defaultOptions

instance WriteForeign LayerPin where
  writeImpl = genericWriteForeignTaggedSum Tagged.defaultOptions

data Constraints
  = AlignGroup { nodes :: Array NodeId, axis :: Axis, alignment :: Alignment, justify :: Justify }
  | SameLayer { nodes :: Array NodeId }
  | LayerConstraint { node :: NodeId, pin :: LayerPin }
  | OrderConstraint { before :: NodeId, after :: NodeId }
  | RelativePosition { anchor :: NodeId, target :: NodeId, offset :: GridPos }

derive instance Eq Constraints
derive instance Generic Constraints _

instance ReadForeign Constraints where
  readImpl = genericReadForeignTaggedSum Tagged.defaultOptions

instance WriteForeign Constraints where
  writeImpl = genericWriteForeignTaggedSum Tagged.defaultOptions

type Port =
  { id :: PortId
  , side :: Side
  , offset :: Int
  , label :: Maybe String
  }

type Node =
  { id :: NodeId
  , size :: GridSize
  , ports :: Array Port
  , label :: Maybe String
  , shape :: Shape
  }

type Endpoint =
  { node :: NodeId
  , port :: Maybe PortId
  }

type Edge =
  { id :: EdgeId
  , from :: Endpoint
  , to :: Endpoint
  , label :: Maybe Label
  }

edgeHasArrowhead :: EdgeId -> Boolean
edgeHasArrowhead (EdgeId edgeId) = case Str.stripPrefix (Str.Pattern "conn:") edgeId of
  Just _ -> false
  Nothing -> true

type Graph =
  { nodes :: Array Node
  , edges :: Array Edge
  , constraints :: Array Constraints
  }

readEnum :: forall a rep. Generic a rep => GenericEnumSumRep rep => Foreign -> F a
readEnum = genericReadForeignEnum Enum.defaultOptions

writeEnum :: forall a rep. Generic a rep => GenericEnumSumRep rep => a -> Foreign
writeEnum = genericWriteForeignEnum Enum.defaultOptions
