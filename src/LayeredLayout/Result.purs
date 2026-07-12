module LayeredLayout.Result where

import Prelude

import Data.Generic.Rep (class Generic)
import Foreign (F, Foreign)
import LayeredLayout.Graph (EdgeId, NodeId)
import LayeredLayout.Grid (GridPos, GridRect, GridSize)
import Yoga.JSON (class ReadForeign, class WriteForeign)
import Yoga.JSON.Generics.EnumSumRep (class GenericEnumSumRep, genericReadForeignEnum, genericWriteForeignEnum)
import Yoga.JSON.Generics.EnumSumRep as Enum

data Direction = H | V

derive instance Eq Direction
derive instance Ord Direction
derive instance Generic Direction _

instance Show Direction where
  show H = "H"
  show V = "V"

instance ReadForeign Direction where
  readImpl = readEnum

instance WriteForeign Direction where
  writeImpl = writeEnum

data BendType = LeftTurn | RightTurn

derive instance Eq BendType
derive instance Ord BendType
derive instance Generic BendType _

instance Show BendType where
  show LeftTurn = "LeftTurn"
  show RightTurn = "RightTurn"

instance ReadForeign BendType where
  readImpl = readEnum

instance WriteForeign BendType where
  writeImpl = writeEnum

type EdgeSegment =
  { start :: GridPos
  , end :: GridPos
  , direction :: Direction
  }

type LineJump =
  { position :: GridPos
  , crossingEdge :: EdgeId
  }

type EdgePath =
  { edge :: EdgeId
  , segments :: Array EdgeSegment
  , bends :: Array GridPos
  , bendType :: Array BendType
  , jumps :: Array LineJump
  -- True when the edge's logical direction was flipped during Phase 1
  -- cycle removal. Equivalent to ELK's ReversedEdgeRestorer marking;
  -- renderers should draw the arrowhead at `from` rather than `to`.
  , reversed :: Boolean
  }

type NodePlacement =
  { node :: NodeId
  , position :: GridPos
  , size :: GridSize
  , layer :: Int
  , order :: Int
  }

type Metrics =
  { crossingCount :: Int
  , bendCount :: Int
  , totalEdgeLength :: Number
  , maxEdgeLength :: Number
  , nodeOverlapCount :: Int
  , constraintViolations :: Int
  , jumpCount :: Int
  }

type LayoutResult =
  { nodes :: Array NodePlacement
  , edges :: Array EdgePath
  , boundingBox :: GridRect
  , metrics :: Metrics
  }

readEnum :: forall a rep. Generic a rep => GenericEnumSumRep rep => Foreign -> F a
readEnum = genericReadForeignEnum Enum.defaultOptions

writeEnum :: forall a rep. Generic a rep => GenericEnumSumRep rep => a -> Foreign
writeEnum = genericWriteForeignEnum Enum.defaultOptions
