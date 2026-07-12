module LayeredLayout.Grid where

import Prelude

import Data.Number (abs)
import Data.Newtype (class Newtype, un)
import Data.Tuple.Nested (type (/\), (/\))
import Yoga.JSON (class ReadForeign, class WriteForeign)
import Yoga.JSON.Derive (readVia, writeVia)

-- | A grid coordinate. Number-valued so the layout algorithm can
-- produce fractional positions matching ELK's precision.
newtype GridPos = GridPos (Number /\ Number)

derive instance Newtype GridPos _
derive newtype instance Eq GridPos
derive newtype instance Ord GridPos
derive newtype instance Show GridPos

instance ReadForeign GridPos where
  readImpl = readVia @(Number /\ Number)

instance WriteForeign GridPos where
  writeImpl = writeVia @(Number /\ Number)

newtype GridSize = GridSize (Number /\ Number)

derive instance Newtype GridSize _
derive newtype instance Eq GridSize
derive newtype instance Ord GridSize
derive newtype instance Show GridSize

instance ReadForeign GridSize where
  readImpl = readVia @(Number /\ Number)

instance WriteForeign GridSize where
  writeImpl = writeVia @(Number /\ Number)

type GridRect = { pos :: GridPos, size :: GridSize }

gridX :: GridPos -> Number
gridX p = do
  let (x /\ _) = un GridPos p
  x

gridY :: GridPos -> Number
gridY p = do
  let (_ /\ y) = un GridPos p
  y

sizeW :: GridSize -> Number
sizeW s = do
  let (w /\ _) = un GridSize s
  w

sizeH :: GridSize -> Number
sizeH s = do
  let (_ /\ h) = un GridSize s
  h

addPos :: GridPos -> GridPos -> GridPos
addPos a b = GridPos ((gridX a + gridX b) /\ (gridY a + gridY b))

subPos :: GridPos -> GridPos -> GridPos
subPos a b = GridPos ((gridX a - gridX b) /\ (gridY a - gridY b))

manhattan :: GridPos -> GridPos -> Number
manhattan a b = abs (gridX a - gridX b) + abs (gridY a - gridY b)

overlaps :: GridRect -> GridRect -> Boolean
overlaps r1 r2 = xOverlap && yOverlap
  where
  x1 = gridX r1.pos
  y1 = gridY r1.pos
  w1 = sizeW r1.size
  h1 = sizeH r1.size
  x2 = gridX r2.pos
  y2 = gridY r2.pos
  w2 = sizeW r2.size
  h2 = sizeH r2.size
  xOverlap = x1 < x2 + w2 && x2 < x1 + w1
  yOverlap = y1 < y2 + h2 && y2 < y1 + h1

contains :: GridRect -> GridPos -> Boolean
contains rect p = inX && inY
  where
  rx = gridX rect.pos
  ry = gridY rect.pos
  w = sizeW rect.size
  h = sizeH rect.size
  px = gridX p
  py = gridY p
  inX = px >= rx && px < rx + w
  inY = py >= ry && py < ry + h
