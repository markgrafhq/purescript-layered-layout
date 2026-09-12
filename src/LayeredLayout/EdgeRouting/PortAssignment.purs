module LayeredLayout.EdgeRouting.PortAssignment
  ( assignPorts
  , PortAssignment
  , EdgePortOffsets
  , portSlots
  , scaleFactor
  , orthoBends
  , sideExit
  , sideEntry
  , distributeAlongSide
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Ord (abs)
import Data.Tuple.Nested (type (/\), (/\))
import LayeredLayout.Graph (Edge, EdgeId(..), Endpoint, NodeId(..), Port, Side(..))
import LayeredLayout.Grid (gridX, gridY, sizeH, sizeW)
import LayeredLayout.DummyNodes (isDummy)
import LayeredLayout.Result (NodePlacement)

scaleFactor :: Int
scaleFactor = 4

-- | Port-distribution result: per-edge x-offset on a node side, in
-- | fine-grid coordinates relative to the node's left edge. Built by
-- | `LayeredLayout.PortDistribution.distributePorts` (post-BK) and
-- | consumed by `assignPorts` (edge routing) so the recorded offset
-- | overrides the per-call `distributeAlongSide` re-derivation.
type EdgePortOffsets = Map (EdgeId /\ Side) Number

type PortAssignment =
  { edge :: Edge
  , fromPos :: Number /\ Number
  , toPos :: Number /\ Number
  , fromSide :: Side
  , toSide :: Side
  }

-- | Dummy ports follow their own BK-assigned centres. A label-dummy switch
-- | can leave consecutive ordinary dummies in different columns; copying
-- | the first dummy's column would move later ports inside unrelated node
-- | or loop-label envelopes. Aligned chains remain straight naturally.
assignPorts :: Array Edge -> Array NodePlacement -> Map NodeId (Array Port) -> Array { edgeId :: EdgeId, nodes :: Array NodeId } -> EdgePortOffsets -> Array PortAssignment
assignPorts edges placements portMap chains portOffsets = natural <#> applyTrunkOverride
  where
  natural = edges <#> assignEdge

  posMap = foldl (\m p -> M.insert p.node p m) M.empty placements

  sourceGroups = groupEdgesBy (\e -> e.from.node) edges
  targetGroups = groupEdgesBy (\e -> e.to.node) edges

  sidesMap = M.fromFoldable (edges <#> \e -> e.id /\ bestSides e)

  dummyPortsBySegId :: Map EdgeId { source :: Maybe Number, target :: Maybe Number }
  dummyPortsBySegId = M.fromFoldable (A.concatMap dummyPortEntries chains)

  dummyPortEntries chain
    | A.length chain.nodes <= 2 = []
    | otherwise = A.zipWith
        ( \source target ->
            EdgeId (un EdgeId chain.edgeId <> ":" <> un NodeId source <> "->" <> un NodeId target)
              /\ { source: dummyCentreX source, target: dummyCentreX target }
        )
        chain.nodes
        (A.drop 1 chain.nodes)

  dummyCentreX node
    | not (isDummy node) = Nothing
    | otherwise =
        M.lookup node posMap <#> \p ->
          gridX p.position * sfN + sizeW p.size * sfN / 2.0
        where
        sfN = Int.toNumber scaleFactor

  applyTrunkOverride a = case M.lookup a.edge.id dummyPortsBySegId of
    Nothing -> a
    Just ports -> do
      let (fx /\ fy) = a.fromPos
      let (tx /\ ty) = a.toPos
      a
        { fromPos = fromMaybe fx ports.source /\ fy
        , toPos = fromMaybe tx ports.target /\ ty
        }

  assignEdge :: Edge -> PortAssignment
  assignEdge edge = case explicitEndpoint edge.from, explicitEndpoint edge.to of
    Just source, Just target ->
      { edge
      , fromPos: source.position
      , toPos: target.position
      , fromSide: source.side
      , toSide: target.side
      }
    source, target -> do
      let sides = bestSides edge
      { edge
      , fromPos: case source of
          Just endpoint -> endpoint.position
          Nothing -> autoPort sides.from edge.from.node edge.id sourceGroups srcOrder _.from
      , toPos: case target of
          Just endpoint -> endpoint.position
          Nothing -> autoPort sides.to edge.to.node edge.id targetGroups tgtOrder _.to
      , fromSide: case source of
          Just endpoint -> endpoint.side
          Nothing -> sides.from
      , toSide: case target of
          Just endpoint -> endpoint.side
          Nothing -> sides.to
      }

  bestSides :: Edge -> { from :: Side, to :: Side }
  bestSides edge = case M.lookup edge.from.node posMap /\ M.lookup edge.to.node posMap of
    Just src /\ Just tgt -> pickBestSides src tgt
    _ -> { from: South, to: North }

  pickBestSides :: NodePlacement -> NodePlacement -> { from :: Side, to :: Side }
  pickBestSides src tgt = do
    let
      candidates =
        [ South /\ North
        , East /\ North
        , West /\ North
        , South /\ East
        , South /\ West
        , North /\ South
        , North /\ East
        , North /\ West
        , East /\ South
        , West /\ South
        , East /\ West
        , West /\ East
        ]
    let
      scored = candidates <#> \(from /\ to) ->
        { from, to, score: spanAwareBends from to src tgt * 10 + sidePenalty src tgt from to }
    case A.sortBy (\a b -> compare a.score b.score) scored # A.head of
      Just best -> { from: best.from, to: best.to }
      Nothing -> { from: South, to: North }

  -- Prefer natural layer-flow sides (South→North, North→South) over
  -- mixed exits (East/West→North, South→East/West). ELK always routes
  -- inter-layer edges through top/bottom ports for cleaner VHV routing.
  -- Penalty must outweigh one bend difference (10 = one bend).
  sidePenalty :: NodePlacement -> NodePlacement -> Side -> Side -> Int
  sidePenalty src tgt South North = if tgt.layer >= src.layer then 0 else 20
  sidePenalty src tgt North South = if tgt.layer <= src.layer then 0 else 20
  sidePenalty _ _ East West = 5
  sidePenalty _ _ West East = 5
  sidePenalty _ _ _ _ = 15

  -- Use span overlap to detect straight paths for opposing sides.
  -- autoPort picks the overlap center, so spans that overlap produce straight edges
  -- even when node centers differ (e.g. narrow "xx" above wide "yyyyyy").
  spanAwareBends :: Side -> Side -> NodePlacement -> NodePlacement -> Int
  spanAwareBends from to src tgt = do
    let exit = sideExit from src
    let entry = sideEntry to tgt
    let base = orthoBends from to exit entry
    if base > 0 then case from /\ to of
      South /\ North -> checkSpanOverlap South src tgt exit entry
      North /\ South -> checkSpanOverlap North src tgt exit entry
      East /\ West -> checkSpanOverlap East src tgt exit entry
      West /\ East -> checkSpanOverlap West src tgt exit entry
      _ -> base
    else base
    where
    checkSpanOverlap side src' tgt' exit' entry' = do
      let srcSpan = nodeSpan side src'
      let tgtSpan = nodeSpan side tgt'
      let hasOverlap = srcSpan.lo < tgtSpan.hi && tgtSpan.lo < srcSpan.hi
      let (_ /\ ey) = exit'
      let (_ /\ ny) = entry'
      let (ex /\ _) = exit'
      let (nx /\ _) = entry'
      let
        dirOk = case from /\ to of
          South /\ North -> ny > ey
          North /\ South -> ny < ey
          East /\ West -> nx > ex
          West /\ East -> nx < ex
          _ -> false
      if hasOverlap && dirOk then 0
      else orthoBends from to exit' entry'

  -- Order source ports by target node position (left targets → left ports)
  srcOrder :: Side -> Edge -> Number
  srcOrder side e = case M.lookup e.to.node posMap of
    Nothing -> 0.0
    Just p -> axisCenter side p

  -- Order target ports by source node position (left sources → left ports)
  tgtOrder :: Side -> Edge -> Number
  tgtOrder side e = case M.lookup e.from.node posMap of
    Nothing -> 0.0
    Just p -> axisCenter side p

  axisCenter :: Side -> NodePlacement -> Number
  axisCenter side p = case side of
    South -> gridX p.position * sfN + sizeW p.size * sfN / 2.0
    North -> gridX p.position * sfN + sizeW p.size * sfN / 2.0
    East -> gridY p.position * sfN + sizeH p.size * sfN / 2.0
    West -> gridY p.position * sfN + sizeH p.size * sfN / 2.0
    where
    sfN = Int.toNumber scaleFactor

  explicitEndpoint :: Endpoint -> Maybe { position :: Number /\ Number, side :: Side }
  explicitEndpoint endpoint = do
    id <- endpoint.port
    placement <- M.lookup endpoint.node posMap
    ports <- M.lookup endpoint.node portMap
    port <- A.find (\candidate -> candidate.id == id) ports
    pure { position: portToFineGrid placement port, side: port.side }

  -- Port assignment. Prefers the pre-computed `portOffsets` (port of
  -- ELK's `NodeRelativePortDistributor`) when available, since that map
  -- is the single source of truth for North/South port positions and
  -- sorts siblings by layer order rather than post-BK x. Falls back to
  -- per-call `distributeAlongSide` for sides absent from the map (East,
  -- West) or for edges without a recorded offset.
  autoPort :: Side -> NodeId -> EdgeId -> Map NodeId (Array Edge) -> (Side -> Edge -> Number) -> ({ from :: Side, to :: Side } -> Side) -> Number /\ Number
  autoPort side nodeId edgeId groups orderFn sideExtract = case M.lookup nodeId posMap of
    Nothing -> 0.0 /\ 0.0
    Just placement -> case M.lookup (edgeId /\ side) portOffsets of
      Just localX -> posOnSide side placement (gridX placement.position * sfN + localX)
      Nothing -> fallback placement
    where
    sfN = Int.toNumber scaleFactor
    fallback placement = do
      let allSiblings = fromMaybe [] (M.lookup nodeId groups)
      let
        siblings = A.filter
          ( \e ->
              case M.lookup e.id sidesMap of
                Just s -> sideExtract s == side
                Nothing -> true
          )
          allSiblings
      let span = nodeSpan side placement
      let sorted = A.sortBy (\a b -> compare (orderFn side a) (orderFn side b)) siblings
      let positions = distributeAlongSide span (sorted <#> _.id)
      let x = fromMaybe ((span.lo + span.hi) / 2.0) (M.lookup edgeId positions)
      posOnSide side placement x

  posOnSide :: Side -> NodePlacement -> Number -> Number /\ Number
  posOnSide side p x = case side of
    South -> x /\ ((gridY p.position + sizeH p.size) * sfN)
    North -> x /\ (gridY p.position * sfN)
    East -> ((gridX p.position + sizeW p.size) * sfN) /\ x
    West -> (gridX p.position * sfN) /\ x
    where
    sfN = Int.toNumber scaleFactor

  portToFineGrid :: NodePlacement -> Port -> Number /\ Number
  portToFineGrid placement port = case port.side of
    North -> (gridX placement.position * sfN + Int.toNumber port.offset * sfN) /\ (gridY placement.position * sfN)
    South -> (gridX placement.position * sfN + Int.toNumber port.offset * sfN) /\ ((gridY placement.position + sizeH placement.size) * sfN)
    East -> ((gridX placement.position + sizeW placement.size) * sfN) /\ (gridY placement.position * sfN + Int.toNumber port.offset * sfN)
    West -> (gridX placement.position * sfN) /\ (gridY placement.position * sfN + Int.toNumber port.offset * sfN)
    where
    sfN = Int.toNumber scaleFactor

  assignSlots :: Array { id :: EdgeId, target :: Int } -> Array Int -> Array { id :: EdgeId, x :: Int }
  assignSlots targets availableSlots = _.result $ foldl go { available: availableSlots, result: [] } targets
    where
    go acc t = case nearestSlot t.target acc.available of
      Nothing -> acc { result = acc.result <> [ { id: t.id, x: t.target } ] }
      Just slot -> acc { available = A.filter (_ /= slot) acc.available, result = acc.result <> [ { id: t.id, x: slot } ] }
    nearestSlot target slots = A.head (A.sortBy (\a b -> compare (abs (a - target)) (abs (b - target))) slots)

-- | Even-distribution port-spacing formula from ELK's `LGraphUtil.placePorts`.
-- |
-- |     position_i = lo + (i + 1) * width / (n + 1)
-- |
-- | with `i` ∈ [0, n), so 1 sibling sits at center; 2 at thirds; 3 at quarters.
-- | The order of the input array decides which sibling lands at which slot —
-- | callers sort by their preferred criterion (target layer-index, source
-- | barycenter, etc.) before passing in.
distributeAlongSide
  :: { lo :: Number, hi :: Number }
  -> Array EdgeId
  -> Map EdgeId Number
distributeAlongSide span sorted = case A.length sorted of
  0 -> M.empty
  1 -> M.fromFoldable (sorted <#> \eid -> eid /\ centre)
  n -> M.fromFoldable
    ( A.mapWithIndex
        (\i eid -> eid /\ (span.lo + Int.toNumber (i + 1) * width / Int.toNumber (n + 1)))
        sorted
    )
  where
  centre = (span.lo + span.hi) / 2.0
  width = span.hi - span.lo

groupEdgesBy :: forall a. Ord a => (Edge -> a) -> Array Edge -> Map a (Array Edge)
groupEdgesBy keyFn = foldl (\m e -> M.insertWith (<>) (keyFn e) [ e ] m) M.empty

nodeSpan :: Side -> NodePlacement -> { lo :: Number, hi :: Number }
nodeSpan side p = case side of
  South -> { lo: gridX p.position * sf, hi: (gridX p.position + sizeW p.size) * sf }
  North -> { lo: gridX p.position * sf, hi: (gridX p.position + sizeW p.size) * sf }
  East -> { lo: gridY p.position * sf, hi: (gridY p.position + sizeH p.size) * sf }
  West -> { lo: gridY p.position * sf, hi: (gridY p.position + sizeH p.size) * sf }
  where
  sf = Int.toNumber scaleFactor

sideDim :: Side -> NodePlacement -> Int
sideDim side p = case side of
  North -> Int.round (sizeW p.size)
  South -> Int.round (sizeW p.size)
  East -> Int.round (sizeH p.size)
  West -> Int.round (sizeH p.size)

portSlots :: Side -> NodePlacement -> Array Number
portSlots side p = A.nub $ A.sort $ cellCenters <> [ overallCenter ]
  where
  span = nodeSpan side p
  dim = sideDim side p
  sfN = Int.toNumber scaleFactor
  cellCenters = A.range 0 (dim - 1) <#> \i -> span.lo + Int.toNumber i * sfN + sfN / 2.0
  overallCenter = (span.lo + span.hi) / 2.0

-- Exit point: center of the given side (in grid coords * 2 to avoid fractions)
sideExit :: Side -> NodePlacement -> Number /\ Number
sideExit side p = case side of
  South -> cx /\ (bot * 2.0)
  North -> cx /\ (top * 2.0)
  East -> (right * 2.0) /\ cy
  West -> (left * 2.0) /\ cy
  where
  left = gridX p.position
  right = left + sizeW p.size
  top = gridY p.position
  bot = top + sizeH p.size
  cx = left * 2.0 + sizeW p.size
  cy = top * 2.0 + sizeH p.size

-- Entry point: center of the given side (in grid coords * 2)
sideEntry :: Side -> NodePlacement -> Number /\ Number
sideEntry = sideExit

-- Minimum orthogonal bends for a path from exit to entry with given side constraints.
-- Exit direction is away from the fromSide; entry direction is into the toSide.
orthoBends :: Side -> Side -> Number /\ Number -> Number /\ Number -> Int
orthoBends fromSide toSide (ex /\ ey) (nx /\ ny) = do
  let
    straight = case fromSide /\ toSide of
      South /\ North -> ex == nx && ny > ey
      North /\ South -> ex == nx && ny < ey
      East /\ West -> ey == ny && nx > ex
      West /\ East -> ey == ny && nx < ex
      _ -> false
  let
    corner1 = isVerticalExit fromSide && isHorizontalEntry toSide
      && exitToward ey ny fromSide
      && entryToward ex nx toSide
  let
    corner2 = isHorizontalExit fromSide && isVerticalEntry toSide
      && exitToward ex nx fromSide
      && entryToward ey ny toSide
  if straight then 0
  else if corner1 || corner2 then 1
  else 2

isVerticalExit :: Side -> Boolean
isVerticalExit South = true
isVerticalExit North = true
isVerticalExit _ = false

isHorizontalExit :: Side -> Boolean
isHorizontalExit East = true
isHorizontalExit West = true
isHorizontalExit _ = false

isVerticalEntry :: Side -> Boolean
isVerticalEntry North = true
isVerticalEntry South = true
isVerticalEntry _ = false

isHorizontalEntry :: Side -> Boolean
isHorizontalEntry East = true
isHorizontalEntry West = true
isHorizontalEntry _ = false

-- Does the exit direction from this side go toward the target coordinate?
exitToward :: Number -> Number -> Side -> Boolean
exitToward from to South = to > from
exitToward from to North = to < from
exitToward from to East = to > from
exitToward from to West = to < from

-- Does the entry direction into this side come from the correct direction?
-- Entry from East means path approaches going left (source x > entry x)
-- Entry from North means path approaches going down (source y < entry y)
entryToward :: Number -> Number -> Side -> Boolean
entryToward exitCoord entryCoord East = exitCoord > entryCoord
entryToward exitCoord entryCoord West = exitCoord < entryCoord
entryToward exitCoord entryCoord North = exitCoord < entryCoord
entryToward exitCoord entryCoord South = exitCoord > entryCoord
