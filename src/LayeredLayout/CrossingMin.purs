-- Copyright (c) 2010, 2012, 2015, 2016, 2020 Kiel University and others.
-- SPDX-License-Identifier: EPL-2.0
-- Port of ELK's SortByInputModelProcessor, BarycenterHeuristic,
-- LayerSweepCrossingMinimizer and two-sided GreedySwitchHeuristic.
module LayeredLayout.CrossingMin
  ( minimize
  , countCrossings
  , countAllCrossings
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl, minimum, sum)
import Data.Int (toNumber)
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Set as S
import Data.Tuple.Nested (type (/\), (/\))
import LayeredLayout.CrossingMin.Ports (Ports)
import LayeredLayout.CrossingMin.Ports as P
import LayeredLayout.CrossingMin.Constraints as ConstraintGroups
import LayeredLayout.CrossingMin.NorthSouth as NS
import LayeredLayout.DummyNodes (isDummy, isLabelDummy)
import LayeredLayout.Graph (Constraints(..), Edge, EdgeId(..), NodeId(..), Port, Side(..))
import LayeredLayout.JavaRandom (Random)
import LayeredLayout.JavaRandom as JR
import LayeredLayout.PortDistribution (PortOrder)
import LayeredLayout.PortDummies as PortDummies

type Config =
  { iterations :: Int
  , constraints :: Array Constraints
  , modelOrder :: Map NodeId Int
  , ports :: Map NodeId (Array Port)
  , chains :: Array { edgeId :: EdgeId, nodes :: Array NodeId }
  , random :: Random
  , reversed :: S.Set EdgeId
  , portDummies :: PortDummies.State
  }

type Order = { layout :: Array (Array NodeId), ports :: Ports }

minimize :: Config -> Array (Array NodeId) -> Array Edge -> { layout :: Array (Array NodeId), random :: Random, portOrder :: PortOrder }
minimize cfg layers edges =
  { layout: final.layout, portOrder: P.toOrder final.ports, random: finalRandom }
  where
  seed /\ constructionRandom = JR.nextLongBI cfg.random
  relative /\ _ = JR.nextBoolean constructionRandom
  initialRandom = JR.mkRandomBI seed
  constraints = A.mapMaybe
    ( case _ of
        OrderConstraint c -> Just c
        _ -> Nothing
    )
    cfg.constraints
  modelActive = not (M.isEmpty cfg.modelOrder)
  northSouth = NS.metadata cfg.portDummies cfg.ports
  initial = seeded
    { layout = map
        ( \layer ->
            if M.isEmpty northSouth.dummies || A.null constraints then enforce constraints layer
            else resolveUnits northSouth constraints (A.mapWithIndex (\i node -> { node, key: toNumber i }) layer)
        )
        seeded.layout
    }
  beforeNS = PortDummies.unprepare cfg.portDummies { layers, edges, chains: cfg.chains }
  modelSorted = preprocess cfg beforeNS.edges { layout: beforeNS.layers, ports: P.build cfg.ports beforeNS.edges }
  seeded =
    if A.null cfg.portDummies.dummies then modelSorted
    else
      { layout: PortDummies.expand cfg.portDummies modelSorted.layout
      , ports: P.rewire cfg.ports edges modelSorted.ports
      }
  active = cfg.iterations > 0 && A.length (A.concat layers) > 1
  trials = foldl trial
    { current: initial, best: initial, score: 2147483647, random: initialRandom }
    (A.range 0 (max 0 cfg.iterations - 1))
  trial acc index
    | not active || acc.score == 0 = acc
    | otherwise =
        let
          -- ELK 0.11.1 effectively preserves model order only on trial zero.
          -- Its FIRST_TRY and SECOND_TRY properties share the same key, so
          -- clearing SECOND_TRY clears both. Subsequent trials randomize.
          preserveInitial = modelActive && index == 0
          bit /\ r1 = JR.nextBoolean acc.random
          forward = if preserveInitial then true else bit
          randomized /\ r2 = if preserveInitial then acc.current /\ r1 else randomize forward acc.current r1
          firstResult =
            if preserveInitial && countPhysical northSouth acc.current edges == 0 then { current: acc.current, best: acc.current, score: 0, random: r1 }
            else
              let
                swept /\ r3 = sweep relative northSouth cfg constraints (not preserveInitial) forward randomized r2
              in
                converge (not forward) swept (countPhysical northSouth swept edges) r3
        in
          { current: firstResult.current
          , best: if firstResult.score < acc.score then firstResult.best else acc.best
          , score: min acc.score firstResult.score
          , random: firstResult.random
          }
  -- A rejected sweep remains the starting state of the next trial. Only the
  -- saved best snapshot is restored for transfer to the following processor.
  converge forward current score random
    | score == 0 = { current, best: current, score, random }
    | otherwise =
        let
          next /\ r = sweep relative northSouth cfg constraints false forward current random
          nextScore = countPhysical northSouth next edges
        in
          if nextScore < score then converge (not forward) next nextScore r
          else { current: next, best: current, score, random: r }
  randomize forward current random =
    let
      index = if forward then 0 else A.length current.layout - 1
      first = fromMaybe [] (A.index current.layout index)
      shuffled /\ r =
        if A.null cfg.portDummies.dummies then JR.randomShuffle random first
        else
          let
            assign (values /\ rng) node = let key /\ next = JR.nextDouble rng in Cons { node, key } values /\ next
            keyed /\ next = foldl assign (Nil /\ random) first
            sorted = A.sortBy (comparing _.key) (A.reverse (A.fromFoldable keyed))
            ordered = resolveUnits northSouth constraints sorted
          in
            ordered /\ next
      ordered = enforce constraints shuffled
    in
      current { layout = fromMaybe current.layout (A.updateAt index ordered current.layout) } /\ r
  originalCount = A.length (A.filter (\n -> not (isDummy n || isLabelDummy n || M.member n northSouth.dummies)) (A.concat layers))
  greedyActive = active && originalCount < 40
  _ /\ afterGreedySeed = JR.nextLongBI trials.random
  greedyForward /\ afterGreedyDirection = JR.nextBoolean afterGreedySeed
  final = if not active then initial else if greedyActive then greedy constraints northSouth cfg.ports edges greedyForward trials.best else trials.best
  finalRandom = if not active then cfg.random else if greedyActive then afterGreedyDirection else trials.random

-- Node comparisons involving dummies depend on already sorted ports in the
-- preceding layer. Two insertion-sort passes bracket each layer's port sort.
preprocess :: Config -> Array Edge -> Order -> Order
preprocess cfg edges initial
  | M.isEmpty cfg.modelOrder = initial
  | otherwise = foldl layer initial (A.mapWithIndex (\i _ -> i) initial.layout)
      where
      edgeOrder = M.fromFoldable (A.mapWithIndex (\i e -> e.id /\ i) edges)
      chainOrder = M.fromFoldable
        ( A.concat
            ( A.mapWithIndex
                ( \i chain ->
                    A.mapMaybe
                      ( \e ->
                          if belongs chain e then Just (e.id /\ i)
                          else Nothing
                      )
                      edges
                )
                cfg.chains
            )
        )
      modelEdge e = fromMaybe (fromMaybe 0 (M.lookup e.id edgeOrder)) (M.lookup e.id chainOrder)
      target e = fromMaybe e.to.node do
        chain <- A.find (\c -> belongs c e) cfg.chains
        A.last chain.nodes
      belongs chain e = chain.edgeId == e.id ||
        ( A.length chain.nodes > 2
            && A.any (\(a /\ b) -> e.id == EdgeId (un EdgeId chain.edgeId <> ":" <> un NodeId a <> "->" <> un NodeId b))
              (A.zip chain.nodes (A.drop 1 chain.nodes))
        )
      reversedSegments = S.fromFoldable
        ( map _.id
            ( A.filter
                (\e -> S.member e.id cfg.reversed || A.any (\c -> S.member c.edgeId cfg.reversed && belongs c e) cfg.chains)
                edges
            )
        )
      layer acc index =
        let
          previous = fromMaybe [] (A.index acc.layout (index - 1))
          nodes = fromMaybe [] (A.index acc.layout index)
          compareNodes ports a b = case M.lookup a cfg.modelOrder /\ M.lookup b cfg.modelOrder of
            Just x /\ Just y -> compare x y
            _ -> case incoming ports previous a /\ incoming ports previous b of
              Just x /\ Just y -> case compare (A.elemIndex x.from.node previous) (A.elemIndex y.from.node previous) of
                EQ -> compare (portRank ports x South) (portRank ports y South)
                result -> result
              Just x /\ Nothing -> compare (modelEdge x) 2147483647
              Nothing /\ Just y -> compare 2147483647 (modelEdge y)
              _ -> EQ
          first = modelSort (compareNodes acc.ports) nodes
          sortedPorts = foldl (sortPorts previous) acc.ports first
          second = modelSort (compareNodes sortedPorts) first
        in
          { layout: fromMaybe acc.layout (A.updateAt index second acc.layout), ports: sortedPorts }
      incoming ports previous node = A.find (\e -> A.elem e.from.node previous)
        (A.concatMap _.edges (A.reverse (P.groups node North ports)))
      portRank ports e side = fromMaybe 0
        ( A.findIndex (A.any (\other -> other.id == e.id) <<< _.edges)
            (P.groups (if side == South then e.from.node else e.to.node) side ports)
        )
      sortPorts previous ports node = foldl (sortSide previous node) ports [ South, North ]
      sortSide previous node ports side =
        let
          ps = P.groups node side ports
          groupMinimum p = fromMaybe 0 do
            e <- A.head p.edges
            let matching = A.filter (\other -> target other == target e && not (S.member other.id reversedSegments)) (A.concatMap _.edges ps)
            pure (fromMaybe (modelEdge e) (minimum (map modelEdge matching)))
          compareOut a b = case compare (groupMinimum a) (groupMinimum b) of
            EQ -> compare (map modelEdge (A.head a.edges)) (map modelEdge (A.head b.edges))
            result -> result
          compareIn a b = case A.head a.edges /\ A.head b.edges of
            Just x /\ Just y -> case compare (A.elemIndex x.from.node previous) (A.elemIndex y.from.node previous) of
              EQ -> compare (portRank ports x South) (portRank ports y South)
              result -> result
            _ -> EQ
          sorted = if P.fixed cfg.ports node then ps else A.sortBy (if side == South then compareOut else compareIn) ps
        in
          P.reorder node side sorted ports

-- ELK remembers each model-order comparison transitively; its mixed
-- node/edge comparator is not a global scalar key.
modelSort :: (NodeId -> NodeId -> Ordering) -> Array NodeId -> Array NodeId
modelSort cmp nodes = (foldl insert { nodes: [], before: M.empty } nodes).nodes
  where
  insert acc node = walk acc (A.length acc.nodes - 1)
    where
    place current index = current { nodes = fromMaybe current.nodes (A.insertAt (index + 1) node current.nodes) }
    walk current index = case A.index current.nodes index of
      Nothing -> place current index
      Just other ->
        let
          known a b = S.member b (fromMaybe S.empty (M.lookup a current.before))
          result = if known other node then LT else if known node other then GT else cmp other node
          smaller = if result == GT then node else other
          bigger = if result == GT then other else node
          successors = S.insert bigger (fromMaybe S.empty (M.lookup bigger current.before))
          predecessors = S.insert smaller
            ( S.fromFoldable
                ( A.mapMaybe (\(n /\ ns) -> if S.member smaller ns then Just n else Nothing)
                    (M.toUnfoldable current.before :: Array (NodeId /\ S.Set NodeId))
                )
            )
          before = foldl (\m n -> M.insertWith S.union n successors m) current.before predecessors
          next = current { before = before }
        in
          if result == GT then walk next (index - 1)
          else place next index

sweep :: Boolean -> NS.Metadata -> Config -> Array { before :: NodeId, after :: NodeId } -> Boolean -> Boolean -> Order -> Random -> Order /\ Random
sweep relative northSouth cfg constraints firstSweep forward initial random = foldl step (initial /\ random) indices
  where
  indices = if forward then A.mapWithIndex (\i _ -> i) initial.layout else A.reverse (A.mapWithIndex (\i _ -> i) initial.layout)
  start = if forward then 0 else A.length initial.layout - 1
  side = if forward then North else South
  opposite = if forward then South else North
  step (state /\ r) index
    | index == start = state /\ r
    | otherwise =
        let
          reference = fromMaybe [] (A.index state.layout (index + if forward then -1 else 1))
          free = fromMaybe [] (A.index state.layout index)
          ranks = P.ranks relative reference opposite state.ports
          sorted /\ r1 = barycenters northSouth state.ports ranks constraints (not firstSweep) forward free r
          ports1 = P.distribute cfg.ports sorted side ranks state.ports
          reverseRanks = P.ranks relative sorted side ports1
          ports2 = P.distribute cfg.ports reference opposite reverseRanks ports1
        in
          { layout: fromMaybe state.layout (A.updateAt index sorted state.layout), ports: ports2 } /\ r1

barycenters :: NS.Metadata -> Ports -> Map EdgeId Number -> Array { before :: NodeId, after :: NodeId } -> Boolean -> Boolean -> Array NodeId -> Random -> Array NodeId /\ Random
barycenters northSouth ports ranks constraints preOrdered forward nodes random = ordered /\ filled.random
  where
  computed = foldl (\acc node -> calculate S.empty node acc) { states: M.empty, random } nodes
  ordered =
    if M.isEmpty northSouth.dummies then enforce constraints (map _.node sorted)
    else resolveUnits northSouth constraints sorted
  calculate visiting node acc
    | M.member node acc.states || S.member node visiting = acc
    | otherwise =
        let
          nextVisiting = S.insert node visiting
          nodePorts = P.groups node (if forward then North else South) ports
          connecting = A.concatMap _.edges (if forward then A.reverse nodePorts else nodePorts)
          aggregateEdges = foldl
            ( \state e ->
                let
                  other = if forward then e.from.node else e.to.node
                in
                  if A.elem other nodes then
                    let
                      rec = calculate nextVisiting other { states: state.states, random: state.random }
                      dependency = fromMaybe { node: other, weight: 0.0, degree: 0, value: Nothing } (M.lookup other rec.states)
                    in
                      state { states = rec.states, random = rec.random, weight = state.weight + dependency.weight, degree = state.degree + dependency.degree }
                  else case M.lookup e.id ranks of
                    Just rank -> state { weight = state.weight + rank, degree = state.degree + 1 }
                    Nothing -> state
            )
            { states: acc.states, random: acc.random, weight: 0.0, degree: 0 }
            connecting
          aggregate = foldl
            ( \state other ->
                if not (A.elem other nodes) then state
                else
                  let
                    rec = calculate nextVisiting other { states: state.states, random: state.random }
                    dependency = fromMaybe { node: other, weight: 0.0, degree: 0, value: Nothing } (M.lookup other rec.states)
                  in
                    state { states = rec.states, random = rec.random, weight = state.weight + dependency.weight, degree = state.degree + dependency.degree }
            )
            aggregateEdges
            (NS.associates northSouth node)
          bits /\ r = if aggregate.degree > 0 then JR.next 24 aggregate.random else 0 /\ aggregate.random
          weight = aggregate.weight + if aggregate.degree > 0 then toNumber bits * 5.9604644775390625e-8 * 0.07000000029802322 - 0.03500000014901161 else 0.0
          value = if aggregate.degree > 0 then Just (weight / toNumber aggregate.degree) else Nothing
          bary = { node, weight, degree: aggregate.degree, value }
        in
          { states: M.insert node bary aggregate.states, random: r }
  raw = A.mapMaybe (\node -> M.lookup node computed.states) nodes
  maximum = 2.0 + foldl (\m b -> max m (fromMaybe 0.0 b.value)) 0.0 raw
  filled = foldl fill { nodes: [], last: -1.0, random: computed.random } (A.mapWithIndex (/\) raw)
  fill acc (index /\ bary) = case bary.value of
    Just value -> acc { nodes = A.snoc acc.nodes { node: bary.node, key: value }, last = value }
    Nothing ->
      let
        next = fromMaybe (acc.last + 1.0) (A.head (A.mapMaybe _.value (A.drop (index + 1) raw)))
        bits /\ r = if preOrdered then 0 /\ acc.random else JR.next 24 acc.random
        value = if preOrdered then (acc.last + next) / 2.0 else toNumber bits * 5.9604644775390625e-8 * maximum - 1.0
      in
        { nodes: A.snoc acc.nodes { node: bary.node, key: value }, last: value, random: r }
  sorted = A.sortBy (comparing _.key) filled.nodes

-- Forster resolves normal-node precedence first. Only then can layout-unit
-- precedence be derived without introducing constraints in the opposite order.
resolveUnits :: NS.Metadata -> Array { before :: NodeId, after :: NodeId } -> Array { node :: NodeId, key :: Number } -> Array NodeId
resolveUnits northSouth constraints values = map _.node
  (ConstraintGroups.resolve (constraints <> NS.constraints northSouth (map _.node first)) first)
  where
  normal node = not (isDummy node || isLabelDummy node || M.member node northSouth.dummies)
  betweenNormals = A.filter (\c -> normal c.before && normal c.after) constraints
  first = if A.null betweenNormals then values else ConstraintGroups.resolve betweenNormals values

-- Explicit constraints are hard precedence, not a barycenter tie breaker.
-- A stable topological selection handles transitive constraints in one pass.
enforce :: Array { before :: NodeId, after :: NodeId } -> Array NodeId -> Array NodeId
enforce constraints = go []
  where
  go done remaining
    | A.null remaining = done
    | otherwise = case A.findIndex (\node -> not (A.any (\c -> c.after == node && A.elem c.before remaining) constraints)) remaining of
        Nothing -> done <> remaining
        Just index -> case A.index remaining index /\ A.deleteAt index remaining of
          Just node /\ Just rest -> go (A.snoc done node) rest
          _ -> done <> remaining

countPhysical :: NS.Metadata -> Order -> Array Edge -> Int
countPhysical northSouth state edges = countWith hyperedgeCrossings state edges
  + NS.crossings northSouth state.ports state.layout

countStraight :: Order -> Array Edge -> Int
countStraight = countWith inversions

countWith :: (Array (Number /\ Number) -> Int) -> Order -> Array Edge -> Int
countWith counter state edges = foldl pair 0 (A.zip state.layout (A.drop 1 state.layout))
  where
  pair acc (left /\ right) =
    let
      a = P.ranks false left South state.ports
      b = P.ranks false right North state.ports
      endpoints = A.mapMaybe (\e -> (/\) <$> M.lookup e.id a <*> M.lookup e.id b) edges
    in
      acc + counter endpoints

-- ELK HyperedgeCrossingsCounter: connected physical endpoints form one
-- hyperedge. Count inversions of its upper corners, then overlapping spans
-- on either side. Singleton hyperedges reduce to ordinary edge inversions.
hyperedgeCrossings :: Array (Number /\ Number) -> Int
hyperedgeCrossings endpoints = inversions (map (\b -> b.leftLo /\ b.rightLo) bounds)
  + overlaps _.leftLo _.leftHi
  + overlaps _.rightLo _.rightHi
  where
  components = foldl join [] endpoints
  join current (left /\ right) =
    let
      touches c = S.member left c.left || S.member right c.right
      matching = A.filter touches current
      merged = foldl (\c other -> { left: S.union c.left other.left, right: S.union c.right other.right })
        { left: S.singleton left, right: S.singleton right }
        matching
    in
      A.snoc (A.filter (not <<< touches) current) merged
  bounds = map
    ( \c ->
        { leftLo: fromMaybe 0.0 (S.findMin c.left)
        , leftHi: fromMaybe 0.0 (S.findMax c.left)
        , rightLo: fromMaybe 0.0 (S.findMin c.right)
        , rightHi: fromMaybe 0.0 (S.findMax c.right)
        }
    )
    components
  overlaps lo hi = foldl (\n (i /\ a) -> n + A.length (A.filter (\b -> lo a < hi b && lo b < hi a) (A.drop (i + 1) bounds)))
    0
    (A.mapWithIndex (/\) bounds)

inversions :: Array (Number /\ Number) -> Int
inversions pairs = foldl (\count (i /\ (a /\ b)) -> count + A.length (A.filter (\(c /\ d) -> (a - c) * (b - d) < 0.0) (A.drop (i + 1) pairs)))
  0
  (A.mapWithIndex (/\) pairs)

-- Public node-only metric retained for callers measuring layer permutations.
-- The minimizer itself always counts ordered physical endpoints.
countCrossings :: Array NodeId -> Array NodeId -> Array Edge -> Int
countCrossings left right edges = inversions (A.mapMaybe endpoint edges)
  where
  endpoint e = case A.elemIndex e.from.node left /\ A.elemIndex e.to.node right of
    Just a /\ Just b -> Just (toNumber a /\ toNumber b)
    _ -> case A.elemIndex e.to.node left /\ A.elemIndex e.from.node right of
      Just a /\ Just b -> Just (toNumber a /\ toNumber b)
      _ -> Nothing

countAllCrossings :: Array (Array NodeId) -> Array Edge -> Int
countAllCrossings layers edges = sum (map (\(a /\ b) -> countCrossings a b edges) (A.zip layers (A.drop 1 layers)))

-- This is a separate two-sided processor, never an in-sweep barycenter
-- refinement. Ports are switched only on strict improvement, preserving ties.
greedy :: Array { before :: NodeId, after :: NodeId } -> NS.Metadata -> Map NodeId (Array Port) -> Array Edge -> Boolean -> Order -> Order
greedy constraints northSouth declared edges = loop
  where
  loop forward state =
    let
      indices = A.mapWithIndex (\i _ -> i) state.layout
      next = foldl (visit forward) state (if forward then indices else A.reverse indices)
    in
      if next == state then state else loop (not forward) next
  visit forward state index =
    let
      start = if forward then 0 else A.length state.layout - 1
      switched = if index == start then sweepNodes index state else repeatNodes index state
      layer = fromMaybe [] (A.index switched.layout index)
      side = if forward then North else South
    in
      foldl (switchPorts side) switched layer
  sweepNodes index state =
    let
      layer = fromMaybe [] (A.index state.layout index)
    in
      foldl (switchNode index) state (A.mapWithIndex (\i _ -> i) (A.drop 1 layer))
  repeatNodes index state =
    let
      next = sweepNodes index state
    in
      if next == state then state else repeatNodes index next
  switchNode index state position = fromMaybe state do
    layer <- A.index state.layout index
    a <- A.index layer position
    b <- A.index layer (position + 1)
    if A.any (\c -> c.before == a && c.after == b) constraints || NS.preventsSwitch northSouth a b then pure state
    else do
      changed <- swap position (position + 1) layer
      layout <- A.updateAt index changed state.layout
      let next = state { layout = layout }
      let local = NS.neighboringCrossings northSouth state.ports a b
      pure (if countStraight next edges + local.after < countStraight state edges + local.before then next else state)
  switchPorts side state node
    | P.fixed declared node = state
    | otherwise =
        let
          ps = P.groups node side state.ports
          next = foldl
            ( \cur index -> fromMaybe cur do
                changed <- swap index (index + 1) (P.groups node side cur.ports)
                let candidate = cur { ports = P.reorder node side changed cur.ports }
                pure (if countStraight candidate edges < countStraight cur edges then candidate else cur)
            )
            state
            (A.mapWithIndex (\i _ -> i) (A.drop 1 ps))
        in
          if next == state then state else switchPorts side next node

swap :: forall a. Int -> Int -> Array a -> Maybe (Array a)
swap i j values = do
  a <- A.index values i
  b <- A.index values j
  first <- A.updateAt i b values
  A.updateAt j a first
