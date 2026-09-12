-- Copyright (c) 2010, 2020 Kiel University and others.
-- SPDX-License-Identifier: EPL-2.0
-- HyperEdgeCycleDetector: shared by ordinary channels and self-loop routing.
module LayeredLayout.EdgeRouting.HyperEdgeCycleDetector
  ( Dep
  , DepKind(..)
  , isCritical
  , detect
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import LayeredLayout.JavaRandom (Random, nextInt)

data DepKind = Regular | Critical

derive instance Eq DepKind

isCritical :: DepKind -> Boolean
isCritical Critical = true
isCritical Regular = false

type Dep = { src :: Int, tgt :: Int, weight :: Int, kind :: DepKind }

type Queue = { front :: List Int, back :: List Int }

queue :: Array Int -> Queue
queue ids = { front: L.fromFoldable ids, back: Nil }

push :: Int -> Queue -> Queue
push id q = q { back = Cons id q.back }

pop :: Queue -> Maybe (Int /\ Queue)
pop { front: Nil, back: Nil } = Nothing
pop { front: Nil, back } = pop { front: L.reverse back, back: Nil }
pop { front: Cons id rest, back } = Just (id /\ { front: rest, back })

-- Negative initial marks order ELK's TreeSet in reverse model order.
-- FIFO discovery order, critical-edge precedence, and even nextInt(1)
-- consumption are observable in downstream routing and component layout.
detect :: Boolean -> Random -> Array Int -> Array Dep -> { marks :: Map Int Int, random :: Random }
detect criticalOnly random ids dependencies =
  { marks: map (\mark -> if mark < count then mark + count + 1 else mark) final.marks
  , random: final.random
  }
  where
  count = A.length ids
  indices = M.fromFoldable (A.mapWithIndex (\index id -> id /\ (-index - 1)) ids)
  relevant = if criticalOnly then A.filter (isCritical <<< _.kind) dependencies else dependencies
  incoming = adjacency _.tgt relevant
  outgoing = adjacency _.src relevant
  adjacency endpoint = map L.reverse <<< foldl
    (\acc dep -> M.insertWith (flip (<>)) (endpoint dep) (Cons dep Nil) acc)
    M.empty
  weights endpoint select = foldl
    (\acc dep -> if select dep then M.insertWith (+) (endpoint dep) dep.weight acc else acc)
    M.empty
    relevant
  ins = weights _.tgt (const true)
  outs = weights _.src (const true)
  criticalIns = weights _.tgt (isCritical <<< _.kind)
  criticalOuts = weights _.src (isCritical <<< _.kind)
  value id = fromMaybe 0 <<< M.lookup id
  initial =
    { remaining: M.fromFoldable (A.mapWithIndex (\index id -> (-index - 1) /\ id) ids)
    , marks: M.empty
    , sources: queue (A.filter (\id -> value id ins == 0 && value id outs > 0) ids)
    , sinks: queue (A.filter (\id -> value id outs == 0) ids)
    , ins
    , outs
    , criticalIns
    , criticalOuts
    , sinkMark: count - 1
    , sourceMark: count + 1
    , random
    }
  final = order initial
  order state | M.isEmpty state.remaining = state
  order state =
    let
      drained = drainSources (drainSinks state)
      choose acc id
        | acc.critical = acc
        | not criticalOnly && value id drained.criticalOuts > 0 && value id drained.criticalIns <= 0 =
            { critical: true, maximum: acc.maximum, candidates: Cons id Nil }
        | otherwise =
            let
              flow = value id drained.outs - value id drained.ins
            in
              if flow > acc.maximum then { critical: false, maximum: flow, candidates: Cons id Nil }
              else if flow == acc.maximum then acc { candidates = Cons id acc.candidates }
              else acc
      selected = foldl choose { critical: false, maximum: (-2147483647), candidates: Nil } drained.remaining
      candidates = A.fromFoldable (L.reverse selected.candidates)
    in
      case A.uncons candidates of
        Nothing -> drained
        Just { head } ->
          let
            index /\ nextRandom = nextInt (A.length candidates) drained.random
            id = fromMaybe head (A.index candidates index)
          in
            order (remove id drained.sourceMark (drained { sourceMark = drained.sourceMark + 1, random = nextRandom }))
  drainSinks state = case pop state.sinks of
    Nothing -> state
    Just (id /\ rest) -> drainSinks (remove id state.sinkMark (state { sinks = rest, sinkMark = state.sinkMark - 1 }))
  drainSources state = case pop state.sources of
    Nothing -> state
    Just (id /\ rest) -> drainSources (remove id state.sourceMark (state { sources = rest, sourceMark = state.sourceMark + 1 }))
  remove id mark state = foldl updateIncoming
    (foldl updateOutgoing removed (fromMaybe Nil (M.lookup id outgoing)))
    (fromMaybe Nil (M.lookup id incoming))
    where
    removed = state
      { remaining = M.delete (value id indices) state.remaining
      , marks = M.insert id mark state.marks
      }
    updateOutgoing acc dep
      | dep.weight > 0 && not (M.member dep.tgt acc.marks) =
          let
            weight = value dep.tgt acc.ins - dep.weight
          in
            acc
              { ins = M.insert dep.tgt weight acc.ins
              , criticalIns = if isCritical dep.kind then M.insert dep.tgt (value dep.tgt acc.criticalIns - dep.weight) acc.criticalIns else acc.criticalIns
              , sources = if weight <= 0 && value dep.tgt acc.outs > 0 then push dep.tgt acc.sources else acc.sources
              }
      | otherwise = acc
    updateIncoming acc dep
      | dep.weight > 0 && not (M.member dep.src acc.marks) =
          let
            weight = value dep.src acc.outs - dep.weight
          in
            acc
              { outs = M.insert dep.src weight acc.outs
              , criticalOuts = if isCritical dep.kind then M.insert dep.src (value dep.src acc.criticalOuts - dep.weight) acc.criticalOuts else acc.criticalOuts
              , sinks = if weight <= 0 && value dep.src acc.ins > 0 then push dep.src acc.sinks else acc.sinks
              }
      | otherwise = acc
