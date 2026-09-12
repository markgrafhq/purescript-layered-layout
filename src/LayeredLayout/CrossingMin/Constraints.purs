-- Copyright (c) 2008, 2020 Kiel University and others.
-- SPDX-License-Identifier: EPL-2.0
-- ForsterConstraintResolver: merge violated groups, preserving their precedence.
module LayeredLayout.CrossingMin.Constraints (resolve) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import LayeredLayout.Graph (NodeId)

type Group = { id :: NodeId, nodes :: Array NodeId, key :: Number, outgoing :: Array NodeId }

resolve :: Array { before :: NodeId, after :: NodeId } -> Array { node :: NodeId, key :: Number } -> Array { node :: NodeId, key :: Number }
resolve constraints values = A.concatMap (\group -> map (\node -> { node, key: group.key }) group.nodes) (mergeAll initial)
  where
  ids = map _.node values
  initial = map (\v -> { id: v.node, nodes: [ v.node ], key: v.key, outgoing: A.mapMaybe (\c -> if c.before == v.node && A.elem c.after ids then Just c.after else Nothing) constraints }) values

  mergeAll :: Array Group -> Array Group
  mergeAll groups = case violation groups of
    Nothing -> groups
    Just (before /\ after) ->
      let
        joined = { id: before.id, nodes: before.nodes <> after.nodes, key: (before.key + after.key) / 2.0, outgoing: A.filter (\id -> id /= before.id && id /= after.id) (A.nub (before.outgoing <> after.outgoing)) }
        retained = A.filter (\g -> g.id /= before.id && g.id /= after.id) groups
        replace g = g { outgoing = if A.elem before.id g.outgoing || A.elem after.id g.outgoing then A.filter (\id -> id /= before.id && id /= after.id) g.outgoing <> [ joined.id ] else g.outgoing }
        index = fromMaybe (A.length retained) (A.findIndex (\g -> g.key > joined.key) retained)
        next = fromMaybe retained (A.insertAt index joined (map replace retained))
      in
        mergeAll next
  violation groups = visit roots M.empty
    where
    byId = M.fromFoldable (map (\g -> g.id /\ g) groups)
    incoming id = A.length (A.concatMap (\g -> A.filter (_ == id) g.outgoing) groups)
    roots = A.filter (\g -> not (A.null g.outgoing) && incoming g.id == 0) groups
    position id = A.findIndex (\g -> g.id == id) groups
    visit queue received = case A.uncons queue of
      Nothing -> Nothing
      Just { head: group, tail: rest } ->
        let
          predecessors = fromMaybe [] (M.lookup group.id received)
          violated = A.find (\id -> position id > position group.id) predecessors
          notify acc id =
            let
              sources = [ group.id ] <> fromMaybe [] (M.lookup id acc.received)
              ready =
                if A.length sources == incoming id then
                  case M.lookup id byId of
                    Just g -> [ g ]
                    Nothing -> []
                else []
            in
              { received: M.insert id sources acc.received, queue: acc.queue <> ready }
          next = foldl notify { queue: rest, received } group.outgoing
        in
          case violated >>= flip M.lookup byId of
            Just predecessor -> Just (predecessor /\ group)
            Nothing -> visit next.queue next.received
