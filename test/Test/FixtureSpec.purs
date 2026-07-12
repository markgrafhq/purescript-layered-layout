module Test.FixtureSpec (fixtureSpec) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import LayeredLayout (defaultConfig, layout)
import Test.Fixtures (allCases)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

fixtureSpec :: Spec Unit
fixtureSpec = describe "LayeredLayout fixture regressions" do
  it "owns all 22 shared graph fixtures" do
    Array.length allCases `shouldEqual` 22

  for_ allCases \fixture ->
    it fixture.name do
      let
        first = layout defaultConfig fixture.graph
        second = layout defaultConfig fixture.graph
      first `shouldEqual` second
      Array.length first.nodes `shouldEqual` Array.length fixture.graph.nodes
      Array.length first.edges `shouldEqual` Array.length fixture.graph.edges
      first.metrics.nodeOverlapCount `shouldEqual` 0
