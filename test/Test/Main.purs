module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.EngineSpec (engineSpec)
import Test.FixtureSpec (fixtureSpec)
import Test.ComponentLayoutSpec (componentLayoutSpec)
import Test.NodeLayeringSpec (nodeLayeringSpec)
import Test.NodeOrderingSpec (nodeOrderingSpec)
import Test.NodePlacementSpec (nodePlacementSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  engineSpec
  fixtureSpec
  componentLayoutSpec
  nodeLayeringSpec
  nodeOrderingSpec
  nodePlacementSpec
