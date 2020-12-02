module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Aff                   (launchAff_)
import Test.Spec.Runner             (runSpec)
import Test.Spec                    (describe)
import Test.Spec.Reporter.Console   (consoleReporter)

import Test.Day1            (testDay1)
import Test.Day2            (testDay2)

main :: Effect Unit
main = do
  launchAff_ $ runSpec [consoleReporter] do
    describe "Test Day 1" testDay1
    describe "Test Day 2" testDay2
