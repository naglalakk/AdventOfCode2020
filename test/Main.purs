module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Test.Day1            (testDay1)

main :: Effect Unit
main = do
  testDay1
