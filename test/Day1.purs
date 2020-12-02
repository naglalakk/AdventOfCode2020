module Test.Day1 where

import Prelude      
import Effect                       (Effect)
import Effect.Aff                   (launchAff_)
import Data.Maybe                   (Maybe(..))
import Data.Tuple                   as T
import Test.Spec                    (describe, it)
import Test.Spec.Reporter.Console   (consoleReporter)
import Test.Spec.Assertions         (shouldEqual)
import Test.Spec.Runner             (runSpec)

import Day1                         (combinations
                                    ,isSumOf)

testDay1 :: Effect Unit
testDay1 = launchAff_ $ runSpec [consoleReporter] do
  describe "day1" do
    it "isSumOf 5 [Tuple 2 3] shouldEqual Just (Tuple 2 3)" do
      let
        nums = [2,3]
        combs = combinations nums

      (isSumOf 5 combs) `shouldEqual` (Just (T.Tuple 2 3)) 
