module Test.Day1 where

import Prelude      
import Effect                       (Effect)
import Effect.Aff                   (launchAff_)
import Data.Maybe                   (Maybe(..))
import Data.Tuple                   as T
import Test.Spec                    (Spec, describe, it)
import Test.Spec.Reporter.Console   (consoleReporter)
import Test.Spec.Assertions         (shouldEqual)
import Test.Spec.Runner             (runSpec)

import Day1                         (combinations
                                    ,isSumOf)

testDay1 :: Spec Unit
testDay1 = do
  describe "Testing isSumOf" do
    it "isSumOf 5 [Tuple 2 3] shouldEqual Just (Tuple 2 3)" do
      let
        nums = [2,3]
        combs = combinations nums

      (isSumOf 5 combs) `shouldEqual` (Just (T.Tuple 2 3)) 
