module Test.Day2 where

import Prelude      
import Effect                       (Effect)
import Effect.Aff                   (launchAff_)
import Data.Maybe                   (Maybe(..))
import Data.Tuple                   as T
import Test.Spec                    (Spec, describe, it)
import Test.Spec.Reporter.Console   (consoleReporter)
import Test.Spec.Assertions         (shouldEqual)
import Test.Spec.Runner             (runSpec)

import Day2                         as Day2

testDay2 :: Spec Unit
testDay2 = do
  describe "Testing validatePassword" do
    it "1-3 a: abcde should be valid" do
      let
        rule = Day2.ValidationRule 1 3 "a"
        password = Day2.Password "abcde"
        line = Day2.Line rule password
        ruleValid = Day2.validatePassword line
      ruleValid `shouldEqual` true
    it "1-3 b: cdefg should be invalid" do
      let
        rule = Day2.ValidationRule 1 3 "b"
        password = Day2.Password "cdefg"
        line = Day2.Line rule password
        ruleValid = Day2.validatePassword line
      ruleValid `shouldEqual` false
    it "2-9 c: ccccccccc should be valid" do
      let
        rule = Day2.ValidationRule 2 9 "c"
        password = Day2.Password "ccccccccc"
        line = Day2.Line rule password
        ruleValid = Day2.validatePassword line
      ruleValid `shouldEqual` true
  describe "Testing parseLine" do
    it "1-3 a: abcde should return Line (ValidationRule 1 3 'a') 'abcde'" do
      let
        line = "1-3 a: abcde"
        parsed = Day2.parseLine line
        equal = 
          Day2.Line 
            (Day2.ValidationRule 1 3 "a") 
            (Day2.Password "abcde")
      parsed `shouldEqual` equal

