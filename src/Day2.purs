module Day2 where

import Debug.Trace
import Prelude
import Data.Array               ((!!), catMaybes, head, last, length, filter, findIndex, mapWithIndex)
import Data.Foldable            (elem)
import Data.Int                 (fromString)
import Data.Maybe               (Maybe(..), fromMaybe)
import Data.Newtype             (class Newtype)
import Data.Generic.Rep         (class Generic)
import Data.Generic.Rep.Show    (genericShow)
import Data.String              (indexOf)
import Data.String.Common       (split, trim, replace)
import Data.String.Pattern      (Pattern(..), Replacement(..))
import Data.String.Utils        (lines, toCharArray)
import Data.Tuple               as T
import Effect                   (Effect)
import Effect.Console           (log)

import Node.Encoding            (Encoding(..))
import Node.FS.Sync             (readTextFile)


newtype Password = Password String

derive instance newtypePassword :: Newtype Password _
derive instance genericPassword :: Generic Password _
derive newtype instance eqPassword :: Eq Password
instance showPassword :: Show Password where
  show = genericShow

-- | Data type for a validation rule
--   ValidationRule min max char
data ValidationRule 
  = ValidationRule Int Int String

derive instance eqValidationRule :: Eq ValidationRule
derive instance genericValidationRule :: Generic ValidationRule _ 
instance showValidationRule :: Show ValidationRule where
  show = genericShow

data Line
  = Line ValidationRule Password

derive instance eqLine :: Eq Line
derive instance genericLine :: Generic Line _
instance showLine :: Show Line where
  show = genericShow

parseLine :: String -> Line
parseLine s = 
  Line
    (ValidationRule (T.fst range) (T.snd range) char)
    (Password pass)
  where
    chunks = map trim $ split (Pattern " ") s
    char   = 
      replace 
        (Pattern ":") 
        (Replacement "") 
        (fromMaybe "" (chunks !! 1))
    pass   = fromMaybe "" (chunks !! 2)
    range = case head chunks of
      Just nums -> do
        let
          nc  = split (Pattern "-") nums
          min = 
            fromMaybe 0 (fromString $ fromMaybe "" $ head nc)
          max = 
            fromMaybe 0 (fromString $ fromMaybe "" $ last nc)
        T.Tuple min max
      Nothing -> T.Tuple 0 0

validatePassword :: Line
                 -> Boolean
validatePassword (Line (ValidationRule min max char) (Password pass)) = charInstances >= min && charInstances <= max 
  where
    charInstances = 
      length $ 
        filter (\p -> p == char) $ toCharArray pass

validatePasswordTobbogan :: Line -> Boolean
validatePasswordTobbogan (Line (ValidationRule min max char) (Password pass)) =  
  ( not (elemMin && elemMax)) && (elemMin || elemMax)
  where
    elemMin = elem min indexes
    elemMax = elem max indexes
    indexes = 
      catMaybes $
        mapWithIndex 
          (\ix x -> case (x == char) of
            true -> Just $ ix + 1
            false -> Nothing ) 
          (toCharArray pass)

main :: Effect Unit
main = do
  passwords <- readTextFile UTF8 "./src/input/day2.txt"
  let 
    passArr = filter (\x -> x /= "") $ lines passwords
    passLines = map parseLine passArr
    validPasswords = 
      length $
        filter (\x -> x == true) $
          map validatePassword passLines
    validPasswordsTobbogan = 
      length $
        filter (\x -> x == true) $
          map validatePasswordTobbogan passLines
  log "Part One: "
  log $ "Number of valid passwords: " <> (show validPasswords)

  log "Part Two: "
  log $ "Number of valid passwords: " <> (show validPasswordsTobbogan)

