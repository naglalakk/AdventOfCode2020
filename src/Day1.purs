module Day1 where 

import Prelude
import Data.Array               (catMaybes, find, head, tail)
import Data.Int                 (fromString)
import Data.Maybe               (Maybe(..), fromMaybe)
import Data.String.Utils        (lines)
import Data.Tuple               as T
import Effect                   (Effect)
import Effect.Console           (log)

import Node.Encoding            (Encoding(..))
import Node.FS.Sync             (readTextFile)


-- | Given a list of numbers
--   Create a list of pairs with all combinations
combinations :: Array Int -> Array (T.Tuple Int Int)
combinations []  = []
combinations arr = 
  (map (\num -> T.Tuple x num) xs) <> combinations xs
  where
    x  = fromMaybe  0 $ head arr
    xs = fromMaybe [] $ tail arr

-- Given a number k, and a list of combinations
-- Check if a combination sums up to k
isSumOf :: Int 
        -> Array (T.Tuple Int Int) 
        -> Maybe (T.Tuple Int Int)
isSumOf k combs = find (\(T.Tuple a b) -> a + b == k) combs

main :: Effect Unit
main = do
  nums <- readTextFile UTF8 "./src/input/day1.txt"
  let 
    numArr = catMaybes $ map fromString $ lines nums
    combs  = combinations numArr
    hit    = isSumOf 2020 $ combinations numArr
  case hit of 
    Just (T.Tuple a b) -> log $ show $ a * b
    Nothing -> log "Christmas is cancelled :("
