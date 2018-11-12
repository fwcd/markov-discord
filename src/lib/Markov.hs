module Markov
    ( markovGenerate
    ) where

import System.Random (randomRIO)
import qualified Data.Map.Strict as Map

markovTable :: (Ord a) => [a] -> Map.Map a [a]
markovTable xs = Map.fromListWith (++) $ zip xs $ map (\x -> [x]) $ drop 1 xs

markovGenerate :: String -> Map.Map String [String]
markovGenerate txt = markovTable $ words txt
