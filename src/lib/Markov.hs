module Markov(
    markovStrTable,
    markovGenerate,
    Table (..)
) where

import System.Random (randomRIO)
import Data.Maybe
import qualified Data.Map.Strict as Map

data Table a = Table {
    tableKeys :: [a],
    tableMappings :: Map.Map a [a]
}

pickRandomFrom :: (Eq a) => [a] -> Maybe (IO a)
pickRandomFrom xs
    | null xs = Nothing
    | otherwise = Just $ fmap (xs !!) $ randomRIO (0, length xs - 1)

markovTable :: (Ord a) => [a] -> Table a
markovTable xs = Table {
    tableKeys = xs,
    tableMappings = Map.fromListWith (++) $ zip xs $ map (\x -> [x]) $ drop 1 xs
}

markovStrTable :: String -> Table String
markovStrTable txt = markovTable $ words txt

markovChain :: Table String -> Int -> String -> IO String
markovChain table i word = if i <= 0
    then defaultVal
    else fromMaybe defaultVal $ if null word
        then let optionalKey = pickRandomFrom $ tableKeys table in
            fmap (\key -> key >>= markovChain table i) optionalKey
        else let optionalNextWord = (Map.lookup word $ tableMappings table) >>= pickRandomFrom in
            fmap (\nextIOWord ->
                nextIOWord >>= (\nextWord ->
                    fmap ((word ++ " ") ++) $ markovChain table (i - 1) nextWord
                )
            ) optionalNextWord
    where defaultVal = return ""

maybeLast :: [a] -> Maybe a
maybeLast [x] = Just x
maybeLast (_:xs) = maybeLast xs
maybeLast [] = Nothing

useInitialWordFromMessage :: Bool
useInitialWordFromMessage = False

pickInitialWord :: [String] -> Table String -> String
pickInitialWord txtWords table = if useInitialWordFromMessage
    then fromMaybe [] $ (maybeLast $ txtWords) >>= (\w -> (Map.lookup w $ tableMappings table) >>= maybeLast)
    else []

markovGenerate :: String -> Int -> IO String
markovGenerate txt len = markovChain table len $ pickInitialWord txtWords table
    where txtWords = words txt
          table = markovTable txtWords
