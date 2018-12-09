module Markov(
    showMarkovStrTable,
    markovStrTableGraph,
    markovGenerate
) where

import System.Random (randomRIO)
import Data.Maybe
import Graph
import qualified Data.Map.Strict as M

data Table a = Table {
    tableKeys :: [a],
    tableMappings :: M.Map a [a]
}

graphFromTable :: (Ord a) => Table a -> Graph a
graphFromTable = (M.foldrWithKey (\k xs -> connectAll $ map (\x -> (k, x)) xs) emptyGraph) . tableMappings

pickRandomFrom :: (Eq a) => [a] -> Maybe (IO a)
pickRandomFrom xs
    | null xs = Nothing
    | otherwise = Just $ (xs !!) <$> randomRIO (0, length xs - 1)

markovTable :: (Ord a) => [a] -> Table a
markovTable xs = Table {
    tableKeys = xs,
    tableMappings = M.fromListWith (++) $ zip xs $ map (\x -> [x]) $ drop 1 xs
}

markovStrTable :: String -> Table String
markovStrTable txt = markovTable $ words txt

markovStrTableGraph :: String -> Graph String
markovStrTableGraph = graphFromTable . markovStrTable

showMarkovStrTable :: String -> String
showMarkovStrTable txt = show $ tableMappings $ markovTable $ words txt

markovChain :: Table String -> Int -> String -> IO String
markovChain table i word = if i <= 0
    then defaultVal
    else fromMaybe defaultVal $ if null word
        then let optionalKey = pickRandomFrom $ tableKeys table in
            (\key -> key >>= markovChain table i) <$> optionalKey
        else let optionalNextWord = (M.lookup word $ tableMappings table) >>= pickRandomFrom in
            (\nextIOWord ->
                nextIOWord >>= (\nextWord ->
                    ((word ++ " ") ++) <$> markovChain table (i - 1) nextWord
                )
            ) <$> optionalNextWord
    where defaultVal = return ""

maybeLast :: [a] -> Maybe a
maybeLast [x] = Just x
maybeLast (_:xs) = maybeLast xs
maybeLast [] = Nothing

useInitialWordFromMessage :: Bool
useInitialWordFromMessage = False

pickInitialWord :: [String] -> Table String -> String
pickInitialWord txtWords table = if useInitialWordFromMessage
    then fromMaybe [] $ (maybeLast $ txtWords) >>= (\w -> (M.lookup w $ tableMappings table) >>= maybeLast)
    else []

markovGenerate :: String -> Int -> IO String
markovGenerate txt len = markovChain table len $ pickInitialWord txtWords table
    where txtWords = words txt
          table = markovTable txtWords
