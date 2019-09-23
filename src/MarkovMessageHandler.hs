module MarkovMessageHandler(
    newMarkovMessage,
    markovTableMessage,
    markovGraphStrMessage,
    markovGraphImgMessage
) where

import qualified Data.Text as T
import Data.Maybe
import Discord
import Discord.Types
import DiscordUtils
import Graph
import GraphPlotter
import Markov
import Config
import Codec.Picture (Image)
import Codec.Picture.Types (PixelRGBA8)

concatMessages :: Maybe User -> [Message] -> String
concatMessages me msgs = foldl (++) "" $ ((++ " ") . T.unpack . messageText) <$> (filterInputMessages me msgs)

filterInputMessages :: Maybe User -> [Message] -> [Message]
filterInputMessages me = filter $ (\m -> fromMaybe True $ (/= (userId $ messageAuthor m)) <$> (userId <$> me))

composeMarkovInput :: DiscordContext -> IO String
composeMarkovInput ctx = (concatMessages me . filterInputMessages me . (m:)) <$> pullMessages dis m 100
    where m = contextMessage ctx
          me = contextUser ctx
          dis = contextHandle ctx

newMarkovMessage :: DiscordContext -> IO String
newMarkovMessage ctx = do
    input <- composeMarkovInput ctx
    markovGenerate input chainLength
    where m = contextMessage ctx

markovTableMessage :: DiscordContext -> IO String
markovTableMessage = (showMarkovStrTable <$>) . composeMarkovInput

markovGraphStrMessage :: DiscordContext -> IO (Graph String)
markovGraphStrMessage = (markovStrTableGraph <$>) . composeMarkovInput

markovGraphImgMessage :: DiscordContext -> IO (Image PixelRGBA8)
markovGraphImgMessage = (>>= plotStrGraph) . (markovStrTableGraph <$>) . composeMarkovInput
