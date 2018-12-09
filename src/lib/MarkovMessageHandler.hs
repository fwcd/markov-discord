module MarkovMessageHandler(
    newMarkovMessage,
    markovTableMessage,
    markovGraphStrMessage,
    markovGraphImgMessage
) where

import qualified Data.Text as T
import Data.Monoid ((<>))
import Data.Maybe
import Discord
import DiscordUtils
import Graph
import GraphPlotter
import Markov
import Config
import Codec.Picture (Image)
import Codec.Picture.Types (PixelRGBA8)

concatMessages :: Maybe User -> [Message] -> String
concatMessages user msgs = foldl (++) "" $ ((++ " ") . T.unpack . messageText) <$> (filterInputMessages user msgs)

filterInputMessages :: Maybe User -> [Message] -> [Message]
filterInputMessages user = filter $ (\m -> fromMaybe True $ (/= (userId $ messageAuthor m)) <$> (userId <$> user))

composeMarkovInput :: DiscordContext -> IO String
composeMarkovInput ctx = (concatMessages user . filterInputMessages user . (m:)) <$> pullInputMessages ctx
    where m = contextMessage ctx
          user = contextUser ctx

newMarkovMessage :: DiscordContext -> IO String
newMarkovMessage ctx = do
    input <- composeMarkovInput ctx
    markovGenerate input chainLength
    where m = contextMessage ctx
          user = contextUser ctx

markovTableMessage :: DiscordContext -> IO String
markovTableMessage = (showMarkovStrTable <$>) . composeMarkovInput

markovGraphStrMessage :: DiscordContext -> IO (Graph String)
markovGraphStrMessage = (markovStrTableGraph <$>) . composeMarkovInput

markovGraphImgMessage :: DiscordContext -> IO (Image PixelRGBA8)
markovGraphImgMessage = (>>= plotStrGraph) . (markovStrTableGraph <$>) . composeMarkovInput

pullInputMessages :: DiscordContext -> IO [Message]
pullInputMessages ctx = do
    inputMsgs <- restCall (contextClient ctx) (GetChannelMessages (messageChannel m) (100, AroundMessage (messageId m)))
    case inputMsgs of
        Left err -> do
            putStrLn ("Error while querying messages from channel: " <> show err)
            return []
        Right msgs -> return msgs
    where m = contextMessage ctx
          user = contextUser ctx
