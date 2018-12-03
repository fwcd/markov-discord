module MarkovMessageHandler(
    newMarkovMessage,
    markovTableMessage
) where

import qualified Data.Text as T
import Data.Monoid ((<>))
import Data.Maybe
import Discord
import DiscordUtils
import Markov
import Config

concatMessages :: Maybe User -> [Message] -> String
concatMessages user msgs = foldl (++) "" $ ((++ " ") . T.unpack . messageText) <$> (filterInputMessages user msgs)

filterInputMessages :: Maybe User -> [Message] -> [Message]
filterInputMessages user = filter $ (\m -> fromMaybe True $ (/= (userId $ messageAuthor m)) <$> (userId <$> user))

newMarkovMessage :: DiscordContext -> IO String
newMarkovMessage ctx = do
    msgs <- pullInputMessages ctx
    markovGenerate (concatMessages user $ filterInputMessages user $ m : msgs) chainLength
    where m = contextMessage ctx
          user = contextUser ctx

markovTableMessage :: DiscordContext -> IO String
markovTableMessage ctx = do
    msgs <- pullInputMessages ctx
    return $ show $ tableMappings $ markovStrTable $ concatMessages user $ filterInputMessages user $ m : msgs
    where m = contextMessage ctx
          user = contextUser ctx

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
