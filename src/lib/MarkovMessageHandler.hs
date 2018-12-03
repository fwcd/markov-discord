module MarkovMessageHandler(
    newMarkovMessage
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

newMarkovMessage :: DiscordContext -> IO (Maybe String)
newMarkovMessage ctx = do
    inputMsgs <- restCall (contextClient ctx) (GetChannelMessages (messageChannel m) (100, AroundMessage (messageId m)))
    case inputMsgs of
        Left err -> do
            putStrLn ("Error while querying messages from channel: " <> show err)
            return Nothing
        Right msgs -> Just <$> markovGenerate (concatMessages user $ filterInputMessages user $ m : msgs) chainLength
    where m = contextMessage ctx
          user = contextUser ctx
