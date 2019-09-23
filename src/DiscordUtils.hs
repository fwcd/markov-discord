module DiscordUtils(
    messageSentBy,
    messageDoesMention,
    pullMessages,
    pullOwnMessagesFromLast,
    DiscordContext(..)
) where

import ContainerUtils
import Data.Maybe
import qualified Data.Text as T
import Discord
import Discord.Types
import qualified Discord.Requests as R

data DiscordContext = DiscordContext {
    contextHandle :: DiscordHandle,
    contextUser :: Maybe User,
    contextMessage :: Message,
    contextChannel :: Snowflake
}

messageSentBy :: Message -> User -> Bool
messageSentBy msg user = msgUserId == cmpUserId
    where msgUserId = userId $ messageAuthor msg
          cmpUserId = userId user

messageDoesMention :: Message -> User -> Bool
messageDoesMention msg user = elem msgUserId mentionsIds
    where mentionsIds = userId <$> messageMentions msg
          msgUserId = userId user

pullMessages :: DiscordHandle -> Message -> Int -> IO [Message]
pullMessages dis m n = do
    inputMsgs <- restCall dis (R.GetChannelMessages ch (n, R.AroundMessage (messageId m)))
    case inputMsgs of
        Left e -> do
            putStrLn $ "Error while querying messages from channel: " <> show e
            return []
        Right msgs -> return msgs
    where ch = messageChannel m

pullOwnMessagesFromLast :: DiscordHandle -> Message -> Maybe User -> Int -> IO [Message]
pullOwnMessagesFromLast dis m me = (filter (\m -> fromMaybe False $ messageSentBy m <$> me) <$>) . pullMessages dis m
