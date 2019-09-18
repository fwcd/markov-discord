module DiscordUtils(
    stringMessageOf,
    textMessageOf,
    fileMessageOf,
    messageSentBy,
    messageDoesMention,
    pullMessages,
    pullOwnMessagesFromLast,
    DiscordContext (..)
) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import ContainerUtils
import Data.Maybe
import Discord

data DiscordContext = DiscordContext {
    contextClient :: (RestChan, Gateway, [ThreadIdType]),
    contextUser :: Maybe User,
    contextMessage :: Message,
    contextChannel :: Snowflake
}

stringMessageOf :: DiscordContext -> String -> ChannelRequest Message
stringMessageOf ctx = textMessageOf ctx . T.pack

textMessageOf :: DiscordContext -> T.Text -> ChannelRequest Message
textMessageOf ctx txt = CreateMessage ch txt Nothing
    where ch = (contextChannel ctx)

fileMessageOf :: DiscordContext -> BL.ByteString -> ChannelRequest Message
fileMessageOf ctx = UploadFile ch "markov.png"
    where ch = (contextChannel ctx)

messageSentBy :: Message -> User -> Bool
messageSentBy msg user = msgUserId == cmpUserId
    where msgUserId = userId $ messageAuthor msg
          cmpUserId = userId user

messageDoesMention :: Message -> User -> Bool
messageDoesMention msg user = elem msgUserId mentionsIds
    where mentionsIds = userId <$> messageMentions msg
          msgUserId = userId user

pullMessages :: DiscordContext -> Int -> IO [Message]
pullMessages ctx n = do
    inputMsgs <- restCall (contextClient ctx) (GetChannelMessages (messageChannel m) (n, AroundMessage (messageId m)))
    case inputMsgs of
        Left err -> do
            putStrLn ("Error while querying messages from channel: " ++ show err)
            return []
        Right msgs -> return msgs
    where m = contextMessage ctx
          user = contextUser ctx

pullOwnMessagesFromLast :: DiscordContext -> Int -> IO [Message]
pullOwnMessagesFromLast ctx = (filter (\m -> fromMaybe False $ messageSentBy m <$> user) <$>) . pullMessages ctx
    where user = contextUser ctx
