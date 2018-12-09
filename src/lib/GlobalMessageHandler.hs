module GlobalMessageHandler(
    shouldRespond,
    responsePrefix,
    prepareResponse,
    respond
) where

import qualified Data.Text as T
import Control.Exception
import Data.Char (toLower)
import Data.List (isSuffixOf)
import Config
import DiscordUtils
import Data.Maybe
import Discord
import StringUtils
import Color
import MarkovMessageHandler
import Codec.Picture (Image, PngSavable, encodePng)
import Codec.Picture.Types
import Graphics.Rasterific
import Graphics.Rasterific.Texture

respond :: DiscordContext -> IO (Maybe (ChannelRequest Message))
respond ctx = do
    response <- (try $ respondDirectly ctx) :: IO (Either IOError (Maybe (ChannelRequest Message)))
    case response of
        (Left ex) -> do
            putStrLn $ show ex
            stringResponse ctx "Oops, an IO error occurred!"
        (Right r) -> return r

respondDirectly :: DiscordContext -> IO (Maybe (ChannelRequest Message))
respondDirectly ctx
    | "draw" `isSuffixOf` cmd = do
        img <- return $ renderDrawing 400 200 white $
            withTexture (uniformTexture blue) $ do
                fill $ circle (V2 10 10) 30
        imageResponse ctx img
    | "clear" `isSuffixOf` cmd = do
        msgs <- pullOwnMessagesFromLast ctx 50
        clearResponse <- restCall (contextClient ctx) $ BulkDeleteMessage (contextChannel ctx, messageId <$> msgs)
        case clearResponse of
            (Left ex) -> do
                putStrLn $ show ex
                stringResponse ctx "Could not clear messages."
            (Right _) -> return $ Nothing
    | "graph" `isSuffixOf` cmd = (markovGraphImgMessage ctx) >>= imageResponse ctx
    | "graphstr" `isSuffixOf` cmd = (take 1800 <$> show <$> markovGraphStrMessage ctx) >>= stringResponse ctx
    | "ping" `isSuffixOf` cmd = stringResponse ctx "Pong"
    | "table" `isSuffixOf` cmd = (("The markov chain table:\n" ++) <$> (++ "...") <$> take 1800 <$> markovTableMessage ctx) >>= stringResponse ctx
    | otherwise = (prepareResponse <$> (responsePrefix m user) <$> newMarkovMessage ctx) >>= textResponse ctx
    where cmd = (T.unpack . messageText . contextMessage) ctx
          m = contextMessage ctx
          user = contextUser ctx

stringResponse :: DiscordContext -> String -> IO (Maybe (ChannelRequest Message))
stringResponse ctx = return . Just . stringMessageOf ctx

textResponse :: DiscordContext -> T.Text -> IO (Maybe (ChannelRequest Message))
textResponse ctx = return . Just . textMessageOf ctx

imageResponse :: (PngSavable a) => DiscordContext -> Image a -> IO (Maybe (ChannelRequest Message))
imageResponse ctx = return . Just . fileMessageOf ctx . encodePng

shouldRespond :: Message -> Maybe User -> Bool
shouldRespond m user = (respondToItself || (fromMaybe True $ not <$> messageSentBy m <$> user))
    && ((fromMaybe False $ messageDoesMention m <$> user) || (T.isPrefixOf (T.pack botPrefix) . T.map toLower) (messageText m))

responsePrefix :: Message -> Maybe User -> String -> String
responsePrefix m user = let authorId = userId $ messageAuthor m in
    if mentionAuthor && (fromMaybe True $ (/= authorId) <$> userId <$> user)
        then (++ ("<@" ++ (show $ authorId) ++ ">"))
        else id

prepareResponse :: String -> T.Text
prepareResponse = (textReplace "&" "") . (T.pack)
