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
import MarkovMessageHandler
import ImageToFile
import Codec.Picture
import Graphics.Rasterific
import Graphics.Rasterific.Texture

respond :: DiscordContext -> IO (Maybe (ChannelRequest Message))
respond ctx = do
    response <- (try $ respondDirectly ctx) :: IO (Either IOError (Maybe (ChannelRequest Message)))
    case response of
        (Left ex) -> return $ Just $ stringMessageOf ctx $ "Oops, an IO error occurred!"
        (Right r) -> return r

respondDirectly :: DiscordContext -> IO (Maybe (ChannelRequest Message))
respondDirectly ctx
    | "draw" `isSuffixOf` cmd = do
        white <- return $ PixelRGBA8 255 255 255 255
        blue <- return $ PixelRGBA8 0 0 255 255
        img <- return $ renderDrawing 400 200 white $
            withTexture (uniformTexture blue) $ do
                fill $ circle (V2 10 10) 30
        Just <$> fileMessageOf ctx <$> imageToFile img
    | "ping" `isSuffixOf` cmd = return $ Just $ stringMessageOf ctx "Pong!"
    | "table" `isSuffixOf` cmd = Just <$> stringMessageOf ctx <$> ("The markov chain table:\n" ++) <$> (++ "...") <$> (take 1800) <$> markovTableMessage ctx
    | otherwise = Just <$> textMessageOf ctx <$> prepareResponse <$> (responsePrefix m user) <$> newMarkovMessage ctx
    where cmd = (T.unpack . messageText . contextMessage) ctx
          m = contextMessage ctx
          user = contextUser ctx

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
