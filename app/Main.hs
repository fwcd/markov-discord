import Control.Exception (finally)
import Control.Monad (when)
import qualified Data.Cache as C
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import DiscordUtils
import Markov
import Config
import GlobalMessageHandler

-- Runs the bot.
main :: IO ()
main = do
    token <- T.strip <$> TIO.readFile "./authtoken.secret"
    cache <- C.newCache Nothing :: IO (C.Cache String User)
    e <- runDiscord $ def {
        discordToken = token,
        discordOnEvent = handleEvent cache
    }
    TIO.putStrLn e

-- Handles an incoming event.
handleEvent :: C.Cache String User -> DiscordHandle -> Event -> IO ()
handleEvent cache dis ev = case ev of
    Ready _ me _ _ _ -> do
        putStrLn "Ready!"
        C.insert cache "me" me
    MessageCreate m -> do
        me <- C.lookup cache "me"
        let ctx = DiscordContext {
            contextHandle = dis,
            contextUser = me,
            contextMessage = m,
            contextChannel = messageChannel m
        } in when (shouldRespond m me) $ do
            rsp <- respond ctx
            case rsp of
                Just msg -> do
                    rr <- restCall dis msg
                    putStrLn $ show rr
                    putStrLn ""
                Nothing -> return ()
