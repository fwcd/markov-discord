-- Source: https://github.com/aquarial/discord-haskell/blob/master/examples/ping-pong.hs

import Control.Exception (finally)
import Control.Monad (when)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord
import DiscordUtils
import Markov
import Config
import GlobalMessageHandler

main :: IO ()
main = do
    tok <- T.strip <$> TIO.readFile "./authtoken.secret"
    client <- loginRestGateway (Auth tok)
    userResponse <- restCall client GetCurrentUser
    user <- case userResponse of
        Left err -> putStrLn("Could not fetch bot user information: " <> show err) >> return Nothing
        Right usr -> return $ Just usr
    putStrLn "Ready!"
    finally (let loop = do
                    e <- nextEvent client
                    case e of
                        Left err -> putStrLn ("Event error: " <> show err)
                        Right (MessageCreate m) -> do
                            when (shouldRespond m user) $ do
                                respMsg <- respond context
                                case respMsg of
                                    Just rMsg -> do
                                        resp <- restCall client rMsg
                                        putStrLn (show resp)
                                        putStrLn ""
                                    Nothing -> return ()
                                where context = DiscordContext {
                                    contextClient = client,
                                    contextUser = user,
                                    contextMessage = m,
                                    contextChannel = (messageChannel m)
                                }
                        _ -> return ()
                    loop
            in loop)
            (stopDiscord client)
