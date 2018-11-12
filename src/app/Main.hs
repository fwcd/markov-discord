{-# LANGUAGE OverloadedStrings #-}

-- Source: https://github.com/aquarial/discord-haskell/blob/master/examples/ping-pong.hs

import Control.Exception (finally)
import Control.Monad (when)
import Data.Char (toLower)
import Data.Monoid ((<>))
import Data.List (isInfixOf)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord
import Markov

main :: IO ()
main = do
    tok <- T.strip <$> TIO.readFile "./authtoken.secret"
    dis <- loginRestGateway (Auth tok)
    userResponse <- restCall dis GetCurrentUser
    user <- case userResponse of
        Left err -> putStrLn("Could not fetch bot user information: " <> show err) >> return Nothing
        Right usr -> return $ Just usr
    finally (let loop = do
                    e <- nextEvent dis
                    case e of
                        Left err -> putStrLn ("Event error: " <> show err)
                        Right (MessageCreate m) -> do
                            when (isMentioned m user) $ do
                                inputMsgs <- restCall dis (GetChannelMessages (messageChannel m) (100, AroundMessage (messageId m)))
                                let chainLength = 40 in case inputMsgs of
                                    Left err -> putStrLn ("Error while querying messages from channel: " <> show err)
                                    Right msgs -> do
                                        respMsg <- cleanResponse <$> markovGenerate (T.unpack $ T.concat [T.concat $ fmap messageText (filterInputMessages user msgs), messageText m]) chainLength
                                        resp <- restCall dis (CreateMessage (messageChannel m) (T.pack respMsg) Nothing)
                                        putStrLn (show resp)
                                        putStrLn ""
                            loop
                        _ -> do loop
            in loop)
            (stopDiscord dis)

botPrefix :: T.Text
botPrefix = "&"

filterInputMessages :: Maybe User -> [Message] -> [Message]
filterInputMessages user = filter $ (\m -> fromMaybe True $ (/= (userId $ messageAuthor m)) <$> (userId <$> user))

cleanResponse :: String -> String
cleanResponse = (T.unpack) . (T.replace "&" "") . (T.pack)

isMentioned :: Message -> Maybe User -> Bool
isMentioned m user = (T.isPrefixOf botPrefix . T.map toLower) (messageText m) || (fromMaybe False $ (\u -> elem (userId u) (userId <$> messageMentions m)) <$> user)
