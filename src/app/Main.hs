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
                            when (shouldRespond m user) $ do
                                inputMsgs <- restCall dis (GetChannelMessages (messageChannel m) (100, AroundMessage (messageId m)))
                                let chainLength = 40 in case inputMsgs of
                                    Left err -> putStrLn ("Error while querying messages from channel: " <> show err)
                                    Right msgs -> do
                                        respMsg <- prepareResponse <$> (responsePrefix m user) <$> markovGenerate (concatMessages user $ filterInputMessages user $ m : msgs) chainLength
                                        resp <- restCall dis (CreateMessage (messageChannel m) respMsg Nothing)
                                        putStrLn (show resp)
                                        putStrLn ""
                            loop
                        _ -> do loop
            in loop)
            (stopDiscord dis)

botPrefix :: T.Text
botPrefix = "&"

mentionAuthor :: Bool
mentionAuthor = True

concatMessages :: Maybe User -> [Message] -> String
concatMessages user msgs = foldl (++) "" $ ((++ " ") . T.unpack . messageText) <$> (filterInputMessages user msgs)

filterInputMessages :: Maybe User -> [Message] -> [Message]
filterInputMessages user = filter $ (\m -> fromMaybe True $ (/= (userId $ messageAuthor m)) <$> (userId <$> user))

responsePrefix :: Message -> Maybe User -> String -> String
responsePrefix m user = let authorId = userId $ messageAuthor m in
    if mentionAuthor && (fromMaybe True $ (/= authorId) <$> userId <$> user)
        then (++ ("<@" ++ (show $ authorId) ++ ">"))
        else id

prepareResponse :: String -> T.Text
prepareResponse = (T.replace "&" "") . (T.pack)

respondToItself :: Bool
respondToItself = False

shouldRespond :: Message -> Maybe User -> Bool
shouldRespond m user = (respondToItself || (fromMaybe True $ ((/= (userId $ messageAuthor m)) . userId) <$> user)) &&
    ((T.isPrefixOf botPrefix . T.map toLower) (messageText m)
    || (fromMaybe False $ (\u -> elem (userId u) (userId <$> messageMentions m)) <$> user))
