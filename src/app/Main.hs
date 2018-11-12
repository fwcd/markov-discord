{-# LANGUAGE OverloadedStrings #-}

-- Source: https://github.com/aquarial/discord-haskell/blob/master/examples/ping-pong.hs

import Control.Exception (finally)
import Control.Monad (when)
import Data.Char (toLower)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord
import Markov

main :: IO ()
main = do
    tok <- T.strip <$> TIO.readFile "./authtoken.secret"
    dis <- loginRestGateway (Auth tok)
    finally (let loop = do
                    e <- nextEvent dis
                    case e of
                        Left err -> putStrLn ("Event error: " <> show err)
                        Right (MessageCreate m) -> do
                            when (hasValidPrefix (messageText m)) $ do
                                inputMsgs <- restCall dis (GetChannelMessages (messageChannel m) (100, AroundMessage (messageId m)))
                                let chainLength = 20 in case inputMsgs of
                                    Left err -> putStrLn ("Error while querying messages from channel: " <> show err)
                                    Right msgs -> do
                                        respMsg <- markovGenerate (T.unpack $ T.concat [T.concat $ fmap messageText msgs, messageText m]) chainLength
                                        resp <- restCall dis (CreateMessage (messageChannel m) (T.pack respMsg) Nothing)
                                        putStrLn (show resp)
                                        putStrLn ""
                            loop
                        _ -> do loop
            in loop)
            (stopDiscord dis)

hasValidPrefix :: T.Text -> Bool
hasValidPrefix = T.isPrefixOf "$" . T.map toLower
