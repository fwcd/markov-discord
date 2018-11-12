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
                        Left er -> putStrLn ("Event error: " <> show er)
                        Right (MessageCreate m) -> do
                            when (hasValidPrefix (messageText m)) $ do
                                respMsg <- let chainLength = 20 in
                                    markovGenerate (T.unpack $ messageText m) chainLength
                                resp <- restCall dis (CreateMessage (messageChannel m) (T.pack respMsg) Nothing)
                                putStrLn (show resp)
                                putStrLn ""
                            loop
                        _ -> do loop
            in loop)
            (stopDiscord dis)

hasValidPrefix :: T.Text -> Bool
hasValidPrefix = T.isPrefixOf "$" . T.map toLower
