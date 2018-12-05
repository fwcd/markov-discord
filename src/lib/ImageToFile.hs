module ImageToFile(
    imageToFile
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Codec.Picture.Types
import Codec.Picture
import System.IO.Temp
import System.IO
import Control.Concurrent
import Control.Monad

imageToFile :: (PngSavable px) => Image px -> IO FilePath
imageToFile img = withSystemTempFile "markov.png" $ \path handle -> do
    -- TODO: The issue is that lazy IO causes the
    -- path to be processed before the image has been
    -- written
    B.hPut handle $ BL.toStrict $ encodePng img
    hClose handle
    waitUntilIO $ hReady handle
    return path

waitUntilIO :: IO Bool -> IO ()
waitUntilIO cond = do
    threadDelay 10000000 -- 10 ms
    condResult <- cond
    unless condResult $ waitUntilIO cond
