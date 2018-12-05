{-# LANGUAGE BangPatterns #-}
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
imageToFile img = do
    tempDir <- getCanonicalTemporaryDirectory
    (path, handle) <- openTempFile tempDir "markov.png"
    !result <- B.hPut handle $ BL.toStrict $ encodePng img
    hClose handle
    putStrLn path
    threadDelay 1000000
    return path
