module ImageToFile(
    imageToFile
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Codec.Picture.Types
import Codec.Picture
import System.IO.Temp

imageToFile :: (PngSavable px) => Image px -> IO FilePath
imageToFile img = withSystemTempFile "markov.png" $ \path handle -> do
    -- TODO: The issue is that lazy IO causes the
    -- path to be processed before the image has been
    -- written
    B.hPut handle $ BL.toStrict $ encodePng img
    return path
