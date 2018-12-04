module ImageToFile(
    imageToFile
) where

import Codec.Picture.Types
import Codec.Picture
import System.IO.Temp

imageToFile :: (PngSavable px) => String -> Image px -> IO FilePath
imageToFile name img = withSystemTempFile name $ \path handle -> do
    writePng path img
    return path
