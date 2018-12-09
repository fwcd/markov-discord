module GraphPlotter(
    plotStrGraph
) where

import Graph
import Color
import Codec.Picture (Image, PngSavable)
import Codec.Picture.Types (PixelRGBA8)
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Text.TrueType

indexToPos :: Int -> Int -> V2 Int -> V2 Float
indexToPos r i m = (V2 (rad * cos ind) (rad * sin ind)) + (fromIntegral <$> m)
    where rad = fromIntegral r
          ind = fromIntegral i

arrangeNodes :: Int -> V2 Int -> [String] -> [(Point, String)]
arrangeNodes r m = ((\(i, s) -> (indexToPos r i m, s)) <$>) . zip [0, 1..]

plotStrGraph :: Graph String -> IO (Image PixelRGBA8)
plotStrGraph g = do
    rawFont <- loadFontFile "resources/Roboto-Regular.ttf"
    font <- case rawFont of
        (Left s) -> error $ "Could not load font " ++ s
        (Right f) -> return f
    return $ renderDrawing 400 400 white $ withTexture (uniformTexture black) $
        foldr (\(p, s) -> (>> printTextAt font size p s)) (return ()) $ arrangeNodes r m $ graphNodes g
        where r = 150
              m = V2 150 150
              size = PointSize 14
