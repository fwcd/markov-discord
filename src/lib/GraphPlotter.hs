module GraphPlotter(
    plotStrGraph
) where

import ContainerUtils
import Graph
import Color
import Codec.Picture (Image, PngSavable)
import Codec.Picture.Types (PixelRGBA8)
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Text.TrueType

indexToPos :: Int -> V2 Int -> Int -> V2 Float
indexToPos r m i = (V2 (rad * cos ind) (rad * sin ind)) + (fromIntegral <$> m)
    where rad = fromIntegral r
          ind = fromIntegral i

arrangeNodes :: Int -> V2 Int -> [String] -> [(Point, String)]
arrangeNodes r m = ((\(i, s) -> (indexToPos r m i, s)) <$>) . zip [0, 1..]

plotStrGraph :: Graph String -> IO (Image PixelRGBA8)
plotStrGraph g = do
    rawFont <- loadFontFile "resources/Roboto-Regular.ttf"
    font <- case rawFont of
        (Left s) -> error $ "Could not load font " ++ s
        (Right f) -> return f
    return $ renderDrawing 400 400 white $ withTexture (uniformTexture black) $ do
        foldr (\(p, s) -> (>> printTextAt font size p s)) (return ()) $ arrangeNodes r m $ graphNodes g
        foldr (\l -> (>> stroke 1 JoinRound (CapRound, CapRound) l)) (return ()) $ (uncurry line) <$> mapTuple (indexToPos r m) <$> graphEdges g
        where r = 150
              m = V2 180 180
              size = PointSize 14
