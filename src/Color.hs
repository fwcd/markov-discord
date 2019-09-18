module Color(
    red,
    yellow,
    green,
    cyan,
    blue,
    magenta,
    black,
    white,
    transparent
) where

import Codec.Picture.Types

red :: PixelRGBA8
red = PixelRGBA8 255 0 0 255

yellow :: PixelRGBA8
yellow = PixelRGBA8 255 255 0 255

green :: PixelRGBA8
green = PixelRGBA8 0 255 0 255

cyan :: PixelRGBA8
cyan = PixelRGBA8 0 255 255 255

blue :: PixelRGBA8
blue = PixelRGBA8 0 0 255 255

magenta :: PixelRGBA8
magenta = PixelRGBA8 255 0 255 255

black :: PixelRGBA8
black = PixelRGBA8 0 0 0 255

white :: PixelRGBA8
white = PixelRGBA8 255 255 255 255

transparent :: PixelRGBA8
transparent = PixelRGBA8 0 0 0 0
