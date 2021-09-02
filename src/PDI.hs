module PDI
    ( imageMap,
      changeBrightness,
      backAndAgain,
      negativeFromRGB,
      negativeFromY,
      redComponent,
      greenComponent,
      blueComponent)
  where

import Codec.Picture.Types(Pixel)
import Codec.Picture
    ( convertRGB8,
      readImage,
      pixelMap,
      Image,
      DynamicImage(ImageRGB8),
      Pixel8,
      PixelRGB8(..))

data YIQ = YIQ Double Double Double

imageMap :: Pixel a =>
  (a -> PixelRGB8) -> Image a -> DynamicImage
imageMap f img = ImageRGB8 $ pixelMap f img

redComponent :: PixelRGB8 -> PixelRGB8
redComponent (PixelRGB8 r g b) = PixelRGB8 r 0 0

greenComponent :: PixelRGB8 -> PixelRGB8
greenComponent (PixelRGB8 r g b) = PixelRGB8 0 g 0

blueComponent :: PixelRGB8 -> PixelRGB8
blueComponent (PixelRGB8 r g b) = PixelRGB8 0 0 b

changeBrightness :: Double -> PixelRGB8 -> PixelRGB8
changeBrightness bright img = yiq $ rgbToYiq img 
  where
    yiq (YIQ y i q) = yiqToRgb $ YIQ (y + bright) i q 

backAndAgain :: PixelRGB8 -> PixelRGB8
backAndAgain = yiqToRgb . rgbToYiq 

trunc :: Pixel8 -> Pixel8
trunc x
  | x <= 0 = 0
  | x >= 255 = 255
  | otherwise = x

negativeFromRGB :: PixelRGB8 -> PixelRGB8 
negativeFromRGB (PixelRGB8 r g b) = PixelRGB8 (-r) (-g) (-b)

negativeFromY :: PixelRGB8 -> PixelRGB8 
negativeFromY img = negative $ rgbToYiq img 
  where
    negative (YIQ y i q) = yiqToRgb $ YIQ (-y) i q

yiqToRgb :: YIQ -> PixelRGB8
yiqToRgb (YIQ y i q) = PixelRGB8 r g b
  where
    r = trunc $ round $ y + (i * 0.956) + (q * 0.621)
    g = trunc $ round $ y - (i * 0.272) - (q * 0.647)
    b = trunc $ round $ y - (i * 1.106) + (q * 1.703)

rgbToYiq :: PixelRGB8 -> YIQ
rgbToYiq (PixelRGB8 r g b) = YIQ y i q
  where
    y = fromIntegral r * 0.299 + fromIntegral g * 0.587 + fromIntegral b * 0.114
    i = fromIntegral r * 0.596 - fromIntegral g * 0.274 - fromIntegral b * 0.322
    q = fromIntegral r * 0.211 - fromIntegral g * 0.523 + fromIntegral b * 0.312
