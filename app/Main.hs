module Main
  where

import Data.Either ()
import Codec.Picture
    ( convertRGB8,
      readImage,
      savePngImage,
      pixelMap,
      DynamicImage(ImageRGB8),
      Pixel8,
      PixelRGB8(..), PixelRGB16 )
import GHC.Real (Fractional)
import Data.Bifoldable (bifoldl')

data YIQ = YIQ Double Double Double
data RGB = RGB Double Double Double

main :: IO ()
main = do
  putStrLn "Insert the image name (including extension): "
  --imgName <- getLine  
  let imgName = "lenna.png"
  input <- readImage $ "images/" ++ imgName
  case input of
    Left err -> putStrLn err
    Right inputImage -> do
      let img = convertRGB8 inputImage
      savePngImage ("images/red_" ++ imgName) $ ImageRGB8(pixelMap (\(PixelRGB8 r g b) -> PixelRGB8 r 0 0) img)
      savePngImage ("images/green_" ++ imgName) $ ImageRGB8(pixelMap (\(PixelRGB8 r g b) -> PixelRGB8 0 g 0) img)
      savePngImage ("images/blue_" ++ imgName) $ ImageRGB8(pixelMap (\(PixelRGB8 r g b) -> PixelRGB8 0 0 b) img)
      savePngImage ("images/negative_RGB_" ++ imgName) $ ImageRGB8(pixelMap (\(PixelRGB8 r g b) -> PixelRGB8 (negate r) (negate g) (negate b)) img)
      savePngImage ("images/negative_Y_" ++ imgName) $ ImageRGB8(pixelMap negativeFromYIQ img)
      savePngImage ("images/to_YIQ_and_back_to_rgb_" ++ imgName) $ ImageRGB8(pixelMap backAndAgain img)
      savePngImage ("images/change_brightness_" ++ imgName) $ ImageRGB8(pixelMap (changeBrightness 10) img)

  print "done"

changeBrightness :: Double -> PixelRGB8 -> PixelRGB8
changeBrightness bright img = yiq $ rgbToYiq img 
  where
    yiq (YIQ y i q) = yiqToRgb $ YIQ (y + bright) i q 


yiqToRgb :: YIQ -> PixelRGB8
yiqToRgb (YIQ y i q) = PixelRGB8 r g b
  where
    r = trunc $ round $ y + (i * 0.956) + (q * 0.621)
    g = trunc $ round $ y - (i * 0.272) - (q * 0.647)
    b = trunc $ round $ y - (i * 1.106) + (q * 1.703)

backAndAgain :: PixelRGB8 -> PixelRGB8
backAndAgain img = imgRGB
  where
    imgYIQ = rgbToYiq img
    imgRGB = yiqToRgb imgYIQ

trunc :: Pixel8 -> Pixel8
trunc x
  | x <= 0 = 0
  | x >= 255 = 255
  | otherwise = x

negativeFromYIQ :: PixelRGB8 -> PixelRGB8 
negativeFromYIQ (PixelRGB8 r g b) = rgb
  where
    y   = toY r g b
    i   = toI r g b
    q   = toQ r g b
    rgb = yiqToRgb $ YIQ (-y) i q

rgbToYiq :: PixelRGB8 -> YIQ
rgbToYiq (PixelRGB8 r g b) = YIQ y i q
  where
    y = toY r g b
    i = toI r g b
    q = toQ r g b

toY :: (Fractional a1, Integral a2, Integral a3, Integral a4) =>
        a2 -> a3 -> a4 -> a1
toY r g b = fromIntegral r * 0.299 + fromIntegral g * 0.587 + fromIntegral b * 0.114


toI :: (Fractional a1, Integral a2, Integral a3, Integral a4) =>
        a2 -> a3 -> a4 -> a1
toI r g b = fromIntegral r * 0.596 - fromIntegral g * 0.274 - fromIntegral b * 0.322


toQ :: (Fractional a1, Integral a2, Integral a3, Integral a4) =>
        a2 -> a3 -> a4 -> a1
toQ r g b = fromIntegral r * 0.211 - fromIntegral g * 0.523 + fromIntegral b * 0.312