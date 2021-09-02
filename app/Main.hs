module Main
  where

import Data.Either ()
import Codec.Picture
import GHC.Real (Fractional)

main :: IO ()
main = do
  input <- readImage "images/gabi.jpg"
  case input of
    Left err -> putStrLn err
    Right inputImage -> do
      let img = convertRGB8 inputImage
      savePngImage "images/gabi_red.png" $ ImageRGB8(pixelMap (\(PixelRGB8 r g b) -> PixelRGB8 r 0 0) img)
      savePngImage "images/gabi_green.png" $ ImageRGB8(pixelMap (\(PixelRGB8 r g b) -> PixelRGB8 0 g 0) img)
      savePngImage "images/gabi_blue.png" $ ImageRGB8(pixelMap (\(PixelRGB8 r g b) -> PixelRGB8 0 0 b) img)
      savePngImage "images/gabi_colorful.png" $ ImageRGB8(pixelMap (\(PixelRGB8 r g b) -> PixelRGB8 b g r) img)
      savePngImage "images/gabi_negative.png" $ ImageRGB8(pixelMap (\(PixelRGB8 r g b) -> PixelRGB8 (negate r) (negate g) (negate b)) img)
      savePngImage "images/gabi_YIQ.png" $ ImageRGB8(pixelMap toYIQ img)

  print "done"

--convert :: Pixel8 -> Pixel8 -> Pixel8 -> Pixel8 -> Pixel8 -> Pixel8 -> (Pixel8 -> Pixel8 -> Pixel8) ->Pixel8
convert :: Pixel8 -> Pixel8 -> Pixel8 -> Double -> Double -> Double -> (Double -> Double -> Double) -> Pixel8
convert a b c x y z op = round $ (fromIntegral a * x) `op` (fromIntegral b * y) `op` (fromIntegral c * z)

toYIQ :: PixelRGB8 -> PixelRGB8
toYIQ (PixelRGB8 r g b) = PixelRGB8 (negate $ toY r g b) (toI r g b) (toQ r g b)

imageToY :: PixelRGB8 -> PixelRGB8
imageToY (PixelRGB8 r g b) = PixelRGB8 y y y
  where y = toQ r g b

toY :: Pixel8 -> Pixel8 -> Pixel8 -> Pixel8
toY r g b = convert r g b 0.299 0.587 0.114 (+)
toI :: Pixel8 -> Pixel8 -> Pixel8 -> Pixel8
toI r g b = convert r g b 0.596 0.274 0.322 (-)
toQ :: Pixel8 -> Pixel8 -> Pixel8 -> Pixel8
toQ r g b = convert r g b 0.211 0.523 0.312 (-)