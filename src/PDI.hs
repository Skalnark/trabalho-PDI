module PDI
    ( imageMap,
      yiqBrightness,
      addBrightnessYIQ,
      multBrightnessYIQ,
      addBrightnessRGB,
      multBrightnessRGB,
      backAndAgain,
      negativeFromRGB,
      negativeFromY,
      redComponent,
      greenComponent,
      blueComponent,
      thresholdY,
      thresholdRGB,
      saveAndLog)
  where

import Codec.Picture.Types(Pixel, PixelRGB16 (PixelRGB16))
import Codec.Picture
    ( convertRGB8,
      readImage,
      pixelMap,
      savePngImage,
      Image,
      DynamicImage(ImageRGB8),
      Pixel8,
      PixelRGB8(..))

data YIQ = YIQ Double Double Double

saveAndLog :: String -> String -> DynamicImage -> IO () 
saveAndLog prefix name img = do
  let path = "images/output/" ++ prefix ++ name
  savePngImage path img 
  putStrLn $ path ++ " done..."

imageMap :: Pixel a =>
  (a -> PixelRGB8) -> Image a -> DynamicImage
imageMap f img = ImageRGB8 $ pixelMap f img

redComponent :: PixelRGB8 -> PixelRGB8
redComponent (PixelRGB8 r g b) = PixelRGB8 r 0 0

greenComponent :: PixelRGB8 -> PixelRGB8
greenComponent (PixelRGB8 r g b) = PixelRGB8 0 g 0

blueComponent :: PixelRGB8 -> PixelRGB8
blueComponent (PixelRGB8 r g b) = PixelRGB8 0 0 b

rgbBrightness :: (Double -> Double -> Double) -> Double -> PixelRGB8 -> PixelRGB8
rgbBrightness op bright (PixelRGB8 r g b) = PixelRGB8 r' g' b'
  where
    r' = round $ applyBoundedOperation (fromIntegral r) bright op
    g' = round $ applyBoundedOperation (fromIntegral g) bright op
    b' = round $ applyBoundedOperation (fromIntegral b) bright op

addBrightnessRGB :: Double -> PixelRGB8 -> PixelRGB8
addBrightnessRGB = rgbBrightness (+)


multBrightnessRGB :: Double -> PixelRGB8 -> PixelRGB8
multBrightnessRGB = rgbBrightness (*)

applyBoundedOperation :: (Ord p, Num p) => t1 -> t2 -> (t1 -> t2 -> p) -> p
applyBoundedOperation l r op 
                | l `op` r >= 255 = 255
                | l `op` r <= 0 = 0
                | otherwise = l `op` r

neg :: Pixel8 -> Pixel8
neg x =
  let x' = fromIntegral x
  in  round $ 255 - x'

yiqBrightness :: (Double -> Double -> Double) -> Double -> PixelRGB8 -> PixelRGB8
yiqBrightness op bright img = yiq $ rgbToYiq img
  where
    yiq (YIQ y i q) = yiqToRgb $ YIQ (applyBoundedOperation y bright op) i q

addBrightnessYIQ :: Double -> PixelRGB8 -> PixelRGB8
addBrightnessYIQ = yiqBrightness (+)

multBrightnessYIQ :: Double -> PixelRGB8 -> PixelRGB8
multBrightnessYIQ = yiqBrightness (*)

backAndAgain :: PixelRGB8 -> PixelRGB8
backAndAgain = yiqToRgb . rgbToYiq

negativeFromRGB :: PixelRGB8 -> PixelRGB8
negativeFromRGB (PixelRGB8 r g b) = PixelRGB8 (neg r) (neg g) (neg b)

negativeFromY :: PixelRGB8 -> PixelRGB8
negativeFromY img = negative $ rgbToYiq img
  where
    negative (YIQ y i q) = yiqToRgb $ YIQ (-y) i q

yiqToRgb :: YIQ -> PixelRGB8
yiqToRgb (YIQ y i q) = PixelRGB8 r g b
  where
    r = round $ y + (i * 0.956) + (q * 0.621)
    g = round $ y - (i * 0.272) - (q * 0.647)
    b = round $ y - (i * 1.106) + (q * 1.703)

rgbToYiq :: PixelRGB8 -> YIQ
rgbToYiq (PixelRGB8 r g b) = YIQ y i q
  where
    y = fromIntegral r * 0.299 + fromIntegral g * 0.587 + fromIntegral b * 0.114
    i = fromIntegral r * 0.596 - fromIntegral g * 0.274 - fromIntegral b * 0.322
    q = fromIntegral r * 0.211 - fromIntegral g * 0.523 + fromIntegral b * 0.312

thresholdRGB :: Double -> PixelRGB8 -> PixelRGB8
thresholdRGB lim (PixelRGB8 r g b) = PixelRGB8 t t t
  where
    sum = fromIntegral r + fromIntegral g + fromIntegral b
    med = sum / 3
    t   = if med > lim then round lim else 0

thresholdY :: Double -> PixelRGB8 -> PixelRGB8
thresholdY lim img = yiqToRgb . yiq $ rgbToYiq img
  where
    yiq (YIQ y i q) = YIQ (threshold y) i q
    threshold y = if y >= lim then lim else y