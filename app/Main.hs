module Main
  where

import Data.Either ()
import PDI
import Codec.Picture
          ( savePngImage,
            convertRGB8,
            readImage)

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
      savePngImage ("images/red_" ++ imgName) $ imageMap redComponent img
      savePngImage ("images/green_" ++ imgName) $ imageMap greenComponent img
      savePngImage ("images/blue_" ++ imgName) $ imageMap blueComponent img
      savePngImage ("images/negative_RGB_" ++ imgName) $ imageMap negativeFromRGB img
      savePngImage ("images/negative_Y_" ++ imgName) $ imageMap negativeFromY img
      savePngImage ("images/to_YIQ_and_back_to_rgb_" ++ imgName) $ imageMap backAndAgain img
      savePngImage ("images/additive_brightness_YIQ_" ++ imgName) $ imageMap (addBrightnessYIQ 12) img
      savePngImage ("images/multiplicative_brightness_YIQ_" ++ imgName) $ imageMap (multBrightnessYIQ 1.2) img
      savePngImage ("images/additive_brightness_RGB_" ++ imgName) $ imageMap (addBrightnessRGB 12) img
      savePngImage ("images/multiplicative_brightness_RGB_" ++ imgName) $ imageMap (multBrightnessRGB 1.2) img
      savePngImage ("images/threshold_RGB_" ++ imgName) $ imageMap (thresholdRGB 128) img
      savePngImage ("images/threshold_Y_" ++ imgName) $ imageMap (thresholdY 128) img

  print "done"

