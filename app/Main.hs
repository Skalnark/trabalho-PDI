{-# LANGUAGE OverloadedStrings #-}

module Main
  where
import Formatting ( (%), fprint )
import Formatting.Clock ( timeSpecs )
import System.Clock ( getTime, Clock(Monotonic) )
import Data.Either ()
import PDI
import Codec.Picture
          ( savePngImage,
            convertRGB8,
            readImage)

main :: IO ()
main = do
  putStrLn "Insert the image name (including extension): "
  imgName <- getLine  
  input <- readImage $ "images/" ++ imgName
  case input of
    Left err -> putStrLn err
    Right inputImage -> do
      start <- getTime Monotonic
      let img = convertRGB8 inputImage
      savePngImage ("images/output/red_" ++ imgName) $ imageMap redComponent img
      putStrLn "red done..."
      savePngImage ("images/output/green_" ++ imgName) $ imageMap greenComponent img
      putStrLn "green done..."
      savePngImage ("images/output/blue_" ++ imgName) $ imageMap blueComponent img
      putStrLn "blue done..."
      savePngImage ("images/output/negative_RGB_" ++ imgName) $ imageMap negativeFromRGB img
      putStrLn "negative rgb done..."
      savePngImage ("images/output/negative_Y_" ++ imgName) $ imageMap negativeFromY img
      putStrLn "negative Y done..."
      savePngImage ("images/output/to_YIQ_and_back_to_rgb_" ++ imgName) $ imageMap backAndAgain img
      putStrLn "to YIQ and back to RGB done..."
      savePngImage ("images/output/negative_RGB_" ++ imgName) $ imageMap negativeFromRGB img
      putStrLn "negative rgb done..."
      savePngImage ("images/output/negative_Y_" ++ imgName) $ imageMap negativeFromY img
      putStrLn "negative Y done..."
      savePngImage ("images/output/to_YIQ_and_back_to_rgb_" ++ imgName) $ imageMap backAndAgain img
      putStrLn "to YIQ and back to RGB done..."
      savePngImage ("images/output/additive_brightness_YIQ_" ++ imgName) $ imageMap (addBrightnessYIQ 10) img
      putStrLn "additive brightness YIQ done..."
      savePngImage ("images/output/multiplicative_brightness_YIQ_" ++ imgName) $ imageMap (multBrightnessYIQ 1.5) img
      putStrLn "multiplicative brightness YIQ done..."
      savePngImage ("images/output/additive_brightness_RGB_" ++ imgName) $ imageMap (addBrightnessRGB 10) img
      putStrLn "additive brightness RGB done..."
      savePngImage ("images/output/multiplicative_brightness_RGB_" ++ imgName) $ imageMap (multBrightnessRGB 2) img
      putStrLn "multiplicative brightness RGB done..."
      savePngImage ("images/output/threshold_RGB_64_" ++ imgName) $ imageMap (thresholdRGB 64) img
      putStrLn "threshold RGB 64 done..."
      savePngImage ("images/output/threshold_RGB_128_" ++ imgName) $ imageMap (thresholdRGB 128) img
      putStrLn "threshold RGB 128 done..."
      savePngImage ("images/output/threshold_RGB_196_" ++ imgName) $ imageMap (thresholdRGB 196) img
      putStrLn "threshold RGB 196 done..."
      savePngImage ("images/output/threshold_Y_64_" ++ imgName) $ imageMap (thresholdY 64) img
      putStrLn "threshold YIQ 64 done..."
      savePngImage ("images/output/threshold_Y_128_" ++ imgName) $ imageMap (thresholdY 128) img
      putStrLn "threshold YIQ 128 done..."
      savePngImage ("images/output/threshold_Y_196_" ++ imgName) $ imageMap (thresholdY 196) img
      putStrLn "threshold YIQ 196 done..."
      end <- getTime Monotonic
      fprint (timeSpecs % "\n") start end

  print "done"

