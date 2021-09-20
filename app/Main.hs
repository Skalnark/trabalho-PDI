{-# LANGUAGE OverloadedStrings #-}

module Main
  where
import Formatting ( (%), fprint )
import Formatting.Clock ( timeSpecs )
import System.Clock ( getTime, Clock(Monotonic) )
import Data.Either ()
import Codec.Picture
          ( convertRGB8,
            readImage)
import PDI

main :: IO ()
main = do
  putStrLn "Insert the file name (including extension): "
  imgName <- getLine  
  input <- readImage $ "images/" ++ imgName
  case input of
    Left err -> putStrLn err
    Right inputImage -> do
      start <- getTime Monotonic
      let img = convertRGB8 inputImage
      saveAndLog "red_" imgName $ imageMap redComponent img
      saveAndLog "green_" imgName $ imageMap greenComponent img      
      saveAndLog "blue_" imgName $ imageMap blueComponent img      
      saveAndLog "negative_from_RGB_" imgName $ imageMap negativeFromRGB img      
      saveAndLog "negative_from_YIQ_" imgName $ imageMap negativeFromY img      
      saveAndLog "RGB_to_YIQ_and_back_to_RGB_" imgName $ imageMap backAndAgain img      
      saveAndLog "add_brightness_YIQ_" imgName $ imageMap (addBrightnessYIQ 30) img      
      saveAndLog "multiply_brightness_YIQ_" imgName $ imageMap (multBrightnessYIQ 1.5) img      
      saveAndLog "add_brightness_RGB_" imgName $ imageMap (addBrightnessRGB 30) img      
      saveAndLog "multiply_brightness_RGB_" imgName $ imageMap (multBrightnessRGB 2) img      
      saveAndLog "threshold_RGB_32_" imgName $ imageMap (thresholdRGB 32) img      
      saveAndLog "threshold_RGB_64_" imgName $ imageMap (thresholdRGB 64) img      
      saveAndLog "threshold_RGB_128_" imgName $ imageMap (thresholdRGB 128) img      
      saveAndLog "threshold_YIQ_32_" imgName $ imageMap (thresholdY 32) img      
      saveAndLog "threshold_YIQ_64_" imgName $ imageMap (thresholdY 64) img      
      saveAndLog "threshold_YIQ_128_" imgName $ imageMap (thresholdY 128) img
      end <- getTime Monotonic
      fprint (timeSpecs % "\n") start end

  print "done"