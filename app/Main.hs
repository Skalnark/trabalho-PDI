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
      savePngImage ("images/change_brightness_" ++ imgName) $ imageMap (changeBrightness 12) img

  print "done"

