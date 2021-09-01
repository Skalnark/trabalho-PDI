import Prelude as P
import Graphics.Image.IO (readImageExact, writeImageExact, displayImage, writeImage)
import Graphics.Image.ColorSpace (toImageRGB, Word8, RGB, Pixel(PixelRGB))
import Graphics.Image.Interface (Image, Array)
import Graphics.Image.Interface.Vector (VS)
import Graphics.Image.IO.Formats as F

main :: IO ()
main = do
  image <- readImageExact F.PNG "images/example1_big.png" :: IO (Either String (Image VS RGB Word8))
  case image of
    Left e -> putStrLn $ "Error reading image: " <> show e
    Right image -> do
      putStrLn "Image read successfully"
      putStrLn "Writing image..."
      writeImageExact F.PNG [] "images/writed_example_big.png" image
      displayImage image
  print "done"
