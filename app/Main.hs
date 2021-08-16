import Graphics.Image.IO (readImageExact, displayImage)
import Graphics.Image.ColorSpace (toImageRGB, Word8, RGB)
import Graphics.Image.Interface (Image)
import Graphics.Image.Interface.Vector (VS)
import Graphics.Image.IO.Formats as F

main :: IO ()
main = do
  image <- readImageExact F.PNG "images/example1_big.png" :: IO (Either String (Image VS RGB Word8))
  case image of
    Left e -> putStrLn $ "Error reading image: " <> show e
    Right image -> do
      putStrLn "Image read successfully"
      displayImage image
  print "done"