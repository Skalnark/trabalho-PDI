import Graphics.Rendering.Chart.Easy
    ( line,
      plot,
      red,
      blue,
      opaque,
      setColors,
      (.=),
      layout_title,
      Default(def),
      points )
import Graphics.Rendering.Chart.Backend.Cairo (toFile)
import Prelude as P
import Graphics.Image as I hiding (zipWith, map, Num, Float, Double)
import Graphics.Image.IO

signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin x*3.14159/5)) | x <- xs ]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib :: Int -> Double 
fib n = fromIntegral (fibs !! n)

fibList :: Int -> [Double]
fibList count = fibList' count []
  where 
    fibList' 0 list = fib 0 : list
    fibList' count list = fibList' (count - 1) (fib count : list)

fibonacci n = zip (fibList n) [0..n]


dots :: [Double]
dots = [0,0.5..400]

dots' :: [Double]
dots' = [0,2.5..400]

main :: IO()
main = do
    --example1 <- readImageRGBA VU "images/example1_big.png"
    toFile def "images/example1_big.png" $ do
      layout_title .= "Amplitude Modulation line and dots"
      setColors [opaque blue, opaque red]
      plot (line "am" [signal dots])
      plot (points "am points" (signal dots'))

    toFile def "images/example2_big.png" $ do
      layout_title .= "Amplitude Modulation line"
      setColors [opaque blue]
      plot $ line "am" [signal dots]

    toFile def "images/example3_big.png" $ do
      layout_title .= "Amplitude Modulation dots"
      setColors [opaque red]
      plot (points "am points" (signal dots'))

    toFile def "images/example4_fib.png" $ do
      layout_title .= "Fibonacci dots and line"
      setColors [opaque blue, opaque red]
      plot (points "fib points" $ fibonacci 18)
      plot (line "Log line" [fibonacci 18])