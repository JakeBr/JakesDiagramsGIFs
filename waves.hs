import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

main :: IO ()
main = mainWith dia

dia :: Double -> Diagram B R2
dia t = (vcat . map hcat $ squares t)
  # translate (r2 (-leHalf,leHalf))
  <> square (fromIntegral num) # lw 0 # fc black
  where leHalf = fromInteger $ floor (fromIntegral num / (2 :: Double))

squares :: Double -> [[Diagram B R2]]
squares t = map (\n -> map
                (mkSqr . fromIntegral . (+ (num * (n - 1)))) [1 .. num])
                [1 .. num]
  where mkSqr n = square 1 # lw 0 # fcA (white `withOpacity` opacityOfAt n t)

opacityOfAt :: Double -> Double -> Double
opacityOfAt x t = (sin (sqrt ( ((xPosOf x - geHalf) / 3) ^ (2 :: Int)
                             + ((yPosOf x - geHalf) / 3) ^ (2 :: Int)
                             ) - (2 * pi * t / 10)) + 2) / 4
  where yPosOf = fromIntegral . (`div` num) . (+ (num - 1)) . round
        xPosOf = fromIntegral . (+ 1) . (`mod` num) . (\a -> a - 1) . round
        geHalf = fromInteger $ ceiling (fromIntegral num / (2 :: Double))

num :: Int
num = 360
