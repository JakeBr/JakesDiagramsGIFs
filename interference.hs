import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

main :: IO ()
main = mainWith dia

dia :: Double -> Diagram B R2
dia t = (vcat . map hcat $ squares t)
  # translate (r2 (-leHalf numX, leHalf numY))
  <> rect (fromIntegral numX) (fromIntegral numY) # lw 0 # fc black
  where leHalf x = fromInteger $ floor (fromIntegral x / (2 :: Double))

numX :: Int
numX  = num

numY :: Int
numY = floor $ fromIntegral num * (2 / 3 :: Double)

squares :: Double -> [[Diagram B R2]]
squares t = map (\n -> map
                (mkSqr . fromIntegral . (+ (numX * (n - 1))))
                ([1 .. numX] :: [Int]))
                [1 .. numY]
  where mkSqr n = square 1 # lw 0 # fcA (white `withOpacity` opacityOfAt n t)

opacityOfAt :: Double -> Double -> Double
opacityOfAt x t = ( sin (sqrt ( ((xPosOf x - geFrac 3 True ) / 3) ^ (2 :: Int)
                              + ((yPosOf x - geFrac 2 False) / 3) ^ (2 :: Int)
                              ) - (2 * pi * t / 10))

                  + sin (sqrt ( ((xPosOf x - 2 * geFrac 3 True ) / 3)
                              ^ (2 :: Int)
                              + ((yPosOf x - geFrac 2 False) / 3) ^ (2 :: Int)
                              ) - (2 * pi * t / 10))
                  + 2) / 4

  where yPosOf = fromIntegral . (`div` numX) . (+ (numX - 1)) . round
        xPosOf = fromIntegral . (+ 1) . (`mod` numX) . (\a -> a - 1) . round
        geFrac n isX = fromInteger $
          ceiling ((fromIntegral
                  (if isX then numX else numY) / fromInteger n) :: Double)

num :: Int
num = 360
