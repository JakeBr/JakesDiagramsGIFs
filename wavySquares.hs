import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

main :: IO ()
main = mainWith dia

dia :: Double -> Diagram B R2
dia t = ((foldl1 (===) . map (foldl1 (|||)) . splitEvery 9 $ squares t)
  # translate (r2 (-4,4)))
  <> square 9 # lw 0 # fc black

squares :: Double -> [Diagram B R2]
squares t = map
  ((\x -> square 1 # lw 0 # fcA (white `withOpacity` opacityOfAt x t)) . fromInteger)
  [1..81]

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = if n > 0
                    then go [] [] xs n
                    else []
  where go xss buf [] _ = xss ++ [buf]
        go xss buf l@(x:xs') m
          | m <= 0    = go (xss ++ [buf]) [] l n
          | otherwise = go xss (buf ++ [x]) xs' (m - 1)

opacityOfAt :: Double -> Double -> Double
opacityOfAt x t = exp (- (t - 30 - delayOf x) ^ 2 / 3 ^ 2) * 0.8
  where delayOf a = - 0.8 * (cos . sqrt $
                     ((yPosOf a - 5) / 2) ^ 2 +
                     ((xPosOf a - 5) / 2) ^ 2)
        yPosOf = fromIntegral . (`div` 9) . (+ 8) . round
        xPosOf = fromIntegral . (+ 1) . (`mod` 9) . (\a -> a - 1) . round
