import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude
import Data.Maybe (fromJust)

main :: IO ()
main = gifMain dia

dia :: [(Diagram Cairo R2, Int)]
dia = map
  (\t -> (sponge # view (p2 (-0.5, -0.5)) (r2 (dist t, dist t)), 5))
  [0 .. 99 :: Int]
  where sponge = fromJust $ menger 1 7
        dist t = 1 / 3 * (3 ** (1 / 99)) ^ t

menger :: Double -> Int -> Maybe (Diagram Cairo R2)
menger size n
  | size <= 0 = Nothing
  | n <= 0    = Just whiteSqr
  | otherwise = Just $ mengerRec size n <> whiteSqr
  where whiteSqr = square size # lw 0 # fc white

mengerRec :: Double -> Int -> Diagram Cairo R2
mengerRec size n
  | n <= 1    = blackSqr
  | otherwise = vcat' (with & sep .~ (iterate (/3) size !! n))
    [ nextMengers
    , hcat1 [nextMenger, blackSqr, nextMenger]
    , nextMengers
    ] # translate (r2 (-nextSize,nextSize))
  where nextMengers = hcat2 $ replicate 3 nextMenger
        nextMenger  = mengerRec nextSize (n - 1)
        blackSqr    = square nextSize # lw 0 # fc black
        nextSize    = size / 3
        hcat1       = hcat' (with & sep .~ (    iterate (/3) size !! n))
        hcat2       = hcat' (with & sep .~ (2 * iterate (/3) size !! n))
