{-# LANGUAGE TypeFamilies #-}

import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

main :: IO ()
main = mainWith dia

dia :: Double -> Diagram B R2
dia t = decorateTrail trail (circles t)
     <> square 1 # fc black # lw 0

circles :: Double -> [Diagram B R2]
circles t = map circulize [ False, True,  True,  False, False
                          , True,  False, False, True,  True
                          , True,  False, False, True,  True
                          , False, True,  True,  False, False
                          , False, True,  True,  False, False
                          ]
  where circulize isOffset
          | isOffset  = aCircle t
          | otherwise = aCircle (t + 50)

aCircle :: Double -> Diagram B R2
aCircle t = circle (size t) # fcA (color t) # lw 0

size :: Double -> Double
size t = (cos ((t / 50) * pi) + 1.1) / 21

color :: Double -> AlphaColour Double
color t = white `withOpacity`
  max 0 ((cos (((t + 50) / 50) * pi) + 0.95) / 1.95)

trail :: Trail R2
trail = fromVertices [ mkP2 i j
                     | i <- poss
                     , j <- poss
                     ]

poss :: [Double]
poss = map (/10) [0,-2,2,-4,4]
