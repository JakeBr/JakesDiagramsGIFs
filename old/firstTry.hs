import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

main :: IO ()
main = mainWith dia

dia :: Double -> Diagram B R2
dia rot = triangle 1   # fc green # rotateBy (Turn rot / 100)
       <> square   1.2 # fc white # lw 0
