{-# LANGUAGE NoMonomorphismRestriction #-}

module WriteSVG where

import Cellular
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

main = defaultMain $ renderU 100 (rule 90) lonelyUniverse

renderU n rule u = renderGrid $ toGrid n rule u

renderGrid = vcat . map (hcat . map boolToShape)

boolToShape True = unitSquare # fc black
boolToShape False = unitSquare # fc white
