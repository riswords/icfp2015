module Viewer where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Text exposing (fromString, monospace)
import Color exposing (..)
import List exposing (indexedMap, (::))

import Util exposing (getGridHeight, getGridWidth, unitToCoordinates, cellToOffset)
import DataStructs exposing (..)
import Search      exposing (..)
import Queue       exposing (push, empty, Queue, peek)


viewer : Running (Queue (HexModel, List Command), (List Command, Int)) -> Element
viewer m =
  case m of
    (More (queue, best)) -> hexView (peek queue)
    (Done (queue, best)) -> hexView (peek queue) 

hexView : Maybe (HexModel, List Command) -> Element
hexView maybeStuff =
    case maybeStuff of
      Nothing -> show maybeStuff
      Just (hexModel, commandHistory) -> showModel hexModel commandHistory

-- Just some important values
hexRadius    = 20
circleRadius = hexRadius / 2
oneHexWidth  = hexRadius * 2 * 0.88
oneHexHeight = hexRadius * 2 * 0.80
xoffset      = 0
yoffset      = 0

evenOffset n = if (n % 2 == 0) then (oneHexWidth / 2) else 0

-- Drawers
drawUnit : Float -> Float -> HexUnit -> List Form
drawUnit h w unit =
  let coords              = unitToCoordinates unit
      computeXY row  hex  = ((toFloat hex) * oneHexWidth + (evenOffset (row + 1)),
                             ((h - 1) - (toFloat row)) * oneHexHeight)
      centerDot           = let (hex, row) = cellToOffset unit.location 
                                (x, y)     = computeXY row hex
                            in circle circleRadius
                                 |> filled unitCenter
                                 |> move (x, y)
      unitNGon (hex, row) = ngon 6 hexRadius
                              |> filled uniter
                              |> rotate (degrees 30)
                              |> move (computeXY row hex)
  in  List.append (List.map unitNGon coords) [centerDot]

showModel : HexModel -> List Command -> Element
showModel model commands = 
    let gridWidth = toFloat <| getGridWidth model.grid
        gridHeight = toFloat <| getGridHeight model.grid
        makeNGon rownum hexnum val =
          let color   = if val == Empty then clearGrey else yeller
              evenoff = evenOffset rownum
          in ngon 6 hexRadius
               |> filled color
               |> rotate (degrees 30)
               |> move ((toFloat hexnum) * oneHexWidth + evenoff,
                        (toFloat rownum) * oneHexHeight         )
        makeNGons index row = group <| indexedMap (makeNGon index) row
        ngons : List Form
        ngons = indexedMap makeNGons (List.reverse model.grid)
        unitgons = drawUnit gridHeight gridWidth model.unit
        collageWidth  = floor (oneHexWidth * gridWidth * 1.2)
        collageHeight = floor (oneHexHeight * gridHeight * 1.2)
        collageOffset = (-1.0 * oneHexWidth * gridWidth * 0.5, 
                         -1.0 * oneHexHeight * gridHeight * 0.5)
    in flow up
            [ collage collageWidth collageHeight 
                      [move collageOffset (group (List.append ngons unitgons))]
            , flow right [renderString "Unit: ", show model.unit]
            , flow right [renderString "Score: ", show model.score]
            , flow right <| (renderString "Moves: " :: (List.map show commands))
            ]

renderString = centered << monospace << fromString

clearGrey : Color
clearGrey = rgba 160 160 160 1.0
yeller : Color
yeller = rgba 230 184 0 1.0
uniter : Color
uniter = rgba 46 184 230 1.0
unitCenter : Color
unitCenter = rgba 25 117 209 1.0
