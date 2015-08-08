module Viewer where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Text exposing (fromString, monospace)
import Color exposing (..)
import List exposing (indexedMap, (::))

import Hex         exposing (getGridHeight, getGridWidth, unitToCoordinates, cellToOffset)
import DataStructs exposing (..)
import Search      exposing (..)
import Queue       exposing (push, empty, Queue, peek)
import Util        exposing (..)

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

evenOffset n = if (n % 2 == 1) then (oneHexWidth / 2) else 0

-- Drawers
drawUnit : Float -> Float -> HexUnit -> List Form
drawUnit h w unit =
  let unitCoords          = unitToCoordinates unit
      coords              = unitCoords.members
      computeXY col row   = ((toFloat col) * oneHexWidth + (evenOffset row),
                             -1.0 * (toFloat row) * oneHexHeight)
      centerDot           = let (col, row) = cellToOffset unit.location 
                                (x, y)     = computeXY col row
                            in circle circleRadius
                                 |> filled unitCenter
                                 |> move (x, y)
      unitNGon (col, row) = ngon 6 hexRadius
                              |> filled uniter
                              |> rotate (degrees 30)
                              |> move (computeXY col row)
  in  List.append (List.map unitNGon coords) [centerDot]

showModel : HexModel -> List Command -> Element
showModel model commands = 
    let gridWidth = toFloat <| getGridWidth model.grid
        gridHeight = toFloat <| getGridHeight model.grid
        makeHexagon rownum colnum val =
          let color   = if val == Empty then clearGrey else yeller
              evenoff = evenOffset rownum
          in ngon 6 hexRadius
               |> filled color
               |> rotate (degrees 30)
               |> move ((toFloat colnum) * oneHexWidth + evenoff,
                        -1.0 * (toFloat rownum) * oneHexHeight         )
        makeHexagons rowIndex row = group <| indexedMap (makeHexagon rowIndex) row
        hexagons = indexedMap makeHexagons model.grid
        unitgons = drawUnit gridHeight gridWidth model.unit
        collageWidth  = floor (oneHexWidth * gridWidth * 1.2)
        collageHeight = floor (oneHexHeight * gridHeight * 1.2)
        collageOffset = (-1.0 * oneHexWidth * gridWidth * 0.5, 
                         oneHexHeight * gridHeight * 0.5)
    in flow down
            [ flow right <| (renderString "Moves: " :: (List.map show commands))
            , flow right [renderString "Score: ", show model.score]
            , flow right [renderString "Unit: ", show (unitToCoordinates model.unit)]
            , collage collageWidth collageHeight 
                      [move collageOffset (group (List.append hexagons unitgons))]
            ]

renderString = centered << monospace << fromString

