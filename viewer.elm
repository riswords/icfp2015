module Viewer where

import Array            exposing (toList)
import Color            exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List             exposing ((::), take, indexedMap)

import DataStructs      exposing (..)
import Hex              exposing (unitToCoordinates, cellToOffset)
import Queue            exposing (push, empty, Queue, peek)
import Search           exposing (..)
import Text             exposing (fromString, monospace)
import Util             exposing (..)

viewer : (HexModel, List Command, List Int ) -> Element
viewer = hexView 

hexView : (HexModel, List Command, List Int ) -> Element
hexView (hexModel, commandHistory, avgScore) = showModel hexModel commandHistory avgScore

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
  let coords              = unitToCoordinates unit
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

makeHexagon : Int -> Int -> Hex -> Form
makeHexagon rownum colnum val =
  let color   = if val == Empty then clearGrey else yeller
      evenoff = evenOffset rownum
  in ngon 6 hexRadius
       |> filled color
       |> rotate (degrees 30)
       |> move ((toFloat colnum) * oneHexWidth + evenoff,
                -1.0 * (toFloat rownum) * oneHexHeight)

showModel : HexModel -> List Command -> List Int -> Element
showModel model commands avgScore = 
    let gridWidth  = toFloat model.width
        gridHeight = toFloat model.height
        makeHexagons rowIndex row = group <| indexedMap (makeHexagon rowIndex) <| toList row
        hexagons      = indexedMap makeHexagons <| toList model.grid
        unitgons      = drawUnit gridHeight gridWidth model.unit
        collageWidth  = floor (oneHexWidth * gridWidth * 1.2)
        collageHeight = floor (oneHexHeight * gridHeight * 1.2)
        collageOffset = (-1.0 * oneHexWidth * gridWidth * 0.5, 
                         oneHexHeight * gridHeight * 0.5)
    in flow down
            [ flow right <| (renderString "Moves: " :: (List.map show <| take 10 commands))
            , flow right [renderString "Score: ", show model.score]
            , flow right [renderString "Heuristic: ", show avgScore]
            , flow right [renderString "Unit Loction: ", show (cellToOffset model.unit.location)]
            , flow right [renderString "Unit Members: ", show (unitToCoordinates model.unit)]
            , flow right [renderString "Game Over: ", show model.isGameOver]
            , collage collageWidth collageHeight 
                      [move collageOffset (group (List.append hexagons unitgons))]
            ]

renderString = centered << monospace << fromString

