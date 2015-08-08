module Viewer where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import List exposing (indexedMap, (::))

import Util exposing (getGridHeight, getGridWidth, unitToCoordinates)
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
        Just (hexModel, commandHistory) -> showModel hexModel

-- Just some important values
hexRadius    = 20
oneHexWidth  = hexRadius * 2 * 0.88
oneHexHeight = hexRadius * 2 * 0.80
xoffset      = 250
yoffset      = 200


evenOffset n = if (n % 2 == 1) then (oneHexWidth / 2) else 0

-- Drawers
drawUnit : HexUnit -> List Form
drawUnit unit =
  let coords              = unitToCoordinates unit
      unitNGon (row, hex) = ngon 6 hexRadius
                              |> filled uniter
                              |> rotate (degrees 30)
                              |> move ((toFloat hex) * oneHexWidth + (evenOffset row) - xoffset, 
                                       (toFloat row) * oneHexHeight                   - yoffset)
  in  List.map unitNGon coords 

showModel : HexModel -> Element
showModel model = 
    let gridWidth = getGridWidth model.grid
        gridHeight = getGridHeight model.grid
        makeNGon rownum hexnum val =
          let color   = if val == Empty then clearGrey else yeller
              evenoff = evenOffset rownum
          in ngon 6 hexRadius
               |> filled color
               |> rotate (degrees 30)
               |> move ((toFloat hexnum) * oneHexWidth + evenoff - xoffset, 
                        (toFloat rownum) * oneHexHeight          - yoffset)
        makeNGons index row = group <| indexedMap (makeNGon index) row
        ngons : List Form
        ngons = indexedMap makeNGons (List.reverse model.grid)
    in collage 800 800 (List.append (drawUnit model.unit)  ngons)


clearGrey : Color
clearGrey = rgba 160 160 160 1.0
yeller : Color
yeller = rgba 230 184 0 1.0
uniter : Color
uniter = rgba 46 184 230 1.0
