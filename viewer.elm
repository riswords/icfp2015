module Viewer where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import List exposing (indexedMap)

import Util exposing (getGridHeight, getGridWidth)
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

showModel : HexModel -> Element
showModel model = 
    let gridWidth = getGridWidth model.grid
        gridHeight = getGridHeight model.grid

        hexRadius = 15
        oneHexWidth = hexRadius * 2 * 0.88
        oneHexHeight = hexRadius * 2

        makeNGon : Int -> List Hex -> Form
        makeNGon index row = ngon 6 hexRadius
                                |> filled clearGrey
                                |> rotate (degrees 30)
                                |> move ((toFloat index) * oneHexWidth, 0)

        ngons : List Form
        ngons = indexedMap makeNGon model.grid
    in collage (ceiling(oneHexWidth) * gridWidth + 100)
               (ceiling(oneHexHeight) * gridHeight + 100)
               ngons


clearGrey : Color
clearGrey =
  rgba 200 200 200 0.6
