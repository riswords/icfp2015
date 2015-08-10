module Viewer2 where

import Array            exposing (toList)
import Color            exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List             exposing ((::), take, indexedMap)
import Tests            exposing (..)
import Signal           exposing (Address)

import String           exposing (append)
import DataStructs      exposing (..)
import Hex              exposing (unitToCoordinates, cellToOffset)
import Queue            exposing (push, empty, Queue, peek)
import Search           exposing (..)
import Text             exposing (fromString, monospace, bold, color, height)
import Util             exposing (..)
import Graphics.Input   exposing (button, dropDown)
import Init             exposing (setupGame)

viewer : Address Action -> GameState -> Element
viewer = showModel 

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

getModelFromState : GameState -> HexModel
getModelFromState state =
  case state of
    GameOver m        -> m
    ComputingMove m t -> m
    RunningGame m c   -> m

showModel : Address Action -> GameState -> Element
showModel addr state =
    let model      = getModelFromState state
        gridWidth  = toFloat model.width
        gridHeight = toFloat model.height
        makeHexagons rowIndex row = group <| indexedMap (makeHexagon rowIndex) <| toList row
        hexagons      = indexedMap makeHexagons <| toList model.grid
        unitgons      = drawUnit gridHeight gridWidth model.unit
        collageWidth  = floor (oneHexWidth * gridWidth * 1.2)
        collageHeight = floor (oneHexHeight * gridHeight * 1.2)
        collageOffset = (-1.0 * oneHexWidth * gridWidth * 0.5, 
                         oneHexHeight * gridHeight * 0.5)
        space = spacer 20 20
    in flow down
            [ space
            , flow right [ space
                         , flow down [ testSelector addr
                                     , space
                                     , button (Signal.message addr Nop) "Start"
                                     ]
                         , space 
                         , flow right [space, scoreText <| String.append "Score: " <| toString model.score]
                         ]
            , space                         
            , collage collageWidth collageHeight 
                      <| List.append [move collageOffset (group (List.append hexagons unitgons))]
                                     (statusOverlay state)
            ]


testSelector addr = 
  dropDown 
    (Signal.message addr)
    [ ("test0",  Init <| setupGame test0)
    , ("test1",  Init <| setupGame test1)
    , ("test2",  Init <| setupGame test2)
    , ("test3",  Init <| setupGame test3)
    , ("test4",  Init <| setupGame test4)
    , ("test5",  Init <| setupGame test5)
    , ("test6",  Init <| setupGame test6)
    , ("test7",  Init <| setupGame test7)
    , ("test8",  Init <| setupGame test8)
    , ("test9",  Init <| setupGame test9)
    , ("test10", Init <| setupGame test10)
    , ("test11", Init <| setupGame test11)
    , ("test12", Init <| setupGame test12)
    , ("test13", Init <| setupGame test13)
    , ("test14", Init <| setupGame test14)
    , ("test15", Init <| setupGame test15)
    , ("test16", Init <| setupGame test16)
    , ("test17", Init <| setupGame test17)
    , ("test18", Init <| setupGame test18)
    , ("test19", Init <| setupGame test19)
    , ("test20", Init <| setupGame test20)
    , ("test21", Init <| setupGame test21)
    , ("test22", Init <| setupGame test22)
    , ("test23", Init <| setupGame test23)
    , ("test24", Init <| setupGame test24)
    ]

statusOverlay : GameState -> List Form
statusOverlay state = 
  case state of 
    GameOver      m   -> [rect 200 100 |> filled messageColor, msgText "  Game Over"]
    ComputingMove m t -> [rect 200 100 |> filled messageColor, msgText "  Computing Move"]
    RunningGame   m c -> []


msgText      = Graphics.Collage.text << Text.height 20 << bold << Text.color white << fromString
scoreText    = centered << Text.height 20 << bold << Text.color black << fromString
renderString = centered << monospace << fromString

