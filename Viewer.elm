module Viewer where

import Array                exposing (toList)
import Color                exposing (..)
import Graphics.Collage     exposing (..)
import Graphics.Element     exposing (..)
import Graphics.Input       exposing (button, dropDown)
import Graphics.Input.Field exposing (field)
import Html                 exposing (toElement, div, input, Html, Attribute)
import Html.Events          exposing (on, targetValue)
import Html.Attributes      exposing (placeholder)
import Signal               exposing (Address)
import Maybe                exposing (withDefault)
import Result               exposing (toMaybe)

import List             exposing ((::), take, indexedMap, drop, map, reverse)
import Tests            exposing (..)
import IO               exposing (toJson, generateOutput)
import Engine           exposing (getModelFromState, getInputInfo)
import String           exposing (append, dropLeft)
import DataStructs      exposing (..)
import Hex              exposing (unitToCoordinates, cellToOffset)
import Search           exposing (..)
import Text             exposing (fromString, monospace, bold, color, height)
import Util             exposing (..)
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

showModel : Address Action -> GameState -> Element
showModel addr state =
    let model      = getModelFromState state
        inputInfo  = getInputInfo state
        gridWidth  = toFloat model.width
        gridHeight = toFloat model.height
        makeHexagons rowIndex row = group <| indexedMap (makeHexagon rowIndex) <| toList row
        hexagons      = indexedMap makeHexagons <| toList model.grid
        unitgons      = drawUnit gridHeight gridWidth model.unit
        collageWidth  = floor (oneHexWidth * gridWidth * 1.2)
        collageHeight = floor (oneHexHeight * gridHeight * 1.2)
        collageOffset = (-1.0 * oneHexWidth * gridWidth * 0.5, 
                         oneHexHeight * gridHeight * 0.5)
        space      = spacer 20 20
        timebox    = toElement 20 20 <| 
                       inputBox addr "100" 
                                     (TimeLimit << withDefault 100 << toMaybe << String.toInt)
        timePanel  = flow down [ labelText "Allowed Time" , spacer 5 5 , timebox] 
        testSelect = flow down [ testSelector addr
                               , space
                               , button (Signal.message addr Nop) "Start"
                               ]
        jsonOut    = flow down [ labelText "Output", space, showOutput state ]
        gameInfo   = flow right 
                          [ flow down 
                                 [ labelText <| String.append "Score: " <| toString model.score
                                 , space
                                 , labelText <| String.append "Pieces Left: " <| toString model.sourceLength
                                 ]
                          , space
                          , flow down
                                 [ labelText <| 
                                   String.append "Elapsed Time: " <| 
                                   toString <|
                                   floor <|
                                   inputInfo.lastTime - inputInfo.startTime
                                 , space
                                 , labelText <| 
                                   String.append "Allowed Time: " <| 
                                   toString <|
                                   floor <|
                                   inputInfo.timeLimit
                                 ]
                          ]
    in flow down [space, flow right
         [ space 
         , flow down [ testSelect
                     , space
                     , timePanel
                     , space
                     , jsonOut
                     ]
         , space
         , flow down [ gameInfo
                     , space
                     ,  collage collageWidth collageHeight <|
                          List.append [move collageOffset (group (List.append hexagons unitgons))]
                                      (statusOverlay state)
                     ]
         ]]
         
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
    GameOver      i m   -> [rect 200 100 |> filled messageColor, msgText "  Game Over"]
    ComputingMove i m t -> [rect 200 100 |> filled messageColor, msgText "  Computing Move"]
    RunningGame   i m c -> []


msgText   = Graphics.Collage.text << Text.height 20 << monospace << Text.color white << fromString
labelText = centered << Text.height 15 << monospace << Text.color black << fromString

renderString : String -> Element
renderString = centered << monospace << fromString

chunkUp : Int -> String -> List String
chunkUp n s = 
  if String.isEmpty s
  then []
  else (String.left n s) :: chunkUp n (dropLeft n s)

inputBox : Address Action -> String -> (String -> Action) -> Html 
inputBox addr ph f = 
  input 
    [ placeholder ph 
    , on "input" targetValue (\ str -> Signal.message addr <| f str )]
    []

showOutput : GameState -> Element
showOutput = flow down << map renderString << chunkUp 20 << toJson << generateOutput 

