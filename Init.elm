module Init where

import DataStructs exposing (..)
import Util        exposing (..)
import Update      exposing (spawnNewUnit)
import List        exposing (map, take, drop, foldr, (::), append, head, tail, length)
import Rand        exposing (next)
import Hex         exposing (setXYCell)
import Array       exposing (repeat)
import IO          exposing (fromJson)
import Tests       exposing (emptyModel)
import Maybe       exposing (withDefault)

initGrid : Int -> Int -> Grid
initGrid width height = repeat height (repeat width Empty)

convertUnit : Unit -> HexUnit
convertUnit {members, pivot} = 
    let hexMembers : List HexCell
        hexMembers = map (convertCell pivot) members
        origin : HexCell
        origin = convertCell { x = 0, y = 0 } pivot
    in { members = hexMembers
       , location = origin
       }

convertCell : Cell -> Cell -> HexCell
convertCell pivot {x, y} = 
    let newX = x - ((y - (y % 2)) // 2)
        newZ = y
    in { x = newX
       , z = newZ
       , y = (-newX) - newZ
       }

fillCell : Cell -> Grid -> Grid
fillCell {x, y} g = setXYCell x y g Filled

setupGame : String -> HexModel
setupGame input = withDefault emptyModel <| head <| initGameState <| fromJson input 

initGameState : Input -> List HexModel
initGameState {id, units, width, height, filled, sourceLength, sourceSeeds} =
  let modelUnits = map convertUnit units
      modelGrid  = foldr fillCell (initGrid width height) filled
  in
    map 
      (\ s -> 
        { id           = id
        , units        = modelUnits
        , unit         = HexUnit [] <| HexCell 0 0 0 -- Dummpy Unit
        , grid         = modelGrid
        , sourceLength = sourceLength
        , sourceSeed   = s
        , score        = 0
        , prevLines    = 0
        , isGameOver   = False
        , history      = []
        , width        = width
        , height       = height
        , originalSeed = s
        } |> spawnNewUnit )
      sourceSeeds
