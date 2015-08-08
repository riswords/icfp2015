module Init where

import DataStructs exposing (..)
import Util        exposing (..)
import Update      exposing (spawnNewUnit)
import List        exposing (repeat, map, take, drop, foldr, (::), append, head, tail, length)
import Rand        exposing (next)
import Hex         exposing (setXYCell)

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
convertCell pivot cell = 
    let (relCol, relRow) = (cell.x - pivot.x, cell.y - pivot.y)
        newX = relCol - ((relRow - (relRow % 2)) // 2)
        newZ = relRow
    in { x = newX
       , z = newZ
       , y = (-newX) - newZ
       }

fillCell : Cell -> Grid -> Grid
fillCell {x, y} g = setXYCell x y g Filled

initGameState : Input -> List HexModel
initGameState {id, units, width, height, filled, sourceLength, sourceSeeds} =
  map 
    (\ s -> 
      { id           = id
      , units        = map convertUnit units
      , unit         = HexUnit [] <| HexCell 0 0 0 -- Dummpy Unit
      , grid         = foldr fillCell (initGrid width height) filled
      , sourceLength = sourceLength
      , sourceSeed   = s
      , score        = 0
      , prevLines    = 0
      , isGameOver   = False
      } |> spawnNewUnit )
    sourceSeeds
