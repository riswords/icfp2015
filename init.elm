module Init where

import DataStructs exposing (..)
import Util        exposing (..)
import List        exposing (repeat, map, take, drop, foldr, (::), append, head, tail)
import Rand        exposing (next)

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
fillCell {x, y} g = 
  let l1          = take y g
      (row::rest) = drop y g
      rowPrefix   = take x row
      rowSuffix   = drop (x+1) row
  in append l1 <| (append rowPrefix (Filled :: rowSuffix)) :: rest

initGameState : Input -> List HexModel
initGameState {id, units, width, height, filled, sourceLength, sourceSeeds} =
  map 
    (\ s -> 
      let (randInt, seed') = next s
          hexUnits = map convertUnit units
      in
        { id           = id
        , units        = hexUnits
        , unit         = getUnit (randInt % sourceLength) hexUnits
        , grid         = foldr fillCell (initGrid width height) filled
        , sourceLength = sourceLength
        , sourceSeed   = seed'
        , score        = 0
        , isGameOver   = False
        })
    sourceSeeds

