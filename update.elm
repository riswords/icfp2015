module Update where

import DataStructs exposing (..)
import Array       exposing (..)
import Util        exposing (all, any, maximum, minimum, toArray)
import Maybe       exposing (withDefault)
import Hex         exposing (..)
import Rand        exposing (next)

-- Model -> (Model, # of Cleared Lines)
clearRows : HexModel -> (HexModel, Int)
clearRows model = 
    let (grid, clearedRows) = 
          foldr 
            (\row (ls, i) -> if all filled row then (ls, i+1) else (append (toArray row) ls, i))
            (empty, 0)
            model.grid
    in ( { model | grid <- padN clearedRows (repeat model.width Empty) grid }
         , clearedRows
       )

padN : Int -> Array Hex -> Grid -> Grid
padN count emptyRow grid = append (repeat count emptyRow) grid


updateScore : Int -> HexUnit -> HexModel -> HexModel
updateScore ls unit model = 
  let size       = List.length unit.members
      lsOld      = model.prevLines
      points     = size + (100 * (1 + ls) * ls // 2)
      lineBonus = if   lsOld > 1 
                  then (lsOld - 1) * points // 10
                  else 0
  in { model | score     <- model.score + points + lineBonus 
             , prevLines <- ls }

updateUnit : Grid -> Command -> HexUnit -> (Bool, HexUnit)
updateUnit grid command =
  if   isRotateCommand command 
  then rotateUnit grid command
  else moveUnit   grid command

-- Performs the requested command if possible
-- clears lines
-- update the score
update : Command -> HexModel -> HexModel
update move model = 
  let newModel = { model | history <- move :: model.history }
      (moveSucceeded, updUnit) = updateUnit newModel.grid move newModel.unit
  in if not moveSucceeded
     then scoreAndSpawn updUnit newModel
     else { newModel | unit <- updUnit }

scoreAndSpawn : HexUnit -> HexModel -> HexModel
scoreAndSpawn updUnit model =
  let (newModel, lineClear) = clearRows model
  in newModel
     |> updateScore lineClear updUnit 
     |> lockUnit updUnit 
     |> spawnNewUnit

lockUnit : HexUnit -> HexModel -> HexModel
lockUnit unit model =
  let updater cell grid = setCell cell.x cell.y cell.z grid Filled
      updatedGrid       = List.foldl updater model.grid unit.members
  in { model | grid <- updatedGrid }

-- duplicated logic in init.elm, keep in sync
spawnNewUnit : HexModel -> HexModel
spawnNewUnit model = 
    let (randInt, seed') = next model.sourceSeed
        newUnit          = getUnit (randInt % List.length model.units) model.units
        locatedUnit      = moveToCenter model newUnit
        spawnSuccess     = isUnitSafe model.grid locatedUnit
    in { model
       | unit       <- locatedUnit
       , sourceSeed <- seed'
       , isGameOver <- not spawnSuccess
       , history    <- P :: model.history
       }

moveToCenter : HexModel -> HexUnit -> HexUnit
moveToCenter model unit = 
    let grid        = model.grid 
        coords       = List.map cellToOffset unit.members
        xes          = List.map fst coords
        ys           = List.map snd coords
        minX         = withDefault 0 (List.minimum xes)
        maxX         = withDefault 0 (List.maximum xes)
        minY         = withDefault 0 (List.minimum ys)
        (curX, curY) = cellToOffset unit.location
        width        = model.width - 1
        idealOffset  = ((width - maxX) + minX) // 2
        offsetX      = idealOffset - minX 
        offsetY      = curY - (curY - minY)
        cellOffset   = offsetToCell (offsetX, offsetY)
    in { members  = List.map (offsetBy cellOffset) unit.members
       , location = offsetBy cellOffset unit.location
       }
