module Update where

import DataStructs exposing (..)
import List        exposing (..)
import Maybe       exposing (withDefault)
import Hex         exposing (..)
import Rand        exposing (next)

-- Model -> (Model, # of Cleared Lines)
clearRows : HexModel -> (HexModel, Int)
clearRows model = 
    let clearOneRow : List Hex -> (Int, List Hex)
        clearOneRow row = if all filled row
                          then (1, map (\ x -> Empty) row)
                          else (0, row)
        rows = map clearOneRow model.grid
    in ( { model | grid <- applyGravity <| map snd rows }
       , sum <| map fst rows 
       )

updateScore : HexModel -> Int -> HexUnit -> HexModel
updateScore model ls unit = 
  let size       = length unit.members
      lsOld      = model.prevLines
      points     = size + (100 * (1 + ls) * ls // 2)
      lineBonus = if   lsOld > 1 
                  then (lsOld - 1) * points // 10
                  else 0
  in { model | score     <- model.score + points + lineBonus 
             , prevLines <- ls }

applyGravity : Grid -> Grid
applyGravity grid = 
  let gridHeight = getGridHeight grid
      emptyRow   = repeat (getGridWidth grid ) Empty
      padTillFull height emptyRow grid = append (repeat (height - getGridHeight grid) emptyRow) grid
  in
    padTillFull gridHeight emptyRow <|
      foldr  
        (\ x ls -> 
           case ls of
             []      -> [x]
             (y::ys) -> if   all (not << filled) y
                        then x::ys
                        else x::y::ys)
        []
        grid

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
  let (moveSucceeded, updUnit) = updateUnit model.grid move model.unit
  in if not moveSucceeded
     then let (newModel, lineClear) = clearRows model 
          in  updateScore newModel lineClear updUnit |> lockUnit updUnit |> spawnNewUnit
     else { model | unit <- updUnit }

lockUnit : HexUnit -> HexModel -> HexModel
lockUnit unit model =
  let updater cell grid = setCell cell.x cell.y cell.z grid Filled
      updatedGrid       = foldl updater model.grid unit.members
  in { model | grid <- updatedGrid }

-- duplicated logic in init.elm, keep in sync
spawnNewUnit : HexModel -> HexModel
spawnNewUnit model = 
    let (randInt, seed') = next model.sourceSeed
        newUnit          = getUnit (randInt % length model.units) model.units
        locatedUnit      = moveToCenter model.grid newUnit
        spawnSuccess     = isUnitSafe model.grid locatedUnit
    in { model
       | unit       <- locatedUnit
       , sourceSeed <- seed'
       , isGameOver <- not spawnSuccess
       }

moveToCenter : Grid -> HexUnit -> HexUnit
moveToCenter grid unit = 
    let coords       = map cellToOffset unit.members
        xes          = map fst coords
        ys           = map snd coords
        minX         = withDefault 0 (minimum xes)
        maxX         = withDefault 0 (maximum xes)
        minY         = withDefault 0 (minimum ys)
        (curX, curY) = cellToOffset unit.location
        width        = getGridWidth grid
        newY         = curY - minY
        newMinX      = ((width - maxX) + minX) // 2
        newX         = curX - (minX - newMinX)
        cellOffset   = offsetToCell (newX, newY)
    in { members  = map (offsetBy cellOffset) unit.members
       , location = offsetBy cellOffset unit.location
       }