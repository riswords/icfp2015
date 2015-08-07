module Update where

import DataStructs exposing (..)
import List        exposing (all, repeat, map, length, sum, foldr)
import Util        exposing (..)
import Hex         exposing (rotateUnit, moveUnit, isRotateCommand)
import Rand        exposing (next)

-- Model -> (Model, # of Cleared Lines)
clearRows : HexModel -> (HexModel, Int)
clearRows model = 
    let clearOneRow : List Hex -> (Int, List Hex)
        clearOneRow row = if all filled row
                          then (1, repeat model.width Empty)
                          else (0, row)
        rows = map clearOneRow model.grid
    in ( { model | grid <- applyGravity <| map snd rows }
       , sum <| map fst rows 
       )

updateScore : HexModel -> Int -> HexUnit -> HexModel
updateScore model ls unit = 
  let size       = length unit.members
      lsOld      = model.prevLines
      points     = size + 100 * (1 + ls) * ls / 2
      lineBonus = if   lsOld > 1 
                  then floor ((lsOld - 1) * points / 10)
                  else 0
  in { model | score     <- model.score + points + lineBonus 
             , prevLines <- ls }

applyGravity : Grid -> Grid
applyGravity = 
  foldr  
    (\ x ls -> 
       case ls of
         []      -> [x]
         (y::ys) -> if   all (not << filled) y
                    then x::ys
                    else x::y::ys)
    []

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
  let (isPlaced, updUnit) = updateUnit model.grid move model.unit
  in if isPlaced
     then let (newModel, lineClear) = clearRows model 
          in  updateScore newModel lineClear updUnit |> spawnNewUnit
     else { model | unit <- updUnit }


-- duplicated logic in init.elm, keep in sync
spawnNewUnit : HexModel -> HexModel
spawnNewUnit model = 
    let (randInt, seed') = next model.sourceSeed
        unit = getUnit (randInt % model.sourceLength) model.units
    in { model
        | unit <- unit
        , sourceSeed <- seed'
        }
