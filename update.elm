module Update where

import DataStructs exposing (..)
import List        exposing (all, repeat, map)
import Util        exposing (..)
import Hex         exposing (rotateUnit, moveUnit)


clearRows : HexModel -> HexModel
clearRows model = 
    let clearOneRow : List Hex -> (Int, List Hex)
        clearOneRow row = if all filled row
                            then (1, repeat width Empty)
                            else (0, row)

        scoredRows : List (Int, List Hex)
        scoredRows = map clearOneRow model.grid

        rowScore = sum (map fst scoredRows)
    in { model 
            | grid <- applyGravity (map snd scoredRows)
            , score <- score + rowScore
            }

applyGravity : Grid -> Grid
applyGravity grid = grid


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
     then model |> clearRows |> updateScore |> spawnNewUnit
     else { model | unit <- updUnit }

