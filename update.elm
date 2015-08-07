module Update where

import DataStructs exposing (..)
import List        exposing (all, repeat)
import Util        exposing (..)
import Hex         exposing (rotateUnit, moveUnit)

clearRows : Int -> Grid -> Grid
clearRows width = 
  let rif    ls  = if all filled ls then repeat width Empty else ls
  in List.map rif                     

updateUnit : Grid -> Command -> HexUnit -> (Bool, HexUnit)
updateUnit grid command =
  if   isRotateCommand command 
  then rotateUnit grid command
  else moveUnit   grid command

-- update : Command -> Model -> (Bool, Model)
-- Performs the requested command
-- clears lines
-- update the score
