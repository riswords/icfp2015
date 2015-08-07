module Update where

import DataStructs exposing (..)
import List        exposing (all, repeat)
import Util        exposing (..)
import Hex         exposing (rotateUnit, moveUnit)

clearRows : Int -> Grid -> Grid
clearRows width = 
  let rif    ls  = if all filled ls then repeat width Empty else ls
  in List.map rif                     

-- put updateUnit here, too

-- update : Command -> Model -> (Bool, Model)
-- Performs the requested command
-- clears lines
-- update the score
