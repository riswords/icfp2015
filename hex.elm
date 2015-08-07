module Hex where

import DataStructs exposing (..)
import List exposing (map)

rotateUnit : Command -> HexUnit -> HexUnit
rotateUnit direction unit = 
    let newMembers = case direction of
                        CW  -> map rotateCellCW unit.members
                        CCW -> map rotateCellCCW unit.members
                        _   -> unit.members
    in { unit | members <- newMembers }

rotateCellCW : HexCell -> HexCell
rotateCellCW { x, y, z } = { x = -z, y = -x, z = -y }

rotateCellCCW : HexCell -> HexCell
rotateCellCCW { x, y, z } = { x = -y, y = -z, z = -x }


moveUnit : Command -> HexUnit -> HexUnit
moveUnit direction unit = 
    let newMembers = map (moveCell direction) unit.members
        newLocation = moveCell direction unit.location
    in { unit 
       | members <- newMembers
       , location <- newLocation
       }

moveCell : Command -> HexCell -> HexCell
moveCell direction {x, y, z} = 
    case direction of
        E  -> { x = x + 1, y = y - 1, z = z }
        W  -> { x = x - 1, y = y + 1, z = z }
        SE -> { x = x, y = y - 1, z = z + 1 }
        SW -> { x = x - 1, y = y, z = z + 1 }
        _  -> { x = x, y = y, z = z}


isRotateCommand : Command -> Bool
isRotateCommand c =
  case c of
    CW  -> True
    CCW -> True
    _   -> False

updateUnit : Command -> HexUnit -> HexUnit
updateUnit command =
  if   isRotateCommand command 
  then rotateUnit command 
  else moveUnit   command 
