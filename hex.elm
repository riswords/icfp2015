module Hex where

import DataStructs exposing (..)
import List exposing (map)

rotateUnitCW : HexUnit -> HexUnit
rotateUnitCW { members, location } = 
    { members = map rotateCellCW members
    , location = location 
    }

rotateCellCW : HexCell -> HexCell
rotateCellCW { x, y, z } = { x = -z, y = -x, z = -y }

rotateUnitCCW : HexUnit -> HexUnit
rotateUnitCCW { members, location } =
    { members = map rotateCellCCW members
    , location = location 
    }

rotateCellCCW : HexCell -> HexCell
rotateCellCCW { x, y, z } = { x = -y, y = -z, z = -x }
