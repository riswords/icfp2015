module Hex where

import DataStructs exposing (..)
import List exposing (map, all, (::))
import Util exposing (get, set)
import Maybe exposing (withDefault)
import List exposing (head, drop, length, map, indexedMap)

------------------------------------------------------------------
-- General Cell introspection
filled : Hex -> Bool
filled f = 
  case f of
    Empty  -> False
    Filled -> True

------------------------------------------------------------------
-- Grid Cell Lookup
getXYCell : Int -> Int -> Grid -> Maybe Hex
getXYCell colIndex rowIndex grid =
    if (rowIndex < 0) || (colIndex < 0)
    then Nothing
    else get colIndex <| withDefault [] (get rowIndex grid)

getCell : Int -> Int -> Int -> Grid -> Maybe Hex
getCell x y z grid = 
  let (col, row) = cellToOffset (HexCell x y z)
  in  getXYCell col row grid

------------------------------------------------------------------
-- Grid Cell Update
setXYCell : Int -> Int -> Grid -> Hex -> Grid
setXYCell col row grid hexVal =
    let gridRow = withDefault [] (get row grid)
    in set row grid <| set col gridRow hexVal

setCell : Int -> Int -> Int -> Grid -> Hex -> Grid
setCell x y z grid hexVal =
    let (col, row) = cellToOffset (HexCell x y z)
    in  setXYCell col row grid hexVal

------------------------------------------------------------------
-- Grid Introspection  
inBounds : Grid -> Int -> Int -> Int -> Bool
inBounds grid x y z = case getCell x y z grid of
                        Nothing -> False
                        Just _  -> True

getGridHeight : Grid -> Int
getGridHeight = length

getGridWidth : Grid -> Int
getGridWidth = length << withDefault [] << head

getUnit : Int -> List HexUnit -> HexUnit
getUnit i ls = withDefault (HexUnit [] (HexCell 0 0 0)) (get i ls)

------------------------------------------------------------------
-- Coordinate and Offset tools for Cells and Units
-- All tuples are x/y (col/row)

cellToOffset : HexCell -> (Int, Int)
cellToOffset {x, y, z} = (x + ((z - (z % 2)) // 2), z)

offsetToCell : (Int, Int) -> HexCell
offsetToCell (col, row) = 
    let x = col - ((row - (row % 2)) // 2)
        z = row
        y = (-x) - z
    in HexCell x y z

offsetBy : HexCell -> HexCell -> HexCell
offsetBy offset {x,y,z} = HexCell (x + offset.x) (y + offset.y) (z + offset.z)

unitToRelativeUnit : HexUnit -> HexUnit
unitToRelativeUnit {members, location} =
  let x = location.x
      y = location.y
      z = location.z
  in HexUnit (map (offsetBy <| HexCell -x -y -z) members) 
             (HexCell x y z)

relativeUnitToAbs : HexUnit -> HexUnit
relativeUnitToAbs {members, location} = 
  HexUnit (map (offsetBy location) members) location

unitToCoordinates : HexUnit -> HexUnit
unitToCoordinates {members, location} = 
    { members = map cellToOffset members
    , location = cellToOffset location
    }

---------------------------------------------------------------------------------
-- Cell Rotation and Movement
rotateUnit : Grid -> Command -> HexUnit -> (Bool, HexUnit)
rotateUnit grid direction unit =
    let relUnit = unitToRelativeUnit unit
        newMembers   = map (rotateCell direction) relUnit.members
        absUnit      = relativeUnitToAbs {members = newMembers, location = unit.location}
        didRotate  = all (isCellSafe grid) <| absUnit.members
    in (didRotate, if didRotate then absUnit else unit)    

rotateCell : Command -> HexCell -> HexCell
rotateCell direction {x, y, z} =
  case direction of
    CW  -> {x = -z, y = -x, z = -y}
    CCW -> {x = -y, y = -z, z = -x}
    _   -> HexCell x y z

moveUnit : Grid -> Command -> HexUnit -> (Bool, HexUnit)
moveUnit grid direction unit =
    let newLoc     = moveCell direction unit.location
        newMembers = map (moveCell direction) unit.members
        didMove    = all (isCellSafe grid) <| newMembers
    in (didMove, if didMove then HexUnit newMembers newLoc else unit)

moveCell : Command -> HexCell -> HexCell
moveCell direction {x, y, z} =
  case direction of
     E  -> HexCell (x + 1) (y - 1) z
     W  -> HexCell (x - 1) (y + 1) z
     SE -> HexCell x       (y - 1) (z + 1)
     SW -> HexCell (x - 1)  y      (z + 1)
     _  -> HexCell x        y      z

isRotateCommand : Command -> Bool
isRotateCommand c =
  case c of
    CW  -> True
    CCW -> True
    _   -> False

---------------------------------------------------------------------------------
-- Cell Safety Checks
isCellSafe : Grid -> HexCell -> Bool
isCellSafe grid {x, y, z} = 
    case getCell x y z grid of
        Nothing     -> False
        Just Filled -> False
        Just Empty  -> True

isUnitSafe : Grid -> HexUnit -> Bool
isUnitSafe grid unit = all (isCellSafe grid) unit.members
