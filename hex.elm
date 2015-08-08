module Hex where

import DataStructs exposing (..)
import List exposing (map, all)
import Util exposing (getCell)

rotateUnit : Grid -> Command -> HexUnit -> (Bool, HexUnit)
rotateUnit grid direction unit = 
    let newMembers : List (Bool, HexCell)
        newMembers = case direction of
                        CW  -> map (rotateCellCW grid) unit.members
                        CCW -> map (rotateCellCCW grid) unit.members
                        _   -> map (\c -> (False, c)) unit.members

        didRotate = all ((==) True) (map fst newMembers)
    in (didRotate, { unit | members <- map snd newMembers })

rotateCellCW : Grid -> HexCell -> (Bool, HexCell)
rotateCellCW grid { x, y, z } = 
    let newLoc = HexCell -z -x -y
        isSafe = isCellSafe grid newLoc
    in if isSafe
            then (isSafe, newLoc)
            else (isSafe, HexCell x y z)

rotateCellCCW : Grid -> HexCell -> (Bool, HexCell)
rotateCellCCW grid { x, y, z } = 
    let newLoc = HexCell -y -z -x
        isSafe = isCellSafe grid newLoc
    in if isSafe
            then (isSafe, newLoc)
            else (isSafe, HexCell x y z)

isCellSafe : Grid -> HexCell -> Bool
isCellSafe grid {x, y, z} = 
    case getCell x y z grid of
        Nothing -> False
        Just Filled -> False
        Just Empty -> True

moveUnit : Grid -> Command -> HexUnit -> (Bool, HexUnit)
moveUnit grid direction unit =
    let newMembers : List (Bool, HexCell)
        newMembers = map (moveCell grid direction) unit.members

        newLoc : HexCell
        newLoc = snd (moveCell grid direction unit.location)

        didMove = all ((==) True) (map fst newMembers)
    in if didMove
        then (didMove, { unit 
                        | members <- map snd newMembers
                        , location <- newLoc
                        })
        else (didMove, unit)

moveCell : Grid -> Command -> HexCell -> (Bool, HexCell)
moveCell grid direction {x, y, z} =
    let newLoc : HexCell
        newLoc = case direction of
                    E  -> HexCell (x + 1) (y - 1) z
                    W  -> HexCell (x - 1) (y + 1) z
                    SE -> HexCell x (y - 1) (z + 1)
                    SW -> HexCell (x - 1) y (z + 1)
                    _  -> HexCell x y z
        isSafe = isCellSafe grid newLoc
    in if isSafe
            then (isSafe, newLoc)
            else (isSafe, HexCell x y z)

isRotateCommand : Command -> Bool
isRotateCommand c =
  case c of
    CW  -> True
    CCW -> True
    _   -> False


isUnitSafe : Grid -> HexUnit -> Bool
isUnitSafe grid unit = 
    let offset = unit.location
        isCellReallySafe cell = HexCell (cell.x + offset.x) 
                                        (cell.y + offset.y)
                                        (cell.z + offset.z)
                                    |> isCellSafe grid
    in all ((==) True) (map isCellReallySafe unit.members)