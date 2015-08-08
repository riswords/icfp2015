module Util where

import DataStructs exposing (..)
import Maybe exposing (withDefault)
import List exposing (head, drop, length, map, indexedMap)

filled : Hex -> Bool
filled f = 
  case f of
    Empty  ->False
    Filled -> True

get : Int -> List a -> Maybe a
get i ls = 
    drop i ls
    |> head
    |> Just
    |> withDefault Nothing

getUnit : Int -> List HexUnit -> HexUnit
getUnit i ls = withDefault (HexUnit [] (HexCell 0 0 0)) (get i ls)

getCell : Int -> Int -> Int -> Grid -> Maybe Hex
getCell x y z grid = 
    let rowIndex = z
        colIndex = x + ((z - (z % 2)) // 2)
        row = withDefault [] (get rowIndex grid)
    in get colIndex row
        |> Just
        |> withDefault Nothing

setCell : Int -> Int -> Int -> Grid -> Hex -> Grid
setCell x y z grid hexVal =
    let (col, row) = cellToOffset (HexCell x y z)
        gridRow = withDefault [] (get row grid)
        updatedRow = set col gridRow hexVal
    in set row grid updatedRow

set : Int -> List a -> a -> List a
set index ls newVal = 
    indexedMap (\i v -> if index == i
                        then newVal
                        else v)
               ls

inBounds : Grid -> Int -> Int -> Int -> Bool
inBounds grid x y z = case getCell x y z grid of
                        Nothing -> False
                        Just _ -> True


getGridHeight : Grid -> Int
getGridHeight = length

getGridWidth : Grid -> Int
getGridWidth = length << withDefault [] << head

                           -- X, Y (col, row)
cellToOffset : HexCell -> (Int, Int)
cellToOffset {x, y, z} = (x + ((z - (z % 2)) // 2), z)

unitToCoordinates : HexUnit -> List (Int, Int)
unitToCoordinates {members, location} = 
  let (colOff, rowOff) = cellToOffset location
  in  map
        (\ m ->  
          let (c, r) = cellToOffset m
          in  (c + colOff, r + rowOff))
        members

offsetToCell : (Int, Int) -> HexCell
offsetToCell (col, row) = 
    let x = col - ((row - (row % 2)) // 2)
        z = row
        y = (-x) - z
    in HexCell x y z