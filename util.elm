module Util where

import DataStructs exposing (..)
import Maybe exposing (withDefault)
import List exposing (head, drop, length, map)

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

inBounds : Grid -> Int -> Int -> Int -> Bool
inBounds grid x y z = case getCell x y z grid of
                        Nothing -> False
                        Just _ -> True


getGridHeight : Grid -> Int
getGridHeight = length

getGridWidth : Grid -> Int
getGridWidth = length << withDefault [] << head

                           -- X, Y (row, column)
cellToOffset : HexCell -> (Int, Int)
cellToOffset {x, y, z} = (z, x + ((z - (z % 2)) // 2))

unitToCoordinates : HexUnit -> List (Int, Int)
unitToCoordinates {members, location} = 
  let (rowOff, colOff) = cellToOffset location
  in  map
        (\ m ->  
          let (r, c) = cellToOffset m
          in  (r + rowOff, c + colOff)) 
        members

offsetToCell : (Int, Int) -> HexCell
offsetToCell (col, row) = 
    let x = col - ((row - (row % 2)) // 2)
        z = row
        y = (-x) - z
    in HexCell x y z