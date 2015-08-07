module Util where

import DataStructs exposing (..)
import Maybe exposing (withDefault)
import List exposing (head, drop)

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