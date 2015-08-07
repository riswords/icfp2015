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

inBounds : Int -> Int -> Int -> Grid -> Bool
inBounds x y z grid = case getCell x y z of
                        Nothing -> False
                        Just _ -> True