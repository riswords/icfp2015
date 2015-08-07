module Update where

import DataStructs exposing (..)
import List        exposing (all, repeat)

newEmptyRow : Int -> List Hex 
newEmptyRow width = repeat width Empty

filled : Hex -> Bool
filled f = 
  case f of
    Empty      ->False
    (Filled _) -> True

boardUpdate : Int -> Grid -> Grid
boardUpdate width = 
  let rif    ls  = if all filled ls then newEmptyRow width else ls
      rclear row = case row of
                     (LeftRow  ls) -> LeftRow  <| rif ls
                     (RightRow ls) -> RightRow <| rif ls
  in List.map rclear                     

