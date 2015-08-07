module Update where

import DataStructs exposing (..)
import List        exposing (all, repeat)
import Util        exposing (..)


boardUpdate : Int -> Grid -> Grid
boardUpdate width = 
  let rif    ls  = if all filled ls then repeat width Empty else ls
  in List.map rif                     

