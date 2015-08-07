module Init where

import DataStructs exposing (..)
import Util        exposing (..)
import List        exposing (repeat, map, take, drop, foldr, (::), append, head, tail)

initGrid : Int -> Int -> Grid
initGrid width height = repeat height (repeat width Empty)

-- TODO: FIX ME
convertUnit : Unit -> HexUnit
convertUnit {members, pivot} = {members = [], location = {x = 0, y = 0, z = 0}}

fillCell : Cell -> Grid -> Grid
fillCell {x, y} g = 
  let l1          = take y g
      (row::rest) = drop y g
      rowPrefix   = take x row
      rowSuffix   = drop (x+1) row
  in append l1 <| (append rowPrefix (Filled :: rowSuffix)) :: rest

initGameState : Input -> List HexModel
initGameState {id, units, width, height, filled, sourceLength, sourceSeeds} =
  map 
    (\ s -> 
      { id           = id
      , units        = map convertUnit units
      , grid         = foldr fillCell (initGrid width height) filled
      , sourceLength = sourceLength
      , sourceSeed   = s
      })
    sourceSeeds
