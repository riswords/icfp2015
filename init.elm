module Init where

import DataStructs exposing (..)
import Util        exposing (..)
import List        exposing (repeat, map)

initGrid : Int -> Int -> Grid
initGrid width height = repeat height (repeat width Empty)

-- TODO: FIX ME
convertUnit : Unit -> HexUnit
convertUnit {members, pivot} = {members = [], location = {x = 0, y = 0, z = 0}}

initGameState : Input -> List HexModel
initGameState {id, units, width, height, filled, sourceLength, sourceSeeds} =
  map 
    (\ s -> 
      { id           = id
      , units        = map convertUnit units
      , grid         = initGrid width height
      , sourceLength = sourceLength
      , sourceSeed   = s
      })
    sourceSeeds
