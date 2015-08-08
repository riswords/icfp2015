module Util where

import DataStructs exposing (..)
import List        exposing (head, drop, indexedMap)
import Color       exposing (rgba, Color)

------------------------------------------------------------------
-- Some list helpers
get : Int -> List a -> Maybe a
get i ls = head <| drop i ls

set : Int -> List a -> a -> List a
set index ls newVal = 
    indexedMap (\i v -> if index == i
                        then newVal
                        else v)
               ls

------------------------------------------------------------------
-- A few colors
clearGrey : Color -- Empty Hexes
clearGrey = rgba 160 160 160 1.0
yeller : Color -- Filled hexes
yeller = rgba 230 184 0 1.0
uniter : Color -- Unit hexes
uniter = rgba 46 184 230 1.0
unitCenter : Color -- Center dot
unitCenter = rgba 25 117 209 1.0
