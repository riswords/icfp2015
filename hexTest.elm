module HexTest where

import Hex exposing (..)
import DataStructs exposing (..)
import Init exposing (initGrid)

emptyGrid : Grid
emptyGrid = initGrid 1 1

singleUnit : HexUnit
singleUnit = {
    members = [ HexCell 0 0 0 ]
    , location = HexCell 0 0 0
    }

testUnitIsSafeOnEmptyGrid : Bool
testUnitIsSafeOnEmptyGrid = isUnitSafe emptyGrid singleUnit