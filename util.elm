module Util where

import DataStructs exposing (..)

filled : Hex -> Bool
filled f = 
  case f of
    Empty  ->False
    Filled -> True
