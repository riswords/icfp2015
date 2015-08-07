module Hex where

type Hex = Empty
         | Filled

type HexRow = LeftRow  (List Hex)
            | RightRow (List Hex)

