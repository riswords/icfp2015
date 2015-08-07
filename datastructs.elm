module DataStructs where

type Hex 
    = Empty
    | Filled Cell

type HexRow 
    = LeftRow  (List Hex)
    | RightRow (List Hex)

type alias Grid  = List HexRow

type alias Cell = 
    { x : Int
    , y : Int
    }

type alias HexCell = 
    { x : Int
    , y : Int
    , z : Int
    }

type alias Unit = 
    { members :  List Cell
    , pivot : Cell
    }

type alias HexUnit =
    { members : List HexCell
    , location : HexCell
    }

type Command 
    = E  Char
    | W  Char
    | SE Char
    | SW Char
    | CW Char
    | CCW Char

type alias HexModel = 
    { id : Int
    , units : List Unit
    , grid : Grid
    , sourceLength : Int
    , sourceSeed : Int
    }

type alias Output = 
    { id : Int
    , seed : Int
    , tag : String
    , solution : List Command
    }

type alias Input =
  { id           : Int
  , unit         : List Unit
  , width        : Int
  , height       : Int
  , filled       : List Cell
  , sourceLength : Int
  , sourceCeeds  : List Int
  }

