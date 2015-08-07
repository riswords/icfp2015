module DataStructs where

type Hex 
    = Empty
    | Filled

type alias HexRow = List Hex

type alias Grid   = List (List Hex)

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
    = CW
    | CCW
    | E
    | W
    | SE
    | SW

type alias HexModel = 
    { id           : Int
    , units        : List HexUnit
    , unit         : HexUnit
    , isGameOver   : Bool
    , grid         : Grid
    , sourceLength : Int
    , sourceSeed   : Int
    , score        : Int
    }

type alias Output = 
    { id : Int
    , seed : Int
    , tag : String
    , solution : List Command
    }

type alias Input =
  { id           : Int
  , units        : List Unit
  , width        : Int
  , height       : Int
  , filled       : List Cell
  , sourceLength : Int
  , sourceSeeds  : List Int
  }

