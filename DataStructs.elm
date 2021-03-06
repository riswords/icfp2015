module DataStructs where

import Random      exposing (int, Generator, Seed, initialSeed)
import Array       exposing (Array)
import Trampoline  exposing (..)

----------------------------------------------
type       Hex    = Empty | Filled 
type alias HexRow = Array Hex
type alias Grid   = Array (Array Hex)

----------------------------------------------
type alias Cell = 
    { x : Int
    , y : Int
    }

type alias HexCell = 
    { x : Int
    , y : Int
    , z : Int
    }

----------------------------------------------
type alias Unit = 
    { members :  List Cell
    , pivot : Cell
    }

type alias HexUnit =
    { members : List HexCell
    , location : HexCell
    }

----------------------------------------------
type Command = CW | CCW | E | W | SE | SW | P

----------------------------------------------
type alias HexModel = 
    { id           : Int
    , units        : List HexUnit
    , unit         : HexUnit
    , isGameOver   : Bool
    , grid         : Grid
    , sourceLength : Int
    , sourceSeed   : Int
    , score        : Int
    , prevLines    : Int
    , history      : List Command
    , width        : Int
    , height       : Int
    , originalSeed : Int
    }

----------------------------------------------
type alias Output = 
    { id       : Int
    , seed     : Int
    , tag      : String
    , solution : String
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

----------------------------------------------

type alias InputInfo =
     { powerWords : List String
     , timeLimit  : Float 
     , startTime  : Float 
     , lastTime   : Float 
     }

type GameState
     = GameOver      InputInfo HexModel
     | ComputingMove InputInfo HexModel (Trampoline (List Command))
     | RunningGame   InputInfo HexModel (List Command)

type Action 
     = Init HexModel
     | Nop
     | TimeLimit Int
