module IO where

import DataStructs exposing (..)
import Json.Encode exposing (encode, object)
import Json.Decode exposing ((:=), decodeString, object2, object7)
import String      exposing (toList, fromList)

-- type Command 
extractOp : Command -> Char
extractOp op = 
  case op of
    (W   x) -> x 
    (E   x) -> x 
    (SE  x) -> x
    (SW  x) -> x
    (CW  x) -> x
    (CCW x) -> x

toJson : Output -> String
toJson {id, seed, tag, solution} =
  let int = Json.Encode.int
      str = Json.Encode.string
  in       
    encode 0 <| object [ ("problemId" , int id)
                       , ("seed",       int seed)
                       , ("tag",        str tag)
                       , ("solution",   List.map extractOp solution
                                         |> fromList
                                         |> str )
                       ]

fromJson : String -> Result String Input
fromJson =
  let int        = Json.Decode.int
      str        = Json.Decode.string
      ls         = Json.Decode.list
      cellParser = object2 Cell ("x" := int) ("y" := int)
      unitParser = object2 Unit ("members" := (ls cellParser))
                                ("pivot"   := cellParser)
  in
    decodeString <|
      object7 Input
        ("id"           := int)
        ("units"        := (ls unitParser))
        ("width"        := int)
        ("height"       := int)
        ("filled"       := (ls cellParser))
        ("sourceLength" := int)
        ("sourceSeeds"  := (ls int))


test1 = """
{"height":10,"width":10,"sourceSeeds":[0],"units":[{"members":[{"x":0,"y":0}],"pivot":{"x":0,"y":0}},{"members":[{"x":0,"y":0},{"x":2,"y":0}],"pivot":{"x":1,"y":0}},{"members":[{"x":0,"y":0},{"x":0,"y":2}],"pivot":{"x":0,"y":1}},{"members":[{"x":2,"y":0},{"x":0,"y":1},{"x":2,"y":2}],"pivot":{"x":1,"y":1}},{"members":[{"x":0,"y":0},{"x":1,"y":1},{"x":0,"y":2}],"pivot":{"x":0,"y":1}},{"members":[{"x":0,"y":0},{"x":1,"y":0}],"pivot":{"x":0,"y":0}},{"members":[{"x":0,"y":0},{"x":1,"y":0}],"pivot":{"x":1,"y":0}},{"members":[{"x":0,"y":0},{"x":0,"y":1}],"pivot":{"x":0,"y":0}},{"members":[{"x":0,"y":0},{"x":0,"y":1}],"pivot":{"x":0,"y":1}},{"members":[{"x":0,"y":0},{"x":1,"y":0},{"x":2,"y":0}],"pivot":{"x":0,"y":0}},{"members":[{"x":0,"y":0},{"x":1,"y":0},{"x":2,"y":0}],"pivot":{"x":1,"y":0}},{"members":[{"x":0,"y":0},{"x":1,"y":0},{"x":2,"y":0}],"pivot":{"x":2,"y":0}},{"members":[{"x":0,"y":0},{"x":0,"y":1},{"x":0,"y":2}],"pivot":{"x":0,"y":0}},{"members":[{"x":0,"y":0},{"x":0,"y":1},{"x":0,"y":2}],"pivot":{"x":0,"y":1}},{"members":[{"x":0,"y":0},{"x":0,"y":1},{"x":0,"y":2}],"pivot":{"x":0,"y":2}},{"members":[{"x":1,"y":0},{"x":0,"y":1},{"x":1,"y":2}],"pivot":{"x":1,"y":0}},{"members":[{"x":1,"y":0},{"x":0,"y":1},{"x":1,"y":2}],"pivot":{"x":1,"y":1}},{"members":[{"x":1,"y":0},{"x":0,"y":1},{"x":1,"y":2}],"pivot":{"x":1,"y":2}}],"id":0,"filled":[],"sourceLength":100}
"""
