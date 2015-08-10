module IO where

import DataStructs exposing (..)
import List exposing (reverse, filter)
import Json.Encode exposing (encode, object)
import Json.Decode exposing ((:=), decodeString, object2, object7)
import String      exposing (toList, fromList)
import PowerWords  exposing (commandsToPowerWords)


generateOutput : HexModel -> Output
generateOutput model = 
  let commands = reverse (filter ((/=) P) model.history)
  in { id = model.id
     , seed = model.originalSeed
     , tag = "hacknslash"
     , solution = commandsToPowerWords commands model.powerWords
     }

toJson : Output -> String
toJson {id, seed, tag, solution} =
  let int = Json.Encode.int
      str = Json.Encode.string
  in       
    encode 0 <| object [ ("problemId", int id)
                       , ("seed",      int seed)
                       , ("tag",       str tag)
                       , ("solution",  solution)
                       ]

fromJson : String -> Input
fromJson s =
  let int        = Json.Decode.int
      str        = Json.Decode.string
      ls         = Json.Decode.list
      cellParser = object2 Cell ("x" := int) ("y" := int)
      unitParser = object2 Unit ("members" := (ls cellParser))
                                ("pivot"   := cellParser)
      dec = decodeString <|
              object7 Input
                ("id"           := int)
                ("units"        := (ls unitParser))
                ("width"        := int)
                ("height"       := int)
                ("filled"       := (ls cellParser))
                ("sourceLength" := int)
                ("sourceSeeds"  := (ls int))
  in case dec s of -- And now I guess we strip out the result type...
       (Ok v)    -> v
       (Err err) -> {id = 0 , units = [], width = 1, height = 1, filled = [], sourceLength = 0, sourceSeeds = []} 
