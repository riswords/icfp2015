module ElmMain where

import Signal
import Signal exposing ((<~), Signal, Address)

import Init        exposing (..)
import Tests       exposing (test0, emptyModel)
import DataStructs exposing (..)
import Engine      exposing (..)
import Graphics.Element exposing (Element)

import Time

import Viewer exposing (viewer)

-- Once upon a time, this was going to be for CLI
-- port output : Signal String
-- port output = Signal.map toString (Time.every Time.second)

main : Signal Element
main = looper emptyModel

box : Signal.Mailbox Action
box = Signal.mailbox <| Init <| setupGame test0 

initialInfo = {powerWords = [], timeLimit = 1000.0, startTime = 0.0, lastTime = 0.0}

looper : HexModel -> Signal Element
looper init = 
  Signal.map (viewer box.address)  <| 
    Signal.foldp
      (\ (action, time) state ->
        case action of
          Init model  -> RunningGame 
                           { initialInfo | startTime <- Time.inSeconds time }
                           model 
                           []
          TimeLimit n -> setTimeLimit n state
          Nop         -> updateGame <| updateTime time state)
      (RunningGame initialInfo init [])
      (Signal.map2 (,) box.signal (Time.every 3))

