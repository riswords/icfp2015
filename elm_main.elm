module ElmMain where

import Signal
import Signal exposing ((<~), Signal, Address)

import IO          exposing (..)
import Tests       exposing (..)
import Init        exposing (..)
import Search      exposing (..)
import List        exposing (map, take, head, (::))
import Array       exposing (get)
import Queue       exposing (push, empty, Queue, peek)
import DataStructs exposing (..)
import Maybe       exposing (..)
import Update      exposing (update)
import Util        exposing (bounce, withDoneValue, isDone)
import Graphics.Element exposing (Element)

import Time
import Task   exposing (Task)
import Debug  exposing (watch)

import Viewer exposing (viewer)

-- Once upon a time, this was going to be for CLI
-- port output : Signal String
-- port output = Signal.map toString (Time.every Time.second)

main : Signal Element
main = looper emptyModel

box : Signal.Mailbox Action
box = Signal.mailbox <| Init <| setupGame test0 

initialInfo = {powerWords = [], time = 100}

looper : HexModel -> Signal Element
looper init = 
  Signal.map (viewer box.address)  <| 
    Signal.foldp
      (\ (action, i) state ->
        case action of
          Init model  -> RunningGame initialInfo model []
          TimeLimit n -> state
          Nop         -> updateGame state)
      (RunningGame initialInfo init [])
      (Signal.map2 (,) box.signal (Time.fps 3))

updateGame : GameState -> GameState
updateGame state =
  case state of
    GameOver i m           -> state
    ComputingMove i m tram -> if isDone tram
                              then let newCmds = withDoneValue [] tram
                                       nextCmds  = List.filter ((/=)P) newCmds
                                   in RunningGame i m nextCmds
                              else ComputingMove i m (bounce tram)     
    RunningGame i m cmds   -> 
      if m.isGameOver
      then GameOver i m
      else case cmds of
             []      -> ComputingMove i m (bouncePickNextMove m)
             (c::cs) -> RunningGame i (update c m) cs

