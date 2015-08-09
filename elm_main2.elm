module ElmMain where

import Signal
import Signal exposing ((<~), Signal, Address)

import IO          exposing (..)
import Tests       exposing (..)
import Init        exposing (..)
import UnitSearch  exposing (..)
import Search
import List        exposing (map, take, head, (::))
import Array       exposing (get)
import Queue       exposing (push, empty, Queue, peek)
import DataStructs exposing (..)
import Maybe       exposing (..)
import Update      exposing (update)
import Graphics.Element exposing (Element)

import Time
import Task   exposing (Task)
import Debug  exposing (watch)

import Viewer2 exposing (viewer)

-- port output : Signal String
-- port output = Signal.map toString (Time.every Time.second)

-- Nasty hack to make the compiler happy
main : Signal Element
main = looper ()

box : Signal.Mailbox Action
box = Signal.mailbox Nop 

looper : () -> Signal Element
looper = \ () ->
  let init = withDefault emptyModel <| head (initGameState (fromJson test4))
  in  Signal.map (viewer box.address)  <| 
        Signal.foldp
          (\ (action, i) state ->
            case action of
              Init model  -> RunningGame model []
              Nop         -> updateGame state)
          (RunningGame init [])
          (Signal.map2 (,) box.signal (Time.fps 3))

updateGame : GameState -> GameState
updateGame state =
  case state of
    GameOver m               -> state
    ComputingMove m          -> let newCmds   = pickNextMove m
                                    nextCmds  = List.filter ((/=)P) newCmds
                                in RunningGame m nextCmds
    (RunningGame m commands) -> 
      if m.isGameOver
      then GameOver m
      else case commands of
             []      -> ComputingMove m
             (c::cs) -> RunningGame   (update c m) cs

