module ElmMain where

import Signal
import Signal exposing ((<~), Signal)

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
import Debug  exposing (watch)

import Viewer exposing (viewer)

-- port output : Signal String
-- port output = Signal.map toString (Time.every Time.second)

-- Nasty hack to make the compiler happy
main : Signal Element
main = looper ()

showTest : () -> Element
showTest = \ () -> 
  let init                     = withDefault emptyModel <| head (initGameState (fromJson test1))
  in viewer (init, [], [])

looper : () -> Signal Element
looper = \ () ->
  let init = withDefault emptyModel <| head (initGameState (fromJson test4))
  in  Signal.map viewer  <| Signal.foldp
                             (\ i (m, commands, s) ->
                               if m.isGameOver
                               then (m, [], s)
                               else case commands of
                                      []      -> let newCmds   = pickNextMove m
                                                     nextCmds  = (List.filter ((/=)P) newCmds)
                                                 in (m, nextCmds, [(heuristic m)])
                                      (c::cs) -> (update c m, cs, [(heuristic m)]))
                             (init, [], []) 
                             (Time.fps 30)

looper2 : () -> Signal Element
looper2 = \ () ->
  let init                     = withDefault emptyModel <| head (initGameState (fromJson test1))
      (samplePlayer, avgScore) = Search.hatchDecentPlayer init 50
      commands                 = List.reverse <| (.history samplePlayer.model)
  in  Signal.map viewer  <| Signal.foldp
                             (\ i (m, commands, avg) ->
                               case commands of
                                 []      -> (m, commands, avg)
                                 (c::cs) -> (update c m, cs, avg))
                             (init, commands, []) 
                             (Time.fps 200)

