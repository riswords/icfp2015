module ElmMain where

import Signal
import Signal exposing ((<~), Signal)

import IO          exposing (..)
import Tests       exposing (..)
import Init        exposing (..)
import Search      exposing (..)
import List        exposing (map, take, head)
import Queue       exposing (push, empty, Queue, peek)
import DataStructs exposing (..)
import Maybe       exposing (..)

import Time
import Debug  exposing (watch)

import Text
import Graphics.Element exposing (Element, show)

-- port output : Signal String
-- port output = Signal.map toString (Time.every Time.second)

-- Nasty hack to make the compiler happy
main : Element
main = show <| isCycle E [E,E,E,CW]



looper : () -> Signal Element
looper = \ () ->
  let init = withDefault emptyModel <| head (initGameState (fromJson test0))
      initModel : Running (Queue (HexModel, List Command), (List Command, Int)) 
      initModel = More ((push empty (init, [])),([], 0))
  in  Signal.map viewer  <| Signal.foldp 
                           (\ i m -> case m of
                                       Done _            -> m
                                       More (queue,best) -> bfStep queue best)
                           initModel
                           (Time.fps 1)

viewer : Running (Queue (HexModel, List Command), (List Command, Int)) -> Element
viewer m =
  case m of
    (More (queue, best)) -> hexView (peek queue)
    (Done (queue, best)) -> hexView (peek queue) 

hexView : Maybe (HexModel, List Command) -> Element
hexView = show

