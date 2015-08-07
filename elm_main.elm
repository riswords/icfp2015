module ElmMain where

import Signal
import Signal exposing ((<~), Signal)

import IO     exposing (..)
import Tests  exposing (..)
import Init   exposing (..)
import Search exposing (..)
import List   exposing (map, take, head)
import Queue  exposing (push, empty)
import Maybe  exposing (..)

import Time
import Debug  exposing (watch)

import Text
import Graphics.Element exposing (Element, show)

-- port output : Signal String
-- port output = Signal.map toString (Time.every Time.second)

-- Nasty hack to make the compiler happy
main : Signal Element
main = 
  let init = withDefault emptyModel <| head (initGameState (fromJson test0))
  in  Signal.map show  <| Signal.foldp 
                           (\ i m -> case m of
                                       Done _            -> m
                                       More (queue,best) -> bfStep queue best)
                           (More ((watch "queue" (push empty (init, []))),([], 0)))
                           (Time.fps 1)


