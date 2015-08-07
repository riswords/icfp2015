module Search where

import Queue       exposing (..)
import Set         exposing (..)
import DataStructs exposing (..)
import Update      exposing (update)
import List        exposing ((::), map)

generateNextStates : HexModel -> List Command -> List (HexModel, List Command)
generateNextStates model history = 
  map (\ x -> (update x model, x :: history)) [E,W,SW,SE,CW,CCW]

breadthFirst : HexModel -> List Command
breadthFirst state = bfIter (push empty (state, [])) ([], 0)

bfIter : Queue (HexModel, List Command) -> (List Command, Int) -> List Command
bfIter queue curBest =
  if   isEmpty queue 
  then curBest
  else let (nq, popped) = pop queue
       in case popped of
            Nothing                  -> curBest
            (Just (model, commList)) ->
              if   model.isGameOver
              then bfIter nq curBest
              else let newStates = generateNextStates model
                       newScores = computeNewScores newStates commList
                   in  bfIter (enqueueAll nq newStates) (sortWith sorter <| curBest :: newScores)

sorter (ca, sa) (cb, sb) = 
  case compare sa sb of
    LT -> GT
    EQ -> EQ
    GT -> LT

computeNewScores : List (HexModel, Command) -> List Command -> List (List Command, Int)
computeNewScores updates history = map (\ (model, cmd) -> (cmd :: history, model.score)) updates
