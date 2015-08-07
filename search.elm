module Search where

import Queue       exposing (..)
import DataStructs exposing (..)
import Update      exposing (update)
import List        exposing ((::), map, sortWith)

generateNextStates : HexModel -> List Command -> List (HexModel, List Command)
generateNextStates model history = 
  map (\ x -> (update x model, x :: history)) [E,W,SW,SE,CW,CCW]

breadthFirst : HexModel -> List Command
breadthFirst state = bfIter (push empty (state, [])) ([], 0)

bfIter : Queue (HexModel, List Command) -> (List Command, Int) -> List Command
bfIter queue curBest =
  if   isEmpty queue 
  then fst curBest
  else let (nq, popped) = pop queue
       in case popped of
            Nothing                  -> fst curBest
            (Just (model, commList)) ->
              if   model.isGameOver
              then bfIter nq curBest
              else let newStates = generateNextStates model commList
                       newScores : List (List Command, Int)
                       newScores = computeNewScores newStates
                   in  bfIter (enqueueAll nq newStates) (findBest curBest newScores)

findBest : (List Command, Int) -> List (List Command, Int) -> (List Command, Int)
findBest base ls = 
  foldl 
    (\ x res -> if (snd x) > (snd res) then x else res)
    base
    ls

computeNewScores : List (HexModel, List Command) -> List (List Command, Int)
computeNewScores updates = 
  let f : (HexModel, List Command) -> (List Command, Int)
      f m = case m of
              (model, history) -> (history, model.score)
  in map f updates
