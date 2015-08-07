module Search where

import Queue       exposing (..)
import DataStructs exposing (..)
import Update      exposing (update)
import List        exposing ((::), map, sortWith, foldl, any, take, filter)
import Trampoline  exposing (..)
import Debug       exposing (watch)

generateNextStates : HexModel -> List Command -> List (HexModel, List Command)
generateNextStates model history = 
  map (\ x -> (update x model, x :: history)) <| 
    filter (\ x -> not <| isCycle x history) [E,W,SW,SE,CW,CCW]

isCycle : Command -> List Command -> Bool
isCycle command history =
  case command of
    E   -> went W 10 history 
    W   -> went E 10 history
    CW  -> went CCW 10 history 
    CCW -> went CW 10 history
    _   -> False

went : a -> Int -> List a -> Bool
went a n = any (\ x -> x == a) << take n 

type Running a = Done a
               | More a

bfStep : Queue (HexModel, List Command) -> 
         (List Command, Int) -> 
         Running (Queue (HexModel, List Command), (List Command, Int))
bfStep queue curBest =
  if   isEmpty queue 
  then Done (queue, curBest)
  else let (nq, popped) = pop queue
       in case popped of
            Nothing                  -> Done (nq, curBest)
            (Just (model, commList)) ->
              if   model.isGameOver
              then More (nq, curBest)
              else let newStates = generateNextStates model commList
                       newScores : List (List Command, Int)
                       newScores = computeNewScores newStates
                   in  More ((enqueueAll nq newStates), (findBest curBest newScores))

-- breadthFirst : HexModel -> List Command
-- breadthFirst state = 
--   trampoline (bfIter (watch "queue" (push empty (state, []))) ([], 0))

-- 
-- bfIter : Queue (HexModel, List Command) -> (List Command, Int) -> Trampoline (List Command)
-- bfIter queue curBest =
--   if   isEmpty queue 
--   then Done <| fst curBest
--   else let (nq, popped) = pop queue
--        in case popped of
--             Nothing                  -> Done <| fst curBest
--             (Just (model, commList)) ->
--               if   model.isGameOver
--               then bfIter nq curBest
--               else let newStates = generateNextStates model commList
--                        newScores : List (List Command, Int)
--                        newScores = computeNewScores newStates
--                    in  Continue (\ () -> bfIter (enqueueAll nq newStates) (findBest curBest newScores))

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
