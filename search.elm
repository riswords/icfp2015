module Search where

import Queue       exposing (..)
import DataStructs exposing (..)
import Update      exposing (update)
import Trampoline  exposing (..)
import Debug       exposing (watch)
import Util        exposing (removeFirst, splitOn)
import Random      exposing (int, Generator, Seed, initialSeed, generate)
import Array       exposing (fromList, get, Array)
import Maybe       exposing (withDefault)
import List        exposing ( (::)
                            , map
                            , sortWith
                            , foldl
                            , any
                            , take
                            , filter
                            , length
                            , sortBy
                            , reverse
                            , head
                            , drop
                            , map2
                            , sum
                            )

import Control.State exposing (..)
{--
PSEUDOCODE

1. Generate population
2. Score population
3. Select top 50
4. Permute top 50, generate 50 more
5. Iterate until we're happy

--}

moveArr : Array Command
moveArr = fromList [E,W,SW,SE,CW,CCW]

(>>=) : State s a -> (a -> State s b) -> State s b
(>>=) = bindS

genNum : Int -> Int -> State Seed Int
genNum lo hi = Control.State.get >>= (\ seed -> 
                          let (res, newSeed) = generate (int lo hi) seed
                          in  (put newSeed) >>= (\ _ -> returnS res))

genCommand : () -> State Seed Command
genCommand = \ () -> (genNum 0 5) >>= (\ index -> returnS <| withDefault E <| Array.get index moveArr)

beforeNextDrop : Command -> List Command -> Bool
beforeNextDrop cmd cmds =
  case cmds of
    []       -> False
    (SW::xs) -> False
    (SE::xs) -> False
    (P::xs) -> False
    (x::xs)  -> if x == cmd
                then True
                else beforeNextDrop cmd xs 

trashCompactor : List Command -> List Command -- for the detention Level
trashCompactor ls =
  case ls of
    []              -> []
    (CW::CCW::rest) -> trashCompactor rest
    (CCW::CW::rest) -> trashCompactor rest
    (W::rest)       -> if   beforeNextDrop E rest
                       then trashCompactor (removeFirst E rest)
                       else W :: trashCompactor rest
    (E::rest)       -> if   beforeNextDrop W rest
                       then trashCompactor (removeFirst W rest)
                       else E :: trashCompactor rest
    (x::xs)         -> x :: trashCompactor xs

runUntilGameOver : List Command -> Ei -> State Seed Ei
runUntilGameOver cmds ei =
  if (.isGameOver ei.model)
  then returnS ei
  else case cmds of
        []      -> generateEi ei.model
        (c::cs) -> runUntilGameOver cs {ei | model <- update c ei.model}

-- Let's make an egg!
generateEi : HexModel -> State Seed Ei 
generateEi model =
  if model.isGameOver
  then returnS <| Ei {model | history <- reverse <| trashCompactor <| reverse model.history}
  else genCommand() >>= (\ comm -> generateEi (update comm model))

genFolder : HexModel -> Int -> State Seed Eier -> State Seed Eier
genFolder init x stateEis =
  stateEis >>= (\ eis -> generateEi init >>= (\ ei -> returnS <| ei :: eis))

generatePopulace : HexModel -> Int -> State Seed Eier
generatePopulace init size = foldl (genFolder init) (returnS []) [1..size]

reproduce : HexModel -> Eier -> Int -> State Seed Eier
reproduce init eier popSize =
  let loop newEier newPopSize = 
        if newPopSize >= (popSize // 3)
        then returnS <| newEier
        else (selectSubPop init eier (popSize - 1) (popSize // 10)) >>=
               (\ subPop -> 
                  let bestTwo = take 2 <| sortByFitness subPop
                  in case bestTwo of
                       [ei1, ei2] -> (crossover init ei1 ei2) >>= 
                                       (\ ei -> loop (ei :: newEier) (newPopSize + 1))
                       _          -> loop newEier newPopSize) -- Should never happen
  in loop [] 0                       

crossover : HexModel -> Ei -> Ei -> State Seed Ei
crossover init ei1 ei2 =
  let e1H         = splitOn P <| (ei1.model).history
      e2H         = splitOn P <| (ei2.model).history
      e1pieces    = length e1H 
      e2pieces    = length e2H
  in (genNum 1 ((min e1pieces e2pieces) - 1)) >>=
       (\ takers -> 
          runUntilGameOver 
            (List.concat <| List.append (take takers e1H) (drop takers e2H)) 
            (Ei init))

selectSubPop : HexModel -> Eier -> Int -> Int -> State Seed Eier 
selectSubPop default pop popSize targetSize =
  let def = Ei default
      loop subPop sPopSize =
      if sPopSize >= targetSize
      then returnS subPop
      else (genNum 0 popSize) >>=
             (\ index -> 
                let newMember = withDefault def <| head <| take index pop
                in  loop (newMember :: subPop) (sPopSize + 1))
  in  loop [] 0

evolve : HexModel -> State Seed Eier -> State Seed Eier
evolve init stateEier =
  stateEier >>= 
    (\ eier ->
       let sortedPop = reverse <| sortByFitness eier
           popSize   = length sortedPop
           keepSize  = popSize // 4
           goodEggs  = take keepSize sortedPop
       in (reproduce init goodEggs popSize) >>=
            (\ hatchlings ->
               let hatchSize = length hatchlings
                   newSize   = keepSize + hatchSize
                   newPop    = take (popSize - newSize) sortedPop
               in returnS <| List.concat [newPop, hatchlings]))

--    let sortedPop        = reverse <| sortByFitness eier
--        popSize          = length sortedPop
--        keepSize         = popSize // 4
--        goodEggs         = take keepSize sortedPop
--        midEggs          = take (keepSize // 2) sortedPop
--        hatchlings       = reproduce init goodEggs
--        mutants          = irradiate init midEggs
--        hatchSize        = length hatchlings
--        mutantSize       = length mutants
--        newSize          = keepSize + hatchSize + mutantSize
--        (newPop, newGen) = (generatePopulace init gen (popSize - newSize))
--    in (List.concat [goodEggs, hatchlings, mutants, newPop], newGen))

sortByFitness : Eier -> Eier
sortByFitness = sortBy (.score << .model)


-- reproduce : HexModel -> Eier -> Eier
-- reproduce init eier = 
--   let popSize  = length eier
--       halfSize = popSize // 2
--       firstHalf = take halfSize eier
--       secondHalf = take halfSize <| drop halfSize eier
--   in map2 (crossover init) firstHalf secondHalf

-- mutate : HexModel -> Ei -> Ei
-- mutate init ei =
--   let eH         = length (ei.model).history
--       seed        = case ei.gen of (GenSet gen seed) -> seed
--       (takers,s)  = generate (int ((eH // 2) + (eH // 4)) (eH - 1)) seed
--   in runUntilGameOver
--        (filter ((/=)P) (take takers (ei.model).history))
--        (Ei init ei.gen)

-- irradiate : HexModel -> Eier -> Eier
-- irradiate init eier = map (mutate init) eier

findBest : Eier -> Ei -> Ei
findBest eis curBest = 
  case eis of
    []      -> curBest
    (e::es) -> findBest es (if e.model.score > curBest.model.score then e else curBest)

fastFoldl : (a -> b -> b) -> b -> List a -> b
fastFoldl f b aes = trampoline <| foldl' f b aes

foldl' : (a -> b -> b) -> b -> List a -> Trampoline b
foldl' f base ls =
  case ls of
    []      -> Done base
    (x::xs) -> Continue (\ () -> foldl' f (f x base) xs)

hatchDecentPlayer : HexModel -> Int -> (Ei, Int)
hatchDecentPlayer init size = 
  let evolver  = evolve init
      finalPop = evalState 
                   (fastFoldl (\ i eg -> evolver eg) (generatePopulace init size) [1..50])
                   (initialSeed 31415)
  in  (findBest finalPop (Ei init) , computeAverageScore finalPop)

computeAverageScore : Eier -> Int
computeAverageScore eier = (sum <| map (.score << .model) eier) // length eier

-- generateNextStates : HexModel -> List Command -> List (HexModel, List Command)
-- generateNextStates model history = 
--   map (\ x -> (update x model, x :: history)) <| 
--     filter (\ x -> not (isCycle x history)) [E,W,SW,SE,CW,CCW]
-- 
-- isCycle : Command -> List Command -> Bool
-- isCycle command history =
--   case command of
--     E   -> went W 10 history 
--     W   -> went E 10 history
--     CW  -> went CCW 10 history ||  (length (filter (\ x -> x == CW) history)  > 5) 
--     CCW -> went CW 10 history  ||  (length (filter (\ x -> x == CCW) history) > 5) 
--     _   -> False
-- 
-- went : a -> Int -> List a -> Bool
-- went a n = any (\ x -> x == a) << take n 
-- 
-- type Running a = Done a
--                | More a
-- 
-- modelInQueue : Queue (HexModel, List Command) -> (HexModel, List Command) -> Bool
-- modelInQueue (top, bottom) (model, comm) =
--   let cmp = \ (m, c) -> m == model
--   in  any cmp top || any cmp bottom
-- 
-- safelyEnqueue : Queue (HexModel, List Command) -> 
--                 List (HexModel, List Command) ->
--                 Queue (HexModel, List Command)
-- safelyEnqueue queue models =
--   case models of
--     []      -> queue
--     (m::ms) -> if modelInQueue queue m
--                then safelyEnqueue queue ms
--                else safelyEnqueue (push queue m) ms
-- 
-- bfStep : Queue (HexModel, List Command) -> 
--          (List Command, Int) -> 
--          Running (Queue (HexModel, List Command), (List Command, Int))
-- bfStep queue curBest =
--   if   isEmpty queue 
--   then Done (queue, curBest)
--   else let (nq, popped) = pop queue
--        in case popped of
--             Nothing                  -> Done (nq, curBest)
--             (Just (model, commList)) ->
--               if   model.isGameOver
--               then More (nq, curBest)
--               else let newStates = watch "newStates" <| generateNextStates model commList
--                        newScores : List (List Command, Int)
--                        newScores = computeNewScores newStates
-- --                 in  More ((enqueueAll nq newStates), (findBest curBest newScores))
--                    in  More ((safelyEnqueue nq newStates), (findBest curBest newScores))

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

-- findBest : (List Command, Int) -> List (List Command, Int) -> (List Command, Int)
-- findBest base ls = 
--   foldl 
--     (\ x res -> if (snd x) > (snd res) then x else res)
--     base
--     ls
-- 
-- computeNewScores : List (HexModel, List Command) -> List (List Command, Int)
-- computeNewScores updates = 
--   let f : (HexModel, List Command) -> (List Command, Int)
--       f m = case m of
--               (model, history) -> (history, model.score)
--   in map f updates
