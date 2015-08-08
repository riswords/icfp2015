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

genCommand : GenSet -> (Command, GenSet)
genCommand (GenSet gen seed) = 
  let (index, newSeed) = generate gen seed
  in  (withDefault E <| get index moveArr, GenSet gen newSeed)

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

runUntilGameOver : List Command -> Ei -> Ei
runUntilGameOver cmds ei =
  if (.isGameOver ei.model)
  then ei
  else case cmds of
        []      -> generateEi ei.model ei.gen
        (c::cs) -> runUntilGameOver cs {ei | model <- update c ei.model}

-- Let's make an egg!
generateEi : HexModel -> GenSet -> Ei 
generateEi model gen =
  if model.isGameOver
  then Ei {model | history <- reverse <| trashCompactor <| reverse model.history} gen
  else let (command, newGen) = genCommand gen
       in  generateEi (update command model) newGen

genFolder : HexModel -> Int -> (Eier, GenSet) -> (Eier, GenSet)
genFolder init x (eis, gen) = 
  let ei = generateEi init gen
  in (ei :: eis, ei.gen)

generatePopulace : HexModel -> GenSet -> Int -> (Eier, GenSet)
generatePopulace init gen size = foldl (genFolder init) ([], gen) [1..size]

sortByFitness : Eier -> Eier
sortByFitness = sortBy (.score << .model)

crossover : HexModel -> Ei -> Ei -> Ei
crossover init ei1 ei2 =
  let e1H         = splitOn P <| (ei1.model).history
      e2H         = splitOn P <| (ei2.model).history
      e1pieces    = length e1H 
      e2pieces    = length e2H
      seed        = case ei2.gen of (GenSet gen seed) -> seed
      (takers,s)  = generate (int 1 ((min e1pieces e2pieces) - 1)) seed
  in runUntilGameOver
       (List.concat (List.append (take takers e1H) (drop takers e2H )))
       (Ei init ei2.gen)

reproduce : HexModel -> Eier -> Eier
reproduce init eier = 
  let popSize  = length eier
      halfSize = popSize // 2
      firstHalf = take halfSize eier
      secondHalf = take halfSize <| drop halfSize eier
  in map2 (crossover init) firstHalf secondHalf

evolve : HexModel -> (Eier, GenSet) -> (Eier, GenSet)
evolve init (eier, gen) = 
    let sortedPop        = reverse <| sortByFitness eier
        popSize          = length sortedPop
        keepSize         = popSize // 4
        goodEggs         = take keepSize sortedPop
        hatchlings       = reproduce init goodEggs
        hatchSize        = length hatchlings
        (newPop, newGen) = (generatePopulace init gen (popSize - (keepSize + hatchSize)))
    in (List.concat [goodEggs, hatchlings, newPop], newGen)

findBest : Eier -> Ei -> Ei
findBest eis curBest = 
  case eis of
    []      -> curBest
    (e::es) -> findBest es (if e.model.score > curBest.model.score then e else curBest)

hatchDecentPlayer : HexModel -> Int -> (Ei, Int)
hatchDecentPlayer init size = 
  let gen      = GenSet (int 0 5) (initialSeed 31415)
      evolver  = evolve init
      finalPop = fst (foldl (\ i eg -> evolver eg) (generatePopulace init gen size) [1..100])
  in  (findBest finalPop (Ei init gen) , computeAverageScore finalPop)

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
