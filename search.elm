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
import List        exposing ( (::) , map , sortWith , foldl
                            , any , take , filter , length
                            , sortBy , reverse , head , drop
                            , map2 , sum
                            )

{--
PSEUDOCODE

1. Generate population
2. Score population
3. Select top 50
4. Permute top 50, generate 50 more
5. Iterate until we're happy

--}

type alias Seeded a = (a, Seed)
withSeed : a -> Seed -> Seeded a
withSeed = (,)
 
moveArr : Array Command
moveArr = fromList [E,W,SW,SE,CW,CCW]

genNum : Int -> Int -> Seed -> Seeded Int
genNum lo hi seed = generate (int lo hi) seed

genCommand : Seed -> Seeded Command
genCommand seed = let (index, newSeed) = genNum 0 5 seed
                  in withSeed (withDefault E <| Array.get index moveArr) newSeed

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

runUntilGameOver : List Command -> Ei -> Seed -> Seeded Ei
runUntilGameOver cmds ei seed =
  if (.isGameOver ei.model)
  then withSeed ei seed
  else case cmds of
        []      -> generateEi ei.model seed
        (c::cs) -> runUntilGameOver cs {ei | model <- update c ei.model} seed

-- Let's make an egg!
generateEi : HexModel -> Seed -> Seeded Ei
generateEi model seed =
  if model.isGameOver
  then withSeed (Ei {model | history <- reverse <| trashCompactor <| reverse model.history}) seed
  else let (cmd, newSeed) = genCommand seed
       in  generateEi (update cmd model) newSeed

genFolder : HexModel -> Int -> Seeded Eier -> Seeded Eier
genFolder init x (eis, seed) =
  let (ei, newSeed) = generateEi init seed
  in  withSeed (ei :: eis) newSeed

generatePopulace : HexModel -> Int -> Seed -> Seeded Eier
generatePopulace init size seed = 
  foldl (genFolder init) (withSeed [] seed) [1..size]

reproduce : HexModel -> Eier -> Int -> Seed -> Seeded Eier
reproduce init eier popSize seed =
  let loop newEier newPopSize seed = 
        if newPopSize >= (popSize // 3)
        then withSeed newEier seed
        else let (subPop, newSeed) = selectSubPop init eier (popSize - 1) (popSize // 10) seed
                 bestTwo           = take 2 <| sortByFitness subPop
             in case bestTwo of
                [ei1, ei2] -> let (ei, newSeed2) = crossover init ei1 ei2 newSeed 
                              in  loop (ei :: newEier) (newPopSize + 1) newSeed2
                _          -> loop newEier newPopSize newSeed -- Should Never Happen
  in loop [] 0 seed

crossover : HexModel -> Ei -> Ei -> Seed -> Seeded Ei
crossover init ei1 ei2 seed =
  let e1H      = splitOn P <| (ei1.model).history
      e2H      = splitOn P <| (ei2.model).history
      e1pieces = length e1H 
      e2pieces = length e2H
      (takers, newSeed) = genNum 1 ((min e1pieces e2pieces) - 1) seed
  in runUntilGameOver 
       (List.concat <| List.append (take takers e1H) (drop takers e2H)) 
       (Ei init)
       newSeed

selectSubPop : HexModel -> Eier -> Int -> Int -> Seed -> Seeded Eier 
selectSubPop default pop popSize targetSize seed =
  let def = Ei default
      loop subPop sPopSize seed =
      if sPopSize >= targetSize
      then withSeed subPop seed
      else let (index, newSeed) = genNum 0 popSize seed
               newMember = withDefault def <| head <| take index pop
           in  loop (newMember :: subPop) (sPopSize + 1) newSeed
  in  loop [] 0 seed

evolve : HexModel -> Seeded Eier -> Seeded Eier
evolve init (eier, seed) =
  let sortedPop             = reverse <| sortByFitness eier
      popSize               = length sortedPop
      keepSize              = popSize // 4
      goodEggs              = take keepSize sortedPop
      (hatchlings, newSeed) = reproduce init goodEggs popSize seed
      hatchSize             = length hatchlings
      newSize               = keepSize + hatchSize
      newPop                = take (popSize - newSize) sortedPop
  in withSeed (List.concat [newPop, hatchlings]) newSeed

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
      finalPop = fst <| foldl 
                          (\ i eg -> evolver eg) 
                          (generatePopulace init size (initialSeed 31415))
                          [1..50]
  in  (findBest finalPop (Ei init) , computeAverageScore finalPop)

computeAverageScore : Eier -> Int
computeAverageScore eier = (sum <| map (.score << .model) eier) // length eier
