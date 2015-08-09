module UnitSearch where

import Queue       exposing (..)
import DataStructs exposing (..)
import Update      exposing (update)
import Trampoline  exposing (..)
import Debug       exposing (watch)
import Util        exposing (removeFirst, splitOn, count)
import Hex         exposing (getXYCell, cellToOffset, rotateGridlessUnit)
import Random      exposing (int, Generator, Seed, initialSeed, generate)
import Array       exposing (fromList, get, Array, indexedMap, empty)
import Maybe       exposing (withDefault)
import List        exposing ( (::) , map , foldl, foldr
                            , any , take , filter , length
                            , sortBy , reverse , head , drop
                            , map2 , sum
                            )

-----------------------------------------------------------------------------
-- Finding a landing spot and thinking about how to get there
computeHeights : HexModel -> List Int
computeHeights model =
  map 
    (\ i -> 
      foldr
        (\ j n -> 
          case (getXYCell i j model.grid) of
             Just Filled -> j
             _           -> n)
        model.height
        [0..(model.height-1)])
    [0..(model.width-1)]

countLiners : HexModel -> Int
countLiners model =
  let filledToNum cell =
        case cell of
          Filled -> 1
          Empty  -> 0
  in Util.maximum <| Array.map (Util.sum << Array.map filledToNum) model.grid

heightHeuristic : List Int -> Int
heightHeuristic = sum

bumpHeuristic : List Int -> Int
bumpHeuristic ls =
  case ls of
    []           -> 0
    [x]          -> 0
    (x::y::rest) -> abs (x - y) + bumpHeuristic (y::rest)

heuristic : Ei -> Int
heuristic {model} = 
  let score   = model.score
      heights = computeHeights model
      height  = heightHeuristic heights
      bump    = bumpHeuristic heights // 5
      lines   = 3 * countLiners model // 4
      finish  = if model.isGameOver then -10000 else 0
  in (score * score) + height + lines - bump + finish

-----------------------------------------------------------------------------
-- Generate a population of landing spots using the height heuristic
-- Genetic Algorithm

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
  let loop cmd cmds = 
        case cmds of
          []       -> Done False
          (SW::xs) -> Done False
          (SE::xs) -> Done False
          (P::xs)  -> Done False
          (x::xs)  -> if x == cmd
                      then Done True
                      else Continue (\ () -> loop cmd xs)
  in trampoline <| loop cmd cmds                        

trashCompactor : List Command -> List Command -- for the detention Level
trashCompactor ls = 
  let trashCompactorLoop ls acc =
        case ls of
          []              -> Done <| reverse acc
          (CW::CCW::rest) -> Continue (\ () -> trashCompactorLoop rest acc)
          (CCW::CW::rest) -> Continue (\ () -> trashCompactorLoop rest acc)
          (W::rest)       -> if   beforeNextDrop E rest
                             then Continue (\ () -> trashCompactorLoop (removeFirst E rest) acc)
                             else Continue (\ () -> trashCompactorLoop rest (W :: acc))
          (E::rest)       -> if   beforeNextDrop W rest
                             then Continue (\ () -> trashCompactorLoop (removeFirst W rest) acc)
                             else Continue (\ () -> trashCompactorLoop rest (E :: acc))
          (x::xs)         -> Continue (\ () -> trashCompactorLoop xs (x :: acc))
  in trampoline <| trashCompactorLoop ls []

dropOneHist : HexModel -> HexModel
dropOneHist model = {model | history <- drop 1 model.history}

runUntilNextPiece : List Command -> Ei -> Seed -> Seeded Ei
runUntilNextPiece cmds ei seed =
  let loop cmds ei seed =
    if count P (ei.model).history > 1
    then Done <| withSeed (Ei (dropOneHist ei.model)) seed
    else case cmds of
          []      -> Continue (\ () -> generateEiTramp ei.model seed)
          (c::cs) -> Continue (\ () -> loop cs {ei | model <- update c ei.model} seed)
  in trampoline <| loop cmds ei seed          

-- Let's make an egg!
generateEiTramp : HexModel -> Seed -> Trampoline (Seeded Ei)
generateEiTramp model seed =
  if count P model.history > 1
  then Done <| withSeed (Ei {model | history <- reverse <| trashCompactor <| reverse <| drop 1 model.history}) seed
  else let (cmd, newSeed) = genCommand seed
       in  Continue (\ () -> generateEiTramp (update cmd model) newSeed)

generateEi : HexModel -> Seed -> Seeded Ei
generateEi model seed = trampoline <| generateEiTramp model seed

genFolder : HexModel -> Int -> Seeded Eier -> Seeded Eier
genFolder init x (eis, seed) =
  let (ei, newSeed) = generateEi init seed
  in  withSeed (ei :: eis) newSeed

generatePopulace : HexModel -> Int -> Seed -> Seeded Eier
generatePopulace init size seed = 
  fastFoldl (genFolder init) (withSeed [] seed) [1..size]

crossover : HexModel -> Ei -> Ei -> Seed -> Seeded Ei
crossover init ei1 ei2 seed =
  let e1H      = (ei1.model).history
      e2H      = (ei2.model).history
      e1pieces = length e1H 
      e2pieces = length e2H
      (takers, newSeed) = genNum 1 ((min e1pieces e2pieces) - 1) seed
  in runUntilNextPiece 
       (List.append (take takers e1H) (drop takers e2H)) 
       (Ei init)
       newSeed

reproduce : HexModel -> Eier -> Int -> Seed -> Seeded Eier
reproduce init eier popSize seed =
  let pairs = pairOff eier
      repros = take (length pairs // 3) pairs
      loop ls acc seed = 
        case ls of
          []               -> Done <| withSeed acc seed
          [p]              -> Done <| withSeed acc seed
          ((p1, p2)::rest) -> Continue (\ () -> let (newEi, newSeed) = crossover init p1 p2 seed
                                                in loop rest (newEi :: acc) newSeed)
  in trampoline <| loop repros [] seed

pairOff : Eier -> List (Ei, Ei)
pairOff ls  = 
  let loop ls acc = 
    case ls of
      []   -> Done <| acc
      [ei] -> Done <| acc
      (ei1::ei2::rest) -> Continue (\ () -> loop rest ((ei1, ei2) :: acc))
  in List.reverse <| trampoline <| loop ls []      

-- reproduce : HexModel -> Eier -> Int -> Seed -> Seeded Eier
-- reproduce init eier popSize seed =
--   let loop newEier newPopSize seed = 
--         if newPopSize >= (popSize // 3)
--         then Done <| withSeed newEier seed
--         else let (subPop, newSeed) = selectSubPop init eier (popSize - 1) (popSize // 10) seed
--                  bestTwo           = take 2 <| sortByFitness subPop
--              in case bestTwo of
--                 [ei1, ei2] -> let (ei, newSeed2) = crossover init ei1 ei2 newSeed 
--                               in  Continue (\ () -> loop (ei :: newEier) (newPopSize + 1) newSeed2)
--                 _          -> Continue (\ () -> loop newEier newPopSize newSeed) -- Should Never Happen
--   in trampoline <| loop [] 0 seed
-- 
-- selectSubPop : HexModel -> Eier -> Int -> Int -> Seed -> Seeded Eier 
-- selectSubPop default pop popSize targetSize seed =
--   let def = Ei default
--       loop subPop sPopSize seed =
--       if sPopSize >= targetSize
--       then Done <| withSeed subPop seed
--       else let (index, newSeed) = genNum 0 popSize seed
--                newMember = withDefault def <| head <| take index pop
--            in  Continue (\ () -> loop (newMember :: subPop) (sPopSize + 1) newSeed)
--   in  trampoline <| loop [] 0 seed

mutate pop seed = fastFoldl irradiate ([], seed) pop

takeHalf : List a -> List a
takeHalf ls = take (length ls // 2) ls

irradiate {model} (res, seed) =
  let (mut, sd) = trampoline <| generateEiTramp 
                                  {model | history <- takeHalf model.history} 
                                  seed
  in (mut :: res, seed)                                    

evolve : HexModel -> Seeded Eier -> Seeded Eier
evolve init (eier, seed) =
  let sortedPop              = sortByFitness eier
      popSize                = length sortedPop
--    (hatchlings, newSeed)  = reproduce init sortedPop popSize seed
      (hatchlings, newSeed)  = ([], seed)
      hatchSize              = length hatchlings
      (mutants, newSeed2)    = mutate (take (popSize // 2) sortedPop) newSeed
      mutantSize             = length mutants
      (immigrants, newSeed3) = generatePopulace init (popSize // 2) newSeed2
      immigrantSize          = length immigrants
      newSize                = hatchSize + mutantSize + immigrantSize
      newPop                 = take (popSize - newSize) sortedPop
  in withSeed (List.concat [newPop, hatchlings, mutants, immigrants]) newSeed3
--    (immigrants, newSeed2) = generatePopulace init hatchSize newSeed
--    newSize                = hatchSize * 2
--    newPop                 = take (popSize - newSize) sortedPop
--in withSeed (List.concat [newPop, hatchlings, immigrants]) newSeed2

sortByFitness : Eier -> Eier
sortByFitness = List.reverse << sortBy heuristic

fastFoldl : (a -> b -> b) -> b -> List a -> b
fastFoldl f b aes = trampoline <| foldl' f b aes

foldl' : (a -> b -> b) -> b -> List a -> Trampoline b
foldl' f base ls =
  case ls of
    []      -> Done base
    (x::xs) -> Continue (\ () -> foldl' f (f x base) xs)

generateUnitRots : HexUnit -> List HexUnit
generateUnitRots unit =
  let loop unit n acc =
        case n of
          0 -> acc
          n -> loop (rotateGridlessUnit CW unit) (n - 1) <| unit :: acc
  in loop unit 6 []

returnValidTargets : Grid -> List (Int, Int) -> HexUnit -> List (HexUnit, (HexCell, (Int, Int)))
returnValidTargets grid targets unit =
  let mapper cell target =
      let (x, y)   = cellToOffset cell
          (tx, ty) = target
          offset   = {x - tx, y - ty}
          cellOff  = offsetToCell offset
          newUnit  = offsetUNit unit cellOff
      in  (isUnitSafe grid newUnit, (unit , (cell, target)))
  in map snd <| filter fst <| map (\ t -> map (\ c -> mapper c t) unit.members) targets

buildPathToTarget : HexModel -> (HexUnit, (HexCell, (Int, Int))) -> List Command
buildPathToTarget = []

-- runUntilNextPiece : List Command -> Ei -> Seed -> Seeded Ei
runCommands : List Command -> HexModel -> HexModel
runCommands cmds model =
  loop cmds model
    if model.isGameOver
    then Done model
    else case cmds of
          []      -> Done model
          (c::cs) -> Continue (\ () -> loop cs (update c model))
  in trampoline <| loop cmds model

pickNextMove : HexModel -> Int -> (HexModel, Int)
pickNextMove init size =
  let unitConfigs  = generateUnitRots init.unit
      bottoms      = List.indexedMap (\ i n -> (i,n)) <| computeHeights init
      validTargets = List.concat <| List.map (returnValidTargets init.grid bottoms) unitConfigs
      paths        = List.map2 buildPathToTarget init validTargets
      initSeed     = initialSeed 31415
      choices      = List.fastFoldl
                      (\ path results -> 
                         let finalConfig = runUntilNextPiece path (Ei init) )
                      []
                      paths
 
      evolver  = evolve init
      finalPop = sortByFitness <|
                  fst <| 
                    fastFoldl
                      (\ i eg -> evolver eg)
                      (generatePopulace init size (initialSeed 31415))
                      [1..100]
      res = withDefault init <| (head (map .model finalPop))
  in  (res, 100 * (heuristic <| Ei res))


-- pickNextMove : HexModel -> Int -> (HexModel, Int)
-- pickNextMove init size =
--   let evolver  = evolve init
--       finalPop = sortByFitness <|
--                   fst <| 
--                     fastFoldl
--                       (\ i eg -> evolver eg)
--                       (generatePopulace init size (initialSeed 31415))
--                       [1..100]
--       res = withDefault init <| (head (map .model finalPop))
--   in  (res, 100 * (heuristic <| Ei res))

computeAverageScore : Eier -> Int
computeAverageScore eier = (sum <| map (.score << .model) eier) // length eier
