module UnitSearch where

import Queue       exposing (..)
import DataStructs exposing (..)
import Update      exposing (update)
import Trampoline  exposing (..)
import Debug       exposing (watch)
import Util        exposing (removeFirst, splitOn, count)
import Hex         exposing (getXYCell, cellToOffset, rotateGridlessUnit
                            , isUnitSafe, offsetUnit, offsetToCell
                            )
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

heuristic : HexModel -> Int
heuristic model = 
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


------------------------------------------------------------------------------------------------------
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

------------------------------------------------------------------------------------------------------
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

------------------------------------------------------------------------------------------------------
fastFoldl : (a -> b -> b) -> b -> List a -> b
fastFoldl f b aes = trampoline <| foldl' f b aes

foldl' : (a -> b -> b) -> b -> List a -> Trampoline b
foldl' f base ls =
  case ls of
    []      -> Done base
    (x::xs) -> Continue (\ () -> foldl' f (f x base) xs)

------------------------------------------------------------------------------------------------------
generateUnitRots : HexUnit -> List HexUnit
generateUnitRots unit =
  let loop unit n acc =
        case n of
          0 -> acc
          n -> loop (rotateGridlessUnit CW unit) (n - 1) <| unit :: acc
  in loop unit 6 []

------------------------------------------------------------------------------------------------------
returnValidTargets : Grid -> List (Int, Int) -> HexUnit -> List (HexUnit, (HexCell, (Int, Int)))
returnValidTargets grid targets unit =
  let mapper cell target =
      let (x, y)   = cellToOffset cell
          (tx, ty) = target
          offset   = (x - tx, y - ty)
          cellOff  = offsetToCell offset
          newUnit  = offsetUnit unit cellOff
      in  (isUnitSafe grid newUnit, (unit , (cell, target)))
  in map snd <| filter fst <| List.concat <| map (\ t -> map (\ c -> mapper c t) unit.members) targets

buildPathToTarget : HexModel -> (HexUnit, (HexCell, (Int, Int))) -> List Command
buildPathToTarget model (unit, (cell, loc)) = []

------------------------------------------------------------------------------------------------------
runCommands : List Command -> HexModel -> HexModel
runCommands cmds model =
  let loop cmds model =
        if model.isGameOver
        then Done model
        else case cmds of
              []      -> Done model
              (c::cs) -> Continue (\ () -> loop cs (update c model))
  in trampoline <| loop cmds model

------------------------------------------------------------------------------------------------------
pickBestPath : List (List Command, HexModel) -> List Command
pickBestPath inputs =
  let loop ls curBest =
        case ls of 
          []      -> Done curBest
          (x::xs) -> let heurRes = heuristic <| snd x
                     in if heurRes > (snd curBest)
                        then Continue (\ () -> loop xs (fst x, heurRes))
                        else Continue (\ () -> loop xs curBest)
  in fst <| trampoline <| loop inputs ([], 0)

------------------------------------------------------------------------------------------------------
pickNextMove : HexModel -> Int -> List Command
pickNextMove init size =
  let unitConfigs  = generateUnitRots init.unit
      bottoms      = List.indexedMap (\ i n -> (i,n)) <| computeHeights init
      validTargets = List.concat <| List.map (returnValidTargets init.grid bottoms) unitConfigs
      paths        = List.map2 buildPathToTarget (List.repeat (length validTargets) init) validTargets
      choices      = fastFoldl
                       (\ path results -> (path, runCommands path init) :: results)
                       []
                       paths
  in  pickBestPath choices 
