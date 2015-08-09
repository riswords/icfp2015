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
                            , map2 , sum, all
                            )
import ListDict    exposing (Dict, member, get, insert, remove, empty)

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

isJust : Maybe a -> Bool
isJust v =
  case v of 
    Nothing  -> False
    (Just x) -> True

------------------------------------------------------------------------------------------------------
fastFoldl : (a -> b -> b) -> b -> List a -> b
fastFoldl f b aes = trampoline <| foldl' f b aes

foldl' : (a -> b -> b) -> b -> List a -> Trampoline b
foldl' f base ls =
  case ls of
    []      -> Done base
    (x::xs) -> Continue (\ () -> foldl' f (f x base) xs)

------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------
generateUnitRots : HexUnit -> List HexUnit
generateUnitRots unit =
  let loop unit n acc =
        case n of
          0 -> acc
          n -> loop (rotateGridlessUnit CW unit) (n - 1) <| unit :: acc
  in loop unit 6 []

------------------------------------------------------------------------------------------------------
returnValidTargets : Grid -> List (Int, Int) -> HexUnit -> List (HexUnit, (Int, Int))
returnValidTargets grid targets unit =
  let mapper cell target =
      let (x, y)   = cellToOffset cell
          (tx, ty) = target
          offset   = (x - tx, y - ty)
          cellOff  = offsetToCell offset
          newUnit  = offsetUnit unit cellOff
      in  (isUnitSafe grid newUnit, (unit , target))
  in map snd <| filter fst <| List.concat <| map (\ t -> map (\ c -> mapper c t) unit.members) targets

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
------------------------------------------------------------------------------------------------------
buildPathToTarget : HexModel -> (HexUnit, (Int, Int)) -> Maybe (List Command)
buildPathToTarget model target = snd <| memoizedPathfinder model target ListDict.empty

isCorrectRotation : HexUnit -> HexUnit -> Bool
isCorrectRotation unit1 unit2 =
  (foldl (&&) True (List.map2 (==) unit1.members unit2.members)) && (unit1.location == unit2.location) 

piecePlaced : HexModel -> (HexUnit, (Int, Int)) -> Bool
piecePlaced model (unit, target) = 
  (isCorrectRotation model.unit unit) && (any ((==)target << cellToOffset) model.unit.members)

nextPiece : HexModel -> Bool
nextPiece model = (count P model.history) > 1

hitTarget : HexModel -> (HexUnit, (Int, Int)) -> Bool
hitTarget model target = piecePlaced model target && nextPiece model

------------------------------------------------------------------------------------------------------
type alias Memo = Dict (Grid, HexUnit) (Maybe (List Command))

memoizedPathfinder : HexModel -> (HexUnit, (Int, Int)) -> Memo -> (Memo, Maybe (List Command))
memoizedPathfinder model target memo =
  if | memberMemo model memo  -> (memo, lookupMemo model memo)
     | model.isGameOver       -> (insertMemo model Nothing memo,   Nothing)
     | hitTarget model target -> (insertMemo model (Just []) memo, Just []) 
     | nextPiece model        -> (insertMemo model Nothing memo,   Nothing)
     | otherwise              ->
       fastFoldl
        (\ direction (memo, currentResult) -> 
           if   isJust currentResult
           then (memo, currentResult) 
           else let (newMemo, newPath) = memoizedPathfinder (update direction model) target memo
                in foldResult model newMemo newPath direction)
        (memo, Nothing)
        [E,W,SW,SE,CW,CCW]

foldResult : HexModel -> Memo -> Maybe (List Command) -> Command -> (Memo, Maybe (List Command))
foldResult model memo path command =
  case path of
    Nothing -> (insertMemo model Nothing memo, Nothing)
    Just ls -> (insertMemo model (Just (command :: ls)) memo, Just (command :: ls))

insertMemo : HexModel -> (Maybe (List Command)) -> Memo -> Memo
insertMemo model val memo = insert (model.grid, model.unit) val memo

lookupMemo : HexModel -> Memo -> Maybe (List Command)
lookupMemo model memo = withDefault Nothing <| ListDict.get (model.grid, model.unit) memo

memberMemo : HexModel -> Memo -> Bool
memberMemo model memo = member (model.grid, model.unit) memo

------------------------------------------------------------------------------------------------------
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
pickNextMove : HexModel -> List Command
pickNextMove init =
  let unitConfigs  = generateUnitRots init.unit
      bottoms      = List.indexedMap (\ i n -> (i,n)) <| computeHeights init
      validTargets = List.concat <| List.map (returnValidTargets init.grid bottoms) unitConfigs
      paths        = map (withDefault []) <|
                       filter isJust <|
                         List.map2 buildPathToTarget 
                         (List.repeat (length validTargets) init) 
                         validTargets
      choices      = fastFoldl
                       (\ path results -> (path, runCommands path init) :: results)
                       []
                       paths
  in  pickBestPath choices 
