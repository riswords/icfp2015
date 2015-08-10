module Search where

import DataStructs exposing (..)
import Update      exposing (update, updateScore, lockUnit)
import Debug       exposing (watch)
import Util        exposing (removeFirst, splitOn, count, pruneDuplicates, isJust)
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
             Just Filled -> (j - 1)
             _           -> n)
        (model.height - 1)
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
      bump    = bumpHeuristic heights // (model.width * 2)
      lines   = countLiners model
      finish  = if model.isGameOver then -100000 else 0
  in (score * 2) + height + lines - bump + finish


------------------------------------------------------------------------------------------------------
beforeNextDrop : Command -> List Command -> Bool
beforeNextDrop cmd cmds =
  case cmds of
    []       -> False
    (SW::xs) -> False
    (SE::xs) -> False
    (P::xs)  -> False
    (x::xs)  -> if x == cmd
                then True
                else beforeNextDrop cmd xs

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
isValidTarget : Grid -> HexUnit -> HexCell -> (Int, Int) -> (Bool, (HexUnit, (Int, Int)))
isValidTarget grid unit cell target = 
  let (x,y)   = cellToOffset cell
      (tx,ty) = target
      offset  = (tx - x, ty - y)
      cellOff = offsetToCell offset
      newUnit = offsetUnit unit cellOff
  in (isUnitSafe grid newUnit, (newUnit, target))

returnValidTargets : Grid -> List (Int, Int) -> HexUnit -> List (HexUnit, (Int, Int))
returnValidTargets grid targets unit =
  let mapper cell target =
      let (x, y)   = cellToOffset cell
          (tx, ty) = target
          offset   = (x - tx, y - ty)
          cellOff  = offsetToCell offset
          newUnit  = offsetUnit unit cellOff
      in  (isUnitSafe grid newUnit, (unit , target))
  in map snd <| filter fst <| List.concat <| map (\ t -> map (\ c -> isValidTarget grid unit c t) unit.members) targets

targetHeuristic : HexModel -> (HexUnit, (Int, Int)) -> Int
targetHeuristic model (unit, target) =
  heuristic <| updateScore 0 unit <| lockUnit unit model

sortTargets : HexModel -> List (HexUnit, (Int, Int)) -> List (HexUnit, (Int, Int))
sortTargets model ls = List.reverse <| sortBy (targetHeuristic model) ls

------------------------------------------------------------------------------------------------------
runCommands : List Command -> HexModel -> HexModel
runCommands cmds model =
  if model.isGameOver
  then model
  else case cmds of
        []      -> model
        (c::cs) -> runCommands cs (update c model)

------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------
buildPathToTarget : HexModel -> (HexUnit, (Int, Int)) -> Maybe (List Command)
buildPathToTarget model target = 
  let newModel = { model | history <- [P] }
  in snd <| memoizedPathfinder newModel target ListDict.empty

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

stopPiece : HexModel -> Maybe (List Command)
stopPiece init = 
  foldl
    (\ cmd result ->
         if isJust result
         then result
         else let res = update cmd init 
              in if res.isGameOver || nextPiece res
              then Just [cmd]
              else Nothing)
    Nothing
    [E,W,SW,SE,CW,CCW]

------------------------------------------------------------------------------------------------------
type alias Memo = Dict (Grid, HexUnit) (Maybe (List Command))

memoizedPathfinder : HexModel -> (HexUnit, (Int, Int)) -> Memo -> (Memo, Maybe (List Command))
memoizedPathfinder model target memo =
  if | memberMemo model memo    -> (memo, lookupMemo model memo)
     | model.isGameOver         -> (insertMemo model Nothing memo,   Nothing)
     | piecePlaced model target -> let res = stopPiece model 
                                   in (insertMemo model res memo, res) 
     | nextPiece model          -> (insertMemo model Nothing memo,   Nothing)
     | otherwise                ->
       foldl
        (\ direction (memo, currentResult) -> 
           if   isJust currentResult
           then (memo, currentResult) 
           else let (newMemo, newPath) = memoizedPathfinder (update direction model) target memo
                in foldResult model newMemo newPath direction)
        (insertMemo model Nothing memo, Nothing)
        (pruneCommands model <| [SE,SW,E,W,CW,CCW])
--      (pruneCommands model <| getSmartCommandOrder model.unit (snd target))
--      [SE,SW,E,W,CW,CCW]

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

invertCommand : Command -> Maybe Command
invertCommand c =
  case c of
    E   -> Just W
    W   -> Just E
    CW  -> Just CCW
    CCW -> Just CW
    _   -> Nothing

pruneCommands : HexModel -> List Command -> List Command
pruneCommands model potentials = List.filter (not << hasDoneOppositeSinceDrop model.history) potentials

hasDoneOppositeSinceDrop : List Command -> Command -> Bool
hasDoneOppositeSinceDrop commands command =
  let invComm = invertCommand command
  in case invComm of
       Nothing -> False
       Just x  -> beforeNextDrop x commands

getSmartCommandOrder : HexUnit -> (Int, Int) -> List Command
getSmartCommandOrder unit target = 
    let approxLoc   = unit.location
        targetCell  = offsetToCell target
    in if | approxLoc.z == targetCell.z -> [E,  W,SE, SW, CW, CCW]
          | targetCell.x > approxLoc.x  -> [SW, E, W, SE, CW, CCW]
          | targetCell.y < approxLoc.y  -> [SE, W, E, SW, CW, CCW]
          | otherwise                   -> [SE,SW, E, W,  CW, CCW]

------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------
pickBestPath : List (List Command, HexModel) -> List Command
pickBestPath inputs =
  let loop ls curBest =
        case ls of 
          []      -> curBest
          (x::xs) -> let heurRes = heuristic <| snd x
                     in if heurRes > (snd curBest)
                        then loop xs (fst x, heurRes)
                        else loop xs curBest
  in fst <| loop inputs ([], 0)

------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------
bouncePick : HexModel ->
             List HexUnit -> 
             List (Int, Int) -> 
             List (HexUnit, (Int, Int)) -> 
             List Command
bouncePick init unitConfigs bottoms validTargets =
  case validTargets of
    []      -> []
    (t::ts) -> let path = buildPathToTarget init t
               in if isJust path
                  then withDefault [] <| path
                  else bouncePick init unitConfigs bottoms ts

bouncePickNextMove : HexModel -> List Command
bouncePickNextMove init =
  let unitConfigs  = pruneDuplicates <| generateUnitRots init.unit
      bottoms      = List.indexedMap (\ i n -> (i,n)) <| computeHeights init
      validTargets = sortTargets init <| List.concat <| List.map (returnValidTargets init.grid bottoms) unitConfigs
  in bouncePick init unitConfigs bottoms validTargets 

------------------------------------------------------------------------------------------------------
folder : HexModel -> (HexUnit, (Int, Int)) -> Maybe (List Command) -> Maybe (List Command)
folder init target bestTarget = 
  if isJust bestTarget
  then bestTarget
  else buildPathToTarget init target

pickNextMove : HexModel -> List Command
pickNextMove init =
  let unitConfigs  = pruneDuplicates <| generateUnitRots init.unit
      bottoms      = List.indexedMap (\ i n -> (i,n)) <| computeHeights init
      validTargets = sortTargets init <| List.concat <| List.map (returnValidTargets init.grid bottoms) unitConfigs
      path         = withDefault [] <| foldl (folder init) Nothing validTargets
  in path
