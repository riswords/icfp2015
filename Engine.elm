module Engine where

import List        exposing (map, take, head, (::))
import Array       exposing (get)
import DataStructs exposing (..)
import Maybe       exposing (..)
import Update      exposing (update)
import Time        exposing (..)
import Search      exposing (..)

updateTime : Time -> GameState -> GameState
updateTime time state = 
  case state of
    GameOver i m           -> GameOver      {i | lastTime <- inSeconds time} m
    ComputingMove i m cmds -> ComputingMove {i | lastTime <- inSeconds time} m cmds
    RunningGame i m cmds   -> RunningGame   {i | lastTime <- inSeconds time} m cmds  

setTimeLimit : Int -> GameState -> GameState
setTimeLimit time state = 
  case state of
    GameOver i m           -> GameOver      {i | timeLimit <- toFloat time} m
    ComputingMove i m cmds -> ComputingMove {i | timeLimit <- toFloat time} m cmds
    RunningGame i m cmds   -> RunningGame   {i | timeLimit <- toFloat time} m cmds  



getInputInfo : GameState -> InputInfo
getInputInfo state = 
  case state of
    GameOver i m           -> i
    ComputingMove i m cmds -> i
    RunningGame i m cmds   -> i

getModelFromState : GameState -> HexModel
getModelFromState state =
  case state of
    GameOver i m        -> m
    ComputingMove i m c -> m
    RunningGame i m c   -> m

outOfTime : GameState -> Bool
outOfTime state =
  let gameInfo = getInputInfo state
  in (gameInfo.lastTime - gameInfo.startTime + 2) > gameInfo.timeLimit

toGameOver : GameState -> GameState
toGameOver state = 
  case state of
    GameOver i m           -> state
    ComputingMove i m cmds -> GameOver i m
    RunningGame i m cmds   -> GameOver i m

updateGame : GameState -> GameState
updateGame state =
  if outOfTime state
  then toGameOver state 
  else case state of
        GameOver i m           -> state
        ComputingMove i m cmds -> let nextCmds  = List.filter ((/=)P) cmds
                                  in RunningGame i m nextCmds
        RunningGame i m cmds   -> 
          if m.isGameOver
          then GameOver i m
          else case cmds of
                 []      -> ComputingMove i m (bouncePickNextMove m)
                 (c::cs) -> RunningGame i (update c m) cs

