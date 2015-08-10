module Engine where

import List        exposing (map, take, head, (::))
import Array       exposing (get)
import DataStructs exposing (..)
import Maybe       exposing (..)
import Update      exposing (update)
import Util        exposing (bounce, withDoneValue, isDone)
import Time        exposing (..)
import Search      exposing (..)

updateTime : Time -> GameState -> GameState
updateTime time state = 
  case state of
    GameOver i m           -> GameOver      {i | lastTime <- inSeconds time} m
    ComputingMove i m tram -> ComputingMove {i | lastTime <- inSeconds time} m tram
    RunningGame i m cmds   -> RunningGame   {i | lastTime <- inSeconds time} m cmds  

setTimeLimit : Int -> GameState -> GameState
setTimeLimit time state = 
  case state of
    GameOver i m           -> GameOver      {i | timeLimit <- toFloat time} m
    ComputingMove i m tram -> ComputingMove {i | timeLimit <- toFloat time} m tram
    RunningGame i m cmds   -> RunningGame   {i | timeLimit <- toFloat time} m cmds  



getInputInfo : GameState -> InputInfo
getInputInfo state = 
  case state of
    GameOver i m           -> i
    ComputingMove i m tram -> i
    RunningGame i m cmds   -> i

getModelFromState : GameState -> HexModel
getModelFromState state =
  case state of
    GameOver i m        -> m
    ComputingMove i m t -> m
    RunningGame i m c   -> m

outOfTime : GameState -> Bool
outOfTime state =
  let gameInfo = getInputInfo state
  in (gameInfo.lastTime - gameInfo.startTime + 2) > gameInfo.timeLimit

toGameOver : GameState -> GameState
toGameOver state = 
  case state of
    GameOver i m           -> state
    ComputingMove i m tram -> GameOver i m
    RunningGame i m cmds   -> GameOver i m

updateGame : GameState -> GameState
updateGame state =
  if outOfTime state
  then toGameOver state 
  else case state of
        GameOver i m           -> state
        ComputingMove i m tram -> if isDone tram
                                  then let newCmds = withDoneValue [] tram
                                           nextCmds  = List.filter ((/=)P) newCmds
                                       in RunningGame i m nextCmds
                                  else ComputingMove i m (bounce tram)     
        RunningGame i m cmds   -> 
          if m.isGameOver
          then GameOver i m
          else case cmds of
                 []      -> ComputingMove i m (bouncePickNextMove m)
                 (c::cs) -> RunningGame i (update c m) cs

