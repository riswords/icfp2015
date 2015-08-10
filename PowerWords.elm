module PowerWords where


import DataStructs exposing (..)
import Dict exposing (insert, Dict, empty, get)
import String
import List exposing ((::), length, foldl, drop, take, indexedMap, sortWith, map)
import Char
import Maybe exposing (Maybe)


commandsToPowerWords : List Command -> List String -> String
commandsToPowerWords commands powerWords = 
    let dict             = charCommandDict
        powerWordCmdSeqs = buildPowerWordSeqs dict powerWords
        sortedPwdCmdSeqs = sortWith revStrLenSorter powerWordCmdSeqs
        cmdTails         = indexedMap (\i _ -> drop i commands) commands

        -- takes commands, tries each power word at beginning, matches longest
        -- returns (matched power word, remaining commands)
        matchPwd : List Command -> (Maybe String, List Command) -> (Maybe String, List Command)
        matchPwd cmdTail (res, tails) = 
            let folder : (String, List Command) -> (Maybe String, List Command) -> (Maybe String, List Command)
                folder (pwd, pwdSeq) prevRes =
                    case prevRes of
                        (Just x, _) -> prevRes
                        (Nothing, remTail) -> if (take (length pwdSeq) remTail) == pwdSeq
                                              then (Just pwd, drop (length pwdSeq) remTail)
                                              else (Nothing, remTail)
            in foldl folder (res, tails) sortedPwdCmdSeqs
    in fst <| foldl
                (\cmd (output, remCmds) ->
                    case remCmds of
                        []        -> (output, [])
                        (c :: cs) -> let (pwdStr, remCmdTail) = matchPwd remCmds (Nothing, remCmds)
                                     in case pwdStr of
                                        Nothing  -> (output ++ (String.fromChar (pickFirstOp c)), cs)
                                        Just pwd -> (output ++ pwd, remCmdTail)
                )
                ("", commands)
                commands

         

revStrLenSorter : (String, List Command) -> (String, List Command) -> Order
revStrLenSorter (s1, cmd1) (s2, ls2) = compare (String.length s2) (String.length s1)

buildPowerWordSeqs : Dict Char Command -> List String -> List (String, List Command)
buildPowerWordSeqs dict powerWords = 
    let mapper : String -> (String, List Command)
        mapper powerWord = String.foldr 
                                (\pwChar (pwd, cmds) -> 
                                    case get pwChar dict of
                                        Just cmd -> (powerWord, cmd :: cmds)
                                        Nothing  -> (powerWord, [])
                                )
                                (powerWord, [])
                                powerWord
    in map mapper powerWords

pickFirstOp : Command -> Char
pickFirstOp op = 
  case op of
    W   -> 'p' 
    E   -> 'b'
    SW  -> 'a'
    SE  -> 'l'
    CW  -> 'd'
    CCW -> 'k'

charCommandDict : Dict Char Command
charCommandDict = buildCharCommandDict 
    [ (W,   ['p', '\'', '!', '.', '0', '3'])
    , (E,   ['b', 'c', 'e', 'f', 'y', '2'])
    , (SW,  ['a', 'g', 'h', 'i', 'j', '4'])
    , (SE,  ['l', 'm', 'n', 'o', ' ', '5'])
    , (CW,  ['d', 'q', 'r', 'v', 'z', '1'])
    , (CCW, ['k', 's', 't', 'u', 'w', 'x'])
    ]

buildCharCommandDict : List (Command, List Char) -> Dict Char Command
buildCharCommandDict commandCharPairs = 
    foldl 
        (\ (cmd, chars) dict ->
            foldl
                (\char dict -> insert char cmd dict)
                dict
                chars
        )
        empty
        commandCharPairs
