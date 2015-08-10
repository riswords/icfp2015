module Queue where

import List exposing (..)

type alias Queue a = (List a, List a)

empty : Queue a
empty = ([], [])

isEmpty : Queue a -> Bool
isEmpty q =
  case q of
    ([], []) -> True
    _        -> False

push : Queue a -> a -> Queue a
push (top, bot) x = (top, x::bot)

pop : Queue a -> (Queue a, Maybe a)
pop q = 
  case q of
    ([], [])    -> (q, Nothing)
    ([], xs)    -> pop (reverse xs, [])
    (x::xs, ys) -> ((xs, ys), Just x)

peek : Queue a -> Maybe a
peek q =
  case q of
    ([], [])    -> Nothing
    ([], xs)    -> peek (reverse xs, [])
    (x::xs, ys) -> (Just x)

enqueueAll : Queue a -> List a -> Queue a
enqueueAll (xs, ys) ls = (xs, foldl (\ x l -> x::l) ys ls)
