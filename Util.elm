module Util where

import DataStructs exposing (..)
import List        exposing (head, drop, indexedMap, (::), reverse, member)
import Array       exposing (Array, foldl, repeat)
import Color       exposing (rgba, Color)

------------------------------------------------------------------
-- Hayup
isJust : Maybe a -> Bool
isJust v =
  case v of 
    Nothing  -> False
    (Just x) -> True

------------------------------------------------------------------
-- Some list helpers
splitOn : a -> List a -> List (List a)
splitOn a ls =
  let helper a ls seen =
        case ls of
          []      -> [reverse seen]
          (x::xs) -> if x == a
                     then reverse seen :: helper a xs []      
                     else helper a xs (x::seen)
  in helper a ls []

removeFirst : a -> List a -> List a
removeFirst a ls =
  case ls of
    []      -> []
    (x::xs) -> if x == a
               then xs
               else x :: removeFirst a xs

count : a -> List a -> Int
count a ls =
  case ls of
    []      -> 0
    (x::xs) -> if a == x then count a xs + 1 else count a xs

pruneDuplicates : List a -> List a
pruneDuplicates ls =
  case ls of
    []      -> []
    (x::xs) -> if member x xs
               then pruneDuplicates xs
               else (x :: pruneDuplicates xs)

------------------------------------------------------------------
-- Array helpers

all : (a -> Bool) -> Array a -> Bool
all pred = foldl (\ x res -> res && pred x) True

any : (a -> Bool) -> Array a -> Bool
any pred = foldl (\ x res -> res || pred x) False

minimum : Array Int -> Int
minimum = foldl min 100000000

maximum : Array Int -> Int
maximum = foldl max -100000000

sum : Array Int -> Int
sum = foldl (+) 0

toArray : a -> Array a
toArray = repeat 1

------------------------------------------------------------------
-- A few colors
clearGrey : Color -- Empty Hexes
clearGrey = rgba 160 160 160 1.0
yeller : Color -- Filled hexes
yeller = rgba 230 184 0 1.0
uniter : Color -- Unit hexes
uniter = rgba 46 184 230 1.0
unitCenter : Color -- Center dot
unitCenter = rgba 25 117 209 1.0
messageColor : Color -- Center dot
messageColor = rgba 100 100 100 0.6

------------------------------------------------------------------
